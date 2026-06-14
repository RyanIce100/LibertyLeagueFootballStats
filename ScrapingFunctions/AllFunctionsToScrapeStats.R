# =============================================================================
# R Script: ScrapeStatsNoRoster.R
# Scrapes cumulative football stats + roster (Pos, Yr) from Sidearm Sports HTML
# =============================================================================

library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# -----------------------------------------------------------------------------
# Helper: split "Made-Att" compound stat into two numeric vectors
# e.g. "41-41" → v1=41, v2=41;  "0-0" → v1=0, v2=0
# -----------------------------------------------------------------------------
parse_hyphen_stat <- function(vec) {
  vec <- as.character(vec)
  vec[is.na(vec) | vec == "" | vec == "-"] <- "0-0"
  vec[!grepl("-", vec)] <- paste0(vec[!grepl("-", vec)], "-0")
  parts <- str_split_fixed(vec, "-", 2)
  list(
    v1 = suppressWarnings(as.numeric(parts[, 1])),
    v2 = suppressWarnings(as.numeric(parts[, 2]))
  )
}

# -----------------------------------------------------------------------------
# Helper: read a stats table by section ID; clean player name; drop non-players
# Returns NULL when section / table is absent or empty
# -----------------------------------------------------------------------------
read_section_table <- function(html, section_id) {
  sec <- html %>% html_element(sprintf("#%s", section_id))
  if (is.na(sec)) return(NULL)
  
  tbl_node <- sec %>% html_element("table")
  if (is.na(tbl_node)) return(NULL)
  
  df <- tryCatch(html_table(tbl_node, fill = TRUE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (is.list(df) && !is.data.frame(df)) df <- df[[1]]
  
  colnames(df) <- make.unique(str_trim(colnames(df)))
  
  p_col <- grep("^Player$", colnames(df), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(p_col)) return(NULL)
  
  df %>%
    filter(!is.na(.data[[p_col]]), .data[[p_col]] != "") %>%
    mutate(
      Player = str_remove(.data[[p_col]], "^[0-9]+") %>%   # strip leading jersey #
        str_remove("[0-9]+.*$") %>%                         # strip trailing "##Name" repeat
        str_trim()
    ) %>%
    # Drop team/opponent aggregate rows
    filter(!grepl("^(TM|Team|Totals?|Opponents?)$", Player, ignore.case = TRUE))
}

# -----------------------------------------------------------------------------
# Roster scraper: returns tibble(Player, Pos, Yr)
# Player name is formatted "Last, First" to match stats tables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Updated Roster scraper: handles newer and older Sidearm Sports layouts
# Returns tibble(Player, Pos, Yr) with Player formatted as "Last, First"
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Updated Roster scraper: Fixed to support new Sidearm card elements
# Handles responsive duplicates and correctly builds "Last, First" format
# -----------------------------------------------------------------------------
scrape_roster <- function(roster_path) {
  html  <- read_html(roster_path)
  
  # Target modern Sidearm player items, fall back to older grid templates
  items <- html %>% html_elements("li.sidearm-roster-player")
  if (length(items) == 0) {
    items <- html %>% html_elements("ul.sidearm-list-card-template-1 li.sidearm-list-card-item")
  }
  
  # Return an empty structured tibble cleanly if nothing is found to prevent join breakage
  if (length(items) == 0) {
    return(tibble(Player = character(), Pos = character(), Yr = character()))
  }
  
  map_dfr(items, function(item) {
    # Check for legacy broken-out name strings
    fname <- item %>% html_element(".sidearm-roster-player-first-name") %>% html_text(trim = TRUE)
    lname <- item %>% html_element(".sidearm-roster-player-last-name")  %>% html_text(trim = TRUE)
    
    if (!is.na(fname) && !is.na(lname) && fname != "" && lname != "") {
      player_formatted <- paste0(lname, ", ", fname)
    } else {
      # Modern Layout: name is in the <a> inside an <h3> inside the name div
      fullname <- item %>% html_element(".sidearm-roster-player-name h3 a") %>% html_text(trim = TRUE)
      if (is.na(fullname) || fullname == "") {
        # Fallback: pull aria-label from the image link (e.g. "Abdoulaye Diallo - View Full Bio")
        fullname <- item %>% html_element(".sidearm-roster-player-image a") %>%
          html_attr("aria-label") %>%
          str_remove("\\s*-\\s*View.*$")
      }
      if (is.na(fullname) || fullname == "") return(NULL)
      
      # Clean up whitespace and line-breaks
      fullname <- str_replace_all(fullname, "\\s+", " ") %>% str_trim()
      
      # Convert "First Last" -> "Last, First"
      parts <- str_split(fullname, " ")[[1]]
      if (length(parts) >= 2) {
        last_name  <- parts[length(parts)]
        first_name <- paste(parts[1:(length(parts) - 1)], collapse = " ")
        player_formatted <- paste0(last_name, ", ", first_name)
      } else {
        player_formatted <- fullname
      }
    }
    
    # ── Position ────────────────────────────────────────────────────────────────
    # Layout A — abbreviation span (e.g. "CB") present in long/short format (2019 Union)
    pos <- item %>%
      html_element(".sidearm-roster-player-position-long-short.hide-on-medium") %>%
      html_text(trim = TRUE)
    
    # Layout B — 2022 card format: <div class="sidearm-roster-player-position-short">
    if (is.na(pos) || pos == "") {
      pos <- item %>% html_element(".sidearm-roster-player-position-short") %>%
        html_text(trim = TRUE)
    }
    
    # Layout C — 2019 HWS format: plain <span class="text-bold"> with no long/short children
    if (is.na(pos) || pos == "") {
      pos <- item %>%
        html_element(".sidearm-roster-player-position span.text-bold") %>%
        html_text(trim = TRUE)
    }
    
    # Layout D — last resort: generic position div, strip height/weight noise
    if (is.na(pos) || pos == "") {
      raw <- item %>% html_element(".sidearm-roster-player-position") %>% html_text(trim = TRUE)
      pos <- str_extract(raw, "^[^\\d'\"]+") %>% str_trim()
    }
    
    # ── Academic Year ────────────────────────────────────────────────────────────
    # Two copies exist in this format:
    #   • .hide-on-large  -> abbreviated (e.g. "FY") — shown on mobile
    #   • no hide class   -> full text  (e.g. "First-Year") — inside hide-on-medium-down div
    # Prefer the full-text version (no hide-on-large class).
    yr <- item %>%
      html_element(".sidearm-roster-player-academic-year:not(.hide-on-large)") %>%
      html_text(trim = TRUE)
    # Fallback: abbreviated version or legacy class name
    if (is.na(yr) || yr == "") {
      yr <- item %>%
        html_element(".sidearm-roster-player-academic-year, .sidearm-roster-player-class-year") %>%
        html_text(trim = TRUE)
    }
    
    # Clean label prefixes that some layouts prepend
    pos <- str_remove(pos, "^Position:\\s*")    %>% str_replace_all("\\s+", " ") %>% str_trim()
    yr  <- str_remove(yr,  "^Academic Year:\\s*|^Class:\\s*") %>%
      str_replace_all("\\s+", " ") %>% str_trim()
    
    tibble(
      Player = player_formatted,
      Pos    = if (is.na(pos) || pos == "") NA_character_ else pos,
      Yr     = if (is.na(yr)  || yr  == "") NA_character_ else yr
    )
  }) %>%
    distinct(Player, .keep_all = TRUE)
}
# -----------------------------------------------------------------------------
# Sub-table parsers
# -----------------------------------------------------------------------------

parse_rushing <- function(html) {
  df <- read_section_table(html, "individual-offense-rushing")
  if (is.null(df)) return(tibble())
  # Columns: #, Player, GP, ATT, Gain, Loss, Net, AVG, TD, Long, AVG/G
  tibble(
    Player      = df$Player,
    GP          = suppressWarnings(as.numeric(df$GP)),
    Rush.Att    = suppressWarnings(as.numeric(df$ATT)),
    Rush.Yds.Gn = suppressWarnings(as.numeric(df$Gain)),
    Rush.Yds.Ls = suppressWarnings(as.numeric(df$Loss)),
    Rush.Yds    = suppressWarnings(as.numeric(df$Net)),
    Rush.TD     = suppressWarnings(as.numeric(df$TD))
  )
}

parse_passing <- function(html) {
  df <- read_section_table(html, "individual-offense-passing")
  if (is.null(df)) return(tibble())
  # Columns: #, Player, GP, Rating, COMP, ATT, INT, %, YDS, TD, Long, AVG/G
  tibble(
    Player     = df$Player,
    GP         = suppressWarnings(as.numeric(df$GP)),
    Pass.Comp  = suppressWarnings(as.numeric(df$COMP)),
    Pass.Att   = suppressWarnings(as.numeric(df$ATT)),
    Int.Thrown = suppressWarnings(as.numeric(df$INT)),
    Pass.Yds   = suppressWarnings(as.numeric(df$YDS)),
    Pass.TD    = suppressWarnings(as.numeric(df$TD))
  )
}

parse_receiving <- function(html) {
  df <- read_section_table(html, "individual-offense-receiving")
  if (is.null(df)) return(tibble())
  # Columns: #, Player, GP, NO, YDS, AVG, TD, Long, AVG/G
  tibble(
    Player  = df$Player,
    GP      = suppressWarnings(as.numeric(df$GP)),
    Rec     = suppressWarnings(as.numeric(df$NO)),
    Rec.Yds = suppressWarnings(as.numeric(df$YDS)),
    Rec.TD  = suppressWarnings(as.numeric(df$TD))
  )
}

parse_defense <- function(html) {
  df <- read_section_table(html, "individual-defense")
  if (is.null(df)) return(tibble())
  # Columns: #, Player, GP, Solo, ASST, TOT, TFL-YDS, Sacks-YDS, INT, BU, QBH, FR, FF, KICK, SAF
  
  res <- tibble(
    Player       = df$Player,
    GP           = suppressWarnings(as.numeric(df$GP)),
    Solo.Tackles = suppressWarnings(as.numeric(df$Solo)),
    Asst.Tackles = suppressWarnings(as.numeric(df$ASST)),
    Int          = suppressWarnings(as.numeric(df$INT)),
    PBU          = suppressWarnings(as.numeric(df$BU)),
    FR           = suppressWarnings(as.numeric(df$FR)),
    FF           = suppressWarnings(as.numeric(df$FF)),
    Blocks       = suppressWarnings(as.numeric(df$KICK))
  )
  
  tfl_col <- grep("^TFL", colnames(df), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(tfl_col)) {
    s <- parse_hyphen_stat(df[[tfl_col]])
    res$TFL     <- s$v1
    res$TFL.Yds <- s$v2
  }
  
  sack_col <- grep("^Sacks", colnames(df), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(sack_col)) {
    s <- parse_hyphen_stat(df[[sack_col]])
    res$Sack     <- s$v1
    res$Sack.Yds <- s$v2
  }
  
  res
}

parse_punting <- function(html) {
  # Section "kicking" is Punting on Sidearm RPI pages
  df <- read_section_table(html, "kicking")
  if (is.null(df)) return(tibble())
  # Columns: #, Player, NO, YDS, AVG, Long, TB, FC, I20, 50+, BLK
  tibble(
    Player    = df$Player,
    Punts     = suppressWarnings(as.numeric(df$NO)),
    Punt.Yds  = suppressWarnings(as.numeric(df$YDS)),
    Punt.TB   = suppressWarnings(as.numeric(df$TB)),
    Punts.I20 = suppressWarnings(as.numeric(df$I20))
  )
}

parse_field_goals <- function(html) {
  df <- read_section_table(html, "field-goals")
  if (is.null(df)) return(tibble())
  # Columns: #, Player, FGM-FGA, %, I20, 20-29, 30-39, 40-49, 50+, Long, BLK
  res <- tibble(Player = df$Player)
  
  dist_map <- list(
    "I20"    = c("FGM.18.19", "FGA.18.19"),
    "20-29"  = c("FGM.20.29", "FGA.20.29"),
    "30-39"  = c("FGM.30.39", "FGA.30.39"),
    "40-49"  = c("FGM.40.49", "FGA.40.49"),
    "50\\+"  = c("FGM.50.59", "FGA.50.59")
  )
  for (pat in names(dist_map)) {
    col <- grep(pat, colnames(df), value = TRUE)[1]
    if (!is.na(col)) {
      s <- parse_hyphen_stat(df[[col]])
      res[[dist_map[[pat]][1]]] <- s$v1
      res[[dist_map[[pat]][2]]] <- s$v2
    }
  }
  
  blk_col <- grep("^BLK$", colnames(df), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(blk_col)) res$FGs.Blocked <- suppressWarnings(as.numeric(df[[blk_col]]))
  
  res
}

parse_kickoffs <- function(html) {
  df <- read_section_table(html, "kickoffs")
  if (is.null(df)) return(tibble())
  # Columns: #, Player, NO, YDS, AVG, TB, OB
  tibble(
    Player = df$Player,
    KO     = suppressWarnings(as.numeric(df$NO)),
    KO.Yds = suppressWarnings(as.numeric(df$YDS)),
    KO.TB  = suppressWarnings(as.numeric(df$TB))
  )
}

parse_kicking <- function(html) {
  fg  <- parse_field_goals(html)
  ko  <- parse_kickoffs(html)
  all_players <- unique(c(fg$Player, ko$Player))
  if (length(all_players) == 0) return(tibble())
  base <- tibble(Player = all_players)
  if (nrow(fg) > 0) base <- left_join(base, fg, by = "Player")
  if (nrow(ko) > 0) base <- left_join(base, ko, by = "Player")
  base
}

parse_returns <- function(html) {
  punt_ret <- read_section_table(html, "individual-special-returns-punt")
  kick_ret <- read_section_table(html, "individual-special-returns-kickoff")
  # Columns for both: #, Player, NO, YDS, AVG, TD, Long
  
  res_list <- list()
  
  if (!is.null(punt_ret)) {
    res_list[["punt"]] <- tibble(
      Player       = punt_ret$Player,
      Punt.Ret     = suppressWarnings(as.numeric(punt_ret$NO)),
      Punt.Ret.Yds = suppressWarnings(as.numeric(punt_ret$YDS)),
      Punt.Ret.TD  = suppressWarnings(as.numeric(punt_ret$TD))
    )
  }
  
  if (!is.null(kick_ret)) {
    res_list[["kick"]] <- tibble(
      Player      = kick_ret$Player,
      KO.Ret      = suppressWarnings(as.numeric(kick_ret$NO)),
      KO.Ret.Yds  = suppressWarnings(as.numeric(kick_ret$YDS)),
      Kick.Ret.TD = suppressWarnings(as.numeric(kick_ret$TD))
    )
  }
  
  if (length(res_list) == 0) return(tibble())
  reduce(res_list, full_join, by = "Player")
}

# -----------------------------------------------------------------------------
# Main scraper
# -----------------------------------------------------------------------------
scrape_rpi_football_stats <- function(stats_path, roster_path = NULL) {
  html       <- read_html(stats_path)
  season_val <- str_extract(basename(stats_path), "\\d{4}")
  if (is.na(season_val)) season_val <- "0"
  
  dfs <- list(
    parse_rushing(html),
    parse_passing(html),
    parse_receiving(html),
    parse_defense(html),
    parse_kicking(html),
    parse_punting(html),
    parse_returns(html)
  )
  dfs <- keep(dfs, ~ nrow(.x) > 0)
  if (length(dfs) == 0) stop("No data tables could be resolved.")
  
  # Consolidate GP: take the max across all tables for each player
  master_gp <- bind_rows(
    map(dfs, ~ if ("GP" %in% colnames(.x)) select(.x, Player, GP) else tibble())
  ) %>%
    filter(!is.na(GP)) %>%
    group_by(Player) %>%
    summarize(GP = max(GP, na.rm = TRUE), .groups = "drop")
  
  dfs_no_gp <- map(dfs, ~ select(.x, -any_of("GP")))
  
  master_stats <- reduce(dfs_no_gp, full_join, by = "Player") %>%
    left_join(master_gp, by = "Player") %>%
    mutate(Team = "Buffalo_St", Season = season_val)
  
  # Optionally join roster Pos + Yr
  if (!is.null(roster_path) && file.exists(roster_path)) {
    roster <- scrape_roster(roster_path)
    # Some players appear twice (same name, different jersey) — keep first
    roster <- roster %>% distinct(Player, .keep_all = TRUE)
    master_stats <- left_join(master_stats, roster, by = "Player")
  } else {
    master_stats$Pos <- NA_character_
    master_stats$Yr  <- NA_character_
  }
  
  # Target column layout
  target_cols <- c(
    "Player", "Team", "Season", "Yr", "Pos", "GP",
    "FR", "Blocks", "FF", "FGs.Blocked",
    "FGM.18.19", "FGA.18.19", "FGM.20.29", "FGA.20.29",
    "FGM.30.39", "FGA.30.39", "FGM.40.49", "FGA.40.49",
    "FGM.50.59", "FGA.50.59",
    "KO", "KO.Yds", "KO.TB", "KO.Ret", "Kick.Ret.TD", "KO.Ret.Yds",
    "PBU", "Int",
    "Pass.Att", "Pass.Comp", "Int.Thrown", "Pass.Yds", "Pass.TD",
    "Punt.Ret", "Punt.Ret.TD", "Punt.Ret.Yds",
    "Punts.I20", "Punts", "Punt.Yds", "Punt.TB",
    "Rec", "Rec.Yds", "Rec.TD",
    "Rush.Att", "Rush.Yds", "Rush.Yds.Gn", "Rush.Yds.Ls", "Rush.TD",
    "Sack", "Sack.Yds",
    "Safeties",
    "Solo.Tackles", "Asst.Tackles", "TFL", "TFL.Yds"
  )
  
  # Add any missing numeric target columns as 0; character cols as NA
  char_cols <- c("Player", "Team", "Season", "Pos", "Yr")
  missing_cols <- setdiff(target_cols, colnames(master_stats))
  for (col in missing_cols) {
    master_stats[[col]] <- if (col %in% char_cols) NA_character_ else 0
  }
  
  final_df <- select(master_stats, all_of(target_cols))
  
  # Replace NA with 0 for numeric cols, "0" for mandatory string cols
  mandatory_str <- c("Player", "Team", "Season")
  for (col in target_cols) {
    if (col %in% mandatory_str) {
      final_df[[col]][is.na(final_df[[col]])] <- "0"
    } else if (!col %in% char_cols) {
      final_df[[col]][is.na(final_df[[col]])] <- 0
    }
    # Pos and Yr are left as NA when not found in roster
  }
  
  final_df
}

# =============================================================================
# Execution
# =============================================================================
stats_file  <- "file"

roster_file <- "file"
