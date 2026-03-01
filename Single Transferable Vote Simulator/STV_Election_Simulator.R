# ============================================================
#  Simple STV Election Simulator
#  - No fractional transfers (whole ballots only)
#  - Tiebreaks resolved by random draw (flagged in output)
#  - Count-by-count summary table
#  - Exports summary table to PDF
# ============================================================

# ------------------------------------------------------------
#  CONFIGURATION  — edit these values to change the election
# ------------------------------------------------------------
N_SEATS     <- 5              # Number of seats up for election
N_VOTERS    <- 1000           # Number of voters
PDF_OUTPUT  <- "stv_results.pdf"  # Output file name
set.seed(42)                  # Remove or change for a live election

# ------------------------------------------------------------
#  1. BUILD CANDIDATE LIST
# ------------------------------------------------------------
factions   <- c("Red", "Blue", "Yellow")
candidates <- c(
  paste(rep(factions, times = N_SEATS),
        rep(seq_len(N_SEATS), each = length(factions)),
        sep = " "),
  "Independent 1"
)
n_cands <- length(candidates)
cat("Candidates:", paste(candidates, collapse = ", "), "\n\n")

# ------------------------------------------------------------
#  2. SIMULATE BALLOTS
# ------------------------------------------------------------
faction_idx <- function(f) which(startsWith(candidates, f))
ind_idx     <- which(candidates == "Independent 1")

straight_ticket <- function(f, last_ind = FALSE) {
  idx <- faction_idx(f)
  if (last_ind) idx <- c(idx[-length(idx)], ind_idx)
  idx
}

voter_types <- list(
  list(prob = 0.25, fn = function() straight_ticket("Red")),
  list(prob = 0.25, fn = function() straight_ticket("Blue")),
  list(prob = 0.10, fn = function() straight_ticket("Yellow")),
  list(prob = 0.02, fn = function() straight_ticket("Red",  last_ind = TRUE)),
  list(prob = 0.02, fn = function() straight_ticket("Blue", last_ind = TRUE)),
  list(prob = 0.36, fn = function() sample(seq_len(n_cands)))
)

probs   <- sapply(voter_types, `[[`, "prob")
types   <- sample(seq_along(voter_types), N_VOTERS, replace = TRUE, prob = probs)
ballots <- lapply(types, function(t) voter_types[[t]]$fn())

cat(sprintf("Generated %d ballots.\n\n", N_VOTERS))

# ------------------------------------------------------------
#  3. HELPER FUNCTIONS
# ------------------------------------------------------------

droop_quota <- function(n_votes, n_seats) floor(n_votes / (n_seats + 1)) + 1

get_surplus_ballots <- function(pile, surplus) {
  n <- length(pile)
  if (n == 0 || surplus <= 0) return(list())
  selected <- pile[sample(seq_len(n), min(surplus, n))]
  lapply(selected, function(b) b[-1])
}

# ------------------------------------------------------------
#  4. PDF EXPORT FUNCTION
# ------------------------------------------------------------
export_summary_pdf <- function(summary_rows, candidates, elected, eliminated,
                               quota, n_seats, n_voters, tiebreaks,
                               filename = "stv_results.pdf") {
  
  library(grid)
  
  n_counts <- length(summary_rows)
  n_cands  <- length(candidates)
  
  # Build table data
  col_headers <- c("Candidate", paste0("Count ", seq_len(n_counts)))
  
  # Fill cell values and determine cell annotations
  cells      <- matrix("", nrow = n_cands, ncol = n_counts)
  cell_notes <- matrix("",  nrow = n_cands, ncol = n_counts)  # "*" elected, "-" eliminated
  
  for (j in seq_len(n_counts)) {
    r <- summary_rows[[j]]
    for (i in seq_len(n_cands)) {
      v   <- r$votes[i]
      tag <- " "
      if (i %in% elected    && any(grepl(candidates[i], r$events))) tag <- "*"
      if (i %in% eliminated && any(grepl(candidates[i], r$events))) tag <- "-"
      cells[i, j]      <- as.character(v)
      cell_notes[i, j] <- tag
    }
  }
  
  # ---- Page layout ----
  page_w <- 11; page_h <- 8.5   # landscape A4-ish (inches)
  
  pdf(filename, width = page_w, height = page_h, paper = "special")
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(
    nrow = 3, ncol = 1,
    heights = unit(c(1.2, page_h - 2.5, 1.3), "inches")
  )))
  
  # ---- Header ----
  pushViewport(viewport(layout.pos.row = 1))
  grid.rect(gp = gpar(fill = "#1a1a2e", col = NA))
  grid.text("STV Election Simulation — Results Summary",
            x = 0.5, y = 0.65,
            gp = gpar(fontsize = 18, fontface = "bold", col = "white"))
  grid.text(
    sprintf("Seats: %d   |   Voters: %d   |   Droop Quota: %d   |   Counts: %d",
            n_seats, n_voters, quota, n_counts),
    x = 0.5, y = 0.28,
    gp = gpar(fontsize = 10, col = "#ccccdd")
  )
  popViewport()
  
  # ---- Table area ----
  pushViewport(viewport(layout.pos.row = 2,
                        x = 0.5, y = 0.5,
                        width  = unit(page_w - 0.8, "inches"),
                        height = unit(page_h - 2.5, "inches")))
  
  total_cols <- 1 + n_counts
  col_widths <- c(0.28, rep(0.72 / n_counts, n_counts))  # as fractions of viewport
  
  row_h      <- 1 / (n_cands + 2)   # +1 header +1 quota footer
  
  # Draw header row
  x_pos <- 0
  header_labels <- col_headers
  for (col_i in seq_len(total_cols)) {
    cx <- x_pos + col_widths[col_i] / 2
    grid.rect(x = x_pos, y = 1 - row_h, width = col_widths[col_i], height = row_h,
              just = c("left", "bottom"),
              gp = gpar(fill = "#2d2d5e", col = "white", lwd = 0.5))
    grid.text(header_labels[col_i], x = cx, y = 1 - row_h / 2,
              gp = gpar(fontsize = 8.5, fontface = "bold", col = "white"))
    x_pos <- x_pos + col_widths[col_i]
  }
  
  # Draw candidate rows
  row_colors <- c("#f0f0f8", "#e0e0f0")
  
  for (row_i in seq_len(n_cands)) {
    y_top  <- 1 - row_h * (row_i + 1)
    fill   <- row_colors[(row_i %% 2) + 1]
    
    # Highlight elected candidates
    if (row_i %in% elected)    fill <- "#d4edda"
    if (row_i %in% eliminated) fill <- "#f8d7da"
    
    x_pos <- 0
    # Candidate name cell
    grid.rect(x = 0, y = y_top, width = col_widths[1], height = row_h,
              just = c("left", "bottom"),
              gp = gpar(fill = fill, col = "#aaaaaa", lwd = 0.4))
    grid.text(candidates[row_i], x = col_widths[1] * 0.05, y = y_top + row_h / 2,
              just = "left",
              gp = gpar(fontsize = 8, col = "#111111"))
    x_pos <- col_widths[1]
    
    # Count cells
    for (col_i in seq_len(n_counts)) {
      val  <- cells[row_i, col_i]
      note <- cell_notes[row_i, col_i]
      
      cell_fill <- fill
      txt_col   <- "#111111"
      bold      <- "plain"
      
      if (note == "*") { cell_fill <- "#28a745"; txt_col <- "white"; bold <- "bold" }
      if (note == "-") { cell_fill <- "#dc3545"; txt_col <- "white"; bold <- "bold" }
      
      cx <- x_pos + col_widths[col_i + 1] / 2
      grid.rect(x = x_pos, y = y_top,
                width = col_widths[col_i + 1], height = row_h,
                just = c("left", "bottom"),
                gp = gpar(fill = cell_fill, col = "#aaaaaa", lwd = 0.4))
      grid.text(if (note %in% c("*", "-")) paste0(val, note) else val,
                x = cx, y = y_top + row_h / 2,
                gp = gpar(fontsize = 8, fontface = bold, col = txt_col))
      
      x_pos <- x_pos + col_widths[col_i + 1]
    }
  }
  
  # Quota footer row
  y_top <- 1 - row_h * (n_cands + 2)
  grid.rect(x = 0, y = y_top, width = col_widths[1], height = row_h,
            just = c("left", "bottom"),
            gp = gpar(fill = "#2d2d5e", col = "white", lwd = 0.5))
  grid.text("Quota", x = col_widths[1] * 0.05, y = y_top + row_h / 2,
            just = "left",
            gp = gpar(fontsize = 8, fontface = "bold", col = "white"))
  x_pos <- col_widths[1]
  for (col_i in seq_len(n_counts)) {
    cx <- x_pos + col_widths[col_i + 1] / 2
    grid.rect(x = x_pos, y = y_top,
              width = col_widths[col_i + 1], height = row_h,
              just = c("left", "bottom"),
              gp = gpar(fill = "#2d2d5e", col = "white", lwd = 0.5))
    grid.text(as.character(quota), x = cx, y = y_top + row_h / 2,
              gp = gpar(fontsize = 8, fontface = "bold", col = "white"))
    x_pos <- x_pos + col_widths[col_i + 1]
  }
  
  popViewport()
  
  # ---- Footer ----
  pushViewport(viewport(layout.pos.row = 3))
  grid.rect(gp = gpar(fill = "#f5f5f5", col = NA))
  
  # Elected seats summary
  elected_names <- paste(
    sapply(seq_along(elected), function(i) sprintf("Seat %d: %s", i, candidates[elected[i]])),
    collapse = "   |   "
  )
  grid.text(paste("ELECTED —", elected_names),
            x = 0.5, y = 0.72,
            gp = gpar(fontsize = 9, fontface = "bold", col = "#1a1a2e"))
  
  # Legend
  legend_items <- list(
    list(col = "#28a745", label = "* Elected this count"),
    list(col = "#dc3545", label = "- Eliminated this count"),
    list(col = "#d4edda", label = "Elected (row)"),
    list(col = "#f8d7da", label = "Eliminated (row)")
  )
  x_start <- 0.1
  for (li in legend_items) {
    grid.rect(x = x_start, y = 0.38, width = 0.025, height = 0.22,
              just = c("left", "bottom"),
              gp = gpar(fill = li$col, col = NA))
    grid.text(li$label, x = x_start + 0.032, y = 0.49,
              just = "left",
              gp = gpar(fontsize = 7.5, col = "#444444"))
    x_start <- x_start + 0.22
  }
  
  # Tiebreak warning if any
  if (length(tiebreaks) > 0) {
    grid.text(paste("⚠ TIEBREAK(S) occurred — record in minutes:", tiebreaks[[1]]),
              x = 0.5, y = 0.12,
              gp = gpar(fontsize = 7, col = "#cc0000"))
  }
  
  popViewport()
  dev.off()
  
  cat(sprintf("\nPDF summary exported to: %s\n", filename))
}

# ------------------------------------------------------------
#  5. MAIN STV FUNCTION
# ------------------------------------------------------------
run_stv <- function(ballots, n_seats) {
  
  quota      <- droop_quota(length(ballots), n_seats)
  elected    <- c()
  eliminated <- c()
  tiebreaks  <- list()
  
  piles <- vector("list", n_cands)
  for (i in seq_len(n_cands)) piles[[i]] <- list()
  for (b in ballots) {
    if (length(b) > 0) piles[[b[1]]] <- c(piles[[b[1]]], list(b))
  }
  
  summary_rows <- list()
  count_no     <- 0
  
  cat(sprintf("Droop Quota : %d\n", quota))
  cat(sprintf("Seats       : %d\n", n_seats))
  cat(sprintf("Total votes : %d\n\n", length(ballots)))
  cat(rep("=", 62), "\n\n", sep = "")
  
  repeat {
    count_no <- count_no + 1
    active   <- setdiff(seq_len(n_cands), c(elected, eliminated))
    
    if (length(elected) >= n_seats || length(active) == 0) break
    
    seats_left <- n_seats - length(elected)
    if (length(active) <= seats_left) {
      cat(sprintf("Count %d: %d candidate(s) remain for %d seat(s) — all elected uncontested.\n\n",
                  count_no, length(active), seats_left))
      for (a in active) {
        elected <- c(elected, a)
        cat(sprintf("  >> ELECTED (uncontested): %s\n", candidates[a]))
      }
      cat("\n")
      break
    }
    
    votes <- setNames(
      sapply(seq_len(n_cands), function(i) length(piles[[i]])),
      seq_len(n_cands)
    )
    
    event_lines  <- character(0)
    newly_elected <- active[votes[active] >= quota]
    
    if (length(newly_elected) > 0) {
      newly_elected <- newly_elected[order(votes[newly_elected], decreasing = TRUE)]
      
      for (e in newly_elected) {
        if (length(elected) >= n_seats) break
        surplus <- votes[e] - quota
        elected <- c(elected, e)
        
        event_lines <- c(event_lines,
                         sprintf("ELECTED: %s  |  Votes: %d  |  Quota: %d  |  Surplus: %d",
                                 candidates[e], votes[e], quota, surplus))
        
        if (surplus > 0) {
          surplus_ballots   <- get_surplus_ballots(piles[[e]], surplus)
          piles[[e]]        <- list()
          remaining_active  <- setdiff(active, elected)
          for (sb in surplus_ballots) {
            next_cand <- sb[sb %in% remaining_active]
            if (length(next_cand) > 0) {
              piles[[next_cand[1]]] <- c(piles[[next_cand[1]]], list(sb))
            }
          }
        } else {
          piles[[e]] <- list()
        }
      }
      
    } else {
      min_votes <- min(votes[active])
      tied      <- active[votes[active] == min_votes]
      
      if (length(tied) > 1) {
        loser  <- tied[sample(seq_along(tied), 1)]
        tb_msg <- sprintf(
          "TIEBREAK at count %d: %s all had %d vote(s). %s eliminated by random draw.",
          count_no,
          paste(candidates[tied], collapse = ", "),
          min_votes,
          candidates[loser]
        )
        tiebreaks[[length(tiebreaks) + 1]] <- tb_msg
        event_lines <- c(event_lines,
                         sprintf("ELIMINATED: %s  |  Votes: %d  [*** TIEBREAK — random draw ***]",
                                 candidates[loser], votes[loser]))
      } else {
        loser       <- tied
        event_lines <- c(event_lines,
                         sprintf("ELIMINATED: %s  |  Votes: %d",
                                 candidates[loser], votes[loser]))
      }
      
      eliminated        <- c(eliminated, loser)
      remaining_active  <- setdiff(active, loser)
      for (b in piles[[loser]]) {
        next_cand <- b[b %in% remaining_active]
        if (length(next_cand) > 0) {
          piles[[next_cand[1]]] <- c(piles[[next_cand[1]]], list(b))
        }
      }
      piles[[loser]] <- list()
    }
    
    # Print count table to console (unchanged from original)
    cat(sprintf("COUNT %d\n", count_no))
    cat(sprintf("  %-22s  %6s\n", "Candidate", "Votes"))
    cat("  ", rep("-", 32), "\n", sep = "")
    for (i in active) {
      marker <- ""
      if (i %in% elected    && votes[i] >= quota) marker <- "  <-- ELECTED"
      if (i %in% eliminated && votes[i] == min(votes[active]))  marker <- "  <-- ELIMINATED"
      cat(sprintf("  %-22s  %6d%s\n", candidates[i], votes[i], marker))
    }
    cat("  ", rep("-", 32), "\n", sep = "")
    cat(sprintf("  %-22s  %6d\n\n", "Quota", quota))
    
    for (ev in event_lines) cat(sprintf("  >> %s\n", ev))
    if (any(grepl("TIEBREAK", event_lines))) {
      cat("  ** Record this tiebreak in the meeting minutes. **\n")
    }
    cat("\n")
    cat(rep("-", 62), "\n\n", sep = "")
    
    summary_rows[[count_no]] <- list(count = count_no, votes = votes,
                                     events = event_lines)
  }
  
  # Final results (console)
  cat(rep("=", 62), "\n", sep = "")
  cat(sprintf("  FINAL RESULT — %d seat(s) filled\n", length(elected)))
  cat(rep("=", 62), "\n", sep = "")
  for (i in seq_along(elected)) {
    cat(sprintf("  Seat %d: %s\n", i, candidates[elected[i]]))
  }
  cat(rep("=", 62), "\n\n", sep = "")
  
  if (length(tiebreaks) > 0) {
    cat("TIEBREAK LOG (record in minutes):\n")
    for (tb in tiebreaks) cat(sprintf("  - %s\n", tb))
    cat("\n")
  }
  
  # Count-by-count summary (console)
  n_counts <- length(summary_rows)
  col_w    <- 7
  
  cat(rep("=", 62), "\n", sep = "")
  cat("  COUNT-BY-COUNT SUMMARY\n")
  cat(rep("=", 62), "\n", sep = "")
  
  header <- sprintf("%-22s", "Candidate")
  for (r in summary_rows) header <- paste0(header, formatC(sprintf("Ct%d", r$count),
                                                           width = col_w, flag = " "))
  cat(header, "\n")
  cat(rep("-", 22 + col_w * n_counts), "\n", sep = "")
  
  for (i in seq_len(n_cands)) {
    line <- sprintf("%-22s", candidates[i])
    for (r in summary_rows) {
      v                  <- r$votes[i]
      is_elected_here    <- i %in% elected    && any(grepl(candidates[i], r$events))
      is_eliminated_here <- i %in% eliminated && any(grepl(candidates[i], r$events))
      tag  <- if (is_elected_here) "*" else if (is_eliminated_here) "-" else " "
      line <- paste0(line, formatC(sprintf("%d%s", v, tag), width = col_w, flag = " "))
    }
    cat(line, "\n")
  }
  
  cat(rep("-", 22 + col_w * n_counts), "\n", sep = "")
  line <- sprintf("%-22s", "Quota")
  for (r in summary_rows) line <- paste0(line, formatC(as.character(quota),
                                                       width = col_w, flag = " "))
  cat(line, "\n\n")
  cat("  * = elected this count     - = eliminated this count\n\n")
  
  # ---- Export PDF ----
  export_summary_pdf(
    summary_rows = summary_rows,
    candidates   = candidates,
    elected      = elected,
    eliminated   = eliminated,
    quota        = quota,
    n_seats      = n_seats,
    n_voters     = length(ballots),
    tiebreaks    = tiebreaks,
    filename     = PDF_OUTPUT
  )
  
  invisible(list(elected    = elected,
                 candidates = candidates,
                 quota      = quota,
                 tiebreaks  = tiebreaks))
}

# ------------------------------------------------------------
#  RUN
# ------------------------------------------------------------
result <- run_stv(ballots, N_SEATS)