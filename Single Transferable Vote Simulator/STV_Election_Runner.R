# ============================================================
#  Simple STV Election Counter
#  - Enter real ballots directly below
#  - No fractional transfers (whole ballots only)
#  - Tiebreaks by random draw (flagged in output)
#  - Count-by-count summary table
# ============================================================


# ------------------------------------------------------------
#  SECTION 1: CONFIGURATION
# ------------------------------------------------------------

N_SEATS <- 3          # How many seats are being elected

# Set a random seed for reproducibility of any tiebreaks/surplus draws.
# Change this number each election, and record it in the minutes
# so results can be independently verified if challenged.
set.seed(123)


# ------------------------------------------------------------
#  SECTION 2: DECLARE CANDIDATES
#  List every candidate exactly as they will appear on ballots.
#  The script will warn you if a ballot contains an unrecognised name.
# ------------------------------------------------------------

candidates <- c(
  "Alice",
  "Bobby",
  "Celine",
  "Dimitri",
  "Emma",
  "Freddie",
  "Georgia", 
  "Herb", 
  "Irene", 
  "Joe"
)


# ------------------------------------------------------------
#  SECTION 3: ENTER BALLOTS
#
#  Format:  ballot("Name1", "Name2", "Name3", ...)
#  Prefix:  N %x% ballot(...)   means N identical ballots
#
#  - Names must match the candidates list exactly (case-sensitive)
#  - You only need to list as many preferences as were expressed;
#    a ballot of just one name is valid
#  - Order matters: first name = first preference
#
#  Example from the problem:
#    3 x Bob, Cedric, Dwayne
#    2 x Cedric, Eddie, Francine
#    1 x Stanley, Francine, Norbert
#    1 x Norbert, Bob, Cedric
# ------------------------------------------------------------

ballot  <- function(...) list(...)          # single ballot helper
`%x%`   <- function(n, b) rep(list(b), n)  # repeat ballot N times

ballots <- list(                            # <-- ADD YOUR BALLOTS HERE
  
  5 %x% ballot("Alice", "Bobby", "Celine"),
  4 %x% ballot("Dimitri",  "Emma", "Freddie"),
  2 %x% ballot("Alice", "Emma", "Celine"),
  1 %x% ballot("Dimitri", "Irene", "Joe")
  
)                                           # <-- END OF BALLOTS


# ============================================================
#  EVERYTHING BELOW THIS LINE RUNS THE ELECTION.
#  YOU DO NOT NEED TO EDIT ANYTHING BELOW.
# ============================================================


# ------------------------------------------------------------
#  Flatten nested lists produced by the %x% operator
# ------------------------------------------------------------
flatten_ballots <- function(x) {
  result <- list()
  for (item in x) {
    if (is.list(item) && length(item) > 0 && is.list(item[[1]])) {
      result <- c(result, item)
    } else {
      result <- c(result, list(item))
    }
  }
  result
}


# ------------------------------------------------------------
#  Validate: check all names on all ballots exist in candidates
# ------------------------------------------------------------
validate_ballots <- function(ballot_list, candidates) {
  errors <- character(0)
  for (i in seq_along(ballot_list)) {
    b       <- unlist(ballot_list[[i]])
    unknown <- setdiff(b, candidates)
    dupes   <- b[duplicated(b)]
    if (length(unknown) > 0)
      errors <- c(errors, sprintf("  Ballot %d: unrecognised name(s): %s",
                                  i, paste(unknown, collapse = ", ")))
    if (length(dupes) > 0)
      errors <- c(errors, sprintf("  Ballot %d: duplicate name(s): %s",
                                  i, paste(unique(dupes), collapse = ", ")))
  }
  if (length(errors) > 0) {
    cat("BALLOT ERRORS — please fix before continuing:\n")
    cat(paste(errors, collapse = "\n"), "\n")
    stop("Stopping due to ballot errors.", call. = FALSE)
  }
  invisible(TRUE)
}


# ------------------------------------------------------------
#  STV helpers
# ------------------------------------------------------------
droop_quota <- function(n_votes, n_seats) floor(n_votes / (n_seats + 1)) + 1

get_surplus_ballots <- function(pile, surplus) {
  if (length(pile) == 0 || surplus <= 0) return(list())
  pile[sample(seq_len(length(pile)), min(surplus, length(pile)))]
}


# ------------------------------------------------------------
#  Main STV function
# ------------------------------------------------------------
run_stv <- function(ballot_list, candidates, n_seats) {
  
  n_cands <- length(candidates)
  quota   <- droop_quota(length(ballot_list), n_seats)
  
  # Convert name-based ballots to index vectors
  idx_ballots <- lapply(ballot_list, function(b) match(unlist(b), candidates))
  
  # Initialise piles (one per candidate)
  piles <- vector("list", n_cands)
  for (i in seq_len(n_cands)) piles[[i]] <- list()
  for (b in idx_ballots) {
    if (length(b) > 0) piles[[b[1]]] <- c(piles[[b[1]]], list(b))
  }
  
  elected      <- integer(0)
  eliminated   <- integer(0)
  tiebreaks    <- list()
  summary_rows <- list()
  count_no     <- 0
  
  cat(rep("=", 62), "\n", sep = "")
  cat("  STV Election\n")
  cat(sprintf("  Seats: %d  |  Candidates: %d  |  Total votes: %d\n",
              n_seats, n_cands, length(ballot_list)))
  cat(sprintf("  Droop Quota: %d\n", quota))
  cat(rep("=", 62), "\n\n", sep = "")
  
  repeat {
    count_no <- count_no + 1
    active   <- setdiff(seq_len(n_cands), c(elected, eliminated))
    
    if (length(elected) >= n_seats || length(active) == 0) break
    
    # Fill remaining seats if active candidates == seats remaining
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
    
    # Tally votes
    votes <- setNames(
      sapply(seq_len(n_cands), function(i) length(piles[[i]])),
      seq_len(n_cands)
    )
    
    event_lines <- character(0)
    
    # --- Check for election ---
    newly_elected <- active[votes[active] >= quota]
    
    if (length(newly_elected) > 0) {
      # Process highest first if multiple elected in same count
      newly_elected <- newly_elected[order(votes[newly_elected], decreasing = TRUE)]
      
      for (e in newly_elected) {
        if (length(elected) >= n_seats) break
        surplus <- votes[e] - quota
        elected <- c(elected, e)
        
        event_lines <- c(event_lines,
                         sprintf("ELECTED: %s  |  Votes: %d  |  Quota: %d  |  Surplus: %d",
                                 candidates[e], votes[e], quota, surplus))
        
        if (surplus > 0) {
          # Randomly sample `surplus` ballots from this candidate's pile
          surplus_pile    <- get_surplus_ballots(piles[[e]], surplus)
          piles[[e]]      <- list()
          remaining_active <- setdiff(active, elected)
          for (sb in surplus_pile) {
            nxt <- sb[sb %in% remaining_active]
            if (length(nxt) > 0)
              piles[[nxt[1]]] <- c(piles[[nxt[1]]], list(sb))
          }
        } else {
          piles[[e]] <- list()
        }
      }
      
    } else {
      # --- Eliminate lowest candidate ---
      min_v <- min(votes[active])
      tied  <- active[votes[active] == min_v]
      
      if (length(tied) > 1) {
        loser  <- tied[sample(seq_along(tied), 1)]
        tb_msg <- sprintf(
          "TIEBREAK at count %d: %s all had %d vote(s). %s eliminated by random draw.",
          count_no,
          paste(candidates[tied], collapse = ", "),
          min_v,
          candidates[loser]
        )
        tiebreaks[[length(tiebreaks) + 1]] <- tb_msg
        event_lines <- c(event_lines,
                         sprintf("ELIMINATED: %s  |  Votes: %d  [*** TIEBREAK — random draw ***]",
                                 candidates[loser], min_v))
      } else {
        loser <- tied
        event_lines <- c(event_lines,
                         sprintf("ELIMINATED: %s  |  Votes: %d", candidates[loser], min_v))
      }
      
      eliminated   <- c(eliminated, loser)
      remaining    <- setdiff(active, loser)
      for (b in piles[[loser]]) {
        nxt <- b[b %in% remaining]
        if (length(nxt) > 0)
          piles[[nxt[1]]] <- c(piles[[nxt[1]]], list(b))
      }
      piles[[loser]] <- list()
    }
    
    # --- Print count table ---
    cat(sprintf("COUNT %d\n", count_no))
    cat(sprintf("  %-22s  %6s\n", "Candidate", "Votes"))
    cat("  ", rep("-", 32), "\n", sep = "")
    for (i in active) {
      marker <- ""
      if (i %in% elected    && any(grepl(candidates[i], event_lines, fixed = TRUE)))
        marker <- "  <-- ELECTED"
      if (i %in% eliminated && any(grepl(candidates[i], event_lines, fixed = TRUE)))
        marker <- "  <-- ELIMINATED"
      cat(sprintf("  %-22s  %6d%s\n", candidates[i], votes[i], marker))
    }
    cat("  ", rep("-", 32), "\n", sep = "")
    cat(sprintf("  %-22s  %6d\n\n", "Quota", quota))
    
    for (ev in event_lines) cat(sprintf("  >> %s\n", ev))
    if (any(grepl("TIEBREAK", event_lines)))
      cat("  ** Record this tiebreak in the meeting minutes. **\n")
    cat("\n", rep("-", 62), "\n\n", sep = "")
    
    summary_rows[[count_no]] <- list(count  = count_no,
                                     votes  = votes,
                                     events = event_lines)
  }
  
  # --- Final results ---
  cat(rep("=", 62), "\n", sep = "")
  cat(sprintf("  FINAL RESULT — %d seat(s) filled\n", length(elected)))
  cat(rep("=", 62), "\n", sep = "")
  for (i in seq_along(elected))
    cat(sprintf("  Seat %d: %s\n", i, candidates[elected[i]]))
  cat(rep("=", 62), "\n\n", sep = "")
  
  if (length(tiebreaks) > 0) {
    cat("TIEBREAK LOG (record in minutes):\n")
    for (tb in tiebreaks) cat(sprintf("  - %s\n", tb))
    cat("\n")
  }
  
  # --- Count-by-count summary table ---
  n_counts <- length(summary_rows)
  if (n_counts > 0) {
    col_w <- 7
    cat(rep("=", 62), "\n", sep = "")
    cat("  COUNT-BY-COUNT SUMMARY\n")
    cat(rep("=", 62), "\n", sep = "")
    
    header <- sprintf("%-22s", "Candidate")
    for (r in summary_rows)
      header <- paste0(header, formatC(sprintf("Ct%d", r$count), width = col_w, flag = " "))
    cat(header, "\n")
    cat(rep("-", 22 + col_w * n_counts), "\n", sep = "")
    
    for (i in seq_len(n_cands)) {
      line <- sprintf("%-22s", candidates[i])
      for (r in summary_rows) {
        v   <- r$votes[i]
        tag <- if      (i %in% elected    && any(grepl(candidates[i], r$events, fixed = TRUE))) "*"
        else if (i %in% eliminated && any(grepl(candidates[i], r$events, fixed = TRUE))) "-"
        else " "
        line <- paste0(line, formatC(sprintf("%d%s", v, tag), width = col_w, flag = " "))
      }
      cat(line, "\n")
    }
    
    cat(rep("-", 22 + col_w * n_counts), "\n", sep = "")
    quota_line <- sprintf("%-22s", "Quota")
    for (r in summary_rows)
      quota_line <- paste0(quota_line, formatC(as.character(quota), width = col_w, flag = " "))
    cat(quota_line, "\n\n")
    cat("  * = elected this count     - = eliminated this count\n\n")
  }
  
  invisible(list(elected = candidates[elected], quota = quota,
                 tiebreaks = tiebreaks))
}


# ------------------------------------------------------------
#  Run the election
# ------------------------------------------------------------
ballots_flat <- flatten_ballots(ballots)
validate_ballots(ballots_flat, candidates)
cat(sprintf("Ballots accepted: %d\n\n", length(ballots_flat)))
result <- run_stv(ballots_flat, candidates, N_SEATS)