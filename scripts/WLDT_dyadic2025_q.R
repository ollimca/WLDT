# --- MAIN (tidied) ------------------------------------------------------------

# deps
library(dplyr)
library(BSDA)
library(ggplot2)
# library(tictoc)  # optional; we call with tictoc:: below

# project fns
source("R/criteria.R")
source("R/proportion.R")
source("R/reporting.R")

# params
Criterion <- function(...) dyadic(..., lag1_dir = "<", lag0_dir = ">")  # check lag0_dir
intRep <- 10000
set.seed(123)  # reproducible permutations

# data
load("./Data/CompleteLabeled_value_message.RData")
df <- CompleteLabeled_value_message

df1 <- df %>%
  filter(phase == 2, role == 0) %>%        # only buyers, phase 2
  select(ID, round, q, p, v, accept, v_hat) %>%
  arrange(ID, round)

# ---- helpers -----------------------------------------------------------------

# Build lagged pairs once, reuse everywhere
build_pairs <- function(df0) {
  df0 %>%
    arrange(ID, round) %>%
    group_by(ID) %>%
    mutate(
      lag.bids     = dplyr::lag(p),
      bids.change  = p - lag.bids,
      lag.decision = dplyr::lag(accept)
    ) %>%
    filter(!is.na(lag.bids), !is.na(lag.decision)) %>%
    ungroup() %>%
    transmute(
      indx = ID,
      lag.decision,
      bids = p,
      lag.bids,
      bids.change
    )
}

# One analysis for one data slice (e.g., a given q)
analyze_slice <- function(df0, label, intRep, print_reports = FALSE) {
  tictoc::tic(paste0("q=", label))
  
  stacked <- build_pairs(df0)
  if (nrow(stacked) == 0L) {
    tictoc::toc()
    return(tibble(
      q = label,
      n_pairs = 0L, n_ids = dplyr::n_distinct(df0$ID), n_ids_ge2 = sum(table(df0$ID) >= 2),
      pihat = NA_real_, pi0 = NA_real_, pimed = NA_real_, pvalue = NA_real_,
      wilcox_p = NA_real_, sign_p = NA_real_, t_p = NA_real_
    ))
  }
  
  # vectors for Criterion()
  lv <- stacked[["lag.decision"]] %>% as.numeric()  # ensure atomic
  db <- stacked[["bids.change"]]  %>% as.numeric()
  
  # permutation-based test
  obsCorrect <- Criterion(lag.values = lv, delta.bids = db)
  permCorrect <- colMeans(Criterion(
    lag.values = replicate(intRep, sample(lv, length(lv), replace = FALSE)),
    delta.bids = db
  ))
  
  pihat  <- mean(obsCorrect)
  pi0    <- mean(permCorrect)
  pimed  <- median(permCorrect)
  pvalue <- mean(permCorrect > pihat)
  
  # spread per buyer
  esse <- sapply(unique(df0$ID), function(indx) {
    tmp.df <- subset(df0, ID == indx)
    findSpread(tmp.df$accept, tmp.df$p, criterion = Criterion, repetitions = intRep)
  })
  
  wilcox_p <- tryCatch(wilcox.test(esse, alternative = "greater")$p.value, error = function(e) NA_real_)
  sign_p   <- tryCatch(SIGN.test(esse, alternative = "greater")$p.value,   error = function(e) NA_real_)
  t_p      <- tryCatch(t.test(esse, alternative = "greater")$p.value,      error = function(e) NA_real_)
  
  if (isTRUE(print_reports)) {
    # your project-specific reporting hooks
    descriptive.stats.print()
    a.test.print(t.test(esse, alternative = "greater"))
  }
  
  tictoc::toc()
  
  tibble(
    q = label,
    n_pairs = nrow(stacked),
    n_ids = dplyr::n_distinct(df0$ID),
    n_ids_ge2 = sum(dplyr::count(df0, ID)$n >= 2),
    pihat = pihat, pi0 = pi0, pimed = pimed, pvalue = pvalue,
    wilcox_p = wilcox_p, sign_p = sign_p, t_p = t_p
  )
}

# ---- run: PER-q (default) ----------------------------------------------------

tictoc::tic("total")
q_vals <- sort(unique(df1$q))
results <- dplyr::bind_rows(lapply(q_vals, function(r) analyze_slice(dplyr::filter(df1, q == r), r, intRep)))
print(results)
tictoc::toc()




# If you also want the overall (no split), uncomment:
# overall <- analyze_slice(df1, "overall", intRep)
# print(overall)
