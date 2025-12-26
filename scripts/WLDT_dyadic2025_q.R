# deps
library(dplyr)
library(ggplot2)

# project fns
source("R/WLDT.R")


# params
Criterion <- function(...) dyadic(..., lag1_dir = "<=", lag0_dir = ">=")  # check lag0_dir
intRep <- 10000
Bboot <- 2000
set.seed(123)  # reproducible permutations

# data
load("./Data/CompleteLabeled_value_message.RData")
df <- CompleteLabeled_value_message

df1 <- df %>%
  filter(phase == 2, role == 0) %>%        # only buyers, phase 2
  select(ID, round, q, p, v, accept, v_hat) %>%
  arrange(ID, round)



# One analysis for one data slice (e.g., a given q)
# One analysis for one *paired* slice (e.g. q=r)
analyze_slice <- function(stacked, label, Criterion, intRep) {
  tictoc::tic(paste0("q=", label))
  
  if (nrow(stacked) == 0L) {
    tictoc::toc()
    return(tibble(
      q = label,
      n_pairs = 0L,
      n_ids = 0L,
      n_ids_ge2 = 0L,
      pihat = NA_real_, pi0 = NA_real_, pimed = NA_real_, pvalue = NA_real_,
      wilcox_p = NA_real_, sign_p = NA_real_, t_p = NA_real_
    ))
  }
  
  lv <- as.numeric(stacked[["lag.decision"]])
  db <- as.numeric(stacked[["bids.change"]])
  
  # permutation-based test (global within this slice)
  obsCorrect <- Criterion(lag.values = lv, delta.bids = db)
  permCorrect <- colMeans(Criterion(
    lag.values = replicate(intRep, sample(lv, length(lv), replace = FALSE)),
    delta.bids = db
  ))
  
  pihat <- mean(obsCorrect)
  pi0   <- mean(permCorrect)
  size <- pihat - pi0
  pimed <- median(permCorrect)
  pvalue <- (sum(permCorrect >= pihat) + 1) / (intRep + 1)  # +1 correction
  boot <- bootstrap_pihat_pairs(stacked, Criterion, Bboot = Bboot, seed = 123)
  se_pihat <- boot$se
  
 
  tictoc::toc()
  
  tibble(
    q = label,
    n_pairs = nrow(stacked),
    n_ids = dplyr::n_distinct(stacked$indx),
    pihat = pihat, pi0 = pi0, pimed = pimed, size=size, se = se_pihat, pvalue = pvalue
  )
}

# ---- run: PER-q (default) ----------------------------------------------------

tictoc::tic("total")
stacked_all <- build_pairs(df1)  # build true (t-1,t) pairs once
  
q_vals <- sort(unique(df1$q))
results <- dplyr::bind_rows(lapply(q_vals, function(r) {
  analyze_slice(dplyr::filter(stacked_all, q == r), r, Criterion, intRep)
}))

print(results)



# If you also want the overall (no split), uncomment:
# overall <- analyze_slice(df1, "overall", intRep)
# print(overall)
