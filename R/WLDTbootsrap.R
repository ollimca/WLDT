# Buyer-cluster bootstrap for pihat (proportion of LDT-consistent revisions)
# df0 must contain columns: ID, p, accept
# Criterion is your function: Criterion(lag.values, delta.bids) -> 0/1 vector
bootstrap_pihat <- function(df0, Criterion, Bboot = 5000, seed = NULL) {
  stopifnot(is.data.frame(df0))
  req_cols <- c("ID", "p", "accept")
  missing_cols <- setdiff(req_cols, names(df0))
  if (length(missing_cols) > 0) {
    stop("df0 is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.function(Criterion)) stop("Criterion must be a function.")
  
  if (!is.null(seed)) set.seed(seed)
  
  IDs <- unique(df0$ID)
  Nbuyers <- length(IDs)
  
  pihat_boot <- replicate(Bboot, {
    draw <- sample(IDs, size = Nbuyers, replace = TRUE)
    
    # Build stacked player transitions from the resampled buyers (duplicates allowed)
    stacked_b <- do.call("rbind", lapply(draw, function(indx) {
      tmp.df <- df0[df0$ID == indx, , drop = FALSE]
      
      # Need at least 2 bids to compute one revision
      if (nrow(tmp.df) < 2) return(NULL)
      
      bids <- tmp.df$p[-1]
      lag.bids <- tmp.df$p[-length(tmp.df$p)]
      bids.change <- bids - lag.bids
      lag.decision <- tmp.df$accept[-length(tmp.df$accept)]
      
      cbind(indx = indx,
            lag.decision = lag.decision,
            bids = bids,
            lag.bids = lag.bids,
            bids.change = bids.change)
    }))
    stacked_b <- as.data.frame(stacked_b)
    
    mean(Criterion(lag.values = stacked_b[["lag.decision"]],
                   delta.bids  = stacked_b[["bids.change"]]))
  })
  
  list(
    pihat_boot = pihat_boot,
    se = sd(pihat_boot),
    ci_95 = as.numeric(stats::quantile(pihat_boot, c(0.025, 0.975), na.rm = TRUE))
  )
}

bootstrap_pihat_pairs <- function(stacked, Criterion, Bboot = 5000, seed = NULL) {
  stopifnot(is.data.frame(stacked))
  req <- c("indx", "lag.decision", "bids.change")
  miss <- setdiff(req, names(stacked))
  if (length(miss) > 0L) stop("bootstrap_pihat_pairs(): missing columns: ", paste(miss, collapse = ", "))
  if (!is.function(Criterion)) stop("bootstrap_pihat_pairs(): Criterion must be a function.")
  
  if (!is.null(seed)) set.seed(seed)
  
  IDs <- unique(stacked$indx)
  Nbuyers <- length(IDs)
  
  pihat_boot <- replicate(Bboot, {
    draw <- sample(IDs, size = Nbuyers, replace = TRUE)
    
    stacked_b <- do.call("rbind", lapply(draw, function(id) {
      stacked[stacked$indx == id, , drop = FALSE]
    }))
    
    lv <- as.numeric(stacked_b$lag.decision)
    db <- as.numeric(stacked_b$bids.change)
    
    mean(Criterion(lag.values = lv, delta.bids = db))
  })
  
  list(
    pihat_boot = pihat_boot,
    se = sd(pihat_boot),
    ci_95 = as.numeric(stats::quantile(pihat_boot, c(0.025, 0.975), na.rm = TRUE))
  )
}
