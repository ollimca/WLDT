# Build lagged (t-1, t) pairs for each buyer, with basic input checks.
# Requires columns: ID, round, p, accept

build_pairs <- function(df0) {
  stopifnot(is.data.frame(df0))
  
  req <- c("ID", "round", "q", "p", "accept")
  missing <- setdiff(req, names(df0))
  if (length(missing) > 0L) {
    stop("build_pairs(): missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # keep only IDs with at least 2 rounds
  id_counts <- dplyr::count(df0, ID, name = "n_rounds")
  keep_ids <- id_counts$ID[id_counts$n_rounds >= 2L]
  if (length(keep_ids) == 0L) {
    warning("build_pairs(): no IDs with >= 2 rounds; returning empty tibble.")
    return(dplyr::tibble(
      indx = character(0),
      round = integer(0),
      q = numeric(0),
      lag.decision = numeric(0),
      bids = numeric(0),
      lag.bids = numeric(0),
      bids.change = numeric(0)
    ))
  }
  
  df0 %>%
    dplyr::filter(ID %in% keep_ids) %>%
    dplyr::arrange(ID, round) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      lag.bids     = dplyr::lag(p),
      bids.change  = p - lag.bids,
      lag.decision = dplyr::lag(accept)
    ) %>%
    dplyr::filter(!is.na(lag.bids), !is.na(lag.decision)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      indx = ID,
      round = round,
      q = q,
      lag.decision = lag.decision,
      bids = p,
      lag.bids = lag.bids,
      bids.change = bids.change
    )
}
