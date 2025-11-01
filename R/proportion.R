#  findSpread<- function(values, bids, criterion, repetitions=1000, sid=99)
#
#             **Description**
#             Takes the observed "values of the firm"/"seller's decision" and bids made by one single buyer through the T rounds.
#             Returns a scalar, the 'spread' of the observed proportion of bid revisions consistent with LDT over
#             the one obtained through random permutations of _values_ keeping _bids_ fixed.
#
#             **Arguments**
#             _values_: values of the firm/seller's decision, a Tx1 vector (T=number of rounds). 
#             _bids_: bids made by the buyer, a Tx1 vector.
#             _criterion_: criterion used to c±±heck consistency with LDT. See criteria list below.
#             _repetitions_: number of permutations of the firm value/seller's decision
#             _sid_: used to set the seed in the permutations, fixed for the reproducibility of results.
#
#             **Value**
#             Returns a scalar, the difference (i.e., 'spread') between the observed proportion of
#                 bid revision consistent with LDT, and an expected value of bid revision consistent with LDT computed under the null 
#                 of random bid revision by randomly permuting the firm values.
#
#             **Details**
#             Passes _values_ to _proportionCorrect_ to compute the proportion of correct bid revisions in the
#               observed data. 
#               Passes a TxM matrix, each column being a permutation of _values_, to _proportionCorrect_
#               to obtain M proportions of LDT consistent bid revisions for the M permutations. 
#               Computes the average of these M values to estimate the expected value of correct bid revision under the null
#               of independence between bid revisions and values of the firms/seller's decisions.
#
#
#  proportionCorrect <- function(matr.values, bids, criterion) 
#
#             **Description**
#             Takes _matr.values_, a TxM matrix of firm values/seller's decision, and a Tx1 vector of _bids_, 
#                 and computes, for each column of _matr.values_,
#                 the proportion of bid revisions consistent with one of the different flavors of LDT, specified by the criterion.
#
#             **Arguments**
#             _values_: a TxM matrix of values of the firm/seller's decision. If M=1, matr.values is the vector of realized firm values/seller's decision,
#                 if M>1, matr.values contains M random permutations of the vector of realized firm values/seller's decision.
#             _bids_: a Tx1 vector of bids
#             _criterion_: criterion used to check consistency with LDT. See criteria list below.
#
#             **Value**
#             A Mx1 vector of proportions, i.e., one for each column of matr.value, of bid revisions consistent with LDT, as specified by _criterion_.
#
#             **Details**
#             The function aligns the lagged firm values/seller's decision (_matr.values_) with the delta bids (_bids.change_) and with the lagged bid (_lag.bids) 
#               before passing them to 'criterion'

findSpread <- function(values, bids, criterion, repetitions=10000, sid=99){
  set.seed(sid)
  permutedv<-replicate( repetitions, sample(values, length(bids)) )     #, replace=TRUE
  return( proportionCorrect(values, bids, criterion) - mean(proportionCorrect(permutedv, bids, criterion) ) )
}

proportionCorrect <- function(matr.values, bids, criterion) {
  bids.change <- bids[-1] - bids[-length(bids)]         # (T-1)x1           
  matr.values <- as.matrix(matr.values)[-length(bids), , drop = FALSE]
  
  # Detect whether criterion needs lag.bids
  needs_lag <- "lag.bids" %in% names(formals(criterion))
  
  correct <- if (needs_lag) {
    criterion(lag.values = matr.values,
              delta.bids = bids.change,
              lag.bids   = bids[-length(bids)])
  } else {
    criterion(lag.values = matr.values,
              delta.bids = bids.change)
  }
  
  return(colSums(as.matrix(correct)) / length(bids.change))
}
