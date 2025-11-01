#  dyadic <- function(lag.values, delta.bids, lag1_dir = "<=", lag0_dir = ">=")
#
#             **Description**
#             First criterion used to classify buyer's bid revisions. Allows to set the "correct" response of the buyer. Defaults are the one predicted by (W)LDT
#               i.e., '<=' after acceptance and ">=" after rejection 
#             Takes _lag.values_, a (T-1)xM matrix of firm values/seller's decision, and a (T-1)x1 vector of _delta.bids_, 
#                 and checks, for each column of _lag.values_, and element by element
#                 whether bid revisions _delta.bids_ are consistent with WLDT, i.e., _delta.bids_ <=0 if _lag.values_ = 1 OR _delta.bids_ >=0 if _lag.values_ = 0
#
#             **Arguments**
#             _values_: a (T-1)xM matrix of values of the firm/seller's decision at t-1
#             _delta.bids_ : a (T-1)x1 matrix of bid revisions between t and t-1
#             lag1_dir = a comparison operator that defines the 'correct' response after the lag decision of the seller was 1 (Accept). Default is "<="
#             lag0_dir = a comparison operator that defines the 'correct' response after the lag decision of the seller was 1 (Accept). Default is ">="
#
#             **Value**
#             A (T-1)xM matrix of 0 or 1, depending on _delta.bid[i]_ being consistent with  _lag.values[i,j]_
#
#             **Details**
#             The check of consistency in bid revisions uses a weak inequality. This is the main difference wrt to standard LDT
#             The name 'dyadic' comes from the Acceptance/Refusal choice of the seller being binary \in\{0, 1\}

# 
# 


dyadic <- function(lag.values, delta.bids, 
                    lag1_dir = "<=",   # default inequality for lag=1
                    lag0_dir = ">=")   # default inequality for lag=0
{
  lag.values <- as.matrix(lag.values)
  
  op_map <- list("<"=`<`, "<="=`<=`, ">"=`>`, ">="=`>=`)
  if (!lag1_dir %in% names(op_map)) stop("lag1_dir must be one of: <, <=, >, >=")
  if (!lag0_dir %in% names(op_map)) stop("lag0_dir must be one of: <, <=, >, >=")
  
  op1 <- op_map[[lag1_dir]]
  op0 <- op_map[[lag0_dir]]
  
  lv <- lag.values
  cond <- (lv == 1 & op1(delta.bids, 0)) |
    (lv == 0 & op0(delta.bids, 0))
  
  return(ifelse(cond, 1L, 0L))
}


# Correct = 1 if if bid change in accord with LDT: v - p and bid change have the same sign, strong inequality
standard <- function(lag.values, delta.bids, ...) {
  args= list(...)
  lag.bids=args$lag.bids
  as.matrix(lag.values - lag.bids) * delta.bids > 0 }
