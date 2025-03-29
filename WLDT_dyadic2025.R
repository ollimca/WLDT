#                                     LDT_dyadic2024.R
#  File build on LDT_dyadic.R .
#  MAIN DIFFERENCE the function proportionCorrect alignes the time series of bids, firm values and acceptance/refusal decisions, so the alignment no more takes place in the "criteria"
#  In this code, the functions LDT.Correct and conflictLDTvhat are dropped, since they are no more used

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


#
#
#  dyadic <- function(lag.values, delta.bids, ...)
#
#             **Description**
#             Takes _lag.values_, a (T-1)xM matrix of firm values/seller's decision, and a (T-1)x1 vector of _delta.bids_, 
#                 and checks, for each column of _lag.values_, and element by element
#                 whether bid revisions _delta.bids_ are consistent with WLDT, i.e., _delta.bids_ <=0 if _lag.values_ = 1 OR _delta.bids_ >=0 if _lag.values_ = 0
#
#             **Arguments**
#             _values_: a (T-1)xM matrix of values of the firm/seller's decision at t-1
#             _delta.bids_ : a (T-1)x1 matrix of bid revisions between t and t-1
#             ... : extra parameters, allowed so that  _proportionCorrect_ can call any of the criterions with the same syntax
#
#             **Value**
#             A (T-1)xM matrix of 0 or 1, depending on _delta.bid[i]_ being consistent with  _lag.values[i,j]_
#
#             **Details**
#             The check of consistency in bid revisions uses a weak inequality. This is the main difference wrt to standard LDT
#             The name 'dyadic' come from the Acceptance/Refusal choice of the seller being binary \in\{0, 1\}
#
#
#  dyadic2 <- function(lag.values, delta.bids, ...)
#
#             **Description**
#             Takes _lag.values_, a (T-1)xM matrix of firm values/seller's decision, and a (T-1)x1 vector of _delta.bids_, 
#                 and checks, for each column of _lag.values_, and element by element
#                 whether bid revisions _delta.bids_ violate WLDT, i.e., _delta.bids_ >0 if _lag.values_ = 1 OR _delta.bids_ <0 if _lag.values_ = 0
#
#             **Arguments**
#             _values_: a (T-1)xM matrix of values of the firm/seller's decision at t-1
#             _delta.bids_ : a (T-1)x1 matrix of bid revisions between t and t-1
#             ... : extra parameters, allowed so that  _proportionCorrect_ can call any of the criterions with the same syntax
#
#             **Value**
#             A (T-1)xM matrix of 0 or 1, depending on _delta.bid[i]_ being consistent with  _lag.values[i,j]_
#
#             **Details**
#             This function check for **violations** of WLDT, so uses strong inequalities, since equalities are allowed in WLDT
# 
# 
#   standard <- function(lag.values, delta.bids, ...)
# 
#           **Description**
#           Takes _lag.values_, a (T-1)xM matrix of firm values/seller's decision, and a (T-1)x1 vector of _delta.bids_, 
#                 and checks, for each column of _lag.values_, and element by element
#                 whether bid revisions _delta.bids_ agree with LDT, i.e., _delta.bids_ * (_lag.values_ - _lag.bids_) > 0 (strong inequality)
#
#             **Arguments**
#             _values_: a (T-1)xM matrix of values of the firm/seller's decision at t-1
#             _delta.bids_ : a (T-1)x1 matrix of bid revisions between t and t-1
#             ... : extra parameters, allowed so that  _proportionCorrect_ can call any of the criteria with the same syntax. 
#                   _standard_ needs a (T-1)x1 vector of _lag.bids_
#
#             **Value**
#             A (T-1)xM matrix of 0 or 1, depending on _delta.bid[i]_ being consistent with  _lag.values[i,j]_
#
#             **Details**
#             This function checks for consistency of bid revisions as in the original formulation of LDT in a AaC game
# 
#
#   descriptive.stats.print <- function()
#              
#           **Description**   
#           Prints descriptive stats on bid revisions
#
#   correct.dist.print <- function()
#              
#           **Description**   
#           Plots the distribution of correct bid revisions under randomness
#
#   a.test.print <- function(theTest, width=20)
# 
#           **Description**
#           Prints a formatted text with the output of the test
#
#             **Arguments**
#           _theTest_ : an object, the test to be printed
#           _width_   : a formatting parameter
#
#########################################################################################################################################
rm(list=ls())

# Functions

findSpread <- function(values, bids, criterion, repetitions=10000, sid=99){
  set.seed(sid)
  permutedv<-replicate( repetitions, sample(values, length(bids)) )     #, replace=TRUE
  return( proportionCorrect(values, bids, criterion) - mean(proportionCorrect(permutedv, bids, criterion) ) )
}

proportionCorrect <- function(matr.values, bids, criterion) { # Works both when 'matr.values' is a matrix or a vector
  bids.change <- bids[-1] - bids[-length(bids)]           # 'Forward' bids - lagged bids, (T-1)x1
  matr.values <- as.matrix(matr.values)[-length(bids), ]  # the row indexed by 'length(bids)' is the most recent
  correct <- criterion(lag.values = matr.values, delta.bids = bids.change, lag.bids=bids[-length(bids)])
    return(colSums(as.matrix(correct)) / length(bids.change))
}

# Correct = 1 if bid change in accord with WLDT (with weak inequality)
dyadic <- function(lag.values, delta.bids, ...){ifelse(as.matrix(lag.values)== 1 & delta.bids <=0, 1,
                              ifelse(as.matrix(lag.values)== 0 & delta.bids >=0, 1, 0) )}

# Correct = 1 checks for violations of WLDT, strong inequality 
dyadic2 <- function(lag.values, delta.bids, ...){ifelse(as.matrix(lag.values)== 1 & delta.bids < 0, 1,
                                                           ifelse(as.matrix(lag.values)== 0 & delta.bids >= 0, 1, 0) )}

# Correct = 1 if if bid change in accord with LDT: v - p and bid change have the same sign, strong inequality
standard <- function(lag.values, delta.bids, ...) {
  args= list(...)
  lag.bids=args$lag.bids
  as.matrix(lag.values - lag.bids) * delta.bids > 0 }


descriptive.stats.print <- function(){
  title.text <- "Descriptive statistics on bid revisions"
  iN <- length(unique(df0$ID))
  iT <- length(unique(df0$round))
  cat(title.text, "\n")
  cat(rep("*", nchar(title.text)), "\n", sep="")
  cat("number of individuals =", iN ,"\n")
  cat("number of rounds =", iT ,"\n")
  cat("number of observations =", iN * iT,"\n")
  # Transform bids.change and lag.decision into factors to use in `table` to create a contingency table
  bids.change.category <- with(stackedPlayers, factor(ifelse(bids.change > 0, 'positive', ifelse(bids.change < 0, 'negative', 'zero')), levels = c('negative', 'zero', 'positive')) )
  lag.decision.category <- factor(ifelse(stackedPlayers$lag.decision==1, 'acceptance', 'rejection'), levels=c('rejection', 'acceptance') )
  obsCorrect.factor<-ifelse(obsCorrect, 'Correct', 'Not Correct')
  Table1bids <- table(lag.decision.category, bids.change.category)
  Table1corr <- table(lag.decision.category, obsCorrect.factor)
  Table1bids.prop <- prop.table(Table1bids, 1)
  Table1corr.prop <- prop.table(Table1corr, 1)
  # Adding totals
  Table1bids <- rbind(Table1bids, colSums(Table1bids))
  Table1corr <- rbind(Table1corr, colSums(Table1corr))
  Table1bids.prop <- rbind(Table1bids.prop, t(prop.table(as.matrix(Table1bids[3,]), 2)))
  Table1corr.prop <- rbind(Table1corr.prop, t(prop.table(as.matrix(Table1corr[3,]), 2)))
  rownames(Table1corr)[3] = rownames(Table1bids)[3] = "Total"
  rownames(Table1corr.prop)[3] = rownames(Table1bids.prop)[3] = "Total"
  # Formatting output
  Table1 <- cbind(Table1bids, Table1corr)
  Table1.prop <- 100*cbind(Table1bids.prop, Table1corr.prop)
  Table1.prop_formatted <- formatted_col_percentages <- apply(Table1.prop, 2, function(x) paste(sprintf("%.2f", x), "%", sep=""))
  row.names(Table1.prop_formatted) <- c("rejection", "acceptance", "Total")
  print(Table1, quote=F)
  print(Table1.prop_formatted, quote=F)
  cat("\n")
}

correct.dist.print <- function(){
  vc.df <- data.frame(permCorrect)
  # Parameter are optimised for tikzDevice
  # Open TikZ device
  tikz("density_plot.tex", width=6, height=3.5)
  
  # Generate the ggplot
  Correct.dist <- ggplot(vc.df, aes(x = permCorrect)) +
    geom_density(stat = "density", n = 200, adjust=2, trim=TRUE, fill = "gray", color= "gray", alpha = 0.3) +
    labs(title = "Density Plot of the proportion of correct bid revisions under Randomness",
         x = "Proportion of correct bid revisions",
         y = "") +
    geom_segment(aes(x = 0.495, xend = 0.665, y = 0, yend = 0), 
                 color = "dimgrey", linewidth = 0.3, alpha=0.2) +    # X axis
    geom_segment(aes(x = pihat, xend = pihat, y=-1, yend=2), 
                 color = "darkgrey",  linewidth = 1) +
    scale_x_continuous(breaks = seq(0.40, 0.60, len=11)) +
    # expand_limits(x = max(permCorrect) * 1.2) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),      # No grids
      panel.grid.minor = element_blank(),      # No grids
      axis.text.x = element_text(size = 8, vjust = 5),  # , vjust adjustment for x-axis text
      axis.title.x = element_text(size = 8, vjust = 0), # Ensure x-axis label is below ticks
      axis.text.y = element_blank(),           # No y-axis text
      plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 25)), # Title adjustment: 
      plot.margin = margin(5, 10, 20, 10)     # Add padding to the plot
    ) +
    annotate("text", x = pihat, y = 4, size = 3,
             label = paste("$\\hat{\\pi}$ =", sprintf("%.3f",pihat)))  # Annotation text
  
  # Print the plot
  print(Correct.dist)
  
  # Close the TikZ device
  dev.off()
}
  
a.test.print <- function(theTest, width=20){
  test.name <- theTest$method
  cat(test.name, "\n")
  cat(rep("*", nchar(test.name)), "\n", sep="")
  A <- paste0("H0 : ", names(theTest$null.value), " = ", theTest$null.value,",")
  B <- paste0("H1 : ", names(theTest$null.value), " ", theTest$alternative, " than ", paste0(theTest$null.value, "."))
  C <- paste0("stat = ", format(theTest$statistic, digits=1, nsmall=3), ",")
  D <- paste0("p-value = ", format(theTest$p.value, digits=1, nsmall=3), ".")
  cat(sprintf("%-*s\t%-*s\n", width, A, width, B))
  cat(sprintf("%-*s\t%-*s\n", width, C, width, D))
  cat("\n")  
}

#########################################################################################################################################
#########################################################################################################################################

# Required packages
#
# install.packages("readxl")
library("BSDA")   # For 'SIGN.test'
library(dplyr)
library(ggplot2)
library(tikzDevice)

#########################################################################################################################################
# MAIN
#########################################################################################################################################

######################
# Parameters to set 
Criterion = dyadic
intRep = 10000      # Number of permutations
######################

# load and filter data
load("../Dati/CompleteLabeled_value_message.RData")
df  <-CompleteLabeled_value_message

df0 <- df %>%
  filter(phase == 2, role == 0) %>%        # only buyer and phase 2
  select("ID", "round", "q", "p", "v", "accept", "v_hat") %>%
  arrange(ID, round)


############################################
## Entirely permutation-based test  

stackedPlayers <- data.frame(do.call("rbind", lapply(unique(df0$ID), 
       function(indx){ 
         tmp.df = subset(df0, df0$ID==indx)
         bids <- tmp.df$p[-1]
         lag.bids <- tmp.df$p[-length(tmp.df$p)]
         bids.change <- bids - lag.bids
         lag.decision <- tmp.df$accept[-length(tmp.df$accept)]
         return(cbind( indx, lag.decision, bids, lag.bids, bids.change))}
)))

# obsCorrect is a vector with 1 for correct observed bid revisions on the vector of stacked players
obsCorrect <- Criterion(lag.values = stackedPlayers[, "lag.decision"], delta.bids = stackedPlayers[, "bids.change"])
# permCorrect is a vector of the size of the number of permutations giving the correct proportion of bid revisions in each permutation
permCorrect <- colMeans(Criterion( 
                            lag.values = replicate( intRep, sample(stackedPlayers[, "lag.decision"], length(stackedPlayers[, "bids.change"])) ),
                            delta.bids = stackedPlayers[, "bids.change"]
                                    )
                       )
pihat <-  mean(obsCorrect)                                 # Statistic: Proportion of correct observed bid revisions                                                      
Pvalue_all = sum( permCorrect > pihat)/length(permCorrect) # P-value

############################################
## Nonparametric and t-tests on the locatin parameter

# vector of spreads for each buyer
esse <- sapply(unique(df0$ID), function(indx){ tmp.df = subset(df0, df0$ID==indx); findSpread(tmp.df$accept, tmp.df$p, criterion=Criterion, repetitions = intRep) })
 
signedrank.statistic <-  wilcox.test(esse, alternative = "greater") # Wilcoxon signed rank test
sign.statistic <- SIGN.test(esse, alternative = "greater")          # Sign test
T.test <- t.test(esse, alternative = "greater")                     # t-test on proportions


############################################
## Print outputs  

# Descriptive Statistics on bids, Table 1
descriptive.stats.print()
# Plots the distribution of correct bid revisions under randomness, Figure 3
correct.dist.print()

## Entirely permutation-based test
cat("Permutation-based test\n")
cat(rep("*", nchar("Permutation-based test")), "\n", sep="")
cat(sprintf("%-*s\t%-*s\n", 20, paste("stat =", format(pihat, digits=1, nsmall=3)), 20, paste("p-value =", Pvalue_all)))
cat("\n")

# Tests on mean spread

a.test.print(T.test)

