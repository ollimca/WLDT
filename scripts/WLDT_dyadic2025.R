#
# MAIN file

#########################################################################################################################################
#########################################################################################################################################

# scripts/main_analysis.R

# Load dependencies
library(dplyr)
library(BSDA)
library(ggplot2)
library(tikzDevice)

# Load project functions
source("R/criteria.R")
source("R/proportion.R")
source("R/reporting.R")


#########################################################################################################################################
# MAIN
#########################################################################################################################################

######################
# Parameters to set 
Criterion = dyadic #function(...) dyadic (..., lag1_dir = "<=", lag0_dir = ">=")
intRep = 10000      # Number of permutations  !! 10000 in the paper !!
######################

# load and filter data
load("./Data/CompleteLabeled_value_message.RData")
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
#correct.dist.print()

## Entirely permutation-based test
cat("Permutation-based test\n")
cat(rep("*", nchar("Permutation-based test")), "\n", sep="")
cat(sprintf("%-*s\t%-*s\n", 20, paste("stat =", format(pihat, digits=1, nsmall=3)), 20, paste("p-value =", Pvalue_all)))
cat("\n")

# Tests on mean spread

a.test.print(T.test)

