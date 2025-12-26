#
# MAIN file

#########################################################################################################################################
#########################################################################################################################################

# scripts/main_analysis.R

# Check that the following R packages are installed: `dplyr`, `BSDA`, `ggplot2`, `tikzDevice`.
required_packages <- c("dplyr", "BSDA", "ggplot2", "tikzDevice")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# Load dependencies
library(dplyr)
library(BSDA)
library(ggplot2)
library(tikzDevice)

# Load project functions
source("R/WLDT.R")

# To time the code
tic <- function() assign(".tic", Sys.time(), envir = .GlobalEnv)
toc <- function() print(Sys.time() - get(".tic", envir = .GlobalEnv))

#########################################################################################################################################
# MAIN
#########################################################################################################################################
tic()
######################
# Parameters to set 
Criterion = function(...) dyadic (..., lag1_dir = "<=", lag0_dir = ">=")
intRep = 10000      # Number of permutations  !! 10000 in the paper !!
Bboot  = 10000      # Number of bootstrap repetitions for se_boot
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

# stackedPlayers <- data.frame(do.call("rbind", lapply(unique(df0$ID), 
#        function(indx){ 
#          tmp.df = subset(df0, df0$ID==indx)
#          bids <- tmp.df$p[-1]
#          lag.bids <- tmp.df$p[-length(tmp.df$p)]
#          bids.change <- bids - lag.bids
#          lag.decision <- tmp.df$accept[-length(tmp.df$accept)]
#          return(cbind( indx, lag.decision, bids, lag.bids, bids.change))}
# )))

stackedPlayers <- build_pairs(df0)
lv <- as.numeric(stackedPlayers$lag.decision)
db <- as.numeric(stackedPlayers$bids.change)
# obsCorrect is a vector with 1 for correct observed bid revisions on the vector of stacked players
obsCorrect <- Criterion(lag.values = lv, delta.bids = db)
# permCorrect is a vector of the size of the number of permutations giving the correct proportion of bid revisions in each permutation
permCorrect <- colMeans(Criterion(
  lag.values = replicate(intRep, sample(lv, length(lv), replace = FALSE)),
  delta.bids = db
))

pihat <-  mean(obsCorrect)                                  # Statistic: Proportion of correct observed bid revisions                                                      
pi0   <- mean(permCorrect)                                  # Proportion of correct bid revisions under the null  
pimed   <- median(permCorrect)                              # Median of correct bid revisions under the null  
Pvalue_all = (sum( permCorrect >= pihat) + 1)/
                    (length(permCorrect) + 1)               # P-value according to Phipson and Smyth (2010): Permutation P-values Should Never Be Zero
boot <- bootstrap_pihat(df0, Criterion, Bboot, seed = 123)

############################################
## Nonparametric and t-tests on the location parameter

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
cat(sprintf("%-*s\t%-*s\n", 20, paste("pi0 =", format(pi0, digits=1, nsmall=3)), 20, paste("pimed =", format(pimed, digits=1, nsmall=3))))
cat(sprintf("%-*s\t%-*s\n", 20, paste("pihat =", format(pihat, digits=1, nsmall=3)), 20, paste("p-value =", format(Pvalue_all, digits=1, nsmall=3))))
cat(sprintf("%-*s\t%-*s\n", 20, paste("pihat - pi0 =", format(pihat-pi0, digits=1, nsmall=3)), 20, paste("se_boot =", format(boot$se, digits=1, nsmall=3))))
cat("\n")

# Tests on mean spread

a.test.print(T.test)

toc()