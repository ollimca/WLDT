# reporting.R
# Contains functions for descriptive statistics, visualization, and formatted test output.
# No library() calls here — all external functions are called using pkg::fun().
#
#
#------------------------------------------------------------
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

descriptive.stats.print <- function() {
  title.text <- "Descriptive statistics on bid revisions"
  iN <- length(unique(df0$ID))
  iT <- length(unique(df0$round))
  cat(title.text, "\n")
  cat(rep("*", nchar(title.text)), "\n", sep = "")
  cat("number of individuals =", iN, "\n")
  cat("number of rounds =", iT, "\n")
  cat("number of observations =", iN * iT, "\n")
  
  # Transform bids.change and lag.decision into factors
  bids.change.category <- with(
    stackedPlayers,
    factor(
      ifelse(bids.change > 0, "positive",
             ifelse(bids.change < 0, "negative", "zero")),
      levels = c("negative", "zero", "positive")
    )
  )
  
  lag.decision.category <- factor(
    ifelse(stackedPlayers$lag.decision == 1, "acceptance", "rejection"),
    levels = c("rejection", "acceptance")
  )
  
  obsCorrect.factor <- ifelse(obsCorrect, "Correct", "Not Correct")
  
  Table1bids <- table(lag.decision.category, bids.change.category)
  Table1corr <- table(lag.decision.category, obsCorrect.factor)
  Table1bids.prop <- prop.table(Table1bids, 1)
  Table1corr.prop <- prop.table(Table1corr, 1)
  
  Table1bids <- rbind(Table1bids, colSums(Table1bids))
  Table1corr <- rbind(Table1corr, colSums(Table1corr))
  Table1bids.prop <- rbind(Table1bids.prop,
                           t(prop.table(as.matrix(Table1bids[3, ]), 2)))
  Table1corr.prop <- rbind(Table1corr.prop,
                           t(prop.table(as.matrix(Table1corr[3, ]), 2)))
  
  rownames(Table1corr)[3] <- rownames(Table1bids)[3] <- "Total"
  rownames(Table1corr.prop)[3] <- rownames(Table1bids.prop)[3] <- "Total"
  
  Table1 <- cbind(Table1bids, Table1corr)
  Table1.prop <- 100 * cbind(Table1bids.prop, Table1corr.prop)
  Table1.prop_formatted <- apply(
    Table1.prop, 2,
    function(x) paste(sprintf("%.2f", x), "%", sep = "")
  )
  row.names(Table1.prop_formatted) <- c("rejection", "acceptance", "Total")
  
  print(Table1, quote = FALSE)
  print(Table1.prop_formatted, quote = FALSE)
  cat("\n")
}

#------------------------------------------------------------
correct.dist.print <- function() {
  vc.df <- data.frame(permCorrect)
  if (!dir.exists("results")) {
    dir.create("results")
  }
  tikzDevice::tikz("results/density_plot.tex", width = 6, height = 3.5)
  
  plot <- ggplot2::ggplot(vc.df, ggplot2::aes(x = permCorrect)) +
    ggplot2::geom_density(stat = "density", n = 200, adjust = 2,
                          trim = TRUE, fill = "gray", color = "gray", alpha = 0.3) +
    ggplot2::labs(
      title = "Density Plot of the proportion of correct bid revisions under Randomness",
      x = "Proportion of correct bid revisions", y = ""
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0.495, xend = 0.665, y = 0, yend = 0),
      color = "dimgrey", linewidth = 0.3, alpha = 0.2
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = pihat, xend = pihat, y = -1, yend = 2),
      color = "darkgrey", linewidth = 1
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0.40, 0.60, len = 11)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 8, vjust = 5),
      axis.title.x = ggplot2::element_text(size = 8, vjust = 0),
      axis.text.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 10, hjust = 0.5,
                                         margin = ggplot2::margin(b = 25)),
      plot.margin = ggplot2::margin(5, 10, 20, 10)
    ) +
    ggplot2::annotate("text", x = pihat, y = 4, size = 3,
                      label = paste("$\\hat{\\pi}$ =", sprintf("%.3f", pihat)))
  
  print(plot)
  grDevices::dev.off()
}

#------------------------------------------------------------
a.test.print <- function(theTest, width = 20) {
  test.name <- theTest$method
  cat(test.name, "\n")
  cat(rep("*", nchar(test.name)), "\n", sep = "")
  A <- paste0("H0 : ", names(theTest$null.value), " = ", theTest$null.value, ",")
  B <- paste0("H1 : ", names(theTest$null.value), " ", theTest$alternative,
              " than ", theTest$null.value, ".")
  C <- paste0("stat = ", format(theTest$statistic, digits = 1, nsmall = 3), ",")
  D <- paste0("p-value = ", format(theTest$p.value, digits = 1, nsmall = 3), ".")
  cat(sprintf("%-*s\t%-*s\n", width, A, width, B))
  cat(sprintf("%-*s\t%-*s\n", width, C, width, D))
  cat("\n")
}
