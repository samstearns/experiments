# Create a boxplot of rolling returns for the S&P: 1, 5, 10, 20, 30 years

# TODO: Use the shift function in the data.table library to simplify code
# http://stackoverflow.com/questions/14689424/use-a-value-from-the-previous-row-in-an-r-data-table-calculation
library(data.table)
library(ggplot2)

setwd("/Users/sjs/Dropbox/dev/git/experiments/sp500returns/")
sp <-read.csv("./sp500.csv")

CalculateCAGR <- function(end, start, years) {
  # Helper function to calculate CAGRs
  
  return ((end / start) ** (1 / years) - 1.0)
}

CalculateRollingReturns <- function(start, end, lookback) {
  # Helper function to calculate rolling retuns
  
  annual.returns <-c()
  for (i in start:end) {
    annual.returns[i] = CalculateCAGR(sp[i,2], sp[i-lookback,2], lookback)
  }
  
  return(annual.returns)
}

# Calculate Rolling returns
result.frame = as.data.frame(matrix(ncol=6, nrow=89))
names(result.frame) = c("One Year", "3 Year", "5 Year", "10 Year", "20 Year", "30 Year")
result.frame[["One Year"]] <- CalculateRollingReturns(31,89, 1)
result.frame[["3 Year"]] <- CalculateRollingReturns(31,89, 3)
result.frame[["5 Year"]] <- CalculateRollingReturns(31,89, 5)
result.frame[["10 Year"]] <- CalculateRollingReturns(31,89, 10)
result.frame[["20 Year"]] <- CalculateRollingReturns(31,89, 20)
result.frame[["30 Year"]] <- CalculateRollingReturns(31,89, 30)

# GG Plot
p <- ggplot(result.frame) 
p + geom_boxplot() + coord_flip()

# Standard plot
boxplot(result.frame, horizontal = TRUE)
title(main="S&P 500 Returns", sub="Distribution of rolling returns, excluding dividends", xlab = "Annual Return")