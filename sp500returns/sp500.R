
# Load Libraries
library(ggplot2)
library(scales)
library(reshape2)

# Load data
setwd("/Users/sjs/Dropbox/dev/git/experiments/sp500returns/")
sp <-read.csv("./sp500.csv")

# Helper functions to calculate rolling returns
CalculateCAGR <- function(end, start, years) {
  # Helper function to calculate CAGRs
  return ((end / start) ** (1 / years) - 1.0)
}

CalculateRollingReturns <- function(start, end, lookback) {
  # Helper function to calculate rolling retuns
  
  annual.returns <-c()
  for (i in start:end) {
    annual.returns[i-start] = CalculateCAGR(sp[i,2], sp[i-lookback,2], lookback)
  }
  
  return(annual.returns)
}

# Calculate Rolling returns
result.frame = as.data.frame(matrix(ncol=7, nrow=58))
names(result.frame) = c("Year", "One Year", "3 Year", "5 Year", "10 Year", "20 Year", "30 Year")
result.frame[["Year"]] <- sp[32:89, 1]
result.frame[["One Year"]] <- CalculateRollingReturns(31,89, 1)
result.frame[["3 Year"]] <- CalculateRollingReturns(31,89, 3)
result.frame[["5 Year"]] <- CalculateRollingReturns(31,89, 5)
result.frame[["10 Year"]] <- CalculateRollingReturns(31,89, 10)
result.frame[["20 Year"]] <- CalculateRollingReturns(31,89, 20)
result.frame[["30 Year"]] <- CalculateRollingReturns(31,89, 30)

# Create a boxplot of rolling returns for the S&P: 1, 5, 10, 20, 30 years
# Use levels as factor to order the columns
result.frame.excl.year <- result.frame[, 2:7]
p <- ggplot(stack(result.frame.excl.year), aes(x = factor(ind, levels = names(result.frame)), y = values))
p + geom_boxplot() + labs(title = "S&P 500 Rolling Returns: 1958-2015\nDistribution of rolling returns, excluding dividends", x="", y="") + scale_y_continuous(labels = percent)

# Plot returns over time. Melt the dataset for input to geom_line chart
dfdata <- melt(result.frame, id=c("Year"))
colnames(dfdata) <- c("Year", "Period", "Return")
r <- ggplot(dfdata, aes(x=Year, y=Return, group=Period)) 
r + geom_line(aes(colour = Period)) + labs(title = "S&P 500 Rolling Returns: 1958-2015\nRolling returns, excluding dividends", x="", y="") + scale_y_continuous(labels = percent)