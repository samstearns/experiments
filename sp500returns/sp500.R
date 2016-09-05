# Create a boxplot of rolling returns for the S&P: 1, 5, 10, 20, 30 years

# TODO: Use the shift function in the data.table library to simplify code
# http://stackoverflow.com/questions/14689424/use-a-value-from-the-previous-row-in-an-r-data-table-calculation
library(data.table)
library(ggplot2)

setwd("/Users/sjs/Dropbox/dev/git/experiments/sp500returns/")
sp <-read.csv("./sp500.csv")

# Helper function to calculate CAGRs
cagr <- function(end, start, years)
{
  return ((end / start) ** (1 / years) - 1.0)
}

# calculate annual returns
annual <- c()
for (i in 2:89) {
  annual[i-1] = cagr(sp[i,2], sp[i-1,2], 1)
}

# calculate three year returns
three.year <- c()
for (i in 4:89) {
    three.year[i-1] = cagr(sp[i,2], sp[i-3,2], 3)
}

# calculate five year returns
five.year <- c()
for (i in 6:89) {
  five.year[i-1] = cagr(sp[i,2], sp[i-5,2], 5)
}

# calculate 10 year returns
ten.year <- c()
for (i in 11:89) {
  ten.year[i-1] = cagr(sp[i,2], sp[i-10,2], 10)
}

# calculate 20 year returns
twenty.year <- c()
for (i in 21:89) {
  twenty.year[i-1] = cagr(sp[i,2], sp[i-20,2], 20)
}

# calculate 30 year returns
thirty.year <- c()
for (i in 31:89) {
  thirty.year[i-1] = cagr(sp[i,2], sp[i-30,2], 30)
}

boxplot(annual, three.year, five.year, ten.year, twenty.year, thirty.year, horizontal = TRUE)

title(main="S&P 500 Returns", sub="Distribution of rolling returns, excluding dividends", xlab = "Annual Return", 
      ylab = c("1 Year", "3 Year", "5 Year", "10 Year", "20 Year", "30 Year"))