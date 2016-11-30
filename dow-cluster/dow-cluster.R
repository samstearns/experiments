# hierarchical clustering of the dow jones stocks
library(quantmod)

# load daily pricing data
# From: http://money.cnn.com/data/dow30/
dow.tickers <- c("MMM", "AXP", "AAPL", "BA", "CAT", 
                 "CVX", "CSCO", "KO", "DIS", "DD", 
                 "XOM", "GE", "GS", "HD", "IBM", 
                 "INTC", "JNJ", "JPM", "MCD", "MRK",
                 "MSFT", "NKE","PFE","PG", "TRV", 
                 "UTX", "UNH", "VZ", "V", "WMT")

getSymbols(dow.tickers)

# load daily returns for 2015 in a dataframe
returns <- data.frame(matrix(ncol = 30, nrow = 252))
names(returns) <- dow.tickers

for (i in 1:length(dow.tickers)) {
  ticker <- get(dow.tickers[i])
  returns[,i] <- periodReturn(ticker, period="daily", subset='2015')
}

# The distance matrix determines similarity between rows. 
# Transpose the values which are stored in a column per ticker
dist.matrix.input <- t(as.matrix(returns))

# run clustering and plot results
clust.complete <- hclust(dist(dist.matrix.input), method="complete")
plot(clust.complete, main="Complete Linkage")

# plot as a heatmap
heatmap(t(as.matrix(returns)), Colv = NA)