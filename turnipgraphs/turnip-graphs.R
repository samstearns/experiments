# This file shows an example of how to great "Turnip Graphs" using ggplot
# Example of an Turnip Graph: http://www.dartmouthatlas.org/data/topic/topic.aspx?cat=22
# More information is available: http://docs.ggplot2.org/0.9.3/geom_dotplot.html
library(ggplot2)

# Load a sample data file on MSSP ACOs
# Downloaded from: https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/Overview.html
setwd("/Users/sjs/Dropbox/dev/git/experiments/turnipgraphs/")
aco <-read.csv("./Final.ACO.SSP.PUF.Y2013.csv")

# This function sets up turnip charts with standard formatting. Following links have references
# http://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
# http://stackoverflow.com/questions/15458526/r-pass-variable-column-indices-to-ggplot2
drawTurnipGraph <- function(input.data,  y_string, title) {
  ggplot(aco, aes_string(x = "1", y = y_string)) +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.75) + 
    ggtitle(title) + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) + 
    labs(y = "Rate per 1,000") + 
    theme(axis.text.x = element_blank())
}

# Example outputs
drawTurnipGraph(aco, "P_SNF_ADM", "SNF Utilization by ACO, \n2013 Performance Year")
drawTurnipGraph(aco, "ADM_S_Trm", "Short Term Inpatient Utilization by ACO, \n2013 Performance Year")