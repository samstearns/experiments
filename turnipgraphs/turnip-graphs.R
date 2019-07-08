# This file shows an example of how to great "Turnip Graphs" using ggplot
# Example of an Turnip Graph: http://www.dartmouthatlas.org/data/topic/topic.aspx?cat=22
# More information is available: http://docs.ggplot2.org/0.9.3/geom_dotplot.html
library(ggplot2)

# Load a sample data file on MSSP ACOs
# Downloaded from: https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/Overview.html
library(mssp)
aco <- load_puf_file(2013)

# This function sets up turnip charts with standard formatting. Following links have references
# http://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
# http://stackoverflow.com/questions/15458526/r-pass-variable-column-indices-to-ggplot2
draw_turnip_graph <- function(input.data,  y_string, title, y_axis_title) {
  ggplot(input.data, aes_string(x = "1", y = y_string)) +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.75) + 
    ggtitle(title) + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) + 
    labs(y = y_axis_title) + 
    theme(axis.text.x = element_blank())
}

# Example outputs
draw_turnip_graph(aco, "P_SNF_ADM", "SNF Utilization by ACO, \n2013 Performance Year", "Rate per 1,000")
draw_turnip_graph(aco, "ADM_S_Trm", "Short Term Inpatient Utilization by ACO, \n2013 Performance Year", "Rate per 1,000")