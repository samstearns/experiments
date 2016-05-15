# from: http://docs.ggplot2.org/0.9.3/geom_dotplot.html
# use ggplot2
library(ggplot2)

# Load the MSSP PUF File
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/Overview.html
setwd("/Users/sjs1/Dropbox/rstats/Onion")
aco <-read.csv("SSP_ACO_PUF/Final.ACO.SSP.PUF.Y2013.csv")

# Example of an onion chart: http://www.dartmouthatlas.org/data/topic/topic.aspx?cat=22
# Onion chart for SNF Usage
#http://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
# http://stackoverflow.com/questions/15458526/r-pass-variable-column-indices-to-ggplot2
drawOnionChart <- function(input.data,  y_string, title) {
  ggplot(aco, aes_string(x = "1", y = y_string)) +
        geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.75) + 
    ggtitle(title) + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) + labs(y = "Rate per 1,000") + theme(axis.text.x = element_blank())
}

drawOnionChart(aco, "P_SNF_ADM", "SNF Utilization by ACO, \n2013 Performance Year")
drawOnionChart(aco, "ADM_S_Trm", "Short Term Inpatient Utilization by ACO, \n2013 Performance Year")


# Onion chart for SNF Utilization
ggplot(aco, aes(x = 1, y = P_SNF_ADM)) + 
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.75) + 
  ggtitle("SNF Utilization by ACO, \n2013 Performance Year") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + 
  labs(y = "Rate per 1,000") + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())

# Onion chart for Short Term Inpatient Usage
ggplot(aco, aes(x = 1, y = ADM_S_Trm)) + 
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.75) + 
  ggtitle("Short Term Inpatient Utilization by ACO, \n2013 Performance Year") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + 
  labs(y = "Rate per 1,000") + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())

# Test of a bubble chart
# http://www.matthewmaenner.com/blog/2010/11/23/how-to-make-bubble-charts-in-ggplot2/
ggplot(aco, aes(x=ADM, y=Per_Capita_Exp_ALL_AGND, size=N_AB_Year),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
  scale_x_continuous(name="Admisions per 1,000")+
  scale_y_continuous(name="Expenditures")+
  geom_text(size=4)+
  theme_bw()


##################

# Example 1
ggplot(mtcars, aes(x = mpg)) + geom_dotplot()

# Example 2
ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5)

# Example 3 - Use fixed-width bins
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(method="histodot", binwidth = 1.5)

# Some other stacking methods
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, stackdir = "center")


# Examples with stacking along y axis instead of x
ggplot(mtcars, aes(x = 1, y = mpg)) + geom_dotplot(binaxis = "y", stackdir = "center")

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_dotplot(binaxis = "y", stackdir = "center")

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_dotplot(binaxis = "y", stackdir = "centerwhole")


# binpositions="all" ensures that the bins are aligned between groups
ggplot(mtcars, aes(x = factor(am), y = mpg)) +   geom_dotplot(binaxis = "y", stackdir = "center", binpositions="all")
