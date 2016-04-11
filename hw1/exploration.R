####
# Setup and Load Data----
####
rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

load("target.rdat")
load("cust.rdat")
load("target2.rdat")
load("cust2.rdat")

####
# Hist ----
####
dat <- subset(cust, spend>0)

cdf.fcn <- ecdf(dat$spend)
df <- data.frame(
  spend=sort(dat$spend),
  cdf=cdf.fcn(sort(dat$spend)))
g <- ggplot(df, aes(x=spend, y=cdf)) + geom_line() + scale_x_log10() +
  theme_bw() + labs(x="log(spend)", y="CDF")
GGPlotSave(g, "cdf")
