####
# Setup ----
####

rm(list=ls())

require(plyr)
require(ggplot2)

source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

# loads histdat into global environment
load('../hw4/Historical_Data.rdat')

beta.list <- sapply(histdat, function(df) {
  logit <- glm(cbind(Unique_Clicks, Unique_Sent-Unique_Clicks) ~ .,
               data=df, family="binomial")
  beta <- coef(logit)[-1]
  return(beta)
})
beta.df <- ldly(beta.list, rbind)

g <- qplot(beta.df$V12, bins = 40, na.rm = T) + 
  geom_vline(xintercept = mean(beta.df$V12, na.rm = T), linetype = 'dashed', color = 'red', size = 1.5) + 
  labs(x = 'coefficeint on beta 1(2)', y = 'frequency') + 
  ggtitle('a histogram of one of our coefficients')
plot(g)

GGPlotSave(g, 'test_histogram')
