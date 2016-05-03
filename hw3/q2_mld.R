####
# Setup ----
####

rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

require(plyr)

load("hw3.rdat")
# Gender should be a factor variable!
RelevelGender <- function(x) {
  x <- factor(x)
  x <- revalue(x, c('1'='female', '2'='male'))
  return(x)
}
df$SenderGender <- RelevelGender(df$SenderGender)
df$ReceiverGender <- RelevelGender(df$ReceiverGender)


####
# Q2: Does the utility/preference function change depending on the  ----
#     looks of the Sender? Are there differences in how these changes for men and women?
####
mdl <- glm(y ~ SenderLooks * SenderGender, data=df, family="binomial")
