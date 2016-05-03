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
df$y <- df$y == 1

####
# q1 with holdout ----
####
indices <- sample.int(nrow(df))
train.idx <- indices[1:floor(nrow(df)*0.66)]
test.idx <- indices[floor(nrow(df)*0.66+1):nrow(df)]

mdl <- glm(y ~ ReceiverLooks*SenderGender, data=df, subset=train.idx, family="binomial")
df.test <- df[test.idx,]
df.test$y_hat <- predict(mdl, newdata = df.test, type="response")

g <- ggplot(data=df.test, aes(y, y_hat)) + geom_boxplot() + 
  facet_wrap("SenderGender", labeller = labeller(
    "SenderGender"=c("female"="Female Sender", "male"="Male Sender"))) +
  theme_bw() + labs(x="Contact Made", y="Predicted Probability of Contact")
GGPlotSave(g, "q1_contact_box")
