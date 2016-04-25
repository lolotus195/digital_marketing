####
# Setup ----
####

rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

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
# Q3: Using the pred.match function explore the match scores between ----
#     men and women at various looks percentiles. Comment on and 
#     explain your findings. For example, you may want to explore 
#     why pred.match(2,10) differs from pred.match(10,2).    
mdl <- glm(y ~ ReceiverLooks * SenderGender, data=df)

summary(mdl)

PredictedMatch <- function(maleLooks, femaleLooks) {
  pred.match.female <- predict(
    mdl, newdata=data.frame(SenderGender="female", 
                            ReceiverLooks=factor(maleLooks, 1:11)), 
    type="response")
  pred.match.male <- predict(
    mdl, newdata=data.frame(SenderGender="male", 
                            ReceiverLooks=factor(femaleLooks, 1:11)), 
    type="response")
  return(sqrt(pred.match.female*pred.match.male))
}

match.scores <- matrix(nrow=11, ncol=11, dimnames = list(c(1:11), c(1:11)))
for (maleLooks in as.numeric(colnames(pred.match.scores))) {
  for (femaleLooks in as.numeric(rownames(pred.match.scores))) {
    match.scores[femaleLooks, maleLooks] <- PredictedMatch(maleLooks, femaleLooks)
  }
}

match.scores.melt <- melt(match.scores, varnames = c("FemaleLooks", "MaleLooks"))
g <- ggplot(match.scores.melt, aes(x=FemaleLooks, y=MaleLooks, fill=value)) + 
  geom_tile() +
  scale_fill_continuous("Match Score") + 
  coord_equal(xlim = c(1, 11), ylim=c(1, 11)) +
  scale_y_continuous(breaks=1:11) + scale_x_continuous(breaks=1:11) +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  labs(x="Female Looks", y="Male Looks")
GGPlotSave(g, "q3_score_heatmap")
