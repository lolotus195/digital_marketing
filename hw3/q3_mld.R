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
# Q3: Using the pred.match function explore the match scores between ----
#     men and women at various looks percentiles. Comment on and 
#     explain your findings. For example, you may want to explore 
#     why pred.match(2,10) differs from pred.match(10,2).    
mdl <- LoadCacheTagOrRun("q3_regr_bin", function() {
  glm(y ~ ReceiverLooks * SenderGender, data=df, family="binomial")
})
summary(mdl)

####
# Predict and Make heatmap ----
####
match.prd <- expand.grid(MaleLooks=sort(unique(df$SenderLooks)),
                         FemaleLooks=sort(unique(df$SenderLooks)))
require(dplyr)
match.prd$yhat.m <- predict(mdl, newdata=
                              match.prd %>% 
                              select(SenderLooks=MaleLooks, 
                                     ReceiverLooks=FemaleLooks) %>%
                              mutate(SenderGender=factor("male", c("female", "male"))),
                            type="response")
match.prd$yhat.f <- predict(mdl, newdata=
                              match.prd %>% 
                              select(SenderLooks=FemaleLooks, 
                                     ReceiverLooks=MaleLooks) %>%
                              mutate(SenderGender=factor("female", c("female", "male"))),
                            type="response")
match.prd$score <- sqrt(match.prd$yhat.m * match.prd$yhat.f)

col.theme <- gg_color_hue(3)
g <- ggplot(match.prd,aes(x=FemaleLooks, y=MaleLooks, fill=score)) + 
  geom_tile() +
  scale_fill_gradient("Match\nScore", low=col.theme[1], high=col.theme[2]) + 
  geom_text(aes(x=FemaleLooks, y=MaleLooks, label=sprintf("%3.2f", score))) +
  coord_equal() + theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  labs(x="Female Looks", y="Male Looks")
GGPlotSave(g, "q3_score_heatmap")
