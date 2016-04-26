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
# Regression ----
####
mdl.contact <- LoadCacheTagOrRun("q4_regr_bin", function() {
  glm(y ~ ReceiverLooks * SenderLooks * SenderGender, data=df, family="binomial")
})
summary(mdl.contact)


####
# Predict Scores ----
####
# Create a grid of male looks and female looks.
match.prd <- expand.grid(MaleLooks = sort(unique(df$SenderLooks)),
                         FemaleLooks = sort(unique(df$ReceiverLooks)))
require(dplyr)
match.prd$yhat.m <- predict(mdl.contact, type="response", newdata=
                              match.prd %>%
                              select(SenderLooks=MaleLooks, 
                                     ReceiverLooks=FemaleLooks) %>%
                              mutate(SenderGender=factor("male", c("female", "male"))))

match.prd$yhat.f <- predict(mdl.contact, type="response", newdata=
                              match.prd %>%
                              select(SenderLooks=FemaleLooks, 
                                     ReceiverLooks=MaleLooks) %>%
                              mutate(SenderGender=factor("female", c("female", "male"))))
match.prd$score <- sqrt(match.prd$yhat.m * match.prd$yhat.f)

g <- ggplot(match.prd, aes(x=MaleLooks, y=FemaleLooks, fill=score)) + 
  geom_tile() + 
  scale_fill_gradient("Match\nScore") +
  coord_equal() + theme_bw()
GGPlotSave(g, "q4_heat")
