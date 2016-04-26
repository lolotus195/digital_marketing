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

col.theme <- gg_color_hue(3)
g <- ggplot(match.prd, aes(y=MaleLooks, x=FemaleLooks, fill=score)) + 
  geom_tile() + 
  scale_fill_gradient("Match\nScore", low=col.theme[1], high=col.theme[2]) + 
  geom_text(aes(x=FemaleLooks, y=MaleLooks, label=sprintf("%3.2f", score))) +
  coord_equal() + theme_bw() + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  labs(y="Male Looks", x="Female Looks")
GGPlotSave(g, "q4_heat")


####
# Boxplot ----
####
df.test <- df
df.test$y_hat <- predict(mdl.contact, newdata = df, type="response")

g <- ggplot(data=df.test, aes(y, y_hat)) + geom_boxplot() + 
  facet_wrap(~ SenderGender, labeller = labeller(
    "SenderGender"=c("female"="Female Sender", "male"="Male Sender"))) +
  theme_bw() + labs(x="Contact Made", y="Predicted Probability Contact")
GGPlotSave(g, "q3_boxplot")

####
# Alternate ----
####
df %>%
  group_by(SenderGender,SenderLooks,ReceiverLooks) %>%
  summarize(avg_y=mean(y)) -> df.tab
df.tab %>%
  filter(SenderGender=="male") %>%
  rename(MaleLooks=SenderLooks, FemaleLooks=ReceiverLooks) -> df.tab.m
df.tab %>%
  filter(SenderGender=="female") %>%
  rename(FemaleLooks=SenderLooks, MaleLooks=ReceiverLooks) -> df.tab.f
merge(df.tab.m, df.tab.f, by=c("MaleLooks", "FemaleLooks")) %>%
  mutate(score = sqrt(avg_y.x * avg_y.y),
         MaleLooks = as.numeric(MaleLooks),
         FemaleLooks = as.numeric(FemaleLooks)) %>%
  select(MaleLooks, FemaleLooks, score) -> df.score

glm(score ~ ., data=df.score)
ggplot(df.score, aes(x=FemaleLooks, y=MaleLooks, fill=score)) + geom_tile()
