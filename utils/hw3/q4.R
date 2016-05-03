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
# Q2 Plots ----
####
q2.grid <- expand.grid(SenderLooks=sort(unique(df$SenderLooks)),
                       ReceiverLooks=sort(unique(df$ReceiverLooks)))
q2.pr.m <- cbind(q2.grid, SenderGender=factor("male", c("female", "male")))
q2.pr.f <- cbind(q2.grid, SenderGender=factor("female", c("female", "male")))
q2.pr.m$y_hat <- predict(mdl.contact, newdata=q2.pr.m, type="response")
q2.pr.m$pcnt <- (q2.pr.m$y_hat/mean(q2.pr.m$y_hat)-1)*100
q2.pr.f$y_hat <- predict(mdl.contact, newdata=q2.pr.f, type="response")
q2.pr.f$pcnt <- (q2.pr.f$y_hat/mean(q2.pr.f$y_hat)-1)*100

PlotStuff <- function(pr) {
  col.theme <- gg_color_hue(3)
  g <- ggplot(pr, aes(x=SenderLooks, y=ReceiverLooks, fill=pcnt)) + geom_tile() +
    scale_fill_gradient2(sprintf("Pr. Contact Rel.\nto Mean %3.2f", mean(pr$y_hat)),
                         low=col.theme[1], mid="white", high=col.theme[2]) + 
    geom_text(aes(x=SenderLooks, y=ReceiverLooks, label=sprintf("%+3.0f%%", pcnt))) +
    coord_equal() + theme_bw() + 
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
    labs(x="Sender Looks", y="Receiver Looks")
  return(g)
}
GGPlotSave(PlotStuff(q2.pr.m), "Q2_Male_heatmap")
GGPlotSave(PlotStuff(q2.pr.f), "Q2_Female_heatmap")

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
match.prd$pcnt <- (match.prd$score/mean(match.prd$score)-1) * 100

col.theme <- gg_color_hue(3)
g <- ggplot(match.prd, aes(y=MaleLooks, x=FemaleLooks, fill=pcnt)) + 
  geom_tile() + 
  scale_fill_gradient2(sprintf("Match Score Rel.\nto Mean %3.3f", mean(match.prd$score)),
                       low=col.theme[1], mid="white", high=col.theme[2]) + 
  geom_text(aes(x=FemaleLooks, y=MaleLooks, label=sprintf("%+3.0f%%", pcnt))) +
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
