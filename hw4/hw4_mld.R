# Digital and Algorithmic Marketing
# Instructor: Sanjog Misra
# Homework #4 (Group)

####
# Setup ----
####
rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()
source("descriptions.R")
require(plyr)
require(dplyr)

# The persado_experiment.xlsx file contains a real experiment run by Persado
# Use the data to answer the following questions.
dat <- read.csv("persado_data.csv")
colnames(dat) <- gsub("\\.AF8\\.", "_", colnames(dat))
dat$unique_received <- dat$unique_sent - dat$bounced
dat.ctrl <- dat[17,]
dat <- dat[-17,]

meas.cols <- c('unique_sent', 'bounced', 'unique_opened', 
               'unique_clicks', 'unique_received')
exp.cols <- c('intro', 'headline', 'main_text', 'button', 
              'action', 'purpose', 'symbol')
for (col in exp.cols) {
  dat[,col] <- factor(dat[,col])
}
desc <- LoadDescriptions(dat)
TestCreateMessageStrings(dat, desc)

# dat <- cbind(
#   dat[, meas.cols],
#   desc[, exp.cols]
# )


####
# Q1: ----
# Assuming that each of all relevant variables were tested how many possible 
#		message combinations are there?
####
levs <- sapply(dat[, exp.cols], nlevels)
print(sprintf("Levels %s = %d", 
              paste(levs, collapse = " x "), 
              prod(levs)))


####
# Q2: ----
# Estimate two logit models based on the data (assuming a simple linear 
#		specification) - one for opens and another for clicks. Discuss your results based
#		on the nature of the variables (see variable names and descriptions tab).
####
generic.fo <- as.formula(paste("~ ", paste(exp.cols, collapse=" + ")))

# Calculate some rates.
mdl.opened <- glm(
  update.formula(generic.fo, 
                 cbind(unique_opened, unique_received - unique_opened) ~ .), 
  data=dat, family="binomial")

mdl.clicks <- glm(
  update.formula(generic.fo, 
                 cbind(unique_clicks, unique_received - unique_clicks) ~ .),
  data=dat, family="binomial")

summary(mdl.clicks)
summary(mdl.opened)

####
# Q3: ----
# Use the estimated models to compute predicted probabilities for all possible message
# 		combinations (separate predictions for opens and clicks). 
#		Which messages have the highest fitted response probabilities? 
#		Are the two messages similar or different? Discuss.
####
all.combinations <- expand.grid(lapply(dat[,exp.cols], levels))
all.combinations$p.opened_rate <- predict(
  mdl.opened, newdata = all.combinations, type="response")
all.combinations$p.clicks_rate <- predict(
  mdl.clicks, newdata = all.combinations, type="response")

####
# Q3 Histogram Plot ----
####
dat.ctrl.melt <- data.frame(
  variable=c("p.opened_rate", "p.clicks_rate"),
  rate=c(dat.ctrl$unique_opened, dat.ctrl$unique_clicks) / 
    dat.ctrl$unique_received)
combi.melt <-melt(all.combinations, measure.vars=c("p.opened_rate", "p.clicks_rate"))

measure.labels <- c("p.opened_rate" = "Predicted Open Rate",
                    "p.clicks_rate" = "Predicted Click Rate")
g <- ggplot(combi.melt, aes(x=value)) + geom_histogram(bins=30) +
  geom_vline(data=dat.ctrl.melt, aes(xintercept=rate, color="Control"), lty=2) +
  facet_wrap(~ variable, scales = "free_x",
             labeller = labeller("variable"=measure.labels)) +
  scale_color_discrete("") +
  labs(x="Probability", y="Count") +
  theme(legend.position="bottom")
GGPlotSave(g, "q3_hist")

####
# Q3 - TopN ----
###
CalculatePercentile <- function(x) { rank(x)/length(x)*100 }
all.combinations$p.opened_pctile <- CalculatePercentile(all.combinations$p.opened_rate)
all.combinations$p.clicks_pctile <- CalculatePercentile(all.combinations$p.clicks_rate)

topN <- 1
opened.topN <- order(all.combinations$p.opened_rate, decreasing = T)[1:topN]
clicks.topN <- order(all.combinations$p.clicks_rate, decreasing = T)[1:topN]
intersect(opened.topN, clicks.topN)

all.combinations[opened.topN,]
all.combinations[clicks.topN,]

CreateMessageStrings(rbind(all.combinations[opened.topN,],
                           all.combinations[clicks.topN,]), desc)

GetMessageLevels(rbind(all.combinations[opened.topN,],
                       all.combinations[clicks.topN,]), desc)

####
# Q4: ----
# Please use the historical data provided in the project section of chalk to provide 
#		me your experimental design. See HistData.R for details.	  
####