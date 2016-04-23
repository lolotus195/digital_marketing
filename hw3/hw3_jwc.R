rm(list=ls())

require(plyr)
require(ggplot2)
require(reshape2)

source("../utils/source_me.R", chdir = T)
source('HotOrNotPlot.R')
CreateDefaultPlotOpts()

load('hw3.rdat')

# Data notes
# - sender looks: integer rating 1-11 --> how good looking is the sender?
# - receiver looks: integer rating 1-11 --> how good looking is the receiver?
# - sender gender: integer 1/2 --> 1 = woman, 2 = man
# - receiver gender: integer 1/2 --> 1 = woman, 2 = man

# Looks should be numeric (helps for regression)
for (c in grep('Sender|Receiver', names(df)))
  df[,c] <- as.numeric(df[,c])  

# Gender should be a factor variable!
RelevelGender <- function(x) {
  x <- factor(x)
  x <- revalue(x, c('1'='female', '2'='male'))
  return(x)
}
df$SenderGender <- RelevelGender(df$SenderGender)
df$ReceiverGender <- RelevelGender(df$ReceiverGender)

# Q1: What can you say about the preferences of sender men and women ----
#     related to the looks of the receiver? Are there differences across 
#     the genders?

count.male <- matrix(0, nrow = 11, ncol = 11) # who do men target
count.female <- matrix(0, nrow = 11, ncol = 11) # who do women target
for (r in 1:nrow(df)) {
  if(r %% 1e4==0) 
    cat(sprintf('%.2f', r/nrow(df)), ',', sep='')
  if (df$SenderGender[r] == 'female') {
    count.female[df$SenderLooks[r], df$ReceiverLooks[r]] <- 
      count.female[df$SenderLooks[r], df$ReceiverLooks[r]] + 1
  } else {
    count.male[df$SenderLooks[r], df$ReceiverLooks[r]] <- 
      count.male[df$SenderLooks[r], df$ReceiverLooks[r]] + 1
  }
}

# Normalize
base.female <- table(df$ReceiverLooks[df$ReceiverGender=='female']) / 
  sum(df$ReceiverGender=='female')
base.male <- table(df$ReceiverLooks[df$ReceiverGender=='male']) / 
  sum(df$ReceiverGender=='male')

target.female <- apply(count.female, 1, function(r) { 
  r / sum(r) - base.female })
target.female <- t(target.female)

target.male <- apply(count.male, 1, function(r) { 
  r / sum(r) - base.male })
target.male <- t(target.male)

g1 <- PlotTarget(target.female, 'Female')
g2 <- PlotTarget(target.male, 'Male')

# Or we could think about it more quantitatively

# Predict receiver looks as a function of sender looks
# Intercept --> unconditional expectation of receiver looks
# Slope --> if the sender is 1 level hotter, how much hotter is the average receiver?
fit.female <- glm(ReceiverLooks ~ SenderLooks, data = df[df$SenderGender=='female',])
fit.male <- glm(ReceiverLooks ~ SenderLooks, data = df[df$SenderGender=='male',])

coef.female <- coef(fit.female)
coef.male <- coef(fit.male)

# Some suprises! Unconditional expectation of receiver looks is...below average
coef.female['(Intercept)']
coef.male['(Intercept)']

# Women: (on average) send messages to men who are BETTER looking than them
coef.female['(Intercept)'] > mean(df$SenderLooks[df$SenderGender=='female'])

# Men: (on average) send messages to women who are WORSE looking than them
coef.male['(Intercept)'] > mean(df$SenderLooks[df$SenderGender=='male'])

# And the increase in receiver looks as a function of sender looks is very flat
# Surprisingly, females are more quick to "trade up"
coef.female['SenderLooks']
coef.male['SenderLooks']

# Not sure how helpful these plots are but it helps vizualize how flat the 
# regression coefficient is
g3 <- PlotCount(count.female, coef.female, gg_color_hue(3)[3], 'Female')
g4 <- PlotCount(count.male, coef.male, gg_color_hue(3)[1], 'Male')

# Save for writeup
GGPlotSave(g1, 'heatmap_female')
GGPlotSave(g2, 'heatmap_male')
GGPlotSave(g3, 'dotplot_female')
GGPlotSave(g4, 'dotplot_male')

tab.fit <- data.frame(alpha=rep(NA, 2), beta=rep(NA, 2), r2=rep(NA,2))
tab.fit$alpha <- c(coef.female['(Intercept)'], coef.male['(Intercept)'])
tab.fit$beta <- c(coef.female['SenderLooks'], coef.male['SenderLooks'])
tab.fit$r2 <- c(1 - fit.female$deviance / fit.female$null.deviance,
                1 - fit.male$deviance / fit.male$null.deviance)
colnames(tab.fit) <- c('$\\alpha_j$', '$\\beta_j$', 'R-squared')
rownames(tab.fit) <- c('Female senders', 'Male senders')
ExportTable(tab.fit, file='receiver_on_sender', 
            caption='Regressing Receiver Looks on Sender Looks',
            digits=3, display=rep('g', 4))
