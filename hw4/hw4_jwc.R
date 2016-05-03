####
# Setup ----
####

rm(list=ls())

library(plyr)
library(AlgDesign)

source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

dat <- read.csv('persado_data.csv') # row 17 is control message
desc <- read.csv('persado_descriptions.csv')

# cleanup
names(dat) <- gsub('.AF8.', '_', names(dat)) # maybe this is an Open Office thing?
dat$start_date <- as.Date(dat$start_date, format = '%d/%m/%Y')
dat$end_date <- as.Date(dat$end_date, format = '%d/%m/%Y')

# we are really interested in open rates and click rates
dat$open_rate <- dat$unique_opened / (dat$unique_sent - dat$bounced)
dat$click_rate <- dat$unique_clicks / (dat$unique_sent - dat$bounced)
dat$received <- dat$unique_sent - dat$bounced

# things are easier if we drop the control message from dat
# I'll just store the original in dat.full
dat.full <- dat
dat <- dat[1:(nrow(dat)-1),]

# but now we need to re-level all the factors to take out the control "level"
for (c in 1:ncol(dat)) {
  if (is.factor(dat[,c]))
    dat[,c] <- as.factor(as.character(dat[,c]))
}

# ----
# Q1: Assuming that each all relevant variables were tested how many possible 
#	message combinations are there? 
# ----

# which vars are we testing
test.vars <- c('intro', 'headline', 'main_text', 'button', 
               'action', 'purpose', 'symbol')

# count levels
var.levels <- sapply(test.vars, function(v) { nlevels(dat[,v]) })

# 1024 possible message combinations
prod(var.levels)

# ----
# Q2: Estimate two logit models based on the data (assuming a simple linear 
#	specification) - one for opens and another for clicks. Discuss your results 
# based on the nature of the variables (see variable names and descriptions 
# tab).
# ----

form.open <- as.formula(paste('open_rate ~', paste(test.vars, collapse = ' + ')))
fit.open <- glm(form.open, weights = dat$received, family = 'binomial', data = dat)

form.click <- as.formula(paste('click_rate ~', paste(test.vars, collapse = ' + ')))
fit.click <- glm(form.click, weights = dat$received, family = 'binomial', data = dat)

# interpret coefficients
# note that the coefficeints on click tend to be more extreme than those on open
# so we can affect the open rate a little bit but the click rate relatively more

Bopen <- coef(fit.open)[-1]
prob.open <- 1 / (1 + exp(-Bopen))
prob.open <- prob.open[order(prob.open, decreasing = T)]

Bclick <- coef(fit.click)[-1]
prob.click <- 1 / (1 + exp(-Bclick))
prob.click <- prob.click[order(prob.click, decreasing = T)]

# Results
tab.res <- data.frame(coef.open = coef(fit.open)[-1],
                      prob.open = 1 / (1 + exp(-coef(fit.open)[-1])),
                      coef.click = coef(fit.click)[-1],
                      prob.click = 1 / (1 + exp(-coef(fit.click)[-1])))
rownames(tab.res) <- gsub('L', ':L', rownames(tab.res))
rownames(tab.res) <- gsub('_', '\\\\_', rownames(tab.res))
rownames(tab.res) <- paste('\\textsf{', rownames(tab.res), 
                           '}', sep = '')
colnames(tab.res) <- c('$\\beta_j^{\\text{open}}$', '$Pr(\\text{open}|j)$', 
                       '$\\beta_j^{\\text{click}}$', '$Pr(\\text{click}|j)$')
ExportTable(tab.res, 'logit_results', 'Logistic Regression Coefficients',
            digits = 3, display = rep('g', 5))

# ----
# Q3: Use the estimated models to compute predicted probabilities for all 
# possible message combinations (separate predictions for opens and clicks). 
# Which messages have the highes fitted response probabilities?  Are the two 
# messages similar or different? Discuss.
# ----

dat.new <- gen.factorial(levels = var.levels,
                         varNames = test.vars,
                         factors = "all") 
for (c in 1:ncol(dat.new)) {
  dat.new[,c] <- mapvalues(dat.new[,c], 
                             from = c('1', '2', '3', '4'), 
                             to = c('L1', 'L2', 'L3', 'L4'))
}

pred.open <- predict(fit.open, dat.new, type = 'response')
pred.click <- predict(fit.click, dat.new, type = 'response')

# control values
control.open <- dat.full$open_rate[nrow(dat.full)]
control.click <- dat.full$click_rate[nrow(dat.full)]

g1 <- qplot(pred.open, bins = 50) +
  geom_vline(xintercept = control.open, linetype = 'dotted')
plot(g1)

g2 <- qplot(pred.click, bins = 50) +
  geom_vline(xintercept = control.click, linetype = 'dotted')
plot(g2)

# ----
# Q4: Please use the historical data provided in the project section of chalk 
# to provide me your experimental design. See HistData.R for details.	  
# ----

