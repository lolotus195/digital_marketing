rm(list = ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()
####
# Level Info --------------------------------------------------------------
####
all.levels <- list(V1=1:6,
                   V2=1:6,
                   V3=1:3,
                   V4=1:3,
                   V5=1:5,
                   V6=1:4,
                   V7=1:2,
                   V8=1:5,
                   V9=1:6)
exp.levels <- list(V1=c(4, 5, 6),
                   V2=c(1, 3, 6),
                   V3=1,
                   V4=c(1, 2, 3),
                   V5=c(1, 5),
                   V6=c(1, 3),
                   V7=c(1, 2),
                   V8=c(2, 3, 4, 5),
                   V9=c(2, 4, 6))

RelevelData <- function(df, level.list) {
  for (colname in names(level.list)) {
    df[,colname] <- factor(df[,colname], levels=level.list[[colname]])
  }
  return(df)
}

####
# Load Data ----
####
dat <- RelevelData(read.csv('experiment_results.csv'), exp.levels)
dat$X <- NULL


####
# Formula We Submitted ----------------------------------------------------
####
base.formula <- formula(~ I(V1 == 4) + I(V2 == 6) + I(V4 == 1) + 
                          I(V4 == 3) + I(V5 == 5) + I(V6 == 3) + I(V7 == 1) + I(V7 == 2) + 
                          I(V8 == 2) + I(V8 == 3) + I(V8 == 4) + I(V8 == 5) + I(V9 == 2) + 
                          I(V9 == 4) + I(V9 == 6) + I(V1 == 5):I(V2 == 3) + I(V2 == 3):I(V1 == 6))
form <- update(base.formula, cbind(Clicks, N - Clicks) ~ .)
reg <- glm(form, dat, family = 'binomial')

####
# Formula w/No Singularities ----
####
dat.test <- RelevelData(expand.grid(V1=c(4,5,6),
                                    V2=c(1,3,6),
                                    V3=1,
                                    V4=c(1,2,3),
                                    V5=c(1,5),
                                    V6=c(1,3),
                                    V7=c(1,2),
                                    V8=c(2,3,4,5),
                                    V9=c(2,4,6)), exp.levels)

mdl <- glm(cbind(Clicks, N-Clicks) ~ V1 + V2 + V4 + V5 + V6 + V7 + I(V8 == 4) + V9 + V1:V2, 
           dat, family="binomial")
summary(mdl)
dat.test$p_click_rate <- predict(mdl, dat.test, type="response")
dat.test[order(dat.test$p_click_rate, decreasing = T)[1:10],]

ggplot(dat.test, aes(x=p_click_rate)) +
  geom_histogram(bins=30) + 
  geom_vline(data=dat, aes(xintercept=Clicks/N), lty=2, color='red')
