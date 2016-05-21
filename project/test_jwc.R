rm(list = ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

require(glmnet)

####
# Level Info --------------------------------------------------------------
####
all.levels <- list(V1=1:6,V2=1:6,V3=1:3,V4=1:3,
                   V5=1:5,V6=1:4,V7=1:2,V8=1:5,V9=1:6)
exp.levels <- list(V1=c(4,5,6),V2=c(1,3,6),V3=1,V4=c(1,2,3),
                   V5=c(1,5),V6=c(1,3),V7=c(1,2),V8=c(2,3,4,5),V9=c(2,4,6))

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

# Model based on historical data
load('../hw4/cached/q4_glmnet_cv_it2.dat')
hist.mdl <- data
rm(data)

hist.coefs <- drop(coef(hist.mdl))
hist.coefs <- hist.coefs[hist.coefs != 0]

####
# Fit historical model onto new data ----
####

mm <- model.matrix(~.+.^2, data=dat[,paste('V',c(1,2,4,5,6,7,8,9),sep='')])
mm <- mm[, which(colnames(mm) %in% names(hist.coefs))]
mm <- mm[,-1] # drop intercept

dat.est <- cbind(dat$Clicks / dat$N, mm)
colnames(dat.est)[1] <- c('click_rate')
dat.est <- as.data.frame(dat.est)
mdl <- glm(click_rate ~ ., data = dat.est, weights = dat$N, family = 'binomial')
summary(mdl)

# Predict with updated model
dat.all <- expand.grid(exp.levels)
dat.all <- apply(dat.all, 2, as.factor)
dat.all <- as.data.frame(dat.all)

dat.all <- dat.all[,-3] # throw out level 3
mm.all <- model.matrix(~.+.^2, data=dat.all)
mm.all <- mm.all[, which(colnames(mm.all) %in% names(hist.coefs))]
# colnames(mm.all) <- paste('mm.',colnames(mm.all), sep='')
mm.all <- mm.all[,-1]

pred <- predict(mdl, newdata = as.data.frame(mm.all), type = 'response', se.fit=T)
max(pred$fit + pred$se.fit*qnorm(.975)) # best case is 13.5 percent
