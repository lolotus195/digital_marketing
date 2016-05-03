####
# Setup -------------------------------------------------------------------
####
rm(list=ls())

source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

require(plyr)
require(dplyr)
require(AlgDesign)
require(glmnet)
require(gamlr)

# load data
load(file = "Historical_Data.rdat")
# Loads a list called histdat
# The list contains 318 elements
length(histdat)

####
# Relevel and combine history ---------------------------------------------
####
exp.cols <- sprintf("V%d", seq(1:9))
VerifyStuff <- function(histdata) {
  res <- ldply(histdat, function(d) {
    dat <- as.data.frame(matrix(nrow=1, ncol=length(exp.cols)))
    
    for (col in exp.cols) {
      if (col %in% colnames(d)) {
        dat[1,col] <- nlevels(d[,col])
      } else {
        dat[1,col] <- NA
      }
    }
    return(dat)
  })
  if (!all(1 == sapply(res, function(x) { length(table(x)) }))) {
    stop("something's seriously wrong, some columns have multiple levels")
  }
  
#   return(unlist(res[complete.cases(res),][1,]))
  return(res[complete.cases(res),])
}

RelevelHistDat <- function(histdat, histdat.levels) {
  lapply(histdat, function(d) {
    dat <- as.data.frame(matrix(nrow=nrow(d), ncol=length(exp.cols)))
    
    for (col in exp.cols) {
      if (col %in% colnames(d)) {
        dat[,col] <- factor(d[,col], levels=0:histdat.levels[col])
      } else {
        dat[,col] <- factor(rep(0, nrow(d)), levels=0:histdat.levels[col])
      }
    }
    cbind(dat, d[,c("Unique_Clicks", "Unique_Sent")])
  })
}
histdat.complete <- VerifyStuff(histdat)
histdat.complete.cases <- ldply(as.numeric(rownames(histdat.complete)), 
                                function(x) histdat[[x]])
histdat.levels <- unlist(histdat.complete[1,])
histdat.releveled <- RelevelHistDat(histdat, histdat.levels)
histdat.all <- ldply(histdat.releveled, function(x) x)

g <- qplot(Unique_Clicks/Unique_Sent, data=histdat.all, bins=30)
GGPlotSave(g, "q4_emp_hist")


####
# Create Binomial Model (GLM) ---------------------------------------------
####
mdl.glm <- glm(cbind(Unique_Clicks, Unique_Sent-Unique_Clicks) ~ .,
               data=histdat.all, family="binomial")
summary(mdl.glm)


####
# Create Interaction Model (cv.gamlr) -------------------------------------
####

GetModelFrame <- function(data, .exp.cols=exp.cols) {
  fo <- as.formula(paste("~ ", paste(.exp.cols, collapse="+")))
  model.frame(fo, data=data)
}

# Be Careful, only modify this formula to deal with interactions.
formula.interact <- formula(~ . + .^2 - 1)
mdl.cv.it <- LoadCacheTagOrRun("q4_cv_it", function() {
  mm.it <- model.matrix(formula.interact, data=GetModelFrame(histdat.all))
  cv.gamlr(mm.it, histdat.all$Unique_Clicks/histdat.all$Unique_Sent,
           family="binomial", verb=T, lambda.min.ratio=exp(-8),
           nfold = 20)
})
plot(mdl.cv.it)

sum(coef(mdl.cv.it, select = "1se") > 0)
sum(coef(mdl.cv.it, select = "min") > 0)


####
# Use GLMNET --------------------------------------------------------------
####

mdl.net.cv.it <- LoadCacheTagOrRun("q4_glmnet_cv_it", function() {
  # Double the # of rows in histdat.all
  # Put number of successes in first part, number of failures in second
  # Use these counts as weights in glmnet binomial.
  histdat.net <- melt(
    histdat.all %>% 
      mutate(NonClicks=Unique_Sent - Unique_Clicks) %>% 
      select(-Unique_Sent), 
    id.vars=exp.cols, measure_vars=c("Unique_Clicks", "NonClicks"))
  y <- histdat.net$variable == "Unique_Clicks"
  weights <- histdat.net$value
  mm.it <- model.matrix(formula.interact, data=GetModelFrame(histdat.net))
  cv.glmnet(mm.it, y, family="binomial", weights=weights, alpha=1, nfolds=20)
})
plot(mdl.net.cv.it)

sum(coef(mdl.net.cv.it, s = "lambda.1se") > 0)
sum(coef(mdl.net.cv.it, s = "lambda.min") > 0)

####
# Predict Using the Models ------------------------------------------------
####
# A simple batcher, only allows one value to be returned per row.
BatchPredict <- function(predictFn, newdata, batchsize=1e4) {
  res <- rep(NA, nrow(newdata))
  
  start_indices <- seq(1, nrow(newdata), by = batchsize)
  stop_indices <- c(start_indices[-1]-1, nrow(newdata))
  print(sprintf("batchsize=%d,# of pieces=%d: ",
                batchsize, length(start_indices)))
  for (i in 1:length(start_indices)) {
    cat(sprintf("%d,", i))
    start_idx <- start_indices[i]
    stop_idx <- stop_indices[i]
    
    res[start_idx:stop_idx] <- predictFn(newdata[start_idx:stop_idx,])    
  }
  cat("done.\n")
  return(res)
}

# Gamlr specific version of BatchPredict.
BatchPredictGamlr <- function(mdl, frm, newdata, select, batchsize=1e4) {
  BatchPredict(function(data) {
    mm <- model.matrix(frm, data=data)
    return(predict(mdl, newdata=mm, select=select, type="response"))
  }, newdata, batchsize)
}

# GLMNET specific version of BatchPredict.
BatchPredictGLMNET <- function(mdl, frm, newdata, s, batchsize=1e4) {
  BatchPredict(function(data) {
    mm <- model.matrix(frm, data=data)
    return(predict(mdl, newx=mm, s=s, type="response"))
  }, newdata, batchsize)
}

# Add 0-level to all data.
RelevelCombinations <- function(d, histdat.levels) {
  for (col in exp.cols) {
    d[,col] <- factor(d[,col], levels=0:histdat.levels[col])
  }
  return(d)
}
GenerateAllCombinations <- function(histdat.levels) {
  d <- gen.factorial(histdat.levels, center=F, factors="all", 
                     varNames=names(histdat.levels))
}

combi <- RelevelCombinations(GenerateAllCombinations(histdat.levels),
                             histdat.levels)

combi$cv.pr.it.1se <- LoadCacheTagOrRun("q4_pr_it_1se", function() {
  BatchPredictGamlr(mdl.cv.it, formula.interact, 
                    GetModelFrame(combi), "1se")
})

combi$cv.pr.it.min <- LoadCacheTagOrRun("q4_pr_it_min", function() {
  BatchPredictGamlr(mdl.cv.it, formula.interact, 
                    GetModelFrame(combi), "min")
})

combi$glm.pr <- LoadCacheTagOrRun("q4_pr_glm", function() {
  predict(mdl.glm, newdata=combi, type="response")
})

combi$cv.pr.net.it.1se <- LoadCacheTagOrRun("q4_pr_net_it_1se", function() {
  BatchPredictGLMNET(mdl.net.cv.it, formula.interact,
                     GetModelFrame(combi), "lambda.1se")
})

####
# Plot the prediction histograms ------------------------------------------
####
combi.melt <- melt(combi, id.vars=c(), measure.vars = c(
  "glm.pr", "cv.pr.it.1se", "cv.pr.net.it.1se"))

histdat.emp <- data.frame(
  rate=histdat.all$Unique_Clicks / histdat.all$Unique_Sent)
plot.melt <- rbind(combi.melt, 
                   melt(histdat.emp, id.vars=c(), measure.vars=c("rate")))
measure.labels=c(
  "cv.pr.it.1se" = "2-Level Interactions (cv.gamlr - 1se)",
  "cv.pr.it.min" = "2-Level Interactions (cv.gamlr - min)",
  "glm.pr" = "Simple Logit Model (GLM)",
  "cv.pr.net.it.1se" = "2-Level Interactions (cv.glmnet - 1se)",
  "rate" = "Historical"
)
g <- ggplot(plot.melt, aes(x=value)) + 
  geom_histogram(aes(y=..density..), bins=30) + 
  facet_wrap(~ variable, labeller=labeller("variable"=measure.labels)) +
  labs(x="Pr(Click)", y="Density")
GGPlotSave(g, "q4_pred_hist")


####
# TopN Results ------------------------------------------------------------
####
TopNIndices <- function(dat, cols, N=10, decreasing=T) {
  sapply(cols, function(col) {
    return(list(order(dat[,col], decreasing = decreasing)[1:N]))
  })
}

# Look at the topN.
topN.idx <- TopNIndices(
  combi, c("cv.pr.it.1se", "cv.pr.net.it.1se", "glm.pr"), 10)
combi[intersect(topN.idx$cv.pr.it.1se, topN.idx$glm.pr),]
combi[intersect(topN.idx$cv.pr.net.it.1se, topN.idx$glm.pr),]
combi[topN.idx$cv.pr.net.it.1se,]
combi[topN.idx$cv.pr.it.1se,]
combi[topN.idx$glm.pr,]

# Look at the botN.
botN.idx <- TopNIndices(
  combi, c("cv.pr.it.1se", "cv.pr.net.it.1se", "glm.pr"), 10, decreasing = F)
combi[intersect(botN.idx$cv.pr.it.1se, botN.idx$glm.pr),]
combi[intersect(botN.idx$cv.pr.net.it.1se, botN.idx$glm.pr),]
combi[botN.idx$cv.pr.it.1se,]
combi[botN.idx$cv.pr.net.it.1se,]
combi[botN.idx$glm.pr,]

# Compare the empirical one.
histdat.all %>% mutate(rate = Unique_Clicks / Unique_Sent) -> histdat.all.rate
topN.emp.idx <- TopNIndices(histdat.all.rate,  "rate", 10)
histdat.all.rate[topN.emp.idx$rate,]

botN.emp.idx <- TopNIndices(histdat.all.rate, "rate", 10, decreasing = F)
histdat.all.rate[botN.emp.idx$rate,]

####
# Selecting the sample size of experiments --------------------------------
## Sample size as a function of proportion (p) and margin of error (m)
## The function below assumes a 95% significance level
####
sample.size = function(p,m)
{
  n = ((1.96^2)*p*(1-p))/m^2
  return(n)
}

# We can either use an educated guess for p (our predicted probabilities)
# or we can use a conservative method where p = 0.5 (this maximizes variance)
# For 0 ≤ p ≤ 1, p(1 - p) achieves its largest value at p=0.5

# Sample sizes for TopN intersection
TopN_int <- combi[intersect(topN.idx$cv.pr.it.1se, topN.idx$glm.pr),]
TopN_int$mean <- rowMeans(TopN_int[,10:12])

# Sample sizes (educated guess and conservative method). 1% margin of error.
TopN_int$sample_ed <- round(sample.size(TopN_int$mean,0.01),0)
TopN_int$sample_cons <- round(sample.size(0.5,0.01),0)


####
# Psuedo-R2 calculations --------------------------------------------------
####
# OOS R-squared
# setwd("C:/Users/Chingono/Documents/GitHub/digital_marketing/hw4")
# p_hat <- predict(mdl.cv.it, newdata=[], type="response")
# D <- deviance(y=[ ], pred=p_hat, family="binomial")
# ybar <- mean(valDf$y==1) # marginal prob(y==1)
# D0 <- deviance(y=[], pred=ybar, family="binomial")
# 
# ## OOS R-squared is
# 1 - D/D0


####
# Experiment Design -------------------------------------------------------
####

# fed1 <- LoadCacheTagOrRun("q4_opt_fed", function() {
#   optFederov(~ .,
#              data = GetModelFrame(combi), #combi[, paste('V', 1:9, sep='')], 
#              nTrials = 48,
#              approximate = T,
#              maxIteration=100,
#              criterion="A",
#              args=TRUE)
# })
# 
# fed.app.a <- LoadCacheTagOrRun("q4_opt_fed_app_a", function() {
#   optFederov(~ ., data = GenerateAllCombinations(histdat.levels),
#              nTrials=36, criterion="A", args=T, approximate=T)
# })
# 
# fed.app.d <- LoadCacheTagOrRun("q4_opt_fed_app_d", function() {
#   optFederov(~ ., data = GenerateAllCombinations(histdat.levels),
#              nTrials=36, criterion="D", args=T, approximate=T)
# })
# 
# fed.app.i <- LoadCacheTagOrRun("q4_opt_fed_app_i", function() {
#   optFederov(~ ., data = GenerateAllCombinations(histdat.levels),
#              nTrials=36, criterion="I", args=T, approximate=T)
# })

fed.a <- LoadCacheTagOrRun("q4_opt_fed_a", function() {
  optFederov(~ ., data = GenerateAllCombinations(histdat.levels),
             nTrials=36, criterion="A", args=T)
})

fed.d <- LoadCacheTagOrRun("q4_opt_fed_d", function() {
  optFederov(~ ., data = GenerateAllCombinations(histdat.levels),
             nTrials=36, criterion="D", args=T)
})

fed.i <- LoadCacheTagOrRun("q4_opt_fed_i", function() {
  optFederov(~ ., data = GenerateAllCombinations(histdat.levels),
             nTrials=36, criterion="I", args=T)
})


# rownames.idx <- LoadCacheTagOrRun("q4_match_ccs", function() {
#   all.cases.num <- matrix(as.numeric(as.matrix(all.cases)), ncol=9)
#   all.complete.num <- matrix(as.numeric(as.matrix(unique(
#     histdat.complete.cases[,exp.cols]))), ncol=9)
#   
#   cat(sprintf("%d complete cases: ", nrow(all.complete.num)))
#   res <- sapply(1:nrow(all.complete.num), function(cc.idx) {
#     cat(sprintf("%d,", cc.idx))
#     which(sapply(1:nrow(all.cases.num), function(ac.idx) {
#       all(all.complete.num[cc.idx,] == all.cases.num[ac.idx,])
#     }))
#   })
#   cat("Done.\n")
#   return(res)
# })

histdat.unique.ccs <- unique(histdat.complete.cases[,exp.cols])
histdat.unique.ccs.idx <- LoadCacheTagOrRun("q4_unique_cc_idx", function() {
  unlist(sapply(1:nrow(histdat.unique.ccs), function(cc.idx) {
    case <- histdat.unique.ccs[cc.idx,]
    acc <- 0
    for (i in length(case):2) {
      acc <- histdat.levels[i-1]*(acc + (as.numeric(case[i])-1))
    }
    acc <- unname(acc + as.numeric(case[1]))
    return(acc)
  }))
})

combi.norelevel <- GenerateAllCombinations(histdat.levels)
VerifyUniqueCCs <- function(ccs, idxs, combi.norelevel) {
  for (i in 1:nrow(ccs)) {
    if (!all(ccs[i,] == combi.norelevel[idxs[i],])) {
      stop("Calculated Combination is not right.")
    }
  }
}
VerifyUniqueCCs(histdat.unique.ccs, histdat.unique.ccs.idx, combi.norelevel)

####
# Augment Experiment ------------------------------------------------------
####
VaryAugment <- function(aug.amt, fo) {
  res <- lapply(aug.amt, function(i) {
    cat(sprintf("%d,", i))
    optFederov(fo, 
               data = combi.norelevel,
               rows = histdat.unique.ccs.idx,
               augment = T,
               criterion = "D",
               nTrials = length(histdat.unique.ccs.idx) + i,
               evaluateI=T,
               args=T)
  })
  cat("done.\n")
  return(res)
}

criterion.meas <- LoadCacheTagOrRun("q4_crit_no_inter", function() {
  VaryAugment(c(0, 5, 10, 15:22), ~ .)
})
criterion.meas.it <- LoadCacheTagOrRun("q4_crit_inter", function() {
  VaryAugment(c(0, 5, 10, 15), ~ . + .^2)
})

# ggplot(melt(criterion.meas, id.vars="aug.size"), aes(x=aug.size, y=value)) +
#   geom_line() + facet_wrap(~ variable, scales="free_y")
# ggplot(melt(criterion.meas.it, id.vars="aug.size"), aes(x=aug.size, y=value)) +
#   geom_line() + facet_wrap(~ variable, scales="free_y")

# exp.to.run <- combi.norelevel[setdiff(as.numeric(rownames(fed.aug.d$design)),
#                                       histdat.unique.ccs.idx),]
# exp.to.run$cv.pr.net.it.1se <- BatchPredictGLMNET(
#   mdl.net.cv.it, formula.interact,
#   GetModelFrame(RelevelCombinations(exp.to.run, histdat.levels)), 
#   "lambda.1se")

#
# 15 Experiments from D criterion (no interaction terms)
#
# V1 V2 V3 V4 V5 V6 V7 V8 V9 cv.pr.net.it.1se
# 153765  3  2  3  2  5  3  2  2  3       0.04580092
# 169656  6  4  3  2  4  1  1  4  3       0.02125988
# 177648  6  4  3  1  4  2  2  4  3       0.04411273
# 181874  2  1  1  2  2  1  1  5  3       0.02009277
# 254666  2  1  1  1  2  2  2  5  4       0.04183826
# 254690  2  5  1  1  2  2  2  5  4       0.04183826
# 254721  3  4  2  1  2  2  2  5  4       0.04183826
# 255043  1  4  2  1  3  2  2  5  4       0.04183826
# 255414  6  5  3  1  4  2  2  5  4       0.04183826
# 298552  4  1  2  2  2  1  1  4  5       0.02164538
# 364596  6  4  3  1  1  2  1  4  6       0.01827670
# 370731  3  1  3  1  5  1  2  4  6       0.05601142
# 371058  6  1  3  1  1  2  2  4  6       0.03804751
# 371076  6  4  3  1  1  2  2  4  6       0.03804751
# 372351  3  1  3  1  5  2  2  4  6       0.05601142


FindClosestN <- function(designs, histdat.all, N,
                         hisdat.levels=histdat.levels, .exp.cols=exp.cols) {
  designs.r <- RelevelCombinations(designs, histdat.levels)
  best <- apply(designs.r, 1, function(design) {
    # Creates a score of length histdat.all
    distances <- apply(histdat.all[,.exp.cols], 1, function(x) {
      sum(design == x)
    })
    matches=cbind(histdat.all[order(distances, decreasing=T)[1:N],], 
                  score=distances[order(distances, decreasing=T)][1:N])
  })
  lapply(1:length(best), function(idx) {
    list(design=designs[idx,], matches=best[[idx]])
  })
}
fed.a.dist <- FindClosestN(fed.a$design, histdat.all, 3)
fed.d.dist <- FindClosestN(fed.d$design, histdat.all, 3)
fed.i.dist <- FindClosestN(fed.i$design, histdat.all, 3)



####
# The Rest ----------------------------------------------------------------
####
# By next Wednesday (May 04) please upload a csv file 
# with the first 9 columns labeled (V1,V2,...,V9) (all caps)
# there should be a 10th column called N 
# Each row in your csv file should correspond to a message
# That is the level of the message element V1 through V9.
# N for each row should be the same and should reflect the number of 
# emails you wish to send out for each message.
# I have placed a shell csv file with the appropriate 
# formatting in the project folder.


# Computing your profit score
# Each message campaign costs the client $200
# So if you have 32 messages it will cost your client $6400 to set up and mail these.
# The number of emails doesnt impact your cost (apart from the opportunity cost).

# Your total project profitability will be calculated as follows

profit = function(unique_clicks,ncampaigns,other)
{
  unique_clicks*.1 - 200*ncampaigns - other
} 

# so if you send out 5,000,000 emails and got a 10% response
# and you tested 64 messages in the first experiment
# and 32 messages in the second experiment
# and you purchased $5000 worth of other data
# you would have made the client
profit(5000000*.1,96,5000)
# $25,800

# If for example the control reponse rate was 3% and you decided to just 
# go with that, do no experimentation and bought no data the client would make
profit(5000000*.03,0,0)
# $15,000
