# Digital and Algorithmic Marketing
# Instructor: Sanjog Misra
# Historical Data for Project


####
# Setup ----
####
rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()
require(plyr)
require(dplyr)

# load data
load(file = "Historical_Data.rdat")
# Loads a list calles histdat
# The list contains 318 elements
length(histdat)
# Each element is a matrix that gives you the results on an experiment that was run.
# For example the 11th element is a experiment with 48 messages
# The first set of variables (named V1,V2, etc.) correspond to message elements
# Unique_Clicks are responses and Unique_Sent are the number of emails sent.

# Different experiments may have different sets of message elements
# They will also have different sample sizes
# However, the variable names are consistent. So V1 in experiment 11
# is the same as V1 in experiment 35.
# In total there are 9 message elements and there will be the same 9 elements in the 
# upcoming experiments related to the project.


####
# Relevel and combine history ----
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
  
  return(unlist(res[complete.cases(res),][1,]))
  # return(res[complete.cases(res)[1]])
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

histdat.levels <- VerifyStuff(histdat)
histdat.releveled <- RelevelHistDat(histdat, histdat.levels)
histdat.all <- ldply(histdat.releveled, function(x) x)

g <- qplot(Unique_Clicks/Unique_Sent, data=histdat.all, bins=30)
GGPlotSave(g, "q4_emp_hist")


####
# Create Binomial Model (GLM) ----
####
mdl.glm <- glm(cbind(Unique_Clicks, Unique_Sent-Unique_Clicks) ~ .,
               data=histdat.all, family="binomial")
summary(mdl.glm)


####
# Create Interaction Model (cv.gamlr) ----
####
require(gamlr)
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
# Predict Using the Models ----
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

combi <- expand.grid(lapply(histdat.all[,exp.cols], levels))
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


####
# Plot the prediction histograms ----
####
combi.melt <- melt(combi, id.vars=c(), measure.vars = c(
  "glm.pr", "cv.pr.it.min", "cv.pr.it.1se"))

histdat.emp <- data.frame(
  rate=histdat.all$Unique_Clicks / histdat.all$Unique_Sent)
plot.melt <- rbind(combi.melt, 
                   melt(histdat.emp, id.vars=c(), measure.vars=c("rate")))
measure.labels=c(
  "cv.pr.it.1se" = "2-Level Interactions (CV - 1se)",
  "cv.pr.it.min" = "2-Level Interactions (CV - min)",
  "glm.pr" = "Simple Logit Model (GLM)",
  "rate" = "Historical"
)
g <- ggplot(plot.melt, aes(x=value)) + 
  geom_histogram(aes(y=..density..), bins=30) + 
  facet_wrap(~ variable, labeller=labeller("variable"=measure.labels)) +
  labs(x="Pr(Click)", y="Density")
GGPlotSave(g, "q4_pred_hist")


####
# TopN Results ----
####
TopNIndices <- function(dat, cols, N=10) {
  sapply(cols, function(col) {
    return(list(order(dat[,col], decreasing = T)[1:N]))
  })
}
topN.idx <- TopNIndices(combi, c("cv.pr.it.1se", "glm.pr"), 10)

# Look at the topN.
combi[intersect(topN.idx$cv.pr.it.1se, topN.idx$glm.pr),]
combi[topN.idx$cv.pr.it.1se,]
combi[topN.idx$glm.pr,]

# Compare the empirical one.
histdat.all %>% mutate(rate = Unique_Clicks / Unique_Sent) -> histdat.all.rate 
topN.emp.idx <- TopNIndices(histdat.all.rate,  "rate", 10)
histdat.all.rate[topN.emp.idx$rate,]


####
# The rest ----
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

# so if you send out 5000,000 emails and got a 10% response
# and you tested 64 messages in the first experiment
# and 32 messages in the second experiment
# and you purchased $5000 worth of other data
# you would have made the client
profit(5000000*.1,96,5000)
# $25,800

# If for example the control reponse rate was 3% and you decided to jsut 
# go with that, do no experimentation and bought no datathe client would make
profit(5000000*.03,0,0)
# $15,000


