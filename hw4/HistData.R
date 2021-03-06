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

sum(abs(coef(mdl.net.cv.it, s = "lambda.1se")) > 0)
sum(abs(coef(mdl.net.cv.it, s = "lambda.min")) > 0)

mdl.net.cv.it2 <- LoadCacheTagOrRun("q4_glmnet_cv_it2", function() {
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
  
  # Set penalty on the interaction terms first.
  penalty.factor <- rep(2/3, ncol(mm.it))
  
  # Then set the penalty on the non-interaction terms.
  penalty.factor[grep("^V\\d+$", colnames(mm.it))] <- 0.5
  
  cv.glmnet(mm.it, y, family="binomial", weights=weights, alpha=1, nfolds=20,
            penalty.factor=penalty.factor)
})
plot(mdl.net.cv.it2)

sum(abs(coef(mdl.net.cv.it2, s = "lambda.1se")) > 0)
sum(abs(coef(mdl.net.cv.it2, s = "lambda.min")) > 0)

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

combi$cv.pr.net.it.min <- LoadCacheTagOrRun("q4_pr_net_it_min", function() {
  BatchPredictGLMNET(mdl.net.cv.it, formula.interact,
                     GetModelFrame(combi), "lambda.min")
})


####
# Plot the prediction histograms ------------------------------------------
####
combi.melt <- melt(combi, id.vars=c(), measure.vars = c(
  "cv.pr.it.1se", "cv.pr.net.it.1se",
  "cv.pr.net.it.min"))

histdat.emp <- data.frame(
  rate=histdat.all$Unique_Clicks / histdat.all$Unique_Sent)
plot.melt <- rbind(combi.melt, 
                   melt(histdat.emp, id.vars=c(), measure.vars=c("rate")))
measure.labels=c(
  "cv.pr.it.1se" = "2-Level Interactions (cv.gamlr - 1se)",
  "cv.pr.it.min" = "2-Level Interactions (cv.gamlr - min)",
  "glm.pr" = "Simple Logit Model (GLM)",
  "cv.pr.net.it.1se" = "2-Level Interactions (cv.glmnet - 1se)",
  "cv.pr.net.it.min" = "2-Level Interactions (cv.glmnet - min)",
  "rate" = "Historical"
)
g <- ggplot(plot.melt, aes(x=value)) + 
  geom_histogram(aes(y=..density..), bins=30) + 
  facet_wrap(~ variable, labeller=labeller("variable"=measure.labels)) +
  labs(x="Pr(Click)", y="Density")
GGPlotSave(g, "q4_pred_hist")


####
# Convert GLMNET Coefs to Formula -----------------------------------------
####
ConvertSignifCoefsToFormula <- function(coefs) {
  signif.coefs.idx <- which(abs(coefs) > 0)
  
  # Remote the intercept if it's there
  if (signif.coefs.idx[1] == 1) {
    signif.coefs.idx <- signif.coefs.idx[-1]
  }
  
  # Parses a single "Vxy" to produce "I(Vx==y)"
  ParseValue <- function(value) {
    gsub("^V(\\d)(\\d)$", "I(V\\1==\\2)", value)
  }
  
  coefnames <- rownames(coefs)[signif.coefs.idx]
  parsed.coefs <- sapply(coefnames, function(coefname) {
    # Determine if is an interaction term or a regular one.
    if (length(grep(":", coefname))) {
      # For interaction terms split into components and parse them.
      parts <- regmatches(coefname, regexec(
        "^(V\\d{2}):(V\\d{2})", coefname))[[1]][2:3]
      return(paste(ParseValue(parts[1]), ParseValue(parts[2]), sep=":"))    
    } else {
      return(ParseValue(coefname))
    }  
  })
  return(as.formula(paste("~", paste(parsed.coefs, collapse="+"))))
}

form.glmnet <- ConvertSignifCoefsToFormula(coef(mdl.net.cv.it2, s="lambda.1se"))

####
# Design experiment with glmnet coefficients ----------------------------------
####

coefs <- coefs.mdl.cv.it2 <- coef(mdl.net.cv.it2, s="lambda.1se")
signif.coefs.idx <- which(abs(coefs) > 0)
# Remote the intercept if it's there
if (signif.coefs.idx[1] == 1) {
  signif.coefs.idx <- signif.coefs.idx[-1]
}
coefnames <- rownames(coefs)[signif.coefs.idx]

# Will -- so low energy, has to hard code it. Sad!
dat.new <- expand.grid(V1=c(4,5,6),
                       V2=c(1,3,6),
                       V4=c(1,2,3),
                       V5=c(1,5),
                       V6=c(1,3),
                       V7=c(1,2), # there are only two levels
                       V8=c(2,3,4,5),
                       V9=c(2,4,6))
dat.new <- sapply(dat.new, as.factor)

opt.d.glmnet <- optFederov(~ . + V1:V2, data = dat.new,
                           nTrials=22,
                           criterion = "D", args = T)

# Take my zero levels and randomly allocate them into all the non-predictive levels
AddRandomLevels <- function(design) {
  design %>%
    mutate(V3=factor(1)) %>%
    select(V1, V2, V3, V4, V5, V6, V7, V8, V9) -> design
  
  res <- sapply(colnames(design), function(c) {
    idx.zeros <- which(design[,c]==0)
  non.levels <- (1:histdat.levels[c])[!(1:histdat.levels[c]) %in% design[,c]]
  if(length(non.levels) > 0) {
      new.col <- as.character(design[,c])
      new.col[idx.zeros] <- sample(non.levels, sum(design[,c]=="0"), replace = T)
      new.col <- as.factor(new.col)
      return(new.col)    
    }
    return(design[,c])
  })
  as.data.frame(res)
}
opt.design.rand <- AddRandomLevels(opt.d.glmnet$design)

WriteDesign <- function(filename, design) {
  if (ncol(design) != 10) {
    stop("need 10 columns")
  }
  # This is really dumb, I hate R.
  mtx <- matrix(as.numeric(as.matrix(design)), ncol=10)
  write.table(mtx, file=sprintf("experiments/%s", filename), 
              row.names = F, sep = ",")
}
opt.design <- cbind(opt.design.rand, N=as.numeric(1e4))
WriteDesign('opt_design_jwc2.csv', opt.design)

####
# Test the opt design -----------------------------------------------------
####
opt.design.relevel <- RelevelCombinations(opt.design, histdat.levels)
opt.design.relevel$p.glmnet.it.2 <- BatchPredictGLMNET(
  mdl.net.cv.it2, formula.interact, 
  GetModelFrame(opt.design.relevel), s="lambda.1se")
opt.design.relevel$p.glm <- predict(
  mdl.glm, newdata=GetModelFrame(opt.design.relevel), type="response")

g <- ggplot(combi, aes(x=cv.pr.net.it2.1se)) +
  geom_histogram(bins=30) + 
  geom_vline(data=opt.design.relevel, aes(xintercept=p.glmnet.it.2), 
             color="red", lty=2)
GGPlotSave(g, "q4_design_sample_hist")

opt.design.relevel %>% 
  mutate(SUCCESS=round(p.glmnet.it.2 * N),
         FAILURE=N-SUCCESS) %>%
  select(SUCCESS, FAILURE, V1, V2, V3, V4, V5, V6, V7, V8, V9) -> opt.design.test

base.formula <- formula(~ I(V1 == 4) + I(V2 == 6) + I(V4 == 1) + 
  I(V4 == 3) + I(V5 == 5) + I(V6 == 3) + I(V7 == 1) + I(V7 == 2) + 
  I(V8 == 2) + I(V8 == 3) + I(V8 == 4) + I(V8 == 5) + I(V9 == 2) + 
  I(V9 == 4) + I(V9 == 6) + I(V1 == 5):I(V2 == 3) + I(V2 == 3):I(V1 == 6))
# base.formula <- formula(~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V1:V2)
#                           I(V1 == 5):I(V2 == 3) + I(V2 == 3):I(V1 == 6))

res <- glm(update(base.formula, cbind(SUCCESS, FAILURE) ~ .), data=opt.design.test, 
    family="binomial")
summary(res)


####
# TopN Results ------------------------------------------------------------
####
# TopNIndices <- function(dat, cols, N=10, decreasing=T) {
#   sapply(cols, function(col) {
#     return(list(order(dat[,col], decreasing = decreasing)[1:N]))
#   })
# }
# 
# # Look at the topN.
# topN.idx <- TopNIndices(
#   combi, c("cv.pr.it.1se", "cv.pr.net.it.min", "glm.pr"), 10)
# combi[intersect(topN.idx$cv.pr.it.1se, topN.idx$glm.pr),]
# combi[intersect(topN.idx$cv.pr.net.it.min, topN.idx$glm.pr),]
# combi[topN.idx$cv.pr.net.it.min,]
# combi[topN.idx$cv.pr.it.1se,]
# combi[topN.idx$glm.pr,]
# 
# # Look at the botN.
# botN.idx <- TopNIndices(
#   combi, c("cv.pr.it.1se", "cv.pr.net.it.1se", "glm.pr"), 10, decreasing = F)
# combi[intersect(botN.idx$cv.pr.it.1se, botN.idx$glm.pr),]
# combi[intersect(botN.idx$cv.pr.net.it.1se, botN.idx$glm.pr),]
# combi[botN.idx$cv.pr.it.1se,]
# combi[botN.idx$cv.pr.net.it.1se,]
# combi[botN.idx$glm.pr,]
# 
# # Compare the empirical one.
# histdat.all %>% mutate(rate = Unique_Clicks / Unique_Sent) -> histdat.all.rate
# topN.emp.idx <- TopNIndices(histdat.all.rate,  "rate", 10)
# histdat.all.rate[topN.emp.idx$rate,]
# 
# botN.emp.idx <- TopNIndices(histdat.all.rate, "rate", 10, decreasing = F)
# histdat.all.rate[botN.emp.idx$rate,]
# 
# ####
# # Selecting the sample size of experiments --------------------------------
# ## Sample size as a function of proportion (p) and margin of error (m)
# ## The function below assumes a 95% significance level
# ####
# sample.size = function(p,m)
# {
#   n = ((1.96^2)*p*(1-p))/m^2
#   return(n)
# }
# 
# # We can either use an educated guess for p (our predicted probabilities)
# # or we can use a conservative method where p = 0.5 (this maximizes variance)
# # For 0 ≤ p ≤ 1, p(1 - p) achieves its largest value at p=0.5
# 
# # Sample sizes for TopN intersection
# TopN_int <- combi[intersect(topN.idx$cv.pr.it.1se, topN.idx$glm.pr),]
# TopN_int$mean <- rowMeans(TopN_int[,10:12])
# 
# # Sample sizes (educated guess and conservative method). 1% margin of error.
# TopN_int$sample_ed <- round(sample.size(TopN_int$mean,0.01),0)
# TopN_int$sample_cons <- round(sample.size(0.5,0.01),0)
# 
# 
# ####
# # Psuedo-R2 calculations --------------------------------------------------
# ####
# # OOS R-squared
# # setwd("C:/Users/Chingono/Documents/GitHub/digital_marketing/hw4")
# # p_hat <- predict(mdl.cv.it, newdata=[], type="response")
# # D <- deviance(y=[ ], pred=p_hat, family="binomial")
# # ybar <- mean(valDf$y==1) # marginal prob(y==1)
# # D0 <- deviance(y=[], pred=ybar, family="binomial")
# # 
# # ## OOS R-squared is
# # 1 - D/D0
# 
# 
# ####
# # The Rest ----------------------------------------------------------------
# ####
# # By next Wednesday (May 04) please upload a csv file 
# # with the first 9 columns labeled (V1,V2,...,V9) (all caps)
# # there should be a 10th column called N 
# # Each row in your csv file should correspond to a message
# # That is the level of the message element V1 through V9.
# # N for each row should be the same and should reflect the number of 
# # emails you wish to send out for each message.
# # I have placed a shell csv file with the appropriate 
# # formatting in the project folder.
# 
# 
# # Computing your profit score
# # Each message campaign costs the client $200
# # So if you have 32 messages it will cost your client $6400 to set up and mail these.
# # The number of emails doesnt impact your cost (apart from the opportunity cost).
# 
# # Your total project profitability will be calculated as follows
# 
# profit = function(unique_clicks,ncampaigns,other)
# {
#   unique_clicks*.1 - 200*ncampaigns - other
# } 
# 
# # so if you send out 5,000,000 emails and got a 10% response
# # and you tested 64 messages in the first experiment
# # and 32 messages in the second experiment
# # and you purchased $5000 worth of other data
# # you would have made the client
# profit(5000000*.1,96,5000)
# # $25,800
# 
# # If for example the control reponse rate was 3% and you decided to just 
# # go with that, do no experimentation and bought no data the client would make
# profit(5000000*.03,0,0)
# # $15,000
