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

WriteDesign <- function(filename, design) {
  # This is really dumb, I hate R.
  mtx <- matrix(as.numeric(as.matrix(design)), ncol=9)
  write.table(mtx, file=sprintf("experiments/%s", filename), 
              row.names = F, sep = ",")
}
WriteDesign("scratch_A.csv", fed.a$design)
WriteDesign("scratch_D.csv", fed.d$design)
WriteDesign("scratch_I.csv", fed.i$design)

d.design <- fed.d$design 
d.design$pr.glmnet.1se <- BatchPredictGLMNET(
  mdl.net.cv.it, formula.interact, 
  RelevelCombinations(fed.d$design, histdat.levels), "lambda.1se")
d.design <- d.design[order(d.design$pr.glmnet.1se),]
ExportTable(d.design, "ordered_d_design", "Ordered D-Design")

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

aug.size.no.inter <- c(0, 5, 10, 15:25)
aug.size.inter <- c(0, 5, 10, 15, 20)
criterion.meas <- LoadCacheTagOrRun("q4_crit_no_inter", function() {
  VaryAugment(aug.size.no.inter, ~ .)
})
criterion.meas.it <- LoadCacheTagOrRun("q4_crit_inter", function() {
  VaryAugment(aug.size.inter, ~ . + .^2)
})

PlotCriterionChanges <- function(criterion, aug.size) {
  crit <- cbind(aug.size=aug.size,
                ldply(criterion, function(x) {
                  data.frame(D=x$D, I=x$I, A=x$A)
                }))
  g <- ggplot(melt(crit, id.vars="aug.size"), aes(x=aug.size, y=value)) +
    geom_line() + facet_wrap(~variable, scales="free_y")
  return(g)
}
GGPlotSave(PlotCriterionChanges(criterion.meas, aug.size.no.inter), "aug_no_inter")
GGPlotSave(PlotCriterionChanges(criterion.meas.it, aug.size.inter), "aug_with_inter")


ExtractNewExperiments <- function(old, all) {
  exp.to.run <- combi.norelevel[setdiff(as.numeric(rownames(all)), old),]
  exp.to.run$cv.pr.net.it.1se <- BatchPredictGLMNET(
    mdl.net.cv.it, formula.interact,
    GetModelFrame(RelevelCombinations(exp.to.run, histdat.levels)), 
    "lambda.1se")
  exp.to.run$cv.pr.net.it.min <- BatchPredictGLMNET(
    mdl.net.cv.it, formula.interact,
    GetModelFrame(RelevelCombinations(exp.to.run, histdat.levels)), 
    "lambda.min")
  exp.to.run$glm.pr <- predict(
    mdl.glm, 
    newdata=RelevelCombinations(exp.to.run, histdat.levels), 
    type="response")
  return(exp.to.run)
}

# aug.size=15
design.no.inter <- ExtractNewExperiments(histdat.unique.ccs.idx, 
                                         criterion.meas[[4]]$design)
WriteDesign("augment_no_inter.csv", design.no.inter[,exp.cols])

# aug.size=15
design.with.inter <- ExtractNewExperiments(histdat.unique.ccs.idx, 
                                           criterion.meas.it[[4]]$design)
WriteDesign("augment_with_inter.csv", design.with.inter[,exp.cols])

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

####
# Another Idea (1se)-------------------------------------------------------
####
# https://piazza.com/class/im6wk9z189a2ha?cid=17
coefs.1se <- coef(mdl.net.cv.it, s="lambda.1se")
idx.non.inter <- 1:42
interaction.terms.1se <- rownames(coefs.1se)[-idx.non.inter][
  which(coefs.1se[-idx.non.inter]!=0)]
# [1] "V15:V23" "V16:V23" "V14:V72" "V26:V72" "V41:V55" "V41:V72" "V55:V72" "V55:V84" "V55:V94"
# [10] "V63:V72" "V72:V84" "V72:V85" "V72:V92" "V72:V94"
base.interaction.terms.1se <- unique(gsub("V(\\d)\\d:V(\\d)\\d", "V\\1:V\\2",
                                          interaction.terms.1se))

search.fo.1se <- as.formula(paste("~ . +", 
                                  paste(base.interaction.terms.1se, collapse=" + ")))
# ~. + V1:V2 + V1:V7 + V2:V7 + V4:V5 + V4:V7 + V5:V7 + V5:V8 + 
#   V5:V9 + V6:V7 + V7:V8 + V7:V9
print(sprintf("# of required experiments for lambda.1se: %d", 
              ncol(model.matrix(search.fo.1se, data=histdat[[3]]))+5))
# 131 + 5 (for safety)

new.search.1se <- LoadCacheTagOrRun("q4_fed_new_search_1se", function() {
  optFederov(search.fo.1se,
             data=combi.norelevel,
             criterion="D",
             args=TRUE)
})
WriteDesign("scratch_interact_1se", new.search.1se$design)

####
# 1se with cutoff ---------------------------------------------------------
####
# https://piazza.com/class/im6wk9z189a2ha?cid=17
coefs.1se <- coef(mdl.net.cv.it, s="lambda.1se")
idx.non.inter <- 1:42
interaction.terms.1se <- rownames(coefs.1se)[-idx.non.inter][
  which(coefs.1se[-idx.non.inter]>0.08)]
# "V55:V72" "V72:V84"
base.interaction.terms.1se <- unique(gsub("V(\\d)\\d:V(\\d)\\d", "V\\1*V\\2",
                                          interaction.terms.1se))
base.interaction.terms.1se

search.fo.1se <- as.formula(paste("~ . +", 
                                  paste(base.interaction.terms.1se, collapse=" + ")))
search.fo.1se
# search.fo.1se <- formula(~. + V7*V8)
# ~. + V5 * V7 + V7 * V8
print(sprintf("# of required experiments for lambda.1se: %d", 
              ncol(model.matrix(search.fo.1se, data=histdat[[3]]))+5))
# 42 + 5 (for safety)

new.search.1se.co <- LoadCacheTagOrRun("q4_fed_new_search_1se_co", function() {
  optFederov(search.fo.1se,
             data=combi.norelevel,
             criterion="D",
             args=TRUE)
})
WriteDesign("scratch_interact_1se_co.csv", new.search.1se.co$design)

####
# Another Idea (min) ------------------------------------------------------
####
coefs <- coef(mdl.net.cv.it, s="lambda.min")
idx.non.inter <- 1:42
interaction.terms <- rownames(coefs)[-idx.non.inter][which(coefs[-idx.non.inter]!=0)]
# [1] "V12:V22" "V14:V22" "V11:V23" "V13:V23" "V14:V23" "V15:V23" "V16:V23" "V15:V25"
# [9] "V15:V26" "V14:V55" "V15:V55" "V14:V63" "V14:V72" "V15:V72" "V15:V84" "V22:V72"
# [17] "V22:V85" "V33:V41" "V33:V55" "V33:V72" "V33:V85" "V41:V55" "V41:V72" "V55:V63"
# [25] "V55:V72" "V55:V84" "V55:V85" "V55:V94" "V61:V72" "V63:V72" "V63:V84" "V61:V94"
# [33] "V72:V81" "V72:V84" "V72:V85" "V72:V92" "V72:V94" "V85:V92"
base.interaction.terms <- unique(gsub("V(\\d)\\d:V(\\d)\\d", "V\\1:V\\2",
                                      interaction.terms))

search.fo <- as.formula(paste("~ . +", 
                              paste(base.interaction.terms, collapse=" + ")))
# ~. + V1:V2 + V1:V5 + V1:V6 + V1:V7 + V1:V8 + V2:V7 + V2:V8 + 
#   V3:V4 + V3:V5 + V3:V7 + V3:V8 + V4:V5 + V4:V7 + V5:V6 + V5:V7 + 
#   V5:V8 + V5:V9 + V6:V7 + V6:V8 + V6:V9 + V7:V8 + V7:V9 + V8:V9
print(sprintf("# of required experiments for lambda.min: %d", 
              ncol(model.matrix(search.fo, data=histdat[[3]]))+5))
# 287 + 5 (for safety)

# new.search <- LoadCacheTagOrRun("q4_fed_new_search", function() {
#   optFederov(search.fo,
#              data=combi.norelevel,
#              criterion="D",
#              evaluateI = T,
#              args=TRUE)
# })
# 

####
# Predicted Quantile Sampling ---------------------------------------------
####
ord.idx <- order(combi$cv.pr.net.it.1se)
samp.quant <- ord.idx[round(c(seq(from=0, to=0.8, length.out = 10),
                              seq(from=0.8, to=1, length.out = 26))*length(ord.idx))]
combi[samp.quant,]
WriteDesign("sampled_quantile.csv", combi[samp.quant,exp.cols])

####
# Criterion Table ---------------------------------------------------------
####
all.criterion <- data.frame(
  Name=c("Scratch-A", "Scratch-D", "Scratch-I", "Non-Augmented (Histdat)",
         "Augmented (Histdat+15)", "Non-Augmented (Interact) (Histdat)",
         "Augmented (Interact) (Histdat+15)", "Scratch-Interact-D",
         "Scratch-Interact-Simpler-D"),
  File=c("scratch_A.csv", "scratch_D.csv", "scratch_I.csv", NA,
         "augmented_no_inter.csv", NA, "augmented_with_inter.csv",
         "scratch_interact_1se.csv", "scratch_interact_1se_co.csv"),
  D=c(fed.a$D, fed.d$D, fed.i$D, criterion.meas[[1]]$D, criterion.meas[[4]]$D,
      criterion.meas.it[[1]]$D, criterion.meas.it[[4]]$D,
      new.search.1se$D, new.search.1se.co$D),
  A=c(fed.a$A, fed.d$A, fed.i$A, criterion.meas[[1]]$A, criterion.meas[[4]]$A,
      criterion.meas.it[[1]]$A, criterion.meas.it[[4]]$A,
      new.search.1se$A, new.search.1se.co$A),
  I=c(NA, NA, fed.i$I, criterion.meas[[1]]$I, criterion.meas[[4]]$I,
      criterion.meas.it[[1]]$I, criterion.meas.it[[4]]$I, NA, NA))

ExportTable(all.criterion, "criterion", "Criterion for Designs", NA.string = "NA", digits = 4)

####
# Closest -----------------------------------------------------------------
####
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
