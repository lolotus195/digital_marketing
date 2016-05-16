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
pred.click <- predict(mdl, dat.test, type="response", se.fit = T)
dat.test$p_click_rate <- pred.click$fit
dat.test$p_click_rate.stderr <- pred.click$se.fit
topN <- dat.test[order(dat.test$p_click_rate, decreasing = T)[1:20],]

FitCIs <- function(df, fit.cn, se.cn, alpha=0.05) {
  data.frame(
    ymin=df[,fit.cn] + qnorm(alpha/2, sd=df[,se.cn]),
    ymax=df[,fit.cn] + qnorm(1-alpha/2, sd=df[,se.cn])
  )
}

GetMessageLabel <- function(df) {
  cnames <- sprintf("V%d", 1:9)
  for (cname in cnames) {
    df[,cname] <- as.character(df[,cname])
  }
  apply(df[,cnames], 1, function(x) { 
    sprintf("(V1, ..., V9) = (%s)", paste(x, collapse=", "))
  })
}

topN.with.CIs <- cbind(topN, FitCIs(topN, "p_click_rate", "p_click_rate.stderr"),
                       Label=GetMessageLabel(topN))

g <- ggplot(topN.with.CIs, aes(x=1:nrow(topN), y=p_click_rate)) + 
  geom_bar(stat='identity', position='identity', fill=gg_color_hue(2)[2]) + 
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.25) +
  geom_text(aes(label=Label, y=0), angle=90, hjust=0, nudge_y = 0.005) +
  scale_x_continuous("Message ID", breaks=1:nrow(topN), labels=rownames(topN)) +
  ylab("Pr(Click)")
GGPlotSave(g, "barplot")

# GetBoxplotInfo <- function(df, fit, se) {
#   data.frame(
#     ymin=df[,fit] + qnorm(0.025, sd=df[,se]),
#     lower=df[,fit] + qnorm(0.25, sd=df[,se]),
#     middle=df[,fit],
#     upper=df[,fit] + qnorm(0.75, sd=df[,se]),
#     ymax=df[,fit] + qnorm(0.975, sd=df[,se]))
# }
# to.plot <- cbind(x=1:10, idx=rownames(topN), 
#                  GetBoxplotInfo(topN, "p_click_rate", "p_click_rate.stderr"))
# ggplot(to.plot, aes(x=x, ymin=ymin, lower=lower, middle=middle, upper=upper, ymax=ymax)) + 
#   geom_boxplot(stat="identity")

g <- ggplot(dat.test, aes(x=p_click_rate)) +
  geom_histogram(bins=30) +
  geom_vline(data=dat, aes(xintercept=Clicks/N), lty=2, color='red')
GGPlotSave(g, "hist")