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
                          I(V5 == 5) + I(V6 == 3) + I(V7 == 2) + 
                          I(V8 == 4) + I(V9 == 4) + 
                          I(V1 == 5):I(V2 == 3) + I(V2 == 3):I(V1 == 6))
form <- update(base.formula, cbind(Clicks, N - Clicks) ~ .)
reg <- glm(form, dat, family = 'binomial')
summary(reg)
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

# mdl <- glm(cbind(Clicks, N-Clicks) ~ V1 + V2 + V4 + V5 + V6 + V7 + V8 + V9 + V1:V2, 
           # dat, family="binomial")
mdl <- glm(cbind(Clicks, N-Clicks) ~ V1 + V2 + I(V4 == 1) + V5 + V6 + V7 + 
             I(V8 == 4) + V9 + V1:V2, 
           dat, family="binomial")

summary(mdl)
r2 <- 1-mdl$deviance/mdl$null.deviance


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


mdl.small <- glm(cbind(Clicks, N-Clicks) ~ V9, 
           dat, family="binomial")
summary(mdl.small)

# Run 575 against 569 (vary level 2 and level 6)

dat.test <- dat.test[order(dat.test$p_click_rate, decreasing = T),]
head(dat.test)

require(bandit)
power.prop.test(p1 = dat.test$p_click_rate[1], 
                p2 = dat.test$p_click_rate[3], 
                sig.level = 0.05, power = 0.95)

# Would this be helpful? ----
require(dplyr)
dat.test[c(2,3),] %>%
  mutate(N=7713) %>%
  select(-p_click_rate, -p_click_rate.stderr) -> dat.exp2
as.data.frame(dat.exp2)
WriteDesign <- function(filename, design) {
  if (ncol(design) != 10) {
    stop("need 10 columns")
  }
  # This is really dumb, I hate R.
  mtx <- matrix(as.numeric(as.matrix(design)), ncol=10)
  write.table(mtx, file=sprintf("experiments/%s", filename), 
              row.names = F, sep = ",")
}
WriteDesign("experiment2.csv", dat.exp2)

as.character(dat.exp2)
write.csv(dat.exp2, "experiment2.csv")
# mdl.new <- glm(cbind(Clicks, N-Clicks) ~ V1 + V2 + I(V4 == 1) + V5 + V6 + V7 + 
#              I(V8 == 4) + V9 + V1:V2, 
#            dat.new, family="binomial")

mdl.new <- glm(cbind(Clicks, N-Clicks) ~ . + V1*V2, 
               select(dat, -V3), family="binomial") # select(dat.new, -V3)


cbind(summary(mdl)$coef, summary(mdl.new)$coef)
