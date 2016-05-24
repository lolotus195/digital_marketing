rm(list = ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()
SlidesTheme()
Global.PlotOpts$Prefix <- "slides/"
require(plyr)
require(dplyr)
require(ggplot2)
source("model.R")

####
# Load Data ----
####
dat1 <- RelevelData(read.csv('results/experiment1.csv'), exp.levels)
dat2 <- RelevelData(read.csv('results/experiment2.csv'), exp.levels)
dat.both <- rbind(dat1 %>% mutate(series="#1"), 
                  dat2 %>% mutate(series="#2"))

####
# Formula We Submitted ----------------------------------------------------
####
base.formula <- formula(~ I(V1 == 4) + I(V2 == 6) + I(V4 == 1) + 
                          I(V5 == 5) + I(V6 == 3) + I(V7 == 2) + 
                          I(V8 == 4) + I(V9 == 4) + 
                          I(V1 == 5):I(V2 == 3) + I(V2 == 3):I(V1 == 6))
form <- update(base.formula, cbind(Clicks, N - Clicks) ~ .)
reg <- glm(form, dat1, family = 'binomial')
summary(reg)
####
# Formula w/No Singularities ----
####
dat.permute <- RelevelData(expand.grid(
  V1=c(4,5,6),
  V2=c(1,3,6),
  V3=1,
  V4=c(1,2,3),
  V5=c(1,5),
  V6=c(1,3),
  V7=c(1,2),
  V8=c(2,3,4,5),
  V9=c(2,4,6)), exp.levels)

mdl1 <- glm(cbind(Clicks, N-Clicks) ~ V1 + V2 + V4 + V5 + V6 + V7 + I(V8 == 4) + V9 + V1:V2, 
            dat1, family="binomial")
summary(mdl1)
mdl2 <- glm(cbind(Clicks, N-Clicks) ~ V1 + V2 + V4 + V5 + V6 + V7 + I(V8 == 4) + V9 + V1:V2, 
            dat.both, family="binomial")
summary(mdl1)

prd1 <- predict(mdl1, dat.permute, type="response", se.fit = T)
prd2 <- predict(mdl2, dat.permute, type="response", se.fit = T)

alpha=0.05
dat.permute %>%
  mutate(pred1 = prd1$fit,
         pred1.se = prd1$se.fit,
         pred1.ci = qnorm(1-alpha/2, sd=pred1.se),
         pred2 = prd2$fit,
         pred2.se = prd2$se.fit,
         pred2.ci = qnorm(1-alpha/2, sd=pred2.se)) -> dat.results

dat.results[order(dat.results$pred2, decreasing = T)[1:10],] -> dat.topN
dat.topN$msg.id <- rownames(dat.topN)
dat.topN$index <- 1:nrow(dat.topN)
rbind(
  dat.topN %>% mutate(series="#1 & #2") %>%
    rename(pred=pred2, pred.ci=pred2.ci) %>%
    select(-pred1, -pred1.ci),
  dat.topN %>% mutate(series="#1") %>%
    rename(pred=pred1, pred.ci=pred1.ci) %>%
    select(-pred2, -pred2.ci)
) -> dat.topN

GetMessageLabel <- function(df) {
  cnames <- sprintf("V%d", 1:9)
  for (cname in cnames) {
    df[,cname] <- as.character(df[,cname])
  }
  apply(df[,cnames], 1, function(x) { 
    sprintf("(V1, ..., V9) = (%s)", paste(x, collapse=", "))
  })
}
dat.topN$label <- GetMessageLabel(dat.topN)

AxisLabels <- function(df) {
  breaks=sort(unique(df$index))
  data.frame(
    breaks=breaks,
    labels=df$msg.id[match(breaks, dat.topN$index)]
  )
}
plot.breaks <- AxisLabels(dat.topN)

g <- ggplot(dat.topN, aes(x=index, y=pred, fill=series)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=pred - pred.ci, ymax=pred + pred.ci),
                width=0.4, position=position_dodge(.9),
                color="gray48") +
  geom_text(data=filter(dat.topN, series=="#1"), 
            aes(x=index, y=0, label=label), 
            angle=90, hjust=0, nudge_y=0.005, nudge_x=0.20,
            fontface="bold") +
  scale_x_continuous("Message ID", 
                     breaks=plot.breaks$breaks,
                     labels=plot.breaks$labels) +
  scale_fill_discrete("Experiment") +
  ylab("Pr(Click)")
ggsave("slides/barplot.pdf", g)
plot(g)

g <- ggplot(dat.results, aes(x=pred2)) +
  geom_histogram(bins=30) +
  geom_vline(data=dat.both, aes(xintercept=Clicks/N, color=series)) +
  scale_color_discrete("Experiment") +
  labs(x="Pr(Click)", y="Count")
ggsave("slides/hist.pdf", g)
plot(g)

mdl.small <- glm(cbind(Clicks, N-Clicks) ~ V9, 
           dat1, family="binomial")
summary(mdl.small)

# Run 575 against 569 (vary level 2 and level 6)
dat.test <- dat.results[order(dat.results$pred1, decreasing = T),]
head(dat.test)

require(bandit)
power.prop.test(p1 = dat.test$pred1[1], 
                p2 = dat.test$pred1[3], 
                sig.level = 0.05, power = 0.95)

# Would this be helpful? ----
dat.test[c(2,3),] %>%
  mutate(N=7713) %>%
  select(-pred1, -pred1.se, -pred1.ci,
         -pred2, -pred2.se, -pred2.ci) -> dat.exp2
as.data.frame(dat.exp2)
WriteDesign <- function(filename, design) {
  if (ncol(design) != 10) {
    stop("need 10 columns")
  }
  # This is really dumb, I hate R.
  mtx <- matrix(as.numeric(as.matrix(design)), ncol=10)
  write.table(mtx, file=sprintf("exp.designs/%s", filename), 
              row.names = F, sep = ",")
}
WriteDesign("exp2.csv", dat.exp2)

# Final prediction ----

final <- slice(dat.topN, 1) %>% select(pred, pred.ci, label) %>%
  mutate(upper = pred + pred.ci, lower = pred - pred.ci)

final$label

sprintf('Predicted click rate: %.5f', 100*final$pred)

sprintf('95 percent confidence interval: (%.5f, %.5f)', 
        100*final$lower, 100*final$upper)

# Cost prediction ----

experiment.cost <- (nrow(dat.both) + 1) * 200
experiment.revenue <- sum(dat.both$Clicks * 0.10)

final.revenue <- final$pred * (5e6 - sum(dat.both$N)) * 0.10
final.revenue_hi <- final$upper * (5e6 - sum(dat.both$N)) * 0.10
final.revenue_lo <- final$lower * (5e6 - sum(dat.both$N)) * 0.10

sprintf('Revenue prediction: $%.2f', 
        final.revenue + experiment.revenue - experiment.cost)

sprintf('95 percent confidence interval: ($%.2f, $%.2f)', 
        final.revenue_lo, final.revenue_lo)
