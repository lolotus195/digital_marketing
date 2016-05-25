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

prd1 <- predict(mdl1, dat.permute, type="link", se.fit = T)
prd2 <- predict(mdl2, dat.permute, type="link", se.fit = T)

LogitLink <- function(ll) {
  1/(1+exp(-ll))
}

alpha=0.05
dat.permute %>%
  mutate(pred1 = LogitLink(prd1$fit),
         pred1.lower = LogitLink(prd1$fit + qnorm(alpha/2, sd=prd1$se.fit)),
         pred1.upper = LogitLink(prd1$fit + qnorm(1-alpha/2, sd=prd1$se.fit)),
         pred2 = LogitLink(prd2$fit),
         pred2.lower = LogitLink(prd2$fit + qnorm(alpha/2, sd=prd2$se.fit)),
         pred2.upper = LogitLink(prd2$fit + qnorm(1-alpha/2, sd=prd2$se.fit))
  ) -> dat.results

dat.results[order(dat.results$pred2, decreasing = T)[1:10],] -> dat.topN
dat.topN$msg.id <- rownames(dat.topN)
dat.topN$index <- 1:nrow(dat.topN)
rbind(
  dat.topN %>% mutate(series="#1 & #2") %>%
    rename(pred=pred2, pred.lower=pred2.lower, pred.upper=pred2.upper) %>%
    select(-pred1, -pred1.lower, -pred1.upper),
  dat.topN %>% mutate(series="#1") %>%
    rename(pred=pred1, pred.lower=pred1.lower, pred.upper=pred1.upper) %>%
    select(-pred2, -pred2.lower, -pred2.upper)
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

fill.values <- c("#1"=gg_color_hue(3)[1], "#2"=gg_color_hue(3)[3])

g <- ggplot(dat.results, aes(x=pred2)) +
  geom_histogram(bins=30) +
  geom_vline(data=dat.both, aes(xintercept=Clicks/N, color=series)) +
  scale_color_manual("Experiment", values=fill.values) +
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
  select(-pred1, -pred1.lower, -pred1.upper,
         -pred2, -pred2.lower, -pred2.upper) -> dat.exp2
as.data.frame(dat.exp2)
WriteDesign <- function(filename, design, final=F) {
  if (final) {
    if (ncol(design) != 9) {
      stop("need 9 columns")
    }
  } else {
    if (ncol(design) != 10) {
      stop("need 10 columns")
    }
  }
  # This is really dumb, I hate R.
  mtx <- matrix(as.numeric(as.matrix(design)), ncol=ifelse(final, 9, 10))
  write.table(mtx, file=sprintf("exp.designs/%s", filename), 
              row.names = F, sep = ",")
}
WriteDesign("exp2.csv", dat.exp2)

# Final prediction ----

dat.topN %>%
  filter(series=="#1 & #2") %>%
  arrange(desc(pred)) %>%
  slice(1) -> final
final$label

sprintf('Predicted click rate: %.5f', 100*final$pred)

sprintf('95 percent confidence interval: (%.5f, %.5f)', 
        100*final$pred.lower, 100*final$pred.upper)

# Cost prediction ----
experiment.cost <- (nrow(dat.both) + 1) * 200
experiment.revenue <- sum(dat.both$Clicks * 0.10)

final.revenue <- final$pred * (5e6 - sum(dat.both$N)) * 0.10 + experiment.revenue - experiment.cost
final.revenue_hi <- final$pred.upper * (5e6 - sum(dat.both$N)) * 0.10 + experiment.revenue - experiment.cost
final.revenue_lo <- final$pred.lower * (5e6 - sum(dat.both$N)) * 0.10 + experiment.revenue - experiment.cost

sprintf('Revenue prediction: $%.2f', 
        final.revenue)

sprintf('95 percent confidence interval: ($%.2f, $%.2f)', 
        final.revenue_lo, final.revenue_hi)

# Fancy plots ----

# default: l=65, c=100
# faded: l=95, c=40

alpha.background <- 0.25
fill.values <- c("#1"=gg_color_hue(3)[1], "#1 & #2"=gg_color_hue(3)[3])

# 1 - red is in focus, blue is faded (lower intensity)
g1 <- ggplot(dat.topN, aes(x=index, y=pred, fill=series, alpha=series)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=pred.lower, ymax=pred.upper),
                width=0.4, position=position_dodge(.9),
                color="gray48") +
  geom_text(data=filter(dat.topN, series=="#1"), 
            aes(x=index-0.25, y=0, label=label), 
            angle=90, hjust=0, nudge_y=0.005) +
  scale_x_continuous("Message ID", 
                     breaks=plot.breaks$breaks,
                     labels=plot.breaks$labels) +
  scale_fill_manual("Experiment", values=fill.values) +
  scale_alpha_manual(values=c("#1"=1, "#1 & #2"=alpha.background), guide=F) +
  ylab("Pr(Click)")
plot(g1)

# 2 - highlight reds that we tested
g2 <- ggplot(dat.topN, aes(x=index, y=pred, fill=series, alpha=series)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=pred.lower, ymax=pred.upper),
                width=0.4, position=position_dodge(.9),
                color="gray48") +
  geom_text(data=dat.topN %>% 
              mutate(label = ifelse(msg.id=='629'|msg.id=='569',label,'')) %>% 
              filter(series=="#1"), 
            aes(x=index-0.25, y=0, label=label), 
            angle=90, hjust=0, nudge_y=0.005) +
  scale_x_continuous("Message ID", 
                     breaks=plot.breaks$breaks,
                     labels=plot.breaks$labels) +
  scale_fill_manual("Experiment", values=fill.values) +
  scale_alpha_manual(values=c("#1"=1, "#1 & #2"=alpha.background), guide=F) +
  ylab("Pr(Click)") +
  annotate('rect', xmin = 1.5, xmax = 2.5, ymin = -0.005, 
           ymax = as.numeric(dat.topN %>% 
                               filter(series == '#1', msg.id == '629') %>% 
                               select(pred.upper)) + 0.005,
           alpha = 0, color = '#000000', size = 1) + 
  annotate('rect', xmin = 4.5, xmax = 5.5, ymin = -0.005, 
           ymax = as.numeric(dat.topN %>% 
                               filter(series == '#1', msg.id == '569') %>% 
                               select(pred.upper)) + 0.005,
           alpha = 0, color = '#000000', size = 1)
plot(g2)

# 3 - add blues
g3 <- ggplot(dat.topN, aes(x=index, y=pred, fill=series, alpha=series)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=pred.lower, ymax=pred.upper),
                width=0.4, position=position_dodge(.9),
                color="gray48") +
  geom_text(data=filter(dat.topN, series=="#1 & #2"), 
            aes(x=index, y=0, label=label), 
            angle=90, hjust=0, nudge_y=0.005, nudge_x=0.2) +
  scale_x_continuous("Message ID", 
                     breaks=plot.breaks$breaks,
                     labels=plot.breaks$labels) +
  scale_fill_manual("Experiment", values=fill.values) +
  scale_alpha_manual(values=c("#1"=alpha.background, "#1 & #2"=1), guide=F) +
  ylab("Pr(Click)")
plot(g3)

# 4 - highlight message that we submitted
g4 <- ggplot(dat.topN, aes(x=index, y=pred, fill=series, alpha=series)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=pred.lower, ymax=pred.upper),
                width=0.4, position=position_dodge(.9),
                color="gray48") +
  geom_text(data=filter(dat.topN, series == '#1 & #2', msg.id == '575'), 
            aes(x=index, y=0, label=label), 
            angle=90, hjust=0, nudge_y=0.005, nudge_x=0.2) +
  scale_x_continuous("Message ID", 
                     breaks=plot.breaks$breaks,
                     labels=plot.breaks$labels) +
  scale_fill_manual("Experiment", values=fill.values) +
  scale_alpha_manual(values=c("#1"=alpha.background, "#1 & #2"=1), guide=F) +
  ylab("Pr(Click)") + 
  annotate('rect', xmin = 0.5, xmax = 1.5, ymin = -0.005, 
           ymax = as.numeric(dat.topN %>% 
                               filter(series == '#1', msg.id == '575') %>% 
                               select(pred.upper)) + 0.005,
           alpha = 0, color = '#000000', size = 1)
plot(g4)

# Re-scale so they are all on the same y-axis
# Should not be hard coded...
# Comes from ggplot_build(g1)$panel$y_scales[[1]]
g1 <- g1 + ylim(c(-0.005, 0.164))
g2 <- g2 + ylim(c(-0.005, 0.164))
g3 <- g3 + ylim(c(-0.005, 0.164))
g4 <- g4 + ylim(c(-0.005, 0.164))

ggsave("slides/barplot1.pdf", g1)
ggsave("slides/barplot2.pdf", g2)
ggsave("slides/barplot3.pdf", g3)
ggsave("slides/barplot4.pdf", g4)

# Write final message ----

WriteDesign("final_message.csv", final[,names(all.levels)], final=T)
