rm(list=ls())

require(ggplot2)
require(glmnet)
require(stringr)

source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()
SlidesTheme()
Global.PlotOpts$Prefix="slides/"
source("model.R")

# load data
load(file = "../hw4/Historical_Data.rdat")
load(file = "../hw4/cached/q4_glmnet_cv_it2.dat")


# plot(data)
pdat <- data.frame(data$lambda, data$cvm, data$cvup, data$cvlo)

g <- ggplot(pdat) + 
  geom_segment(aes(x = log(data.lambda), xend = log(data.lambda),
                   y = data.cvlo, yend = data.cvup),
               color = '#9e9e9e',
               arrow = arrow(angle = 90, length = unit(0.03, 'inches'), ends = 'both')) +
  geom_point(aes(x = log(data.lambda), y = data.cvm))

# Arrow marking complexity
g <- g + 
  geom_segment(aes(x = min(log(data$lambda)), 
                   xend = log(data$lambda[length(data$lambda)*0.8]), 
                   y = 0.2495, yend = 0.2495),
               size = 0.3, color = '#000000', 
               arrow = arrow(ends = 'first')) + 
  geom_segment(aes(x = log(data$lambda[length(data$lambda)*0.2]), 
                   xend = max(log(data$lambda)), 
                   y = 0.2495, yend = 0.2495),
               size = 0.3, color = '#000000', 
               arrow = arrow(ends = 'last')) +
  annotate('text', x = min(log(data$lambda)), y = 0.25,
           hjust = 0, vjust = 0,
           label = 'more complex', size = 5) + 
  annotate('text', x = max(log(data$lambda)), y = 0.25,
           hjust = 1, vjust = 0,
           label = 'less complex', size = 5)

# Labels and formatting
g <- g + 
  labs(x = 'log(Lambda)', y = 'Binomial Deviance (Out-of-Sample Accuracy)') +
  expand_limits(y = 0.249) +
  theme(axis.title.x = element_text(size = 15, margin = margin(10,0,0,0)),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0,15,0,0)),
        axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5))
# plot(g)

# Vertical lines to mark lambda.min, lambda.1se (initial plot)
g1 <- g +  
  geom_vline(xintercept = log(data$lambda.min), linetype = 'dashed',
             color = '#000080', size = 1) + 
  geom_vline(xintercept = log(data$lambda.1se), linetype = 'dashed', color = '#9e9e9e') + 
  annotate('text', x = log(data$lambda.min), y = 0.261,
           hjust = 1, vjust = 1, size = 5,
           label = sprintf('lambda.min  \n%.d params  ', 
                           sum(coef(data,'lambda.min')!=0)),
           color = '#000080', fontface = 'bold') +
  annotate('text', x = log(data$lambda.1se), y = 0.261,
           hjust = 0, vjust = 1, size = 5,
           label = sprintf('  lambda.1se\n  %.d params', 
                           sum(coef(data,'lambda.1se')!=0)), color = '#9e9e9e')

# Vertical lines to mark lambda.min, lambda.1se (secondary plot)
g2 <- g +  
  geom_vline(xintercept = log(data$lambda.min), linetype = 'dashed', color = '#9e9e9e') + 
  geom_vline(xintercept = log(data$lambda.1se), linetype = 'dashed', 
             color = '#000080', size = 1) + 
  annotate('text', x = log(data$lambda.min), y = 0.261,
           hjust = 1, vjust = 1, size = 5,
           label = sprintf('lambda.min  \n%.d params  ', 
                           sum(coef(data,'lambda.min')!=0)), color = '#9e9e9e') +
  annotate('text', x = log(data$lambda.1se), y = 0.261,
           hjust = 0, vjust = 1, size = 5,
           label = sprintf('  lambda.1se\n  %.d params', 
                           sum(coef(data,'lambda.1se')!=0)), 
           color = '#000080', fontface = 'bold')

ggsave('slides/regpath1.pdf', g1)
ggsave('slides/regpath2.pdf', g2)

# Predictive coefficients from cv.gamlr run ----

load("../hw4/cached/q4_glmnet_cv_it2.dat")
mdl <- data
rm(data)

mdl.coefs <- drop(coef(mdl))[-1]
mdl.coefs <- mdl.coefs[mdl.coefs!=0]

pdat <- data.frame(coef=mdl.coefs,
                   name=names(mdl.coefs))
CategoryName <- Vectorize(function(n) {
  if(grepl(':',n)) { # interaction  
    v1 <- str_split(n,':')[[1]][1]
    v2 <- str_split(n,':')[[1]][2]
    return(paste(substr(v1,1,2), ':', substr(v2,1,2), sep=''))
  }
  else(return(substr(n,1,2)))
})
pdat$cat <- CategoryName(pdat$name)

# Base plot
g.base <- ggplot(pdat) + geom_bar(aes(y=coef,x=name,fill=cat), stat = 'identity') +
  labs(x = 'Variable Name', y = 'Coefficient') +
  scale_fill_discrete('Variable') +
  theme(axis.text.x=element_text(angle=90))
plot(g.base)

# Annotate interaction terms
g.int <- g.base + 
  annotate('rect', xmin = 1.5, xmax = 3.5, 
           ymin = min(pdat$coef[(nrow(pdat)-1):nrow(pdat)]) - 0.01, ymax = 0.01, 
           alpha = 0, color = '#000000') +
  annotate('text', x = 2.5, y = min(pdat$coef[(nrow(pdat)-1):nrow(pdat)]) - 0.03,
           hjust = 0.5, vjust = 1, label = 'Only 2 interaction\nterms (V1 and V2)')
plot(g.int)

# Annotate missing variable 3 and almost missing variable 6
g.var3 <- g.base + 
  geom_segment(aes(x = 4.5, xend = 4.5,
                   y = 0, yend = -0.2),
               arrow = arrow(ends='first')) + 
  geom_segment(aes(x = 8, xend = 8,
                   y = 0, yend = -0.2),
               arrow = arrow(ends='first')) + 
  annotate('text', x = 6.1, y = -0.22, 
           hjust = 0.5, vjust = 1, 
           label = 'Variables 3 and 6 have\nlittle to no predictive power')
plot(g.var3)

g.comb <- g.int + 
  geom_segment(aes(x = 4.5, xend = 4.5,
                   y = 0, yend = -0.3),
               arrow = arrow(ends='first')) + 
  geom_segment(aes(x = 8, xend = 8,
                   y = 0, yend = -0.3),
               arrow = arrow(ends='first')) + 
  annotate('text', x = 6.1, y = -0.32, 
           hjust = 0.5, vjust = 1, 
           label = 'Variables 3 and 6 have\nlittle to no predictive power')
plot(g.comb)

# ggsave('slides/coefs_base.pdf', g.base)
# ggsave('slides/coefs_int.pdf', g.int)
# ggsave('slides/coefs_missing.pdf', g.var3)
ggsave('slides/coef_all.pdf', g.comb)


# New Coefficients --------------------------------------------------------
dat.exp <- RelevelData(rbind(
  read.csv("results/experiment1.csv"),
  read.csv("results/experiment2.csv")), exp.levels)
mdl.final <- FinalModel(dat.exp)
mdl.final1 <- FinalModel(RelevelData(read.csv("results/experiment1.csv"), 
                                     exp.levels))
summary(mdl.final)
summary(mdl.final1)

ExtractNameValueSe <- function(mdl) {
  coef.mtx <- summary(mdl)$coefficients[-1,]
  data.frame(
    name=rownames(coef.mtx),
    value=coef.mtx[,'Estimate'], 
    value.se=coef.mtx[,'Std. Error'],
    row.names = NULL)
}
alpha.ci <- 0.05
rbind(
  cbind(ExtractNameValueSe(mdl.final1), series="#1"),
  cbind(ExtractNameValueSe(mdl.final), series="#1 & #2")) %>%
  mutate(name=gsub("I\\(V(\\d+)\\s+==\\s+(\\d+)\\).+", "V\\1\\2", name),
         category=CategoryName(name),
         value.upper=value + qnorm(1-alpha.ci/2, sd=value.se),
         value.lower=value + qnorm(alpha.ci/2, sd=value.se)) %>%
  group_by(series) %>%
  arrange(category, name) -> mdl.final.coefs.unordered

sort.series <- "#1 & #2"
indices <- (1:nrow(filter(mdl.final.coefs.unordered, series==sort.series)))-1
filter(mdl.final.coefs.unordered, series==sort.series) %>% 
  ungroup() %>%
  mutate(index=indices) %>%
  select(name, index) %>%
  left_join(mdl.final.coefs.unordered, by=c("name")) -> mdl.final.coefs

g <- ggplot(mdl.final.coefs, aes(x=index, y=value, fill=category, alpha=series)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=value.lower, ymax=value.upper), 
                width=0.5, position=position_dodge(0.9), 
                color="gray48", show.legend=F) +
  scale_x_continuous(breaks=unique(mdl.final.coefs$index),
                     labels=unique(mdl.final.coefs$name),
                     expand=c(0.01, 0)) +
  scale_alpha_discrete("Experiment", range=c(0.5, 1)) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_fill_discrete("Variable") +
  labs(x="Variable Name", y="Coefficient")
plot(g)
ggsave('slides/final_model_coefs_crazy.pdf', g)

g <- ggplot(filter(mdl.final.coefs, series=="#1 & #2"), 
            aes(x=index, y=value, fill=category)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=value.lower, ymax=value.upper), 
                width=0.4, position=position_dodge(0.9), 
                color="gray48", show.legend=F) +
  scale_x_continuous(breaks=unique(mdl.final.coefs$index),
                     labels=unique(mdl.final.coefs$name),
                     expand=c(0.01, 0)) +
  scale_alpha_discrete("Experiment", range=c(0.5, 1)) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_fill_discrete("Variable") +
  labs(x="Variable Name", y="Coefficient")

# Annotate final_model_coefs.pdf
g <- g +   
  annotate('rect', xmin = 1.5, xmax = 5.5, 
           ymin = as.numeric(mdl.final.coefs %>% 
                               filter(name=='V15:V23', series=='#1 & #2') %>% 
                               select(value.lower)) - 0.05, 
           ymax = as.numeric(mdl.final.coefs %>% 
                               filter(name=='V15:V26', series=='#1 & #2') %>% 
                               select(value.upper)) + 0.05, 
           alpha = 0, color = '#000000') +
  annotate('text', x = 3.5, 
           y = as.numeric(mdl.final.coefs %>% 
                            filter(name=='V15:V26', series=='#1 & #2') %>% 
                            select(value.upper)) + 0.15,
           hjust = 0.5, vjust = 0, label = 'Interact V1 and V2\nlevels (5,6) with (2,6)') +
  annotate('text', x = 11, 
           y = as.numeric(mdl.final.coefs %>% 
                            filter(name=='V63', series=='#1 & #2') %>% 
                            select(value.lower)) - 0.1,
           hjust = 0.5, vjust = 1,
           label = 'V6 is probably\njust noise')
plot(g)
ggsave('slides/final_model_coefs.pdf', g)

# Appendix plots -----

load('../hw4/Historical_Data.rdat')

# Gut check -- do we observe all vars about equally in the historical data?
# Yes, thankfully
inc.vars <- sapply(histdat, function(df) {
  return(1:9 %in% as.numeric(gsub('V','', grep('V',colnames(df),value=T))))
})
inc.vars <- data.frame(var = paste('V', 1:9, sep = ''),
                       obs = rowMeans(inc.vars),
                       lab = sprintf('%.1f', 100*rowMeans(inc.vars)))

g <- ggplot(inc.vars) + geom_bar(aes(x = var, y = obs - .03), stat = 'identity') + 
  geom_text(aes(x = var, y = obs, label = lab)) + 
  labs(x = 'Variable', y = '% of Historical Experiments with Variable')
plot(g)
ggsave('./slides/hist_freq.pdf', g)

