rm(list=ls())

require(ggplot2)
require(glmnet)
require(stringr)

source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

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
  theme_bw() + 
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
pdat$cat <- sapply(pdat$name, function(n) {
  if(grepl(':',n)) { # interaction  
    v1 <- str_split(n,':')[[1]][1]
    v2 <- str_split(n,':')[[1]][2]
    return(paste(substr(v1,1,2), ':', substr(v2,1,2), sep=''))
  }
  else(return(substr(n,1,2)))
})

# Base plot
g.base <- ggplot(pdat) + geom_bar(aes(y=coef,x=name,fill=cat), stat = 'identity') +
  labs(x = 'Variable Name', y = 'Coefficient') +
  scale_fill_discrete('Variable') +
  theme_bw() + theme(axis.text.x=element_text(angle=90))
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
ggsave('slides/coef_all.eps', g.comb)

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
  labs(x = 'Variable', y = '% of Historical Experiments with Variable') + 
  theme_bw()
plot(g)
ggsave('./slides/hist_freq.pdf', g)
