rm(list=ls())

require(ggplot2)
require(glmnet)

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
               arrow = arrow(angle = 20, length = unit(0.2, 'inches'), 
                             type = 'closed', ends = 'first')) + 
  geom_segment(aes(x = log(data$lambda[length(data$lambda)*0.2]), 
                   xend = max(log(data$lambda)), 
                   y = 0.2495, yend = 0.2495),
               size = 0.3, color = '#000000', 
               arrow = arrow(angle = 20, length = unit(0.2, 'inches'), 
                             type = 'closed', ends = 'last')) +
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