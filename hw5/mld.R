rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

require(plyr)
require(dplyr)

# Load Data ---------------------------------------------------------------
LoadData <- function() {
  foo <- load("ZipDat.Rd")
  return(eval(parse(text=foo[[1]])))
}

dat <- LoadData()
dat$job_state <- factor(dat$job_state)

# alpha for calculating error bounds.

alpha <- 0.05
# Data Exploration --------------------------------------------------------
qplot(dat$prc)

dat %>%
  group_by(prc) %>%
  summarize(r_sub=mean(SUB)) -> dat.summary

# Q1 ----------------------------------------------------------------------
mdl.simple <- glm(SUB ~ prc, data=dat, family="binomial")
summary(mdl.simple)

pr.df <- data.frame(prc=seq(19, 600, length.out = 100))
pr.df$r_sub <- predict(mdl.simple, newdata=pr.df, type='response')
to.plot <- rbind(select(mutate(dat.summary, series="Data"),
                        prc, r_sub, series),
                 filter(select(mutate(pr.df, series="Fitted"), 
                               prc, r_sub, series),
                        prc < max(dat.summary$prc)))
g <- ggplot(data=to.plot, aes(x=prc, y=r_sub, color=series)) +
  geom_line() +
  scale_color_discrete("Series") +
  labs(x="Price [$]", y="Subscription Rate")
GGPlotSave(g, "q1_fit")

# Find and Plot the Optimums ----------------------------------------------
WithinPercentage <- function(v, l, u, pcnt) {
  (abs((v-u)/u) < pcnt) | (abs((v-l)/l) < pcnt)
}

FindOptimalProfit <- function(mdl, mc, lower, upper, N, 
                              length.out=100, fixed.params=NULL, 
                              bounds.tolerance=0.001, alpha.predict=0.05) {
  # Adds the extra params needed to make a prediction (job_state, etc)
  AddExtraParams <- function(dat) {
    if (!is.null(fixed.params)) {
      dat <- cbind(dat, fixed.params)
    }
    return(dat)
  }
  
  # Returns a function to use when predicting profits
  # if se.fit is true then the upper/lower bounds of the fit are returned.
  PredictProfitFn <- function(se.fit) {
    function(prc) {
      newdata <- AddExtraParams(data.frame(prc=prc))
      
      # Suppress Warnings during prediction.
      options(warn=-1)
      subs.rate <- predict(mdl, newdata=newdata, 
                           type="response", se.fit=se.fit)
      options(warn=0)
      
      # If the user bound the function with se.fit = False then just return
      # the fitted value.
      if (!se.fit) {
        return((prc-mc)*subs.rate * N)
      }
      
      # If the user wants the fit bounds, then calculate the se as well.
      return(data.frame(profit=(prc-mc)*subs.rate[["fit"]] * N,
                        profit.se=(prc-mc)*subs.rate[["se.fit"]] * N))
    }
  }
  
  # Find the optimum value.
  opt <- optim(lower, PredictProfitFn(F), lower=lower, upper=upper,
               method = "Brent", control=list(fnscale=-1))
  # Brent will always "converge", so need to check the tolerance.
  if (WithinPercentage(opt$par[1], lower, upper, bounds.tolerance)) {
    # Was within tolerance of upper/lower bound, toss the result out.
    opt.df <- data.frame(prc=NA, profit=NA, profit.se=NA)
  } else {
    # Calculate the optimum with upper/lower bounds.
    opt.df <- data.frame(prc=opt$par[1], PredictProfitFn(T)(opt$par[1]))
  }
  
  # Now get some data to plot.
  plot.df <- data.frame(prc=seq(lower, upper, length.out = length.out))
  plot.df <- cbind(plot.df, PredictProfitFn(T)(plot.df$prc))
  
  # Bind the two data-series together and return them.
  rbind(mutate(opt.df, series="optim"),
        mutate(plot.df, series="pred"))
}

N=nrow(dat)
optim.simple <- ldply(c(0, 10, 30, 50, 100), function(mc) {
  cbind(mc=mc, FindOptimalProfit(mdl.simple, mc, 0.1, 500, N,
                                 alpha.predict = alpha))
}, .progress="text")

g <- ggplot(filter(optim.simple, series=="pred"),
            aes(x=prc, y=profit, color=factor(mc))) + 
  geom_line() +
  geom_point(data=filter(optim.simple, series=="optim"), pch=4, size=4, 
             show.legend = F) +
  scale_color_discrete("Marginal\nCost [$]") +
  labs(x="Price [$]", y=sprintf("Profit [$] (N=%d)", N))
GGPlotSave(g, "q1_profits")

# Q2 - Find Optimal Revenue Source ----------------------------------------
mdl.cat <- glm(SUB ~ prc*job_category_classified_by_ai, data=dat, 
               family="binomial")
mdl.state <- glm(SUB ~ prc*job_state, data=dat, family="binomial")

# Run Optimization --------------------------------------------------------
OptimCategory <- function(mc, alpha, price.min=0.1, price.max=1000) {
  ddply(
    dat %>% group_by(job_category_classified_by_ai) %>% tally(), 
    .(job_category_classified_by_ai, n),
    
    function(param) {
      FindOptimalProfit(
        mdl.cat, mc, price.min, price.max, 
        param$n, alpha.predict=alpha,
        fixed.params=data.frame(
          "job_category_classified_by_ai"=
            param$job_category_classified_by_ai
        )
      )
    }, .progress = "text") %>% 
    rename(segment=job_category_classified_by_ai) %>%
    mutate(segment=as.character(segment),
           type="category",
           mc=mc)
}

OptimState <- function(mc, alpha, price.min=0.1, price.max=1000) {
  ddply(
    dat %>% group_by(job_state) %>% tally(),
    .(job_state, n),
    
    function(param) {
      FindOptimalProfit(
        mdl.state, mc, price.min, price.max, 
        param$n, alpha.predict = alpha, 
        fixed.params=data.frame(
          "job_state"=param$job_state
        )
      )
    }, .progress = "text") %>%
    rename(segment=job_state) %>%
    mutate(segment=as.character(segment),
           type="state",
           mc=mc)
}

rbind(OptimCategory(0, alpha),
      OptimState(0, alpha),
      OptimCategory(10, alpha),
      OptimState(10, alpha)) %>%
  mutate(segment=factor(segment)) -> optim.all

# Summarize and Plot ------------------------------------------------------
g <- ggplot(filter(optim.all, series=="pred"), 
       aes(x=prc, y=profit, color=segment)) +
  geom_line() + 
  facet_wrap(mc ~ type, labeller=labeller(
    mc=function(x) sprintf("Marginal Cost: $%s", x),
    type=c("category"="Segemented by Job Category",
           "state"="Segemented by Job Location (State)"))) +
  scale_color_discrete(guide=F) +
  labs(x="Price [$]", y="Profit [$]")
GGPlotSave(g, "q3_predicted")

# Q3 More Summarization ---------------------------------------------------
# Which categories/states have undetermined profits.
optim.all %>%
  filter(series=="optim") %>%
  group_by(type) %>%
  filter(is.na(profit)) %>%
  select(segment, n) -> optim.na

# Summarize the profits and standard errors.
optim.all %>% 
  group_by(type, mc) %>%
  na.omit() %>% 
  filter(series=="optim") %>%
  select(-series) %>%
  summarize(profit=sum(profit), 
            profit.se=sqrt(sum(profit.se^2))) -> optim.summary.tmp

# Now add the no segementation value as well.
optim.summary.tmp %>%
  bind_rows(filter(optim.simple, series=="optim", mc %in% c(0, 10)) %>%
              mutate(type="none") %>% select(-prc, -series)) %>%
  mutate(profit.upper=profit + qnorm(1-alpha/2, sd=profit.se),
         profit.lower=profit + qnorm(alpha/2, sd=profit.se)) %>%
  select(-profit.se) -> optim.summary

g <- ggplot(optim.summary, 
            aes(y=profit, x=relevel(factor(type), ref="none"))) + 
  geom_bar(stat='identity') + 
  facet_wrap(~ mc, labeller=labeller(.default=function(x) {
    sprintf("Marginal Cost: $%s", x)
  })) +
  scale_x_discrete(labels=c("none"="None", 
                            "state"="Job Location (State)",
                            "category"="Job Category")) +
  geom_errorbar(aes(ymin=profit.lower, ymax=profit.upper), width=0.25) +
  labs(x="Segmentation Type", y="Profit [$]")
GGPlotSave(g, "q3_profits_summary")
