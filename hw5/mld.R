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
IsElasticityPositive <- function(mdl, var.prefix, var.name, alpha) {
  # Form the path for the interaction term
  var.path <- sprintf('prc:%s%s', var.prefix, var.name)
  
  coefs <- summary(mdl)$coef

  if (!(var.path %in% rownames(coefs))) {
    if (!(var.path %in% names(coef(mdl)))) {
      warning(sprintf(
        "could not find coefficient: %s, assumming base-level", var.path))
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Get the coefficients' standard errors
  se.price <- coefs['prc','Std. Error']
  se.price.int <- coefs[var.path,'Std. Error']
  
  # Form the overall coefficient standard error
  se.both <- sqrt(se.price^2 + se.price.int^2)
  
  elasticity <- unname(
    coefs['prc','Estimate'] + coefs[var.path, 'Estimate'] +
      qnorm(1-(alpha/2), sd=se.both))
  return(elasticity > 0)
}

FindOptimalProfit <- function(mdl, mc, lower, upper, N, 
                              length.out=100, fixed.params=NULL, 
                              alpha.elasticity=0.50,
                              optim.non.target.mdl = NULL) {
  # Adds the extra params needed to make a prediction (job_state, etc)
  AddExtraParams <- function(dat) {
    if (!is.null(fixed.params)) {
      dat <- cbind(dat, fixed.params)
    }
    return(dat)
  }
  
  # Returns a function to use when predicting profits
  # if se.fit is true then the upper/lower bounds of the fit are returned.
  PredictProfitFn <- function(mdl, se.fit) {
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

  if (is.null(optim.non.target.mdl)) {
    # Find the optimum value.
    opt <- optim(lower, PredictProfitFn(mdl, F), lower=lower, upper=upper,
                 method = "Brent", control=list(fnscale=-1))
    opt.df <- data.frame(prc=opt$par[1], PredictProfitFn(mdl, T)(opt$par[1]),
                         simple=F)
  } else {
    prefix <- colnames(fixed.params)[1]
    varname <- as.character(fixed.params[1,1])

    # Find the optimum value.
    use.mdl <- mdl
    simple <- F
    if (IsElasticityPositive(mdl, prefix, varname, alpha.elasticity)) {
      use.mdl <- optim.non.target.mdl
      simple <- T
    }
    opt <- optim(lower, PredictProfitFn(use.mdl, F),
                 lower=lower, upper=upper, 
                 method = "Brent", control=list(fnscale=-1))
    opt.df <- data.frame(
      prc=opt$par[1], PredictProfitFn(use.mdl, T)(opt$par[1]),
      simple=simple)
  }

  # Now get some data to plot.
  plot.df <- data.frame(prc=seq(lower, upper, length.out = length.out))
  plot.df <- cbind(plot.df, PredictProfitFn(mdl, T)(plot.df$prc))
  
  # Bind the two data-series together and return them.
  rbind(mutate(opt.df, series="optim"),
        mutate(plot.df, series="pred", simple=F))
}

N=nrow(dat)
optim.simple <- ldply(c(0, 10, 30, 50, 100), function(mc) {
  FindOptimalProfit(mdl.simple, mc, 0.1, 500, N) %>%
    mutate(type="none", mc=mc)
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
OptimCategory <- function(mc, price.min=0.1, price.max=1000) {
  ddply(
    dat %>% group_by(job_category_classified_by_ai) %>% tally(), 
    .(job_category_classified_by_ai, n),
    
    function(param) {
      FindOptimalProfit(
        mdl.cat, mc, price.min, price.max, 
        param$n,
        fixed.params=data.frame(
          "job_category_classified_by_ai"=
            param$job_category_classified_by_ai
        ),
        optim.non.target.mdl = mdl.simple
      )
    }, .progress = "text") %>% 
    rename(segment=job_category_classified_by_ai) %>%
    mutate(segment=as.character(segment),
           type="category",
           mc=mc)
}

OptimState <- function(mc, price.min=0.1, price.max=1000) {
  ddply(
    dat %>% group_by(job_state) %>% tally(),
    .(job_state, n),
    
    function(param) {
      FindOptimalProfit(
        mdl.state, mc, price.min, price.max, 
        param$n,
        fixed.params=data.frame(
          "job_state"=param$job_state
        ),
        optim.non.target.mdl = mdl.simple
      )
    }, .progress = "text") %>%
    rename(segment=job_state) %>%
    mutate(segment=as.character(segment),
           type="state",
           mc=mc)
}

rbind(OptimCategory(0),
      OptimState(0),
      OptimCategory(10),
      OptimState(10)) %>%
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
alpha <- 0.05
optim.summary.tmp %>%
  bind_rows(filter(optim.simple, series=="optim", mc %in% c(0, 10)) %>%
              select(-series, -prc)) %>%
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

optim.all %>% 
  group_by(type) %>% 
  filter(series=="optim", simple==T, mc==0) %>% 
  select(segment, n) -> optim.simple

g <- ggplot(optim.all %>% filter(series=="optim", simple==F) %>% 
         select(prc, type, mc)) +
  geom_histogram(aes(x=prc), bins=30) + 
  facet_wrap(mc ~ type, labeller=labeller(
    mc=function(x) sprintf("Marginal Cost: $%s", x),
    type=c("category"="Segemented by Job Category",
           "state"="Segemented by Job Location (State)"))) +
  labs(x="Optimal Price [$]", y="Count")
GGPlotSave(g, "q2_hist")


# Source: local data frame [6 x 5]
# 
# type    mc   profit profit.upper profit.lower
# (chr) (dbl)    (dbl)        (dbl)        (dbl)
# 1 category     0 518223.9     577892.9     458554.8
# 2 category    10 501230.7     561153.6     441307.8
# 3    state     0 462444.4     491844.5     433044.4
# 4    state    10 444697.6     474271.0     415124.3
# 5     none     0 481084.9     515457.7     446712.2
# 6     none    10 464962.1     499593.6     430330.6