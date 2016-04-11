# Digital and Algorithmic Marketing
# Session 2, April 06, 2016
# Instructor: Sanjog Misra
# Group Homework #1


####
# Setup and Load Data----
####
rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

load("target.rdat")
load("cust.rdat")
load("target2.rdat")
load("cust2.rdat")


####
# Determine Significant Coefs ----
####
require(gamlr)

GetSignificantCoefs <- function(data, criteria) {
  # This function just runs a gamma lasso and then extracts the 
  # variable-names from the model matrix that are significant.
  mm <- model.matrix(spend ~ . - machine_id + 0, data=data)
  mdl <- gamlr(mm, data$spend, family='gaussian')
  
  g <- PlotICs(mdl, criteria, plot.smooth = F)

  IC = switch(criteria, 
              BIC=BIC(mdl),
              AIC=AIC(mdl),
              AICc=AICc(mdl))
  cfs <- coef(mdl, select=which.min(IC))
  # Drop the intercept and find out which coefs are significantly different
  # from zero. Because we used gamlr, any non-zero coefficient will be
  # significant.  Two sets of [-1s] to ensure that we're selecting the
  # correct coefficient name.
  coefs <- names(drop(cfs)[-1][which(cfs[-1] > 0)])
  return(list(icPlot=g, coefs=coefs))
}

PrintSignifCoefs <- function(name, signif) {
  print(sprintf("'%s' has %d significant coefs: %s", 
                name, length(signif[["coefs"]]), 
                paste(signif[["coefs"]], collapse = ", ")))
}

# Original Customer/Target List
cust.signif <- GetSignificantCoefs(cust, "BIC")
GGPlotSave(cust.signif$icPlot, "cust_signif")
PrintSignifCoefs("cust", cust.signif)

# New Customer/Target List
cust2.signif <- GetSignificantCoefs(cust2, "BIC")
GGPlotSave(cust2.signif$icPlot, "cust2_signif")
PrintSignifCoefs("cust2", cust2.signif)


####
# KNN using significant coefs ----
####
CalcProfits <- function(cust.data, target.data, 
                        unit_cost, signif.coefs,
                        spend_thresh=10, max_nn=20) {
  # First get the indices of the real customers.
  cst.real <- cust.data[cust.data$spend > spend_thresh,]
  
  # Then create model matrices for both the customers and the target data
  cst.mm <- model.matrix(~ . + 0, cst.real)[,signif.coefs]
  tgt.mm <- model.matrix(~ . + 0, target.data)[,signif.coefs]

  require(plyr)
  require(FNN)
  results <- ldply(1:max_nn, function(k) {
    cat(sprintf("%d,", k))
    
    # Get the k nearest target customers that are neighbors with current ones.
    query <- get.knnx(tgt.mm, cst.mm, k=k, algorithm="brute")
    
    # Uniquify the results.
    nn <- unique(as.vector(query$nn.index))
    
    # Calculate revenue and cost.
    revenue <- sum(target.data[nn, "spend"])
    cost <- unit_cost * length(nn)
    
    # Calculate other metrics.
    # (# we thought we'd get)/(# we'd get if we targeted all)
    frac.customers.captured =
      sum(target.data[nn, "spend"] > 0) / # num of customers that we thought would spend
      sum(target.data$spend > 0)          # num of customers that actually spent
    
    frac.revenue.captured =
      sum(target.data[nn, "spend"]) / sum(target.data$spend)
    
    # (# of customers we actually got) / (# we thought we'd get)
    frac.matches = mean(target.data[nn, "spend"] > 0)
    
    return(data.frame(k, revenue, unique.neighbors=length(nn), 
                      cost, profit=revenue-cost,
                      frac.customers.captured,
                      frac.revenue.captured,
                      frac.matches))
  })
  cat("done.\n")
  
  return(results)  
}

# A new DMP claims that hey have a better set of variables
# to match your customers on. 
# In particular, they have an ecom_index which they claim 
# Offers an inrementally better match for you.
# Unfortunately the cost of matching customers via them is 
# significantly higher!
cost_orig <- 0.5
cost_new <- 3.25

res.orig.gamlr <- CalcProfits(cust, target, cost_orig, cust.signif[["coefs"]])

# @k=3, revenue captured should be 0.5588503 (from lecture).
res.orig.class <- CalcProfits(cust, target, cost_orig, c("retail_index", 
                                                         "household_income6", 
                                                         "household_size6"))

res.ecom.gamlr <- CalcProfits(cust2, target2, cost_new, cust2.signif[["coefs"]])

res.ecom2.gamlr <- CalcProfits(cust2, target2, cost_new, 
                               c("hoh_oldest_age2",
                                 "hoh_oldest_age7",
                                 "ecom_index"), 
                               spend_thresh=127)

####
# Plot Profits ----
####
res.combined <- rbind(
  cbind(res.orig.gamlr, series="No ecom (Lasso)"),
  cbind(res.orig.class, series="No ecom (In-Class)"),
  cbind(res.ecom.gamlr, series="With ecom (Lasso)"),
  cbind(res.ecom2.gamlr, series="With ecom (cutoff=$127)"))

# Extract the measures we care about for the facet-wrap.
res.melt <- melt(res.combined, id.vars = c("k", "series"), 
                 measure.vars = c("profit",
                                  "frac.customers.captured",
                                  "frac.revenue.captured",
                                  "frac.matches"))
measure_to_label_map = c(
  "profit"="Profit [$]",
  "frac.customers.captured"="Fraction of Customers Captured",
  "frac.revenue.captured"="Fraction of Revenue Captured",
  "frac.matches"="Fraction of Customers Matched") # I don't like this title.

g <- ggplot(res.melt, aes(k, value, color=series)) + 
  geom_line() + geom_point() + scale_color_discrete("Series") +
  facet_wrap("variable", scales="free_y", ncol=2, 
             labeller = labeller("variable"=measure_to_label_map)) +
  theme_bw() + labs(x="# of Nearest Neighbors Included", y="")
GGPlotSave(g, "Profits")


# They have given you a sample target dataset 
# and a matched version of your customer dataset
# These are in cust2.rdat and target2.rdat
# Hint: Careful the ecom_index is in the 12th column

# Use this new data to answer the following questions.
# 1. Would you go with the new DMP? Justify your answer.
# 2. Is there any circomstance when you would ignore the 
#    matching and go after the entire target file audience? Justify.

