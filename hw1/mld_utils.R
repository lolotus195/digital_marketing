PlotICs <- function(mdl, select, plot.smooth=FALSE) {
  ICs <- data.frame(lambda=mdl$lambda,
                    AIC=AIC(mdl),
                    AICc=AICc(mdl),
                    BIC=BIC(mdl))
  
  IC = switch(select, 
              BIC=ICs$BIC, 
              AIC=ICs$AIC, 
              AICc=ICs$AICc)
  
  min.idx <- which.min(IC)
  
  if (plot.smooth) {
    geom.sel = geom_smooth
  } else {
    geom.sel = geom_line
  }
  require(reshape2)
  g <- ggplot(data=melt(ICs, id.vars = "lambda")) +
    geom_vline(aes(xintercept=log(ICs$lambda[min.idx])), lty=2, alpha=0.5) +
    geom.sel(aes(x=log(lambda), y=value, color=variable), alpha=0.75, lwd=1.25) +
    scale_color_discrete("Criteria") +
    labs(title="Model ICs", x="ln(lambda)", y="Log-Likelihood") +
    theme_bw()
  return(g)
}
