require(ggplot2)
require(reshape2)
require(gridExtra)

BoxTest <- function(residuals, lags) {
  ret <- do.call(rbind, lapply(1:lags, function(lag) {
    test.res <- Box.test(residuals, lag=lag, type="Ljung-Box")
    data.frame("Lag"=lag,
               "Q-Stat"=unname(test.res$statistic),
               "P-Value"=unname(test.res$p.value))
  }))
  return(ret)
}

PlotAcf <- function(acf.out, acf_type, alpha, n, label.ci=T, style='bar') {
  dat <- with(acf.out, data.frame(lag, acf))
  if (any(dat$lag == 0)) {
    dat <- dat[-which(dat$lag == 0),]
  }
  ci <- qnorm(1-alpha/2, 0, 1)/n^.5
  lims <- 1.75*ci
  if (max(abs(dat$acf)) > lims) {
    lims <- ceiling(max(abs(dat$acf)*5))/5
    label.ci <- F
  }
  
  fill.palette <- gg_color_hue(2)
  if (all(abs(dat$acf) >= ci)) {
    fill.palette <- fill.palette[1]
  }
  if (all(abs(dat$acf) < ci)) {
    fill.palette <- fill.palette[2]
  }
  dat$fill <- abs(dat$acf) < ci
  
  g <- ggplot(data=dat, mapping=aes(x=lag, y=acf)) +
    geom_hline(aes(yintercept=0), color=alpha('black', 0.5))
  if (style=='bar') {
    g <- g + geom_bar(stat='identity', position='identity', aes(fill=fill)) + 
      geom_hline(aes(yintercept=-ci), lty='dashed', color=alpha("blue", 0.5), lwd=0.75) +
      geom_hline(aes(yintercept=ci), lty='dashed', color=alpha("blue", 0.5), lwd=0.75) +
      scale_fill_manual(values=fill.palette)
  } else {
    g <- g + 
      geom_hline(aes(yintercept=-ci), lty='dashed', color=alpha('#000000', 0.6), lwd=0.75) +
      geom_hline(aes(yintercept=ci), lty='dashed', color=alpha('#000000', 0.6), lwd=0.75) + 
      geom_point(aes(color=fill)) + 
      geom_segment(aes(x=lag, xend=lag, y=0, yend=acf, color=fill)) +
      scale_color_manual(values=fill.palette)
  }
  g <- g + guides(fill=F) + 
    coord_flip(ylim=c(-lims, lims), xlim=c(0, max(dat$lag))) + scale_x_reverse() + 
    theme_bw() + theme(legend.position='none')
  
  if (label.ci) {
    g <- g + scale_y_continuous(
      breaks=seq(-ci, ci, length.out=2),
      labels=rep_len(sprintf("%d%%CI", (1-alpha)*100), 2))
  }
  return(g)
}

Correlogram <- function(d, lag.max, alpha=0.05, label.ci = T, style = 'bar') {
  
  if (style!='bar' & style!='line')
    stop('Correlogram style must be "line" or "bar"')
  
  n <- length(d)
  alpha <- 0.05
  acf.out <- acf(d, lag.max=lag.max, plot=F)
  pacf.out <- pacf(d, lag.max=lag.max, plot=F)
  acf.box <- BoxTest(d, lag.max)
  
  g_acf <- PlotAcf(acf.out, "Autocorrelation", alpha, n, label.ci=label.ci, style=style)
  g_pacf <- PlotAcf(pacf.out, "Partial Corr.", alpha, n, label.ci=label.ci, style=style)
  g_text <- ggplot() + 
    geom_text(aes(0:lag.max, y=0, label=c("Lag", 1:lag.max), hjust="right")) +
    geom_text(aes(0:lag.max, y=0.25, label=c(
      "AC", sprintf("%0.3f", with(acf.out, acf))[-1]), hjust="right")) + 
    geom_text(aes(0:lag.max, y=0.5, label=c(
      "PAC", sprintf("%0.3f", with(pacf.out, acf))), hjust="right")) + 
    geom_text(aes(0:lag.max, y=0.75, label=c(
      "Q-Stat", sprintf("%6.3f", with(acf.box, Q.Stat))), hjust="right")) + 
    geom_text(aes(0:lag.max, y=1, label=c(
      "Prob", sprintf("%0.3f", with(acf.box, P.Value))), hjust="right")) + 
    coord_flip(ylim=c(-0.1,1), xlim=c(0, lag.max)) + scale_x_reverse(breaks=NULL) +
    labs(title='Statistics', x='', y='') +
    theme_bw() + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color="#00000000"),
      axis.ticks.x = element_line(color="#00000000"),
      plot.margin = unit(c(1, 1, 0, 0), "mm"))
  
  g_acf <- g_acf + theme(plot.margin=unit(c(1, 0, 0, 0), "mm")) + 
    labs(title="Autocorrelation", x='', y='')
  g_pacf <- g_pacf + theme(plot.margin=unit(c(1, 0, 0, 0), "mm")) + 
    labs(title="Partial Corr.", x='', y='')
  
  g <- arrangeGrob(g_acf, g_pacf, g_text, layout_matrix=matrix(c(1, 2, 3, 3), ncol=4))
  return(g)
}

VARCorrelogram <- function(resids, lag.max=20, alpha=0.05) {
  # Calculate the CCF
  ccs <- data.frame(lag=1:lag.max)
  for (first in colnames(resids)) {
    for (second in colnames(resids)) {
      res <- ccf(resids[,first], resids[,second], lag.max=lag.max, plot=F)[1:lag.max]
      df <- data.frame(lag=res$lag, acf=res$acf)
      colnames(df)[2] <- sprintf("Cor(%s,%s)", first, second)
      ccs <- merge(ccs, df, by="lag")
    }
  }

  ci <- qnorm(1-alpha/2, 0, 1)/nrow(resids)^.5
  
  ccs.melt <- melt(ccs, id.var='lag')
  # Determine the Color of the Bars
  fill.palette <- gg_color_hue(2)
  if (all(abs(ccs.melt$value) >= ci)) {
    fill.palette <- fill.palette[1]
  }
  if (all(abs(ccs.melt$value) < ci)) {
    fill.palette <- fill.palette[2]
  }
  ccs.melt$fill <- abs(ccs.melt$value) < ci

  g <- ggplot(ccs.melt, aes(x=lag, y=value, fill=fill)) +
    geom_bar(stat='identity', position='identity') + facet_wrap(~variable) +
    scale_fill_manual(values=fill.palette) +
    geom_hline(aes(yintercept=ci), lty='dashed', color=alpha('black', 0.6), lwd=0.75) +
    geom_hline(aes(yintercept=-ci), lty='dashed', color=alpha('black', 0.6), lwd=0.75) +
    theme_bw() + theme(legend.position='none') + labs(x="Lag", y="Cor")
  return(g)
}
