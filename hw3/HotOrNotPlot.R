gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

PlotTarget <- function(target, sender.gender) {
  pdat <- melt(target, 
               varnames = c('sender.looks', 'receiver.looks'), 
               value.name = 'diff')
  pdat$positive <- pdat$diff > 0
  pdat$label <- sprintf('%.1f', 100*pdat$diff)
  pdat$label[pdat$positive] <- paste('+', pdat$label[pdat$positive], sep='')
  pdat$diff <- abs(pdat$diff)
  
  # for labeling axes
  receiver.gender <- c('Male', 'Femae')[c('Male', 'Female')!=sender.gender]
  xlab <- paste('Receiver (', receiver.gender, ') Looks', sep = '')
  ylab <- paste('Sender (', sender.gender, ') Looks', sep = '')
  
  g <- ggplot(pdat) + geom_tile(aes(x=receiver.looks, y=sender.looks, 
                                    fill=positive, alpha = diff)) +
    geom_text(aes(x=receiver.looks, y=sender.looks, label=label)) +
    scale_fill_manual(values = c(gg_color_hue(3)[1], gg_color_hue(3)[2])) + 
    labs(x = xlab, y = ylab) + 
    scale_x_continuous(breaks = c(1:11), labels = c(1:11)) + 
    scale_y_continuous(breaks = c(1:11), labels = c(1:11)) + 
    theme_minimal() + theme(legend.position='none')
  
  plot(g)
  return(g)
}

PlotCount <- function(count, coef, line.color, sender.gender) {
  pdat <- melt(count, varnames = c('sender.looks', 'receiver.looks'), 
               value.name = 'count')
  
  # for labeling axes
  receiver.gender <- c('Male', 'Femae')[c('Male', 'Female')!=sender.gender]
  xlab <- paste('Receiver (', receiver.gender, ') Looks', sep = '')
  ylab <- paste('Sender (', sender.gender, ') Looks', sep = '')
  
  g <- ggplot(pdat) +
    geom_abline(slope = coef['SenderLooks'], intercept = coef['(Intercept)'],
                color = line.color, size = 2) + 
    geom_point(aes(x=receiver.looks, y=sender.looks, size = count), alpha = 0.5) + 
    scale_size(range = c(0, 12)) + 
    labs(x = xlab, y = ylab) + 
    scale_x_continuous(breaks = c(1:11), labels = c(1:11)) + 
    scale_y_continuous(breaks = c(1:11), labels = c(1:11)) + 
    theme_minimal() + theme(legend.position='none')
  plot(g)
  return(g)
}
