library(xtable)

TableAlignMultilineCenteredCM <- function(widths) {
  sprintf(">{\\centering\\let\\newline\\\\\\arraybackslash\\hspace{0pt}}m{%3.2fcm}", widths)
}

ExportTable <- function(table, file, caption, colnames=NULL, 
                        align=NULL, digits=NULL, display=NULL,
                        include.rownames=T, floating=TRUE,
                        NA.string="", align.cols=NULL) {
  if (!is.null(colnames)) { colnames(table) = colnames }
  xtb <- xtable(table, label=paste('tab:', file, sep=''), caption=caption,
                align=align, digits=digits, display=display)
  if (!is.null(align.cols)) { align(xtb) <- align.cols }
  print(xtb,
        caption.placement="top",
        sanitize.text.function=function(x){x},
        file=GetFilename(paste(file, '.tex', sep='')),
        include.rownames=include.rownames, floating=floating,
        NA.string=NA.string)
}

ExportWideTable <- function(table, file, caption, colnames=NULL, 
                            align=NULL, digits=NULL, display=NULL,
                            include.rownames=T) {
  if (!is.null(colnames)) { colnames(table) = colnames }
  
  sink(GetFilename(paste(file, '.tex', sep='')))
  cat("\\begin{table}[ht]")
  cat("\\begin{adjustwidth}{-1in}{-1in}")
  cat(print(xtable(table, label=paste('tab:', file, sep=''), caption=caption,
               align=align, digits=digits, display=display),
        caption.placement="top",
        sanitize.text.function=function(x){x},
        file="",
        include.rownames=include.rownames,
        floating = F))
  cat("\\end{adjustwidth}")
  cat("\\end{table}")
  sink()
  
}

ExportArmaTable <- function(arma, fname, caption, row.style='greek') {
  
  if (row.style!='greek' & row.style!='english') 
    stop('Arg row.style must be "greek" or "english"')
  
  b <- coef(arma)
  rhos <- b[grep('ar', names(b))]
  thetas <- b[grep('ma', names(b))]
  int <- b[rhos <- b[grep('intercept', names(b))]]
  
  tab <- data.frame(coefs=b, se=sqrt(diag(arma$var.coef)))
  
  rnames <- c()
  if (row.style=='greek') {
    if (length(rhos)>0)
      rnames <- c(rnames, paste('$\\rho_', 1:length(rhos), '$', sep=''))
    if (length(thetas)>0)
      rnames <- c(rnames, paste('$\\theta_', 1:length(thetas), '$', sep=''))
    rnames <- c(rnames, 'Intercept')
  } else {
    rnames <- names(b)
  }
  rownames(tab) <- rnames
  
  ExportTable(table = tab, file = fname, caption = caption, 
              colnames = c('Coefficient', 'Std. Error'), 
              digits=4, include.rownames = TRUE)
  
}