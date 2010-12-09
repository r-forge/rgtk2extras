# Taken from ?identify
## A function to use identify to select points, and overplot the
## points with another symbol as they are selected
identifyPch <- function(x, y=NULL, txt=NULL, n=length(x), col="black", pch=19, ...)
{
    xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
    sel <- rep(FALSE, length(x)); res <- integer(0)
    while(sum(sel) < n) {
        rv <- identify(x[!sel], y[!sel], n=1, pos=TRUE, plot=FALSE, ...)
        ans <- rv$ind
        pos <- rv$pos
        if(!length(ans)) break
        ans <- which(!sel)[ans]
        points(x[ans], y[ans], pch = pch, cex=0.5, col="red")
        if(!is.null(txt)) text(x[ans], y[ans], txt[ans], pos=pos, cex=0.7, col=col)
        sel[ans] <- TRUE
        res <- c(res, ans)
    }
    res
}

  # Specifically for statistical results having data as estimate, p-value columns.
VolcanoPlot <- function(data, setAxes=FALSE, add.labels=FALSE, add.counts=FALSE, effect.line = 0, xmin=-4, xmax=4, ymin=1E-20, ymax=1, add.line=0.05, do.density=FALSE, sampleN=1000, do.identify=FALSE, theColor=NULL, pch=NULL, cex=NULL){
  if(!class(data)%in%c("data.frame", "matrix", "array")) stop("Function requires tabular data")  
  if(!(!is.null(dim(data)) && length(dim(data)) == 2 && dim(data)[2] > 0 && dim(data)[2]%%2 == 0))
    stop("Function requires data with even number of columns for estimates and p-values")  
  old.par <- par(no.readonly = TRUE) # all par settings which
  on.exit(par(old.par))
   
  p.idx <- seq(2, ncol(data), by=2)
  ncols = ceiling(sqrt(ncol(data)/2))
  nrows = ceiling((ncol(data)/2)/ncols)
  if(!do.density)
    par(mfrow = c(nrows, ncols))
  if(!setAxes) { 
    p.matrix <- data.matrix(data[,p.idx,drop=F])
    #if(any(p.matrix < 0 | 1+1E-6 < p.matrix, na.rm=T)) stop("P-values outside the range [0, 1] detected. Please check the data input.")
    p.zeroes <- p.matrix==0
    n.zeroes <- sum(p.zeroes, na.rm=T)
    ylims <- range(p.matrix[!p.zeroes], na.rm=T)
    if(n.zeroes) {
      p.matrix[p.zeroes] <- ylims[1]
      data[,p.idx] <- p.matrix
      quick_message(paste(n.zeroes, "p-values of 0 converted to smallest observed value over 0"))
    }
    x.matrix <- data.matrix(data[,p.idx-1,drop=F])
    xlims <- range(x.matrix, na.rm=T)
  } else {
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)    
  } 
  par(mar=c(2,2,2,1))
  par(oma=c(2,2,2,0))  


  for(ii in p.idx)
  {
    xx <- na.omit(data[,(ii-1):ii])
    if(!missing(theColor))
      if(!is.null(attr(xx, "na.action"))) theColor <- theColor[-attr(xx, "na.action")]
    
      # Plot points in random order if they're colored
    if(!missing(theColor)){
      theSample <- sample(nrow(xx))
      xx <- xx[theSample,,drop=F]       
      theColor <- theColor[theSample]  
    }
    
    xx <- xx[rowSums(is.finite(xx))==2,,drop=F]
    if(is.null(rownames(xx)) && dim(xx)[1]) rownames(xx) <- 1:dim(xx)[1]
    eff <- xx[,1]
    pval <- xx[,2]
    if(any(pval < 0 | 1+1E-6 < pval)) warning("Function requires all values in p-value columns to be between 0 and 1")
    #nlp <- -log(pval)
    nlp <- pval
    if(do.density){
     nlp <- -log(pval)
      x11()
      if(sampleN > 0){
        ss <- sample(length(eff), sampleN, replace=T)
        nlp <- nlp[ss]; eff <- eff[ss]
      }
      ss <- sample(length(eff), sampleN, replace=T)
      nlp = -log(pval[ss])
      f2 <- kde2d(eff[ss], nlp, n = 100,
        lims = c(range(eff[ss], na.omit=T), range(nlp, na.omit=T)),
        h = c(width.SJ(eff[ss], method="dpi"), width.SJ(nlp, method="dpi")) )
    #point.densities <- f2$z[cbind(findInterval(eff, f2$x), findInterval(nlp, f2$y))]
    ncol = 50
    #colsToUse <- rev(terrain.colors(ncol))
    #point.cols <- colsToUse[findInterval(point.densities, seq(min(f2$z),
    #  max(f2$z),length=ncol))]

    filled.contour(f2$z, x =f2$x, y = f2$y,  plot.title = paste(colnames(data)[ii], "p-value vs. fold change"),
    	xlim=ifelse(rep(setAxes, 2), c(xmin, xmax), range(f2$x)), ylim=ifelse(rep(setAxes, 2), c(ymin, ymax), range(f2$y)),
    	levels=seq(min(f2$z), max(f2$z),length=ncol),
    	col=rev(terrain.colors(ncol)), main = colnames(data)[ii],
        xlab = "Effect Size", ylab = "-Log(p)",
         plot.axes={ axis(1); axis(2); points(eff, nlp, pch="."); abline(h = -log(add.line), lty=2) }
        )
    #plot(eff, nlp, col=point.cols, main = colnames(data)[ii],
    # xlab = "Effect Size", ylab = "-Log(p)", pch = ifelse(length(eff) > 5000, ".", 18))
    } else {
        # use whatever's before the "." in the column name as the title, if possible    
      if(!is.null(colnames(data)))
        ppt <- strsplit(colnames(data)[ii], ".", fixed=T)[[1]][1]
      else 
        ppt <- "p-value vs. fold change"
      sigidx <- nlp <= add.line & abs(eff) > effect.line
            
      if(missing(theColor)) theColor=ifelse(sigidx, "navy", "gray50")
      if(missing(pch)) pch = 19
      if(missing(cex)) cex=0.5
      plot(eff, nlp, main = ppt, xlim=xlims, ylim=rev(ylims),
        xlab = "", ylab = "", pch=pch, cex=cex, col=theColor, log="y")
      
      if(add.labels)
        text(eff[sigidx], nlp[sigidx], rownames(xx)[sigidx], pos=4, cex=0.7)
      
      if(add.counts){
        sig_up <- sum(sigidx & eff > 0, na.rm=T)
        sig_down <- sum(sigidx & eff < 0, na.rm=T)        
        sig_tot <- nrow(xx)
        mtext(paste(sig_down, "down, ", sig_up, "up\n", sig_tot, "total ( +", nrow(data)-sig_tot, "NA)"), side=3, padj=1.2, cex=0.7)
      }
            
    abline(h = add.line, lty=2)
    text(xlims[1], add.line, signif(add.line, 2), cex=0.7, adj=c(0, 1))
    
    if(effect.line > 0){
      abline(v = c(-1, 1)*effect.line, lty=3, col="grey50")    
      par("srt" = -90)
      text(effect.line, sqrt(prod(ylims)),  sprintf("%.1f", effect.line), adj=c(0, 1), cex=0.7, col="grey50")
      par("srt" = 0)
    }
    if(do.identify)  
      identifyPch(x=eff, y=nlp, txt=rownames(xx), col=ifelse(add.labels, "red", "black"))
   }
  }
  
  mtext("Log fold change", side = 1, outer=T)
  mtext("p-value", side = 2, outer=T, padj=-0.5)    
  mtext("Volcano plot (p-value vs. log fold change)", side = 3, outer=T)  
  
  return(recordPlot())
}

VolcanoPlot.dialog <- list(
  title = "Volcano-style effects plot",
  label = "Plot effect size versus negative log P-value", keep.open=TRUE,
  data.dataframeItem = "", label = "Table of Effects and P-values",
  add.line.numericItem = 0.05, label = "P-Value Cutoff",
  effect.line.numericItem = 0.0, label = "Log Fold Change Cutoff",  
    tooltip = "Leave this 0 for no line",
  add.labels.trueFalseItem = FALSE, label = "Add Labels",
    tooltip = "Add labels to all points lying within cutoffs",
  add.counts.trueFalseItem = TRUE, label = "Add Counts",    
    tooltip = "Add a count of significant values",  
  do.identify.trueFalseItem = FALSE, label = "Pick Points",
  BREAK=TRUE,        
  setAxes.trueFalseItem = FALSE, label = "Set Axis Limits",
    signal = c("default", "toggle.sensitive", "xmin", "xmax", "ymin", "ymax"),  
  xmin.numericItem = -4, label = "X-Axis Minimum", indent=10,
  xmax.numericItem = 4, label = "X-Axis Maximum",  indent=10,
  ymin.numericItem = 1E-20, label = "Minimum P-Value", indent=10,
  ymax.numericItem = 1, label = "Maximum P-Value", indent=10
)
