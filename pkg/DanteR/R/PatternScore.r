# Written by Ashoka D. Polpitiya
# for the Department of Energy (PNNL, Richland, WA)
# Copyright 2007, Battelle Memorial Institute
# E-mail: ashoka.polpitiya@pnl.gov
# Website: http://omics.pnl.gov/software
# -------------------------------------------------------------------------
#
# Licensed under the Apache License, Version 2.0; you may not use this file except
# in compliance with the License.  You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#
# R Plotting functions used in DAnTE
# -------------------------------------------------------------------------

PatternSearch <- function(Data, patterns)
{
    corrTable <- numeric(0)
    columnames <- numeric(0)
    Data <- Data[complete.cases(Data),]
    for (i in 1:dim(patterns)[2])
    {
        # Get correlations:
        corrVals <- cor(t(Data), patterns[,i], method="kendall",
                    use="pairwise.complete.obs")
        corrTable <- cbind(corrTable, corrVals)
        columnames <- c(columnames, paste("Pattern", i, sep="_"))
    } 
    colnames(corrTable) <- columnames
    return (corrTable)    
}


  # A "GUI" for specifying patterns
  # Can create a nonexistent return value if window is closed.
get_pattern <- function(items){
  N <- length(items)
  ylim <- c(0, 1.1)
  range_y <- c(0, 1)
  text_pos <- c(0.5, 1.1)
  X11()
  best.width <- function(v) 2 + max(0.3*nchar(v))
  par("mar" = c(best.width(items),4,1.5, 4))            
  plot(1, type = "n", ylim=ylim, xlim=c(0.5, N+0.5), main = "Click on vertical lines to set values, then 'Finish'", xaxt="n", xlab = "", ylab = "Relative Value", yaxt="n")
  axis(1, at=1:N, label=items, las=2)
  
  yax <- seq(0, 100, by=25)
  axis(2, at=yax/100, label=paste(yax, "%", sep=""))
  
  abline(v=1:N, lty=2)
  fstr <- "Click To Finish"
  xm <- strwidth(fstr)
  ym <- strheight(fstr)
  my.mar = 0.25
  
  rcc <- c(0.5-xm*(my.mar), text_pos[2]-my.mar*ym, 
    0.5+(1+my.mar)*xm, text_pos[2]+ym*(1+my.mar)) # rectangle coordinates
  rect(rcc[1], rcc[2], rcc[3], rcc[4], col="white")
  text(text_pos[1], text_pos[2],  fstr, adj=c(0,0), col="red")    
  sel.points <- rep(0.5, N)  
  points(1:N, sel.points, pch = 18, cex=2)
 
  names(sel.points) <- items
  while(1) {
      ac <- locator(1)
        # clicked in rectangle or stopped locating
      if(is.null(ac) || (rcc[1] < ac$x && ac$x < rcc[3] && rcc[2] < ac$y && ac$y < rcc[4])){ 
        graphics.off()
        break
      }
      if(ac$y < range_y[1]) ac$y <- range_y[1]
      if(range_y[2] < ac$y) ac$y <- range_y[2]
      
      ans.x <- findInterval(ac$x, seq(1.5, N-0.5))+1
      points(ans.x, sel.points[ans.x], pch = 18, col="white", cex=2)
      abline(v=ans.x, col="white", lwd=3) # erase the old line
      abline(v=ans.x, lty=2)
      
      sel.points[ans.x] <- ac$y
      points(ans.x, sel.points[ans.x], pch = 18, cex=2) #redraw point      
      if(ans.x < rcc[3]){ # redraw rect
        rect(rcc[1], rcc[2], rcc[3], rcc[4], col="white")
        text(text_pos[1], text_pos[2], fstr, adj=c(0,0), col="red")           
      }
  }

  return(sel.points)
}

  # Data - your crosstab
  # pattern - vector of numbers
  # by.factor - if this is false, just correlate data with pattern
  #   if true, create a pattern with one number per factor level
  # factor_field - which field to use from data's column metadata
PatternScore <- function(data, patternList, by_factor=FALSE, factors=NULL){

  if(length(patternList) < 2) stop("No pattern specified. Please click 'Specify Pattern' to fill the list.")
  pattern <- patternList
  if(by_factor){
    cmt <- get.column.metadata.table(data)
    cmt_key <- get.column.metadata.key(data)    
    stopifnot(factors%in%colnames(cmt))
    fcol <- cmt[,factors]
    stopifnot(length(pattern) == length(unique(fcol)))
    pattern <- pattern[match(fcol, unique(fcol))]
    pattern <- pattern[match(colnames(data), cmt[,cmt_key])]
  }
  pattern <- as.numeric(pattern)
  #return()
  scores <- t(cor(pattern, t(scale(data)), use="pairwise.complete.obs"))
  colnames(scores) <- "Pattern.Score"
  scores <- scores[order(scores[,1], decreasing=TRUE),,drop=F] # Sort
  attr(scores, "Row_Metadata") <- attr(data, "Row_Metadata")
  return(scores)
}

PatternScore.dialog <- list(
  label = "Find the correlation between dataset rows and a specified set of numbers",
  title = "Score Patterns",
  data.dataframeItem = "T_Data", label = "Choose Dataset",
    signal = c("default", "get.dataset.factors", "factors"),  
    signal = c("default", "set.to", "patternList", user.data = NULL),

  by_factor.trueFalseItem = FALSE, label = "Specify A Factor Pattern",
    signal = c("default", "toggle.sensitive", "factors"),
  factors.choiceItem = NULL, label = "Factors to specify patterns over", indent=10,
    signal = c("default", "set.to", "patternList", user.data = NULL), BREAK=T,
  launchPattern.buttonItem = "   Specify Pattern...   ", suppress=TRUE,
    signal = c("clicked", function(item, dataW, patternList, by_factor, factors){
      data <- safe.eval(get.value(dataW))
      cmt <- get.column.metadata.table(data)
      cmt_key <- get.column.metadata.key(data)  
      if(get.value(by_factor)){
        if(is.null(cmt_key) || !cmt_key%in%colnames(cmt)) {
          quick_message("Data has badly specified column metadata/factors.")
          return()
        }      
        factor_field <- get.value(factors)
        stopifnot(factor_field%in%colnames(cmt))
        fcol <- cmt[,factor_field]
        items <- as.character(unique(fcol))
      } else {
        items <- colnames(data)
      }
      xx <- get_pattern(items) # bring up graphical interface 
      set.value(patternList, xx)
    }, "data", "patternList", "by_factor", "factors"),     
  patternList.listItem = NULL, show.arrows = FALSE, label = "Specified Pattern"
)







