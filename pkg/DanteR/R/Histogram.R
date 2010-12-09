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



Histogram.dialog <- list(
  keep.open = TRUE,
  label = "Compute and show a histogram of the given data columns",
  Data.dataframeItem="T_Data", label="Data Source",
    signal = c("default", "get.colnames", "dummy"),
    signal = c("default", "set.to", "Data.Columns", user.data = character(0)),      
    signal = c("default", "get.dataset.factors", "color.key"),                    
  dummy.listItem = NULL, label = "Available Columns", suppress=T, show.arrows=F,
  Data.Columns.listItem=NULL, label="Selected Data Columns",  BREAK=T,
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy"),    
  #cells.radiobuttonItem = c(value="Sturges", "Scott", "Freedman-Diaconis"), label = "Algorithm for Histogram Breakpoints",  
  add.dist.trueFalseItem = FALSE,  label = "Superimpose Normal?", tooltip = "Add the data density and a normal distribution curve over the top?",
  do.cells.trueFalseItem = FALSE,  label = "Manual Breakpoints?", tooltip = "User specifies how many cells to divide the visible histogram into",
    signal = c("default", "toggle.sensitive", "cells"),
    cells.integerItem = 50, label = "N", indent=10,
  addRug.trueFalseItem = FALSE, label = "Add points below",tooltip = "Add a 'rug' of lines to show density",
  do.colorByFactor.trueFalseItem = FALSE, label = "Color By Factor?",
    signal = c("default", "toggle.sensitive", "color.key"),    
    color.key.choiceItem = NULL, label = "Choose A Factor", indent=10,
  do.xlim.trueFalseItem = FALSE,  label = "Set x-axis?", tooltip = "Set the horizontal limit of all histograms",
    signal = c("default", "toggle.sensitive", "xmin", "xmax"),  
    xmin.numericItem = 0, label = "Minimum x", indent=10,
    xmax.numericItem = 1, label = "Maximum x", indent=10,
  do.ylim.trueFalseItem = FALSE,  label = "Set y-axis?", tooltip = "Set the vertical limit of all histograms",
    signal = c("default", "toggle.sensitive", "ymin", "ymax"),  
    ymin.numericItem = 0, label = "Minimum y", indent=10,
    ymax.numericItem = 1, label = "Maximum y", indent=10    
)


# Plot the histograms to either a JPEG or PNG file
Histogram <- function(Data,
                      Data.Columns=colnames(Data),
                      do.cells = FALSE,                      
                      cells=50,
                      file="deleteme.png",
                      bkground="white",
                      colF="#ffc38a",
                      colB="#5FAE27",
                      addRug=TRUE,
                      stamp=NULL,
                      add.dist = FALSE,
                      do.xlim = F,
                      xmin=0,
                      xmax = 1,
                      do.ylim = F,
                      ymin=0,
                      ymax = 1,
                      do.colorByFactor = F,
                      color.key = NULL,
                      ...)
{
    # Plot histograms, the distribution profile and the reference profile

    #png(filename=file,width=1152,height=864,pointsize=12,
    #        bg=bkground,res=600)
    ##require(Cairo)
    ##CairoPNG(filename=file,width=IMGwidth,height=IMGheight,pointsize=FNTsize,bg=bkground,res=600)
    if(!length(Data.Columns)) stop("\nUser must specify at least one data column to plot.\nUse the arrows in 'Selected Data Columns'\nto bring in a selection in 'Available Columns'")
    colF <- rep(colF, length(Data.Columns))
    if(do.colorByFactor && length(color.key)==1){
      colF <- ColorByFactor(Data, color.key)$color[Data.Columns]
    }
    data <- Data[,Data.Columns,drop=F]
    ncols <- ceiling(sqrt(length(Data.Columns)))
    if (ncols == 0) ncols <- 1
    m <- ceiling((NCOL(data))/ncols)
    #par(mfrow=c(m,ncols), cex=.5, mex=.5,mar=c(6,7,4,4))
    if(all(par("mfrow") == c(1,1))) par(mfrow=c(m,ncols), cex=.6, mex=.6, oma=c(2, 2, 2, 2), mar=c(2,2,2,1))

    #tryCatch(
    #{
        for (i in 1:NCOL(data))
        {
          htitle = colnames(data)[i]                  
          xx <- data[,i]          
          
          if(do.xlim && xmin < xmax){
            xx <- xx[xmin < xx & xx <= xmax & !is.na(xx)]
          }
          if(!any(!is.na(xx))) {
            plot(c(1,1),type="n",axes=F,xlab="",ylab="")
            text(1.5,1,paste("Nothing visible!"),cex=2)          
            next;
          }

         if(do.cells) # we recalculate h
           h <- hist(xx, breaks=cells,plot=FALSE)
         else 
           h <- hist(xx, plot=FALSE)
           
          if(do.xlim) {
            my.xlim <- c(xmin, xmax)
          } else {
            my.xlim <- range(h$breaks)
          }                    
          if(do.ylim) {
            ylim1 <- c(ymin, ymax)
          } else {
            ylim1 <- c(0, max(h$counts))
          }                              

          #curve(dnorm(x, mean=mean(xx,na.rm=T), sd=sd(xx,na.rm=T)),
          #        add=TRUE, lty=2, col="red")
          #  curve(dnorm(x, mean=0, sd=sd(xx,na.rm=T)), add=TRUE, lty=2,
          #        col="blue")
         if(add.dist){
                 
            d <- density(xx, na.rm=T)
            if(do.ylim && ymin < ymax){
              ylim1 <- c(ymin, ymax)
            } else {
              ylim1 <- c(0, max(h$intensities,d$y))
            }
            
            plot(function(y) dnorm(y, mean(xx, na.rm=T), sd(xx, na.rm=T)),
              from=min(xx, na.rm=T), to=max(xx, na.rm=T),
              main = htitle, xlim=my.xlim, ylim=ylim1,
              col="white",xlab="",ylab="")
            hist(xx, breaks=h$breaks, prob=TRUE,  xlim=my.xlim,
                  col=colF[i], border=colB,xpd=TRUE,ylim=ylim1,add=TRUE)
            plot(function(y) dnorm(y, 0, sd(xx, na.rm=T)),
                from=min(xx, na.rm=T), to=max(xx, na.rm=T),
                col="blue", add=TRUE, lty = "dashed")
            plot(function(y) dnorm(y, mean(xx, na.rm=T), sd(xx, na.rm=T)),
                from=min(xx, na.rm=T), to=max(xx, na.rm=T),
                col="black", lwd=1.75, add=TRUE)
          } else {            
            hist(xx, breaks=h$breaks, main = htitle,  xlim=my.xlim, ylim=ylim1, xlab="", ylab = "", col = colF[i])
          }
          if (addRug)
            rug(xx,col=colB)
          #box()
        }
        if (!is.null(stamp))
            mtext(paste("DAnTE : ", format(Sys.time(), "%m-%d-%Y %I:%M%p"),
                  " (", stamp, ")", sep=""),col=1,cex=.6,line=2, side=1,
                  adj=1, outer=T)
#    },
#    interrupt = function(ex)
#    {
#      cat("An interrupt was detected.\n");
#      print(ex);
#    },
#    error = function(ex)
#    {
#      plot(c(1,1),type="n",axes=F,xlab="",ylab="")
#      text(1.5,1,paste("Error:", ex),cex=2)
#      cat("An error was detected.\n");
#      print(ex);
#    },
#    finally =
#    {
#      #cat("Releasing tempfile...");
#      par(mfrow=c(1,1),pch=1)
#      ##dev.off()
#      #cat("done\n");
#    }) # tryCatch()
  return(recordPlot())    
}
