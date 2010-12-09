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

#####################################################
# Plotting

Boxplots.dialog = list(
  keep.open = TRUE,
  label = "Produce box-and-whisker plot(s) of the given (grouped) values.",
  x.dataframeItem = "T_Data", label="Data Selection",
    signal = c("default", "get.colnames", "dummy"),
    signal = c("default", "set.to", "Columns", user.data = character(0)),      
    signal = c("default", "get.dataset.factors", "colorByFactorChoice"),                    
  dummy.listItem = NULL, label = "Available Columns", suppress=T, show.arrows=F,
  Columns.listItem=NULL, label="Selected Data Columns",  BREAK=T,
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy"),    
  colorByFactor.trueFalseItem=FALSE, label="Color By Factors?",
    signal = c("default", "toggle.dataset.factors", "x", "colorByFactorChoice"),                
    colorByFactorChoice.choiceItem=NULL, label="Choose Factor", indent=10,
  showlegend.trueFalseItem = TRUE, label = "Show Legend?",    
  labelscale.rangeItem=c(value=1, from=0, to=2, by=0.1), label="Label Scale",
  boxwidth.rangeItem=c(value=1, from=0, to=2, by=0.1), label = "Box Width",
  outliers.trueFalseItem=TRUE, label="Show Outliers",
  showcount.trueFalseItem=TRUE, label = "Show Counts",
  showpoints.trueFalseItem=FALSE, label = "Show Points",
    tooltip= "Display data points over box plots",  
  do.ylim.trueFalseItem=  FALSE, label = "Set Y-Axis Limit",
    signal = c("default", "toggle.sensitive", "ymin", "ymax"),
    ymin.numericItem = 0, label = "Minimum Y", indent=10, 
    ymax.numericItem = 30, label = "Maximum Y", indent=10
)

Boxplots <- function(x,
                         Columns=colnames(x),
                         file="deleteme.png",
                         colorByFactor = FALSE,
                         colorByFactorChoice = NULL,                                           
                         outliers=TRUE,
                         color="wheat2",
                         bkground="white",
                         labelscale=0.8,
                         boxwidth=1,
                         showcount=TRUE,
                         showpoints=FALSE,
                         showlegend = TRUE,
                         stamp=NULL,
                         do.ylim=FALSE,
                         ymin=NULL,
                         ymax = NULL,
                         ...)
{
  #png(filename=file,width=1152,height=864,pointsize=12,bg=bkground,
  #          res=600)
  ##require(Cairo)
  ##CairoPNG(filename=file,width=IMGwidth,height=IMGheight,pointsize=FNTsize,bg=bkground,res=600)
  if(!length(Columns)) stop("\nUser must specify at least one data column to plot.\nUse the arrows in 'Selected Data Columns'\nto bring in a selection in 'Available Columns'")  
  #par(oma=c(3.4, 2, 2, 2), mar=c(5,4,4,1))

  colF <- rep(color, length(Columns))
  if(colorByFactor && length(colorByFactorChoice)==1){
    cbf <- ColorByFactor(x, colorByFactorChoice)
    colF <- cbf$color[Columns]
    legend <- cbf$legend[cbf$legend%in%colF]
  }
    
  x <- x[,Columns, drop=F]
  

 #tryCatch(
 # {
      #par(omd=c(0,1,0.1,1))
      x <- data.matrix(x)
      if(do.ylim)
        boxplot(x,outline=outliers,notch=T,las=2,
            boxwex=boxwidth,col=colF,cex.axis=labelscale,ylim=c(ymin, ymax),...)
      else 
        boxplot(x,outline=outliers,notch=T,las=2,
          boxwex=boxwidth,col=colF,cex.axis=labelscale,...)      
      if (showpoints)
         points( cbind(jitter(rep(1:ncol(x), each=nrow(x))),  as.numeric(x)),  pch=ifelse(nrow(x)<250, 1, "."))          
      if (showlegend && colorByFactor && !is.null(colorByFactorChoice)) {
        a <- legend("topleft",names(legend),col=legend,pch=19,bg='transparent', plot=F)      
        rect(a$rect$left+a$rect$w, a$rect$top-a$rect$h, a$rect$left, a$rect$top, col="white")
        legend("topleft",names(legend),col=legend,pch=19,bg='transparent')
        }
      if (showcount)
      {
        axis(side=3, at=1:dim(x)[2], labels=colSums(!is.na(x)), tick=FALSE,
            cex.axis=.7, las=2)
      }
#      if (!is.null(stamp))
#        mtext(paste("DAnTE : ", format(Sys.time(), "%m-%d-%Y %I:%M%p"),
#                  " (", stamp, ")", sep=""),col=1,cex=.6,line=2, side=1,
#                  adj=1, outer=T)                         
#  },
#  interrupt = function(ex)
#  {
#    cat("An interrupt was detected.\n");
#    print(ex);
#  },
#  error = function(ex)
#  {
#    plot(c(1,1),type="n",axes=F,xlab="",ylab="")
#    text(1.5,1,paste("Error:", ex),cex=2)
#    cat("An error was detected.\n");
#    print(ex);
#  },
#  finally =
#  {
#    cat("Releasing tempfile...");
#    par(mfrow=c(1,1),pch=1)
#    ##dev.off()
#    cat("done\n");
#  }) # tryCatch()
  return(recordPlot())
}
