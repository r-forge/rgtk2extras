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

CorrelationEllipses <- function(Data, Columns,
                            file="deleteme.png",
                            color="green",
                            bkground="white",
                            labelscale=0.8,
                            stamp=NULL,...)
{
print("CALLED")
  #png(filename=file,width=1152,height=864,pointsize=12,bg=bkground,
  #          res=600)
  ##require(Cairo)
  ##CairoPNG(filename=file,width=IMGwidth,height=IMGheight,pointsize=FNTsize,bg=bkground)
  par(oma=c(3.4, 2, 2, 2), mar=c(5,4,4,1))
  x <- Data[,Columns,drop=F]
  Xcor <- cor(x,method="pearson",use="pairwise.complete.obs")
  library(ellipse)
  plotcorr(Xcor,col=color,cex.lab=labelscale);
  return(recordPlot())
}
