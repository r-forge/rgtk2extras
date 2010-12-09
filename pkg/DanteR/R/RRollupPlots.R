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

# Plot RRollup data
RRollupPlots <- function(pdata,
                           Data,
                           ProtInfo,
                           file="deleteme.png",
                           bkground="white",
                           IPI="IPI:IPI00009793.1",
                           datalabels=TRUE)
# pdata : output from protein rollup (scale.proteins)
{
    #png(filename=file,width=1152,height=864,pointsize=12,bg=bkground,
    #        res=600)
    #require(Cairo)
    #CairoPNG(filename=file,width=IMGwidth,height=IMGheight,pointsize=FNTsize,bg=bkground,res=600)

#    tryCatch(
#    {
        #Data <- Eset
        if (datalabels)
          xlabels <- colnames(Data)
        else
          xlabels <- 1:dim(Data)[2]
        protIPI <- ProtInfo[,2]
        MassTags <- ProtInfo[,1]
        Prots <- pdata$pData[,-1]
        pidx <- which(protIPI==IPI)
        data_idx <- is.element(row.names(Data),MassTags[pidx])
        currProtData <- Data[data_idx,]
        data_idx <- is.element(row.names(pdata$sData),MassTags[pidx])
        currSData <- pdata$sData[data_idx,]
        data_idx <- is.element(row.names(pdata$orData),MassTags[pidx])
        currORData <- pdata$orData[data_idx,]
        Pdata <- Prots[rownames(Prots)==IPI,]

        par(mfrow=c(3,1))
        if (length(pidx) == 1)
        {
            plot(currProtData,type="o",pch=19,main=IPI,ylab="Raw Data",
                col="blue", xaxt="n")
            axis(1,at=1:length(xlabels),labels=xlabels, las = 2)
            grid()
        }
        else
        {
            matplot(t(currProtData),type="o",main=IPI,ylab="Raw Data",
                xlab="", xaxt="n")
            axis(1,at=1:length(xlabels),labels=xlabels, las = 2)
            grid()
            matplot(t(currSData),type="o",ylab="Scaled Data")
            grid()
            matplot(t(currORData),type="o",ylab="Scaled and Outlier removed",
                xlab=paste(dim(currProtData)[1],"Peptides",sep=" "))
            lines(Pdata,type="l",lwd=2)
            grid()
            #mtext(paste(dim(currProtData)[1],"Peptides",sep=" "), 3)
        }
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
#      cat("Releasing tempfile...");
#      dev.off()
#      cat("done\n");
#    }) # tryCatch()
}
