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
# Linear Regression based Normalization
# -------------------------------------------------------------------------

require(MASS)

#replicate.factors <- "Condition"
#reference= 3
#Data=Eset
#xx <- LinearRegression(Data, replicate.factors, factors=factors, plotflag=FALSE,
#                            reference=1, folder="C:/temp/")
#{  
 
 
LR.dialog = list(
  show.progress=TRUE,
  Data.dataframeItem="T_Data", label="Data Source", 
    tooltip = "Choose data frame for source",    
    signal = c("default", "get.dataset.factors", "replicate.factors"),
  replicate.factors.choiceItem=NULL, label="Factor Denoting Replicates",
  reference.radiobuttonItem=c(1,2,3,value=4),
     item.labels = c("First Set in Replicate Category",
       "Create the Median Set in Replicate Category",
       "Dataset with Least Amount of Missing Data",
       "Mean of All Datasets"), label="Pick Reference",
  do.weighting.trueFalseItem = TRUE, label = "Apply Variance Weighting", tooltip = "Use the variance weighting exp(0.25*y)",
  plotflag.trueFalseItem=FALSE, label="Save Diagnostic Plots (WARNING: Could be slow)",
   signal = c("default", "toggle.sensitive", "folder"),
  folder.fileItem="C:/temp/", label="Choose Folder", indent=10
)


# Do the Linear Regression normalization
# Assumes that factors are called factors.
LR <- function(Data, replicate.factors, plotflag=FALSE, do.weighting=TRUE,
                            reference=1, folder="C:/temp/", progressbar=NULL)
{  

  if(reference == 4){ # regress 
    means <- apply(Data, 1, mean, na.rm=T)
    require(MASS)
    for(jj in 1:ncol(Data)){
       y = Data[,jj]
       if(do.weighting) 
         fit1 <- rlm(means~I(y), weights=exp(0.25*means))
       else 
         fit1 <- rlm(means~I(y))    
       zz <- !is.na(y)
       MM <- model.matrix(means~I(y))    
       Data[zz,jj] <- MM%*%fit1$coef
       if(!missing(progressbar)) progressbar$setFraction(jj/ncol(Data))
    }    
  } else {
    factors <- get.factors(Data)
    if(is.null(factors)) stop("No column metadata found")
    my.factors <- factors[,replicate.factors,drop=F]
    replicates <- my.factors[colnames(Data),]
    Nreps <- unique(as.vector(t(replicates)))  
    for (i in 1:length(Nreps)) # for each unique sample
    {
      idx <- which(replicates == Nreps[i])
      #print(idx)
      dataset <- Data[,idx] # extract data for sample i with all the replicates
  
      if (length(idx) > 1) # do LOESS
      {
        fittedData <- doLinearRegression(dataset, plotflag=plotflag,
                                      folder=folder, reference=reference)
        Data[,idx] <- fittedData
      }
      if(!missing(progressbar)) progressbar$setFraction(i/ncol(Nreps))      
    }# for
  }
  return(Data)
}# function

#------------------------------------------------------------------
doLinearRegression <- function(dataset, plotflag=FALSE,
                                folder="C:/temp/", reference=1)
{
  fittedData <- dataset
  header <- colnames(dataset)
                                
  if (reference == 1) # pick the first dataset
  {
    #print("LinReg: First set")
    refset <- 1
    indexSet <- dataset[,refset]
    set2normalize <- 1:dim(dataset)[2]
    set2normalize <- set2normalize[set2normalize != refset]
    idxSetName <- header[refset]
  }
  if (reference == 2) # median
  {
    #print("LinReg: Median")
    indexSet <- apply(dataset,1,"median",na.rm=TRUE)
    set2normalize <- 1:dim(dataset)[2]
    idxSetName <- "Median"
  }
  if (reference == 3) # least missing
  {
    #print("LinReg: Least missing")
    xx <- 1:dim(dataset)[2]
    missTotal <- colSums(is.na(dataset))
    refset <- xx[missTotal==min(missTotal,na.rm=TRUE)]
    refset <- refset[1]
    indexSet <- dataset[,refset]
    set2normalize <- 1:dim(dataset)[2]
    set2normalize <- set2normalize[set2normalize != refset]
    idxSetName <- header[refset]
  }

  for (cycle in set2normalize)
  {
    matchSet <- dataset[,cycle]
    #browser()
    LMfit <- rlm(matchSet~indexSet,na.action=na.exclude)
    Coeffs <- LMfit$coefficients
    a <- Coeffs[2] # y = aX + b
    b <- Coeffs[1] # y = aX + b
    fittedMatchSet <- (matchSet - b) / a
    fittedData[,cycle] <- fittedMatchSet

    if (plotflag)
    {
        tryCatch(
        {
          pic1 <- paste(folder, header[cycle],"_LinReg.png",sep="")
          png(filename=pic1, width=1024, height=768, pointsize=12, bg="white", units="px")
          ##require(Cairo)
          ##CairoPNG(filename=pic1,width=IMGwidth,height=IMGheight,pointsize=FNTsize,bg="white",res=600)
          nf <- layout(matrix(c(1,1,2,2,3,4,5,6), 2, 4, byrow=TRUE))
          layout.show(nf)
          plot(indexSet,matchSet, pch=21,bg="green", xlab=paste("Reference -",idxSetName),
            ylab=header[cycle], main=paste("Scatter Plot (",idxSetName," - ",
            header[cycle],")",sep=""))
          abline(Coeffs,col=2,pch=20)
          abline(a=0, b=1, col=1, lwd=1)
          #Coeffs1 <- lm(matchSet~indexSet,na.action=na.exclude)$coefficients
          #abline(Coeffs1,col=4,pch=20)
          mtext(paste("Y = ",format(a,digits=2),"X + ",format(b,digits=2),sep=""),line=-1,adj=0)

          plot(indexSet,fittedMatchSet, pch=21,bg="purple", xlab=paste("Reference -",idxSetName),
            ylab=header[cycle], main=paste("Adjusted Scatter Plot (",
            idxSetName," - ", header[cycle],")",sep=""))
          Coeff1 <- rlm(fittedMatchSet~indexSet, na.action=na.exclude)$coefficients
          #Coeff2 <- lm(fittedMatchSet~indexSet, na.action=na.exclude)$coefficients
          abline(Coeff1, col=1, pch=20)
          #abline(Coeff2, col=3, pch=20)
          mtext(paste("Y = ", format(Coeff1[2],digits=2),"X + ", format(Coeff1[1],digits=2),
            sep=""),line=-1,adj=0)
          plot(LMfit)
        },
        interrupt = function(ex)
        {
          cat("An interrupt was detected.\n");
          print(ex);
        },
        error = function(ex)
        {
          cat("An error was detected.\n");
          print(ex);
        },
        finally =
        {
          cat("Releasing figure file...");
          dev.off()
          cat("done\n");
        }) # tryCatch()
    }# if
  } # for
  return(fittedData)
}
