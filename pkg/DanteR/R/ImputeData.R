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
#############################################################
# Imputation functions used in DAnTE         

ImputeData.dialog = list(
  Data.dataframeItem="T_Data", label="Data Source",
  cutoff.rangeItem=c(value=20, from=0, to=100, by=1), label="Impute Threshold (%)",tooltip="Percentage Missing Allowed : Entries with missing values more than this threshold will be imputed with overall mean per sample (kNN and SVD methods only) unless specified below.",
  mode.radiobuttonItem=c("mean", "median", value="const", "rowmean", "knn", "knnw", "svd"), item.labels=c("Mean", "Median", "Substitute Constant", "Row Mean", "K Nearest Neighbors", "Weighted KNN (slow)", "SVD Impute"), label="Choose Method",
  constant.numericItem = 1.0, label="Substitute Constant Value", BREAK=T,
  noFill.trueFalseItem=FALSE, label="Don't impute missing over threshold",
    tooltip = "Do not impute entries with missing values more than the threshold (only in advance methods).",
#    signal = c("default", "toggle.sensitive", "k", "nPcs", "thresholdSVD", "maxSteps"),
  k.numericItem = 10, label = "KNN-impute: Number of neighbors",
  nPcs.integerItem = 5, label="SVD-impute: Number of PCs",
  thresholdSVD.numericItem = 0.01, label="SVD-impute: Threshold for Iterations",
  maxSteps.integerItem = 100, label = "SVD-impute: Maximum iterations"
)

               #

ImputeData <- function(Data,
                       cutoff=20,
                       mode="mean",
                       k=10,
                       nPcs = 5,
                       thresholdSVD = 0.01,
                       maxSteps = 100,
                       constant=1,
                       Factor = 1,
                       noFill=FALSE)
## filtercutoff - remove rows which have more than this percentage of missing
## mode      - Imputation method
## k         - nearest neighbors for knnImpute
## nPcs      - Number of components used for SVDimpute
## center    - mean center the data if TRUE
## thresholdSVD - threshold for SVDimpute iterations
## maxSteps  - Maximum number of SVDimpute iteration steps
{
    if (mode=="const")
        Data[is.na(Data)] <- constant
    else
    {
        require(MASS)
        require(impute)
        require(e1071)
        cutoff <- cutoff/100
        if (length(Factor) == 1)
            Data <- fillmissing(Data, mode=mode, cutoff=cutoff, k=k, nPcs=nPcs,
                    thresholdSVD=thresholdSVD, maxSteps=maxSteps,
                    noFill=noFill, verbose=TRUE)
        else
        {
            Data <- Data[order(as.numeric(rownames(Data))),]
            Nreps <- unique(as.vector(t(Factor)))
            for (i in 1:length(Nreps)) # for each unique factor level
            {
                idx <- which(Factor == Nreps[i])
                dataset <- Data[,idx] # extract data for factor i
                if (length(idx) > 1) # Impute
                {
                    imputedData <- fillmissing(dataset, mode=mode, cutoff=cutoff,
                                        k=k, nPcs=nPcs,thresholdSVD=thresholdSVD,
                                        maxSteps=maxSteps,noFill=noFill,
                                        verbose=TRUE)
                    Data[,idx] <- imputedData
                }
            }
        }
    }
    return(Data)
}

#--------------------------------------------------------------------------
## Main method for imputing missing values
## fillmissing()	Ashoka Polpitiya	11/13/2006

fillmissing <- function(Data,
                        mode="mean",
                        cutoff=0.25,
                        k=10,
                        nPcs = 5,
                        thresholdSVD = 0.01,
                        maxSteps = 100,
                        noFill=FALSE,
                        verbose = interactive())
{
## nPcs      - Number of components used for SVDimpute
## center    - mean center the data if TRUE
## thresholdSVD - threshold for SVDimpute iterations
## maxSteps  - Maximum number of SVDimpute iteration steps
## verbose   - Print some output if TRUE (SVDimpute)

    ## Missing values will be replaced by the mean
    if (mode=="mean"){
        Data <- e1071::impute(Data, what="mean")
    }
    ## Missing values will be replaced by the median
    if (mode=="median"){
       Data <- e1071::impute(Data, what="median")
    }
    if (mode=="rowmean"){
#        splitData <- splitmissing(Data, thres=cutoff)
        if (noFill)
        {
            meanCol <- rowMeans(splitData$Imputable, na.rm=TRUE)
            imp <- subsNAvectorRowWise(splitData$Imputable, meanCol)
            nonImp <- splitData$Nonimputable
            Data <- rbind(imp, nonImp, splitData$Allmissing)
            #Data <- Data[order(as.numeric(rownames(Data))),]
        }
        else
        {
#            meanRow <- colMeans(Data, na.rm=TRUE)          # What? -- TT
#            meanCol <- rowMeans(splitData$Imputable, na.rm=TRUE)
#            imp <- subsNAvectorRowWise(splitData$Imputable, meanCol)
#            nonImp <- subsNAvectorColumnWise(splitData$Nonimputable, meanRow)
#            Data <- rbind(imp, nonImp, splitData$Allmissing)
            rmM <- array(rowMeans(Data, na.rm=TRUE), dim(Data))  
            rmIdx <- is.na(Data)
            Data[rmIdx] <- rmM[rmIdx]
            
            #Data <- Data[order(as.numeric(rownames(Data))),]
        }
    }
    if (mode=="knn"){
        tmpData <- Data
        tryCatch(
        {
            if (noFill)
            {
                splitData <- splitmissing(Data, thres=cutoff)
                imp <- impute.knn(splitData$Imputable, k=k, rowmax=cutoff)$data
                nonImp <- splitData$Nonimputable
                Data <- rbind(imp, nonImp, splitData$Allmissing)
                #Data <- Data[order(as.numeric(rownames(Data))),,drop=F]
            }
            else
            {
                splitData <- splitmissing(Data, thres=cutoff)
                imp <- rbind(splitData$Imputable, splitData$Nonimputable)              
                Data <- rbind(impute.knn(imp, k=k, rowmax=cutoff)$data, splitData$Allmissing)
                #Data <- Data[order(as.numeric(rownames(Data))),,drop=F]
            }
        },
        interrupt = function(ex)
        {
            cat("An interrupt was detected.\n");
            print(ex);
            Data <- tmpData
        },
        error = function(ex)
        {
            cat("An error was detected.\n");
            print(ex);
            Data <- tmpData
        },
        finally =
        {
            cat("done\n");
        }) # tryCatch()
    }
    ## Same as knn, but values averaged are weighted by the distance to the neighbours
    if (mode=="knnw"){
        tmpData <- Data
        tryCatch(
        {
            if (noFill)
            {
                splitData <- splitmissing(Data, thres=cutoff)
                nonImp <- splitData$Nonimputable
                imp <- KNNWsplitImpute(splitData$Imputable, k=k, N=1000)
                Data <- rbind(imp, nonImp, splitData$Allmissing)
                #Data <- Data[order(as.numeric(rownames(Data))),]
            }
            else
            {
                meanRow <- colMeans(Data, na.rm=TRUE)
                splitData <- splitmissing(Data, thres=cutoff)
                nonImp <- subsNAvectorColumnWise(splitData$Nonimputable, meanRow)
                imp <- KNNWsplitImpute(splitData$Imputable, k=k, N=1000)
                Data <- rbind(imp, nonImp, splitData$Allmissing)
                #Data <- Data[order(as.numeric(rownames(Data))),]
            }
        },
        interrupt = function(ex)
        {
            cat("An interrupt was detected.\n");
            print(ex);
            Data <- tmpData
        },
        error = function(ex)
        {
            cat("An error was detected.\n");
            print(ex);
            Data <- tmpData
        },
        finally =
        {
            cat("done\n");
        }) # tryCatch()
    }
    if (mode=="svd"){
        tmpData <- Data
        tryCatch(
        {
            if (noFill)
            {
                splitData <- splitmissing(Data, thres=cutoff)
                nonImp <- splitData$Nonimputable
                imp <- SVDimpute(splitData$Imputable,nPcs=nPcs, thresholdSVD=thresholdSVD,
                        maxSteps=maxSteps, verbose=TRUE)
                Data <- rbind(imp, nonImp, splitData$Allmissing)
                #Data <- Data[order(as.numeric(rownames(Data))),]
            }
            else
            {
                meanRow <- colMeans(Data, na.rm=TRUE)
                splitData <- splitmissing(Data, thres=cutoff)
                nonImp <- subsNAvectorColumnWise(splitData$Nonimputable, meanRow)
                imp <- SVDimpute(splitData$Imputable,nPcs=nPcs, thresholdSVD=thresholdSVD,
                        maxSteps=maxSteps, verbose=TRUE)
                Data <- rbind(imp, nonImp, splitData$Allmissing)
                D#ata <- Data[order(as.numeric(rownames(Data))),]
            }
        },
        interrupt = function(ex)
        {
            cat("An interrupt was detected.\n");
            print(ex);
            Data <- tmpData
        },
        error = function(ex)
        {
            cat("An error was detected.\n");
            print(ex);
            Data <- tmpData
        },
        finally =
        {
            cat("done\n");
        }) # tryCatch()
    }

    return(Data)
}

#-----------------------------------------------------------------------------
# Split data into imputable and nonimputable (too much missing)
# based on the given threshold 'thres'
splitmissing <- function(Data,thres=0.25)
{
    #Data[Data==0] <- NA
    allmissIdx <- apply(is.na(Data),1,sum) == dim(Data)[2]
    missingData <- Data[allmissIdx,,drop=FALSE]
    okData <- Data[!allmissIdx,]
    index <- apply(okData, 1, function(x,threshold) ((sum(is.na(x))/length(x)) > threshold),
                threshold=thres)
    out <- list(Imputable=okData[!index, ,drop=FALSE],
                Nonimputable=okData[index, ,drop=FALSE],
                Allmissing=missingData)
    return(out)
}

#-----------------------------------------------------------------------------
# Used to substitute values in vector vec for the missing values in each column
subsNAvectorColumnWise <- function(Data,vec)
{
    Data <- rbind(Data, vec)
    retval <- apply(Data, 2, function(z) {
                    z[is.na(z)] <- z[length(z)]
                    z
                })
    return(retval[1:(dim(retval)[1]-1),])
}

#-----------------------------------------------------------------------------
# Used to substitute values in vector vec for the missing values in each row
subsNAvectorRowWise <- function(Data,vec)
{
    Data <- cbind(Data, vec)
    retval <- t(apply(Data, 1, function(z) {
                    z[is.na(z)] <- z[length(z)]
                    z
                }))
    return(retval[,1:(dim(retval)[2]-1)])
}

