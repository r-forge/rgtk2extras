######################################################
# Peptide scaling and rollup functions used in DAnTE #
#                                                    #
# By Ashoka D. Polpitiya                             #
######################################################

ZRollup.dialog <- list(
  label = "\nCrosstab Rollup. Peptides are median centered first and then scaled\n by the row standard deviation.\n Protein abundance is obtained as median of\n the abundances of the peptides in the group.\n",long.running=TRUE,
  Data.dataframeItem="T_Data", label="Data Source (must be log transformed)", tooltip = "This method assumes that the data is in log scale.",
    signal = c("default", "get.dataset.row.metadata.fields", "rollup.field"),
  rollup.field.choiceItem = NULL, label = "Choose Field To Roll Up On",
  minPresence.numericItem=50, label = "Minimum presence % of at least one peptide",
  Mode.radiobuttonItem=c(value="median", "mean"), labels=c("Median", "Mean"), label="Mode", 
  oneHitWonders.trueFalseItem=FALSE, label="Include One Hit Wonders", BREAK=TRUE,
  gminPCount.integerItem=5, label="Minimum Number of Peptides required for Grubbs' Test",  
  gpvalue.numericItem=0.2, label="p-value Cutoff for Grubbs' Test"
)

ZRollup <- function(Data, rollup.field, minPresence=50, Mode="median",
                           minCountPerP=2, gpvalue=0.05, gminPCount=5,
                           oneHitWonders=TRUE, plotflag=FALSE, outfolder = "")
# This function scales peptides, remove outliers and find the protein
# abundance as the median of the peptide abundances.
#
# Inputs:
# ------
# Data :
#
# minPresence : how many non-NA's are allowed.
# minCountPerP : minimum number of peptides per protein.
# gpvalue : pvalue cutoff for the Grubbs outlier test.
# gminPCount : minimum peptides required for the outlier test.
#
# Depends on:
# ----------
# "outliers" package
# internal functions written below: scale.data(),
#   protein.rollup(), rm.outlier.1(), outlier.1()
#
# Output:
# ------
# A list: sData - Scaled data
#         orData - Outliers removed scaled data
#         pData - Protein abundances
#
# Ashoka Polpitiya - June 2007
#
{
    minCountPerP <- 2
    minPresence <- minPresence/100
    
    ProtInfo <- get.ProtInfo(Data)
    if(!rollup.field%in%colnames(ProtInfo)) stop("Rollup field not found in row metadata table")
    ProtInfo <- unique(ProtInfo)
    MassTags <- ProtInfo[,1]    
    protIPI <- ProtInfo[,rollup.field]
       
    uProtCounts <- table(protIPI)
    sigIPI <- names(uProtCounts[uProtCounts >= minCountPerP])
    restIPI <- names(uProtCounts[(uProtCounts < minCountPerP) & (uProtCounts > 0)])
    threshold <- round(dim(Data)[2] * minPresence)
    
    scaledData <- rep(numeric(0),dim(Data)[2])
    olRmData <- rep(numeric(0),dim(Data)[2])
    protData <- rep(numeric(0),dim(Data)[2]+1)
    singlePepProtData <- rep(numeric(0),dim(Data)[2]+1)

    proteinNames <- "empty"
    oneHitProtNames <- "empty"
    
    k = 1
    for (prot in 1:length(sigIPI))
    {
        pidx <- which(sigIPI[prot]==protIPI)
        data_idx <- is.element(row.names(Data),MassTags[pidx])
        currProtData <- Data[data_idx,]
        
        if (is.matrix(currProtData))
            if (dim(currProtData)[1] > 1)
            {
                xPresenceCount <- rowSums(!is.na(currProtData))
                if (max(xPresenceCount, na.rm=TRUE) >= threshold)
                {
                    #browser()
                    sData <- scale.data(currProtData)
                    pData <- protein.rollup(sData, Mode=Mode, minPs=gminPCount,
                                pvalue=gpvalue)
                    scaledData <- rbind(scaledData,sData)
                    olRmData <- rbind(olRmData,pData$orData)
                    protData <- rbind(protData,pData$proteinVals)

                    proteinNames[k] <- sigIPI[prot]

                    if (plotflag)
                    {
                        outfile = paste(outfolder,k,".png",sep="")
                        plotCurrProt.2(currProtData,sData,pData,
                                file=outfile,IPI=sigIPI[prot])
                    }
                    k = k + 1
                }
            }
    }
    rownames(protData) <- proteinNames

    if ((oneHitWonders) && (length(restIPI) > 0))
    {
        k = 1
        for (prot in 1:length(restIPI))
        {
            pidx <- which(restIPI[prot]==protIPI)
            data_idx <- is.element(row.names(Data),MassTags[pidx[1]])
            currProtData <- Data[data_idx,]
            xPresenceCount <- sum(!is.na(currProtData))
            if (xPresenceCount >= threshold)
            {
                singlePepProt <- c(PepCount=1,currProtData)
                #browser()
                singlePepProtData <- rbind(singlePepProtData,singlePepProt)
                oneHitProtNames[k] <- restIPI[prot]
                k = k + 1
            }
        }
        rownames(singlePepProtData) <- oneHitProtNames
        outProtData <- rbind(protData,singlePepProtData)
    }
    else
        outProtData <- protData

    scaledData <- remove.duplicates(scaledData)
    olRmData <- remove.duplicates(olRmData)
    
       
    outProtData <- outProtData[,-1,drop=F]
    
    attr(outProtData, "Column_Metadata") <- attr(scaledData, "Column_Metadata") <- attr(Data, "Column_Metadata")
    attr(scaledData, "Row_Metadata") <- attr(Data, "Row_Metadata")
    attr(outProtData, "Row_Metadata") <- c(table=attr(Data, "Row_Metadata")[["table"]], key = rollup.field)
    
    out <- list(ScaledData=scaledData, RolledUp=outProtData)
    return(out)
}

#------------------------------------------------------------------
scale.data <- function(Data)
# internal function used by normalize.proteins()
{
    med <- apply(Data, 1, median, na.rm=T)
    Data <- Data - med #kronecker(matrix(1, 1, dim(Data)[2]), med)
    Data <- apply(Data, 1, function(y)y/sd(y,na.rm=T)) # divide by SDev
    return(t(Data))
}

#------------------------------------------------------------------
protein.rollup <- function(Data, Mode="median", minPs=5, pvalue=0.005)
# internal function used by normalize.proteins()
# Calculates the protein abundances after removing outliers
# Depends on package "outliers"
{
    library(outliers)
    
    ColNames = colnames(Data)
    proteinValue <- matrix(0,1,dim(Data)[2])
    xPeptideCount <- colSums(!is.na(Data))
    for (i in 1:dim(Data)[2])
    {
        if (xPeptideCount[i] >= minPs)
        {
            repeat
            {
                grubbs <- grubbs.test(Data[,i]) # Grubb's test
                if ( (grubbs$p.value < pvalue) && (!is.nan(grubbs$statistic[2])) &&
                        (grubbs$statistic[2] != 0) ) # pass the p-value cutoff
                {
                    Data[,i] <- rm.outlier.1(Data[,i],fill=TRUE,median=TRUE)
                    # fill the outlier with the median value
                }
                else { break }
            }
        }
        if (Mode=="median")
            proteinValue[i] <- median(Data[,i],na.rm=T) # median may be better
        else
            proteinValue[i] <- mean(Data[,i],na.rm=T)
    }
    proteinVal <- c(dim(Data)[1],proteinValue)
    names(proteinVal) <- c("PepCount", ColNames)
    out = list(orData=Data,proteinVals=proteinVal)
    # orData : Outlier Removed Data
    return(out)
}

#------------------------------------------------------------------
remove.duplicates <- function(Data)
{
     rowIDs <- rownames(Data)
     rowIDs <- rowIDs[rowIDs != ""]
     Data <- Data[!duplicated(rowIDs),]
     return(Data)
}

##############################################################################
plotCurrProt.2 <- function(currData,sData,pdata,file="deleteme.png",
                            bkground="transparent",
                            IPI="IPI:IPI00009793.1")
# pdata : output from protein rollup (scale.proteins)
{
    #require(Cairo)
    #CairoPNG(filename=file,width=IMGwidth,height=IMGheight,pointsize=FNTsize,bg=bkground,res=600)
    #png(filename=file,width=1024,height=768,pointsize=12,bg=bkground,
    #        res=600)
    
    par(mfrow=c(3,1))
    tryCatch(
    {
        matplot(t(currData),type="b",main=IPI,ylab="Raw Data")
        matplot(t(sData),type="b",ylab="Scaled Data")
        matplot(t(pdata$orData),type="b",ylab="Scaled and Outlier removed",
                xlab=paste(dim(currData)[1],"Peptides",sep=" "))
        lines(pdata$proteinVals[-1],type="l",lwd=2)
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
      cat("Releasing tempfile...");
      dev.off()
      cat("done\n");
    }) # tryCatch()
}


###############################################################################
# modified R outier functions from "outlier" package to handle missing
rm.outlier.1 <- function (x, fill = FALSE, median = FALSE,
                         opposite = FALSE,
                         na.rm = TRUE)
{
    if (is.matrix(x))
        apply(x, 2, rm.outlier.1, fill = fill, median = median,
            opposite = opposite, na.rm = na.rm)
    else if (is.data.frame(x))
        as.data.frame(sapply(x, rm.outlier.1, fill = fill, median = median,
            opposite = opposite, na.rm = na.rm))
    else {
        res <- x
        if (!fill)
            res[-which(x == outlier.1(x, opposite))]
        else {
            if (median)
                res[which(x == outlier.1(x, opposite))] <- median(x[-which(x ==
                  outlier.1(x, opposite))], na.rm = na.rm)
            else res[which(x == outlier.1(x, opposite))] <- mean(x[-which(x ==
                outlier.1(x, opposite))], na.rm = na.rm)
            res
        }
    }
}

# modified R outier functions to handle missing
outlier.1 <- function (x, opposite = FALSE, logical = FALSE, na.rm = TRUE)
{
    if (is.matrix(x))
        apply(x, 2, outlier.1, opposite = opposite, logical = logical)
    else if (is.data.frame(x))
        sapply(x, outlier.1, opposite = opposite, logical = logical)
    else {
        if (xor(((max(x, na.rm = na.rm) - mean(x, na.rm = na.rm)) <
            (mean(x, na.rm = na.rm) - min(x, na.rm = na.rm))), opposite)) {
            if (!logical)
                min(x, na.rm = na.rm)
            else x == min(x, na.rm = na.rm)
        }
        else {
            if (!logical)
                max(x, na.rm = na.rm)
            else x == max(x, na.rm = na.rm)
        }
    }
}
###############################################################################
