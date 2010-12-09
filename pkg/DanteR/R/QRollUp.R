######################################################
# Peptide Qrollup functions used in DAnTE            #
#                                                    #
# By Ashoka D. Polpitiya                             #
######################################################
                                        
                                                            
QRollup.dialog <- list(
  label = "Data Rollup. QRollup method takes the top user selected percentage\n of peptides and averages to obtain protein abundance.\n", long.running=TRUE,
  Data.dataframeItem="T_Data", label="Data Source (must be log transformed)", tooltip = "This method assumes that the data is in log scale.",
    signal = c("default", "get.dataset.row.metadata.fields", "rollup.field"),
  rollup.field.choiceItem = NULL, label = "Choose Field To Roll Up On",
  minPresence.numericItem=50, label = "Minimum presence % of at least one peptide",
  method.radiobuttonItem=c(value="median", "mean"), labels=c("Median", "Mean"), label="Mode", BREAK=T,
  Top.numericItem = 33, label = "Threshold (%)", tooltip = "Roll up peptides above this threshold",
  minPresence.numeric=50, label="Minimum dataset presence (%)", tooltip="Exclude peptides from scaling if they are at least not present in this % datasets",
  oneHitWonders.trueFalseItem=FALSE, label="Include One Hit Wonders"
)
                      
QRollup <- function(Data, rollup.field, minPresence=50, Top=33, method="median",
                           oneHitWonders=TRUE)
# This function find the protein
# abundance as ....
#
# Inputs:
# ------
# Data :

# Depends on:
# ----------
# Output:
# ------
#     pData - Protein abundances
#
# Ashoka Polpitiya - June 2007
#
{
    Mean <- FALSE
    if(method == "mean") Mean <- TRUE
    
    ProtInfo <- get.ProtInfo(Data)
    if(!rollup.field%in%colnames(ProtInfo)) stop("Rollup field not found in row metadata table")
    ProtInfo <- unique(ProtInfo)
    MassTags <- ProtInfo[,1]    
    protIPI <- ProtInfo[,rollup.field]
    
    
    minCountPerP <- 2
    minPresence <- minPresence/100
    Top <- Top/100
    protIPI <- ProtInfo[,2]
    MassTags <- ProtInfo[,1]
    uProtCounts <- table(protIPI)
    sigIPI <- names(uProtCounts[uProtCounts >= minCountPerP])
    restIPI <- names(uProtCounts[(uProtCounts < minCountPerP) & (uProtCounts > 0)])
    threshold <- round(dim(Data)[2] * minPresence)

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
                    pData <- protein.Qrollup(currProtData, top=Top, Mean=Mean)
                    protData <- rbind(protData,pData)
                    proteinNames[k] <- sigIPI[prot]
                    k = k + 1
                }
            }
    }
    rownames(protData) <- proteinNames
    
    if (oneHitWonders && length(restIPI))
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
        
    outProtData <- outProtData[,-1,drop=F]
    attr(outProtData, "Column_Metadata") <- attr(Data, "Column_Metadata")
    attr(outProtData, "Row_Metadata") <- c(key = rollup.field, table=attr(Data, "Row_Metadata")[["table"]])
    

    return(outProtData)
}

#------------------------------------------------------------------
protein.Qrollup <- function(Data, top=.33, Mean=FALSE)
{
    ColNames = colnames(Data)
    proteinValue <- matrix(0,1,dim(Data)[2])
    for (i in 1:dim(Data)[2])
    {
        peps <- Data[,i]
        baseline <- abs(min(peps,na.rm=TRUE)) + 1 # -ve values can skew results
        peps <- peps + baseline
        thres <- top*max(peps,na.rm=TRUE)
        peps <- peps[peps>thres]
        peps <- peps - baseline # Switch back to originals
        if (Mean)
            proteinValue[i] <- mean(peps,na.rm=T)
        else
            proteinValue[i] <- median(peps,na.rm=T) # median may be better
    }
    proteinVal <- c(dim(Data)[1],proteinValue)
    names(proteinVal) <- c("PepCount", ColNames)
    return(proteinVal)
}

#------------------------------------------------------------------
protein.Qrollup.1 <- function(Data, top=.33, Mean=FALSE)   # not used
{
    ColNames = colnames(Data)
    proteinValue <- matrix(0,1,dim(Data)[2])
    for (i in 1:dim(Data)[2])
    {
        peps <- Data[,i]
        
        if (sum(is.na(peps)) == length(peps))
            proteinValue[i] <- NA
        else
        {
            positive <- (sum(peps>0,na.rm=TRUE) > sum(peps<0,na.rm=TRUE))
            negative <- (sum(peps>0,na.rm=TRUE) < sum(peps<0,na.rm=TRUE))
            if (positive)
            {
                thres <- top*max(peps,na.rm=TRUE)
                peps <- peps[peps > thres]
            }
            if (negative)
            {
                thres <- top*min(peps,na.rm=TRUE)
                peps <- peps[peps < thres]
            }
            if (Mean)
                proteinValue[i] <- mean(peps,na.rm=T)
            else
                proteinValue[i] <- median(peps,na.rm=T) # median may be better
        }
    }
    
    colnames(proteinValue) <- ColNames
    return(proteinValue)
}

#------------------------------------------------------------------
