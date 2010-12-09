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
# Spectral Count data loading
####################################################################

#createMSMSdt.ObsCount <- function(fileList, dataFolder,
SpectralCounting <- function(fileList, dataFolder,
                         XcRank=1,
                         XCorr1Th=1.5,
                         XCorr2Th=1.5,
                         XCorr3Th=1.5,
                         XCorrOTh=1.5,
                         DelCn2Th=0.1,
                         TrypStateNone = TRUE,
                         TrypStatePartial = TRUE,                         
                         TrypStateFull = TRUE)
{
    columnNames <- character(0)
    Z1 <- integer(0)
    TrypState = paste(sapply(c(TrypStateNone, TrypStatePartial, TrypStateFull), ifelse, "1", "0"), collapse="")

    for (i in 1:length(fileList))
    {
        browser()
        currFile <- paste(dataFolder, "/", fileList[i], "_syn.txt", sep="")
        X1 <- try(read.csv(currFile, header=T, sep="\t"), silent=TRUE)
        if (!is.null(X1))
        {
            ## Filters ####
            # Charge States
            idx1 <- ((X1[,4] == 1) + (X1[,6] >= XCorr1Th)) == 2
            idx2 <- ((X1[,4] == 2) + (X1[,6] >= XCorr2Th)) == 2
            idx3 <- ((X1[,4] == 3) + (X1[,6] >= XCorr3Th)) == 2
            idx4 <- ((X1[,4] > 3) + (X1[,6] >= XCorrOTh)) == 2
            idxCS <- ((idx1 + idx2 + idx3 + idx4) > 0)

            idxRank <- (X1[,14] <= XcRank) # XCorr rank
            idxDelCn2 <- (X1[,12] >= DelCn2Th) # DelCn threshold

            #Tryptic state
            idxTrNone <- (X1[,19] == 0)
            idxTrPartial <- (X1[,19] == 1)
            idxTrFully <- (X1[,19] == 2)

            switch (TrypState,
                '111' = { idx <- (idxCS + idxRank + idxDelCn2) == 3 },
                '110' = { idxtmp <- (idxTrNone + idxTrPartial) == 1
                          idx <- (idxCS + idxRank + idxDelCn2 + idxtmp) == 4
                        },
                '100' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrNone) == 4 },
                '010' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrPartial) == 4 },
                '001' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrFully) == 4 },
                '011' = { idxtmp <- (idxTrFully + idxTrPartial) == 1
                          idx <- (idxCS + idxRank + idxDelCn2 + idxtmp) == 4
                        }
            )

            ###############
            if (length(idx) > 1)
            {
                Y1 <- as.vector(X1[idx,9])
                Z2 <- as.matrix(table(Y1))
                if (i == 1)
                {
                    columnNames <- fileList[1]
                    Z1 <- Z2
                    colnames(Z1) <- columnNames
                }else
                {
                    columnNames <- c(columnNames, fileList[i])
                    Z1 <- merge(Z1,Z2,by="row.names",all.y=T,all.x=T)
                    row.names(Z1) <- Z1[,1]
                    Z1 <- Z1[,-1]
                    colnames(Z1) <- columnNames
                }
            }
        }
    }
    Z1 <- as.matrix(Z1)
    return(list(eset=Z1, rows=dim(Z1)[1]))
}
#-------------------------------------------------------------------
createMSMSdt.SpectralCount.1 <- function(fileList, dataFolder,
                         XcRank=1,
                         XCorr1Th=1.5,
                         XCorr2Th=1.5,
                         XCorr3Th=1.5,
                         XCorrOTh=1.5,
                         DelCn2Th=0.1,
                         TrypState='111')
{
    columnNames <- character(0)
    Z1 <- integer(0)

    for (i in 1:length(fileList))
    {
        #browser()
        currFile <- paste(dataFolder, "/", fileList[i], "_syn.txt", sep="")
        X1 <- try(read.csv(currFile, header=T, sep="\t"), silent=TRUE)
        if (!is.null(X1))
        {
            ## Filters ####
            # Charge States
            idx1 <- ((X1[,4] == 1) + (X1[,6] >= XCorr1Th)) == 2
            idx2 <- ((X1[,4] == 2) + (X1[,6] >= XCorr2Th)) == 2
            idx3 <- ((X1[,4] == 3) + (X1[,6] >= XCorr3Th)) == 2
            idx4 <- ((X1[,4] > 3) + (X1[,6] >= XCorrOTh)) == 2
            idxCS <- ((idx1 + idx2 + idx3 + idx4) > 0)

            idxRank <- (X1[,14] <= XcRank) # XCorr rank
            idxDelCn2 <- (X1[,12] >= DelCn2Th) # DelCn threshold

            #Tryptic state
            idxTrNone <- (X1[,19] == 0)
            idxTrPartial <- (X1[,19] == 1)
            idxTrFully <- (X1[,19] == 2)

            switch (TrypState,
                '111' = { idx <- (idxCS + idxRank + idxDelCn2) == 3 },
                '110' = { idxtmp <- (idxTrNone + idxTrPartial) == 1
                          idx <- (idxCS + idxRank + idxDelCn2 + idxtmp) == 4
                        },
                '100' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrNone) == 4 },
                '010' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrPartial) == 4 },
                '001' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrFully) == 4 },
                '011' = { idxtmp <- (idxTrFully + idxTrPartial) == 1
                          idx <- (idxCS + idxRank + idxDelCn2 + idxtmp) == 4
                        }
            )

            ###############
            if (length(idx) > 1)
            {
                Y1 <- as.vector(X1[idx,11])
                Z2 <- as.matrix(table(Y1))
                if (i == 1)
                {
                    columnNames <- fileList[1]
                    Z1 <- Z2
                    colnames(Z1) <- columnNames
                }else
                {
                    columnNames <- c(columnNames, fileList[i])
                    Z1 <- merge(Z1,Z2,by="row.names",all.y=T,all.x=T)
                    row.names(Z1) <- Z1[,1]
                    Z1 <- Z1[,-1]
                    colnames(Z1) <- columnNames
                }
            }
        }
    }
    Z1 <- as.matrix(Z1)
    return(list(eset=Z1, rows=dim(Z1)[1]))
}



############################################################################
createMSMSdt.old <- function(fileList, dataFolder,
                         XcRank=1,
                         XCorr1Th=1.5,
                         XCorr2Th=1.5,
                         XCorr3Th=1.5,
                         XCorrOTh=1.5,
                         DelCn2Th=0.1,
                         TrypState='111')
{
    columnNames <- character(0)
    Z1 <- integer(0)

    for (i in 1:length(fileList))
    {
        #browser()
        currFile <- paste(dataFolder, "/", fileList[i], "_syn.txt", sep="")
        X1 <- try(read.table(currFile, header=T), silent=TRUE)
        if (!is.null(X1))
        {
            ## Filters ####
            # Charge States
            idx1 <- (X1[,4] == 1)
            idx2 <- (X1[,4] == 2)
            idx3 <- (X1[,4] == 3)
            idx4 <- (X1[,4] > 3)
            idxCS <- ((idx1 + idx2 + idx3 + idx4) > 0)

            idxRank <- (X1[,14] <= XcRank) # XCorr rank
            idxDelCn2 <- (X1[,12] >= DelCn2Th) # DelCn threshold

            #Tryptic state
            idxTrNone <- (X1[,19] == 0)
            idxTrPartial <- (X1[,19] == 1)
            idxTrFully <- (X1[,19] == 2)

            switch (TrypState,
                '111' = { idx <- (idxCS + idxRank + idxDelCn2) == 3 },
                '110' = { idxtmp <- (idxTrNone + idxTrPartial) == 1
                          idx <- (idxCS + idxRank + idxDelCn2 + idxtmp) == 4
                        },
                '100' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrNone) == 4 },
                '010' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrPartial) == 4 },
                '001' = { idx <- (idxCS + idxRank + idxDelCn2 + idxTrFully) == 4 },
                '011' = { idxtmp <- (idxTrFully + idxTrPartial) == 1
                          idx <- (idxCS + idxRank + idxDelCn2 + idxtmp) == 4
                        }
            )

            ###############
            if (length(idx) > 1)
            {
                Y1 <- as.vector(X1[idx,9])
                Z2 <- as.matrix(table(Y1))
                if (i == 1)
                {
                    columnNames <- fileList[1]
                    Z1 <- Z2
                    colnames(Z1) <- columnNames
                }else
                {
                    columnNames <- c(columnNames, fileList[i])
                    Z1 <- merge(Z1,Z2,by="row.names",all.y=T,all.x=T)
                    row.names(Z1) <- Z1[,1]
                    Z1 <- Z1[,-1]
                    colnames(Z1) <- columnNames
                }
            }
        }
    }
    Z1 <- as.matrix(Z1)
    return(list(eset=Z1, rows=dim(Z1)[1]))
}
