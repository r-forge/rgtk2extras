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
################ Quantile Normalization ###################
QuantileN <- function (Data,method="median")
{
    old_attr <- attributes(Data)    
    
    Data <- Data[complete.cases(Data),]
    sortedData <- apply(Data, 2, sort, na.last=TRUE)
    rowMedians <- apply(sortedData, 1, method, na.rm=TRUE)
    dataRanks <- c(apply(Data, 2, rank, na.last = TRUE)) # ... and concatenate
    normedData <- array(approx(1:nrow(Data), rowMedians, dataRanks)$y,
                 dim(Data), dimnames(Data))
    normedData <- normedData[order(as.numeric(rownames(normedData))),]
                                    
    normedData <- copy.metadata(normedData, old_attr)
    return(normedData)
}

IsCompleteData <- function(Data)
# Quantile Normalization works only with complete data
{
    return(sum(complete.cases(Data)) > 50)
}
############################################################