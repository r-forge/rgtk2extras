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
# Merge Data columns
# -------------------------------------------------------------------------
#################################################################
MergeDataColumns <- function(Data, replicates, pmode='sum')
{
    #browser()
    columnNames <- character(0)
    Z1 <- integer(0)
    Nreps <- unique(as.vector(t(replicates)))

    for (i in 1:length(Nreps)) # for each unique sample
    {
        idx <- which(replicates == Nreps[i])
        dataset <- Data[,idx,drop=FALSE] # extract data for sample i with all the replicates
        columnNames <- c(columnNames,paste(colnames(dataset)[1],Nreps[i],sep="_"))

        if (length(idx) > 1) # Process
        {
            Z2 <- switch(pmode,
                      'sum' = rowSums(dataset, na.rm=TRUE),
                      'mean' = rowMeans(dataset, na.rm=TRUE),
                      'median' = apply(dataset, 1, median, na.rm=TRUE) )
        }
        else
            Z2 <- dataset
        Z1 <- cbind(Z1, Z2)
        colnames(Z1) <- columnNames
    }# for
    return(Z1)
}# function
