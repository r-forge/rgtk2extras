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
# Filter based on ANOVA results

FilterOnANOVA <- function(pvals, Data, thres, column,
                        smode=c("LT","GT"))
{
    out <- numeric(0)
    nodata <- TRUE
    idx <- switch(match.arg(smode),
        LT = rownames(pvals[pvals[,column] < thres,,drop=FALSE]), #less than
        GT = rownames(pvals[pvals[,column] > thres,,drop=FALSE]), #greater than
        rownames(pvals[pvals[,column] < thres,,drop=FALSE])
    )
    dRows <- rownames(Data)
    found <- sum(idx %in% dRows)
    if (found)
    {
        out <- Data[idx,]
        nodata=(dim(out)[1]==0)
    }
    return(list(Filtered=out, NoData=nodata, error=(found==0)))
}
