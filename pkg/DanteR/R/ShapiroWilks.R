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
# Shapiro Wilks test

ShapiroWilks.dialog <- list(
  label = "Shapiro-Wilks Test for Normality of Rows in Crosstab\n",
  x.dataframeItem = "T_Data", label = "Choose Data Set",
  thres.integerItem = c(value=3, from=0, to=10), label = "At Least This Number Present"
)

ShapiroWilks <- function(x, thres=3)
{
    splitIdx <- splitmissing.fLevel(x,thres)
    Data.good <- as.matrix(x[splitIdx$good,,drop=FALSE])
    Data.bad <- as.matrix(x[splitIdx$bad,,drop=FALSE])
    if (dim(Data.bad)[1]==1)
      Data.bad <- rbind(Data.bad, Data.bad)

    result <- t(t(apply(Data.good, 1,
                function(x) { tmp <- shapiro.test(x);
                              pval <- tmp["p.value"];
                              return(pval[[1]])})))
    normals <- vector(mode="logical", length=length(result))
    normals[result > 0.1] <- TRUE
    result <- data.frame(normals, result)
    colnames(result) <- c("Normal", "pvalue")
    attr(result, "Row_Metadata") <- attr(x, "Row_Metadata")
    #return(list(pvals=result, miss=Data.bad, allused=(dim(Data.bad)[1]==0)))
    return(result)    
}
        