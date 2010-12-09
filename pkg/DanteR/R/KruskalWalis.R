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
# Kruskal-Walis

KruskalWalis.dialog <- list(
  #long.running=TRUE,
  label = "Kruskal-Walis Test on Crosstab Data",
  
  x.dataframeItem = "T_Data", label = "Choose Data Crosstab",
    signal = c("default", function(item, fEff){
      obj.name <- get.value(item)
      obj <- safe.eval(obj.name)
      if(exists(obj.name)) {
        col.key <- get.column.metadata.key(obj)        
        cmt <- get.column.metadata.table(obj)
        if(is.null(cmt))
          quick_message("Data Has No Column Metadata Set.\n\nUse Metadata->Column Metadata or File->Load Column Metadata to load factors")
        set.value(fEff, colnames(cmt)[!colnames(cmt)%in%col.key])        
      }      
    }, "fEff"),
  fEff.choiceItem = NULL, label = "Choose One Factor"
)

KruskalWalis <- function(x, fEff)
{                               
    Factors <- get.factors(x)
    if(!fEff%in%colnames(Factors)) stop(paste("Can't find factor", fEff, "in data set factors"))
    Factors <- Factors[,fEff]
    if(0%in%dim(x)) stop("Data frame is too small to use")
    retarr <- array(NA, c(dim(x)[1], 2))
    rownames(retarr) <- rownames(x)
    colnames(retarr) <- c("Count.Observed", "pval")
    for (i in 1:dim(x)[1])
    {
      xx <- x[i,]
      retarr[i,] <- c(sum(!is.na(xx)), tryCatch(kruskal.test(xx, Factors, na.action=na.omit)$p.value, error = function(e) NA))
    }       
    attr(retarr, "Row_Metadata") <- attr(x, "Row_Metadata")
    return(retarr)
}