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
# Fold change calculations

# TT- Set Factor to default of factors
# Fval0 is the factor column to use...
FoldChanges <- function(Data, fVal0, fVal1, fVal2, logScale=TRUE)
{
    #browser()
    Factor <- get.column.metadata.table(Data) 
    if(is.null(Factor)) stop("Data has no column metadata/factors defined")
    stopifnot(fVal0%in%colnames(Factor))
    Factor <- as.character(Factor[,fVal0])
    fold.change <- integer(0)

    idx1 <- which(Factor == fVal1)
    idx2 <- which(Factor == fVal2)
    set.1 <- Data[,idx1,drop=FALSE] # extract data for fVal1
    set.2 <- Data[,idx2,drop=FALSE] # extract data for fVal1
    set1.N <- rowSums(!is.na(set.1), na.rm=TRUE)
    set2.N <- rowSums(!is.na(set.2), na.rm=TRUE)

    set.1 <- rowMeans(set.1, na.rm=TRUE)
    set.2 <- rowMeans(set.2, na.rm=TRUE)
    if (logScale)
      fold.change <- set.1 - set.2
    else
    {
      fold.change <- set.1 / set.2
      fold.change.down <- -1/fold.change
      fold.change[fold.change < 1 & !is.na(fold.change)] <-
          fold.change.down[fold.change < 1 & !is.na(fold.change)]
    }
    fold.change.abs <- abs(fold.change)
    out <- cbind(set.1, set1.N, set.2, set2.N, fold.change, fold.change.abs)
    colnames(out) <- c(fVal1, paste(fVal1,"(#non-missing)",sep=""), fVal2,
                    paste(fVal2,"(#non-missing)",sep=""), "Fold Change",
                    "Absolute Fold Change")
    return (out)
}


FoldChanges.dialog = list(
  label = "Calculate Fold Changes for Data Set",
  Data.dataframeItem="T_Data", label="Data Source",
    signal = c("default", "get.dataset.factors", "fVal0"),  
    #signal = c("default", "set.to", user.data=character(0), "fVal1", "fVal2"),            
  fVal0.choiceItem=NULL, label="Factor Choice",
    signal = c("default", function(item, Data, dummy){
      obj <- safe.eval(get.value(Data))
      cmt <- get.column.metadata.table(obj)
      cmt.key <- get.column.metadata.key(obj)      
      col.choice <- get.value(item)
      uu <- (unique(as.character(cmt[cmt[,cmt.key]%in%colnames(obj), col.choice])))
      set.value(dummy, uu)
    }, "Data", "dummy"),
    signal = c("default", "set.to", user.data=character(0), "fVal1", "fVal2"),
  dummy.listItem = "", label = "Choose Factor Levels", suppress=TRUE, show.arrows=FALSE, BREAK=T,
  logScale.trueFalseItem = FALSE, label="Recenter At Zero?",
  fVal1.listItem ="", label="Factor Level 1", max.items=1,
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy"),
  fVal2.listItem = "", label="vs. Factor Level 2", max.items=1,
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy")
)