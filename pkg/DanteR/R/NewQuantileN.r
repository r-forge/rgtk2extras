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

  # Stolen from limma:::normalizeQuantiles until we change the requirements
stolen_limma_normalizeQuantiles <- function (A, ties = TRUE)
{
    n <- dim(A)
    if (is.null(n))
        return(A)
    if (n[2] == 1)
        return(A)
    O <- S <- array(, n)
    nobs <- rep(n[1], n[2])
    i <- (0:(n[1] - 1))/(n[1] - 1)
    for (j in 1:n[2]) {
        Si <- sort(A[, j], method = "quick", index.return = TRUE)
        nobsj <- length(Si$x)
        if (nobsj < n[1]) {
            nobs[j] <- nobsj
            isna <- is.na(A[, j])
            S[, j] <- approx((0:(nobsj - 1))/(nobsj - 1), Si$x,
                i, ties = "ordered")$y
            O[!isna, j] <- ((1:n[1])[!isna])[Si$ix]
        }
        else {
            S[, j] <- Si$x
            O[, j] <- Si$ix
        }
    }
    m <- rowMeans(S)
    for (j in 1:n[2]) {
        if (ties)
            r <- rank(A[, j])
        if (nobs[j] < n[1]) {
            isna <- is.na(A[, j])
            if (ties)
                A[!isna, j] <- approx(i, m, (r[!isna] - 1)/(nobs[j] -
                  1), ties = "ordered")$y
            else A[O[!isna, j], j] <- approx(i, m, (0:(nobs[j] -
                1))/(nobs[j] - 1), ties = "ordered")$y
        }
        else {
            if (ties)
                A[, j] <- approx(i, m, (r - 1)/(n[1] - 1), ties = "ordered")$y
            else A[O[, j], j] <- m
        }
    }
    A
}

NewQuantileN.dialog <- list(
  label = "Quantile Normalization on Data",
  Data.dataframeItem = "", label = "Data Set", tooltip = "Numeric data. Missing values are allowed.",
  ties.trueFalseItem = TRUE, label = "Ties?"
)

NewQuantileN <- function (Data, ties=TRUE)
{
    old_attr <- attributes(Data)    
    Data <- data.matrix(Data)
    Data <- stolen_limma_normalizeQuantiles(df, ties)    
    Data <- copy.metadata(Data, old_attr)
    return(Data)
}