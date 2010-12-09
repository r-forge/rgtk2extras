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
# ANOVA

require(qvalue)
require(car)
require(nlme)
require(qvalue)

#--------------------------------------------------------------
anovaPvals <- function(x, fEff, rEff,
                   Factors,
                   interact=FALSE,
                   unbalanced=TRUE,
                   Np=2,
                   test=FALSE,
                   useREML=TRUE)
{
    allF <- c(fEff,rEff)
    X <- data.frame(Factors[, allF, drop=FALSE], x)
    
    for (i in 1:(dim(X)[2]-1))
    {
        names(X)[i] <- allF[i]
    }

    lhs <- fEff[1]
      # Set up interactions and additive effects
    if (length(fEff) > 1)
    {
        for (i in 2:length(fEff))
        {
            if (interact)
                lhs <- paste(lhs, "*", fEff[i])
            else
                lhs <- paste(lhs, "+", fEff[i])
        }
    }
    lm.Formula <- as.formula(paste('x~', lhs))
    modelF <- lm(lm.Formula, X)
    
    if (useREML)
        Method <- "REML"
    else
        Method <- "ML"
    
    if (!is.null(rEff))
    {
        rEffects <- paste("~1|", rEff[1], sep="")
        if (length(rEff) > 1)
        {
            for (i in 2:length(rEff))
            {
                rEffects <- paste(rEffects, ", ~1|", rEff[i])
            }
            rEffects <- paste("random=list(", rEffects, ")", sep="")
        }
        else
            rEffects <- paste("random=",rEffects, sep="")

        #modelR <- lme(eval(parse(text=lm.Formula)), data=X, eval(parse(text=rEffects)))
        modelR <- lme(lm.Formula, data=X, eval(parse(text=rEffects)),
                      method=Method, na.action = na.omit)

        options(warn = -1)
        if (unbalanced)
            anova.result <- try(anova(modelR, type="marginal"),silent=TRUE)
        else
            anova.result <- try(anova(modelR),silent=TRUE)

        options(warn = 0)
        
        if(inherits(anova.result, "try-error"))
        {
            return(rep(NA, Np))
        }
        else
        {
            #anova.result <- anova(modelR)
            pvals <- anova.result$"p-value"
            names(pvals)<-rownames(anova.result)

            if (names(pvals)[1] == "(Intercept)")
                    pvals <- pvals[-1]
            idx <- length(names(pvals))
            if (names(pvals)[idx] == "Residuals")
                    pvals <- pvals[-idx]

            pvals[is.nan(pvals)] <- NA
            if (test)
                return(pvals)
            else
            {
                tmp <- rep(NA, Np)
                tmp[1:length(pvals)] <- pvals
                return(tmp)
            }
        }
    }
    else  # No random effects  
    {
        do.silent = TRUE
        if (unbalanced)                                          
            anova.result <- try(Anova(modelF, type="III"),silent=do.silent)
        else
            anova.result <- try(Anova(modelF),silent=do.silent)

        if(inherits(anova.result, "try-error"))
        {
            return(rep(NA, Np))
        }
        else
        {
            pvals <- anova.result["Pr(>F)"][[1]]
            names(pvals)<-rownames(anova.result)

            if (names(pvals)[1] == "(Intercept)")
                    pvals <- pvals[-1]
            idx <- length(names(pvals))
            if (names(pvals)[idx] == "Residuals")
                    pvals <- pvals[-idx]

            if (test)
                return(pvals)
            else
            {
                tmp <- rep(NA, Np)
                tmp[1:length(pvals)] <- pvals
                return(tmp)
            }
        }
    }
}

#--------------------------------------------------------------
splitForAnova <- function(Data,Factors,thres=3)
{
    anovaIdx <- integer(0)
    anovaIdxNon <- integer(0)
    allIdx <- 1:dim(Data)[1]

    if (is.matrix(Factors))
    {
        N <- dim(Factors)[2]  # how many factors?
    }
    else
        N <- 1
    for (k in 1:N)
    {
        if (N > 1)
            currFac <- Factors[,k]
        else
            currFac <- Factors

        splitIdx <- splitmissing.factor(Data, currFac, thres=thres)
        if (k == 1)
            anovaIdx <- splitIdx$good
        else
            anovaIdx <- intersect(anovaIdx, splitIdx$good)
    }
    anovaIdxNon <- allIdx[!(allIdx %in% anovaIdx)]
    out <- list(Good=anovaIdx,Bad=anovaIdxNon)
    return(out)
}

#--------------------------------------------------------------
factor.values <- function(factors)
{
    out <- list()
    for (i in 1:dim(factors)[1])
    {
        out[[rownames(factors)[i]]] <- as.matrix(factors[i,!duplicated(t(factors[i,]))])
    }
    return(out)
}

#--------------------------------------------------------------
splitmissing.factor <- function(Data, Factor, thres=3)
{
    anovaIdx <- integer(0)
    anovaIdxNon <- integer(0)
    allIdx <- 1:dim(Data)[1]
    Nreps <- unique(as.vector(t(Factor))) # Factor Levels
    for (i in 1:length(Nreps)) # for each unique factor level
    {
        idx <- which(Factor == Nreps[i])
        dataset <- Data[,idx,drop=FALSE]
        if (length(idx) > 1) # multiple columns
        {
            splitIdx <- splitmissing.fLevel(dataset, thres=thres)
            if (i == 1)
                anovaIdx <- splitIdx$good
            else
                anovaIdx <- intersect(anovaIdx, splitIdx$good)
        }
    }
    badIdx <- allIdx[!(allIdx %in% anovaIdx)]
    return(list(good=anovaIdx, bad=badIdx))
}

#--------------------------------------------------------------
splitmissing.fLevel <- function(Data,thres=3)
{
    allIdx <- 1:dim(Data)[1]
    validIdx <- rowSums(!is.na(Data)) >= thres
    #validIdx <- apply(!is.na(Data),1,sum) >= thres

    goodIdx <- unique(which(validIdx))
    badIdx <- allIdx[!(allIdx %in% goodIdx)]
    return(list(good=goodIdx, bad=badIdx))
}



