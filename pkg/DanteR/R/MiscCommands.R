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
# General R functions used in DAnTE
# -------------------------------------------------------------------------

#source("Rscripts/Dialogs.r") # Load all the functions and dialogs

  # Set the options file name as a persistent object
  # append sets overwrite
my.setwd <- function(dir, append=TRUE, file=".options.file.name"){
  tryCatch({
    cmd.string <- paste("\nsetwd(",deparse(dir),")", sep="")
    write(cmd.string, file=file, append=append)
    setwd(dir)
  }, error = function(e) {
     print(e)
     setwd(dir)
  }
  )
}


# Set some graphics parameters
setpar <- function(nr, nc, byrow, marx1, mary1, marx2, mary2, Title, xaxt, yaxt){
  plot.new()
  if(byrow) par(mfrow=c(nr, nc))
  else(par(mfcol=c(nr, nc)))
  par(mar = c(marx1, mary1, marx2, mary2))
  par(xaxt=ifelse(xaxt, "s", "n"))
  par(yaxt=ifelse(yaxt, "s", "n"))  
  if(nchar(Title)){
    title(Title)
  }
}

setpar.dialog <- list(
  title = "Set Graphics Options",
  label = "Choose graphics layout and margin size",
  Title.stringItem = "", label = "Title to Use (Optional)",
  nr.integerItem = c(value=1, from=1, to=10, by=1), label = "Number of Rows",
  nc.integerItem = c(value=1, from=1, to=10, by=1), label = "Number of Columns",
  byrow.trueFalseItem = TRUE, label = "Fill Across Rows?",
  xaxt.trueFalseItem = TRUE, label = "Show X-Axis",  
  yaxt.trueFalseItem = TRUE, label = "Show Y-Axis",    
  BREAK=TRUE, 
  marx1.numericItem = 5.1, label = "Bottom Margin",
  mary1.numericItem = 4.1, label = "Left Margin",
  marx2.numericItem = 4.1, label = "Top Margin",
  mary2.numericItem = 2.1, label = "Right Margin"
)

# Log transform
logTransform <- function(Data, logBase=2, bias=0, add=TRUE)
{
    if (add)
        out <- log(Data + bias, base=logBase)
    else
        out <- log(Data * bias, base=logBase)
    #out <- asinh(Data)
    return(out)
}

# Load a file
loadfile <- function(filename,stripwhite=TRUE,header=TRUE,separator){
  Abund <- read.csv(filename,strip.white=stripwhite,header=header,sep=separator,row.names=1)
  Abund[Abund==0]<-NA
  Abund
}

# Send matrix to the App. So, 'NA' should be replaced by '999999'
sendmatrix <- function(x){
  x[is.na(x)] <- 999999
  x <- as.matrix(x)
  return(x)
}

# Get a matrix from the App. Missing values are substituted by 'NA'
getmatrix <- function(x){
  x[x==0] <- NA
  return(as.matrix(x))
}

RVersionOK <- function(major=2, minor=6.0)
{
    rver <- R.Version()
    rver.maj <- as.numeric(rver$major)
    rver.min <- as.numeric(rver$minor)
    out <- (major <= rver.maj && minor <= rver.min)
    return (out)
}

checkPackage <- function(package="gplots"){
  ins <- installed.packages()
  x <- grep(package,rownames(ins))
  out <- (length(x)>0)
  return(out)
}

installPackage <- function(package="gplots",
                    repository="http://lib.stat.cmu.edu/R/CRAN")
{
    if (!checkPackage(package))
      install.packages(package,.libPaths()[1],repository)
}

installPackages <- function(packages,
                        repository="http://lib.stat.cmu.edu/R/CRAN")
{
    for (num in 1:length(packages))
    {
        installPackage(packages[num], repository=repository)
    }
    #cat(print(paste('Packages installed:', packages)))
}

SaveWithProts <- function(Data, ProtInfo, filename)
{
    Data1 <- data.frame(Row_ID=rownames(Data),Data)
    X <- merge(ProtInfo,Data1,by="Row_ID")
    X <- X[order(X[,1]),]
    #return(X)
    write.table(X, file=filename, quote=FALSE, sep=",", col.names=TRUE,
        row.names=FALSE,na="")
}

remove.duplicates <- function(Data)
{
     rowIDs <- rownames(Data)
     Data <- Data[!duplicated(rowIDs),]
     return(Data)
}

remove.emptyProtInfo <- function(Data, checkEset=FALSE)
{
    outData <- Data[!(Data[,1]==""),]
    outData <- outData[!(outData[,2]==""),]
    if (checkEset)
    {
       RN <- rownames(Data)
       Idx <- outData[,1] %in% RN
       outData <- outData[Idx,]
    }
    return(unique(outData))
}

#-------------------------------------------------------------
customScale <- function(x,y)
{
    xmin <- min(x,na.rm=T)
    xmax <- max(x,na.rm=T)
    ymin <- min(y,na.rm=T)
    ymax <- max(y,na.rm=T)
    y <- (ymax - ymin)/(xmax-xmin) * (x - xmin) + ymin
    return(y)
}


#---------------------------------------------------------------
zscores <- function(x)
{
    mu <- mean(x, na.rm=TRUE)
    sigma <- sd(x, na.rm=TRUE)
    z <- (mu - x)/sigma
    return(z)
}

pvalsNormal <- function(x)
{
    z <- zscores(x)
    return(2*pnorm(-abs(z)))
}
