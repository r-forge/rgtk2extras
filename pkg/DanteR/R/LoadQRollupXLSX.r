LoadQRollupXLSX <- function(filename, 
  make.factors=TRUE,
  experiment.name.regex="^Eco_45-(.*?)-?(M[0-9])_([0-9])(.)_(.*)$", 
  factor.names='c("tolerance", "mutant", "biol.rep", "tech.rep")')
{  
  library(RODBC)
    # Load the xlsx in
  xls <- odbcConnectExcel2007(filename)

    # Make summary into factors
  xls.summary <- sqlFetch(xls, "Summary", colnames=1, as.is=T)  
    # Get crosstab
  xls.crosstab <- sqlFetch(xls, "Proteins with Peptides Crosstab", colnames=1, as.is=T)
  odbcClose(xls)  
  
  job.regex <- "^Job ([0-9]*) .*$"
  which.are.jobs <- which(regexpr(job.regex, colnames(xls.crosstab), perl=T) > -1)
  
  job.colnames <- colnames(xls.crosstab)[which.are.jobs]
  job.ids <- sub(job.regex, "\\1", job.colnames, perl=T)
  
    # Check that we don't have duplicate job names
  stopifnot(length(which(duplicated(xls.summary$Jobs)))==0)
  
    # Check that our job listings match identically
  stopifnot(identical(job.ids, as.character(xls.summary$Jobs)))
  
    # Check that we don't have duplicate row names
  stopifnot(length(which(duplicated(row.names(xls.crosstab))))==0)
  
    # remove duplicate MTID's
  mtid.list <- xls.crosstab$"Mass Tag ID"
  mtid.dupes <- mtid.list[duplicated(mtid.list)]
  dupe.idx <- which(mtid.list%in%mtid.dupes)
  message(paste(length(dupe.idx), "duplicates found, removing."))
  xls.crosstab2 <- xls.crosstab[-dupe.idx,]
    # check it worked
  stopifnot(length(which(duplicated(xls.crosstab2$"Mass Tag ID")))==0)
  
    #make MSdat logged data
  MSdat1 <- xls.crosstab2[,which.are.jobs]
  ProtInfo <- xls.crosstab2[,-which.are.jobs]
  
  xls.crosstab2$"Mass Tag ID" <- as.character(xls.crosstab2$"Mass Tag ID")
  rownames(MSdat1) <- xls.crosstab2$"Mass Tag ID"
  colnames(MSdat1) <- job.ids
  
  stopifnot(identical(rownames(MSdat1), as.character(ProtInfo$"Mass Tag ID")))
  
    # Plot to check what the data looks like
  #PlotDataAgainstMean(MSdat1[,1:10])
  
  message(paste(length(unique(ProtInfo$Reference)), "unique proteins found"))
  
    # Plot distribution of peptides by protein
  barplot(table(table(ProtInfo$Reference)), 
  main=bquote("Distribution of peptides in each protein, n" == .(length(unique(ProtInfo$Reference)))), 
  xlab="Number of peptides observed", ylab="Number of proteins observed")
  
  assign("Eset", MSdat1, envir=.GlobalEnv)
  assign("ProtInfo", ProtInfo, envir=.GlobalEnv) 
  
    
  if(make.factors){
    factor.names <- eval(parse(text=factor.names))
    #experiment.name.regex <- "^Eco_45-(.*?)-?(M[0-9])_([0-9])(.)_(.*)$"  
    all.factors <- lapply(1:length(factor.names), 
      function(x) {
       x <- sub(experiment.name.regex, paste("\\", x, sep=""), xls.summary$Experiment, perl=TRUE)
       x[x==""] <- "None"
       return(x)
      })
  
    all.factors <- as.data.frame(all.factors)      
    colnames(all.factors) <- factor.names
    rownames(all.factors) <- xls.summary$Jobs 
        
    stopifnot(identical(colnames(MSdat1), rownames(all.factors))) 
   
    assign("factors", all.factors, envir=.GlobalEnv)    
  }

  invisible(NULL)
}         
