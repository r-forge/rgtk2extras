make.count.xtab <- function(ProtInfo, Eset, all.rownames, progresslabel=NULL){
  myTfrm <- function(x) {
    rownames(x) <- x[,1]
    x[,-1,drop=F]
  }
  library(reshape)
  cat1 <- function(txt) if(!is.null(progresslabel)) progresslabel$setText(txt)
  cat1("Reshaping data...\n")
  MSdat <- data.frame(ProtInfo, Total=1, Eset, check.names=F,stringsAsFactors =F)
  id.vars <- colnames(ProtInfo)
  if(length(id.vars) != 2) stop("The ProtInfo table must have two columns")

  cat1("Melting data...\n")
  cc.melt <- melt(MSdat, id.vars = id.vars, variable_name = "Job")
  cat1("Data melt done. Casting...\n")
  MSpivot <- cast(cc.melt, as.formula(paste(id.vars[2], "~Job")), function(x) sum(!is.na(x)))
  cat1("Data cast done.\n")
  MSpivot <- as.matrix(as.data.frame(myTfrm(MSpivot), as.is=T))
  retval <- array(NA, c(length(all.rownames), ncol(MSpivot)))
#  print(dim(retval))
  rownames(retval) <- all.rownames
  colnames(retval) <- colnames(MSpivot)
  retval[rownames(MSpivot),] <- MSpivot
  return(retval)
}

set.metadata.row.fields <- function(item, ...){
  the.args <- list(...)
  dataset.name <- get.value(item)
#  cat(dataset.name)
  if(exists(dataset.name)){
    dataset <- get(dataset.name)
    metadata.table <- get.row.metadata.table(dataset)
    metadata.key <- get.column.metadata.key(dataset)
    sapply(the.args, function(x) set.value(x, colnames(metadata.table)[!colnames(metadata.table)%in%metadata.key]))
  }
}

set.metadata.column.fields <- function(item, ...){
  the.args <- list(...)
  dataset.name <- get.value(item)
  if(exists(dataset.name)){
    dataset <- get(dataset.name)
    metadata.table <- get.column.metadata.table(dataset)
    metadata.key <- get.column.metadata.key(dataset)
    sapply(the.args, function(x) set.value(x, colnames(metadata.table)[!colnames(metadata.table)%in%metadata.key] ))
    sapply(the.args, function(x) RGtk2Extras:::signal(x, "default"))
  }
}

Fisher.dialog <- list(
  title = "Fisher's Exact Test",
  show.progress=TRUE,
  label = "Log-likelihood tests of independence & goodness of fit",
  dataset.dataframeItem = "", label = "Data Crosstab",
    signal = c("default", set.metadata.row.fields, "row.metadata.split"),
    signal = c("default", "get.dataset.row.metadata.fields", "row.metadata.split"),  
    signal = c("default", set.metadata.column.fields, "column.metadata.field"),
  do.split.trueFalseItem = TRUE, label = "Combine Rows By Protein?",
    signal = c("default", "toggle.sensitive", "row.metadata.split"),
  row.metadata.split.choiceItem = NULL, label = "Protein ID Field", indent=10,
  column.metadata.field.choiceItem = NULL, label = "Factor To Compare",
    signal = c("default", function(item, dataset, ref.level){
      dataset.name <- get.value(dataset)
      metadata.key <- get.value(item)
      if(object.exists(dataset.name)){
        dataset <- safe.eval(dataset.name)
        metadata.table <- get.column.metadata.table(dataset)
        metadata.primary <- get.column.metadata.key(dataset)
        the.levels <- unique(metadata.table[,metadata.key])
        the.useful.levels <- intersect(the.levels, metadata.table[metadata.table[,metadata.primary]%in%colnames(dataset),metadata.key])
        set.value(ref.level, the.useful.levels)
      }
    }, "dataset", "ref.level"),
  ref.level.choiceItem = NULL, label = "Reference Level Within Factor", 
    tooltip = "Choose the category of data you wish to use as the reference",
  BREAK=TRUE,
  min.peptides.integerItem = c(from=1, to=10, value=1, by=1), label = "Minimum Peptides/Items",
    tooltip = "Exclude N-hit wonders - ie, proteins (or other items) where the number of peptides is less than this number",
  infinity.replacement.numericItem = 1024, label = "Largest Possible Count Ratio",
    tooltip = "Choose the largest possible ratio to report, for the case where there are zero observations in one group",
  log.ratio.trueFalseItem = TRUE, label = "Log Transform Count Ratio",
    tooltip = "Report the log transformed ratio of observation counts, for compatibility with other statistical functions",
    signal = c("default", "toggle.sensitive", "log.base"),
  log.base.radiobuttonItem = c(2, exp(1), 10), item.labels = c("Base 2", "Natural Log", "Base 10"), label = "Log Base", indent=10,
  export.count.trueFalseItem = TRUE, label = "Include Table of Counts?",
    tooltip = "Include a summary table of observation counts in the output"
)
   
################################################################################
# Do Fisher Test on two identically sized crosstabs for peptide count seen/unseen
# Input: ProtInfo table with "Reference", "Mass Tag ID", and pivot table of same
# number of rows.
# Output: up, down, lists of significant proteins by count
################################################################################
Fisher <- function(dataset, do.split=TRUE, row.metadata.split = NULL, column.metadata.field = NULL,
    ref.level = NULL, infinity.replacement = 1024, log.ratio=TRUE, log.base=2, export.count = TRUE, 
    min.peptides = 1, 
    progressbar = NULL, progresslabel = NULL, ...){

  old_attr <- attributes(dataset)
  row.metadata.table <- get.row.metadata.table(dataset)
  column.metadata.table <- get.column.metadata.table(dataset)
  row.metadata.key <- get.row.metadata.key(dataset)
  column.metadata.key <- get.column.metadata.key(dataset)

  if(!do.split){  # Don't split by protein, just create a "fake protein info table"
    row.metadata.key <- "Peptide"
    row.metadata.split <- "Protein"      
    row.metadata.table <- cbind(Peptide = rownames(dataset), Protein = rownames(dataset))
  }

  stopifnot(!is.null(row.metadata.split))
  stopifnot(!is.null(row.metadata.table))
  stopifnot(!is.null(row.metadata.key))
  stopifnot(!is.null(column.metadata.table))
  stopifnot(!is.null(column.metadata.field))
  stopifnot(!is.null(column.metadata.key))

  Eset <- dataset

  ProtInfo <- row.metadata.table[match(rownames(dataset), row.metadata.table[,row.metadata.key]),c(row.metadata.key, row.metadata.split),drop=F]

  the.factors <- column.metadata.table[match(colnames(dataset), column.metadata.table[,column.metadata.key]),c(column.metadata.key, column.metadata.field)]
  rownames(the.factors) <- the.factors[,1]
  the.factors <- the.factors[,-1,drop=F]
  stopifnot(all(ProtInfo[,1] == rownames(dataset)))
  
  factor.columns <- column.metadata.field

  if(length(factor.columns) > 1) {
    factor.columns <- factor.columns[1]
    warning(paste("Fisher test is only set up for 1 factor; using", factor.columns))
  }
  
  factors <- the.factors[,factor.columns]
#  print(the.factors)
  f.levels <- unique(factors)
  if(length(f.levels) < 2) stop("Can't do count test on < 2 factor levels")
  
  if(is.null(ref.level)) ref.level <- f.levels[1]
  stopifnot(ref.level%in%f.levels)
  
#  all.rownames <- unique(row.metadata.table[,row.metadata.split])
  all.rownames <- unique(ProtInfo[,row.metadata.split])
  seen.xtab <- make.count.xtab(ProtInfo, Eset, all.rownames, progresslabel)
  all.effects <- NULL
  all.counts <- NULL
  #if(export.count) Counts_Crosstab <<- seen.xtab  

  jobs1 <- rownames(the.factors)[factors == ref.level & !is.na(factors)]
  
  MSsum <- array(NA, c(dim(seen.xtab)[1], 2))
  MSsum1 <- array(NA, c(dim(seen.xtab)[1], 2))
  
  MSsum[,1] <- rowSums(seen.xtab[,jobs1,drop=F])
  MSsum1[,1] <- seen.xtab[,"Total"]*length(jobs1) - MSsum[,1]
  MSsums <- cbind(MSsum, MSsum1)
  
  rownames(MSsums) <- rownames(seen.xtab)
  colnames(MSsums) <- make.names(paste(c(ref.level, ""), c("", "", "NA", "NA"), sep="_"), unique=T)
  MSsums <- data.frame(MSsums)

  all.counts <- MSsums[,c(1,3)]
  colnames(all.counts) <- make.names(paste(paste(factor.columns, ref.level, sep=""), c("seen", "NA")))

  if(!missing(progresslabel)) progresslabel$setText("Doing p-value calculations....")
  levels.to.do <- na.omit(setdiff(f.levels, ref.level))
#  print("levels.to.do")
#  print(levels.to.do)
  for(level in levels.to.do){
    if(!missing(progresslabel)) progresslabel$setText(paste("  Level", level, "\n"))
  
    theName <- make.names(paste(factor.columns, level, sep=""))
    jobs2 <- rownames(the.factors)[factors == level & !is.na(factors)]
    
    MSsum[,2] <- rowSums(seen.xtab[,jobs2,drop=F])
    MSsum1[,2] <- seen.xtab[,"Total"]*length(jobs2) - MSsum[,2]
    MSsums <- cbind(MSsum, MSsum1)

    count.ratio <- array((MSsum[,2]/MSsum1[,2])/(MSsum[,1]/MSsum1[,1]), c(dim(MSsums)[1], 1))
    count.ratio[is.infinite(count.ratio)] <- infinity.replacement
    count.ratio[count.ratio==0] <- 1/infinity.replacement
    if(log.ratio) count.ratio <- log(count.ratio, base=log.base)
    rownames(count.ratio) <- rownames(seen.xtab)
    colnames(count.ratio) <- theName

    colnames(MSsums) <- make.names(paste(c(factor.columns, ref.level, level), c("", "", "NA", "NA"), sep="_"))

    count.pvals <- array(NA, c(dim(MSsums)[1], 1))
    rownames(count.pvals) <- rownames(seen.xtab)
    colnames(count.pvals) <- theName
    Nsums <- dim(MSsums)[1]
    for(i in 1:Nsums)
      tryCatch({
        if(i %% 50 == 0) {
          if(!missing(progresslabel))
            progresslabel$setText(paste("Level:", level, ":", i, "/", Nsums))
          if(!missing(progressbar))
            progressbar$setFraction(i/Nsums)
        }
        count.pvals[i] <- fisher.test(matrix(as.numeric(MSsums[i,]), nrow=2))$p
        },
          error = function(e){})
    all.effects <- cbind(all.effects, count.ratio, count.pvals)
    all.counts <- cbind(all.counts, MSsums[,c(2,4)])
    colnames(all.effects)[dim(all.effects)[2] - c(1, 0)] <- make.names(paste(theName, c("est", "pval")))    
    colnames(all.counts)[dim(all.counts)[2] - c(1, 0)] <- make.names(paste(theName, c("seen", "NA")))
  }
  all.counts <- as.matrix(all.counts)
  rownames(all.counts) <- rownames(seen.xtab)
  rownames(all.effects) <- rownames(seen.xtab)  
  
  if(min.peptides > 1){
    prot.counts <- table(ProtInfo[,2])
    accept.prots <- names(prot.counts)[prot.counts >= min.peptides]  
    all.counts <- all.counts[rownames(all.counts)%in%accept.prots,,drop=F]
    all.effects <- all.effects[rownames(all.effects)%in%accept.prots,,drop=F]
  }
  
  attr(all.counts, "Row_Metadata") <- c(key = row.metadata.split, table = old_attr[["Row_Metadata"]][["table"]])    
  attr(all.counts, "Column_Metadata") <- c(key = column.metadata.field, table = old_attr[["Column_Metadata"]][["table"]])
  storage.mode(all.counts) <- "integer"   

#  cat("Done.\n"); flush.console()
  attr(all.effects, "Row_Metadata") <- c(key = row.metadata.split, table = old_attr[["Row_Metadata"]][["table"]])  

  if(export.count)
    return(list(counts = all.counts, effects = all.effects))
  else 
    return(all.effects)
}
