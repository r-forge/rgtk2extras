##########################################################
#
#
# Linear regression at peptide level
#
#
##########################################################

# __Arguments__
# all.factors: MxN table where col names are factors and rows are experiments
# MSdat1: PxM table where col names are factor row names
# ProtInfo: Px(>2) table containing "Mass Tag ID" identical to MSdat1 row names
# and "References" being unique protein ID's.
# factors.to.use: one or more column names in the factors
# N_MAX: how many peptides to use at most for each protein
# N_MIN: reject peptides below this cutoff

    # return vector of estimate followed by pr > 0
  names.vector <- function(fit1, M=1) {
    nn <- summary(fit1)$coef
    if(ncol(nn) == 4){
      nn <- nn[,c(1, 4),drop=F]    
    } else {
      pval <- 2*pt(-abs(summary(fit1)$coef[,3]), summary(fit1)$df[2]) 
      nn <- cbind(nn[,1,drop=F], pval)
    }
    rn <- rownames(nn)
    to.remove <- sort(union(which(rn == "(Intercept)"),  which(regexpr("^.DanteR_block_name", rn)==1) ))
    if(length(to.remove)) 
      nn <- nn[-to.remove,,drop=F]        
    colnames(nn) <- c("est", "pval")
    xx <- c(t(nn))
    names(xx) <- c(t(outer(rownames(nn), colnames(nn), FUN=paste, sep="."))) 
    return(xx)
  }     
  
make.formula.string <- function(factors, do.interactions=FALSE){
  fs <- "1"
  if(length(factors)){
    fs <- paste(factors, collapse=" + ")
    if(do.interactions && length(factors) > 1)
      fs <- paste(unlist(lapply(as.data.frame(t(combinations(length(factors), 2, factors)), stringsAsFactors=F), paste, collapse="*")), collapse = " + ")
  }
  return(fs)
}

ANOVA.dialog <- list(
  #long.running=TRUE,
  label = "ANOVA or GLM On Data Crosstab",
  help = "ANOVA",
  show.progress=TRUE,
  dataset.dataframeItem = "", label = "Choose Data Crosstab",
    signal = c("default", function(item, split.row.metadata.field, dummy, column.metadata.fields){
      obj.name <- get.value(item)
      if(object.exists(obj.name)) {
        obj <- safe.eval(obj.name)
        row.key <- get.row.metadata.key(obj)
        rmt <- get.row.metadata.table(obj)
        set.value(split.row.metadata.field, colnames(rmt)[!colnames(rmt)%in%row.key])        
        col.key <- get.column.metadata.key(obj)        
        cmt <- get.column.metadata.table(obj)
        if(is.null(cmt))
          quick_message("Data Has No Column Metadata Set.\n\nUse Metadata->Column Metadata or File->Load Column Metadata to load factors")
        set.value(dummy, colnames(cmt)[!colnames(cmt)%in%col.key])
        set.value(column.metadata.fields, NULL)        
      }      
    }, "split.row.metadata.field", "dummy", "column.metadata.fields"),
  useglm.choiceItem = c("lm", "rlm", "poisson", "quasipoisson", "gamma"), item.labels = c("Linear Model (lm)", "Robust Linear Model (rlm)", "Poisson (Counts)", "Quasipoisson (Counts)", "Gamma"), label = "Model Type", 
   tooltip = "Choose the type of analysis you want to do. Intensity type data should use lm or rlm. Count type data should use Poisson.",

  split.by.row.metadata.trueFalseItem = FALSE, label = "Do Protein-Level ANOVA",
    tooltip = "Split peptide data into groups based on protein identity, then do the analysis protein by protein",  
    signal = c("default", "toggle.sensitive", "split.row.metadata.field", "nrow.block.min", "nrow.block.max", "block.order.function.name"), 
  split.row.metadata.field.choiceItem = NULL, label = "Choose Protein ID Field", indent=10,
    tooltip = "The row metadata column containing proteins",    

  block.order.function.name.choiceItem = c("median", "mean"), label = "Order Peptides Using", indent=10,
    tooltip = "When peptides are split into groups, take the most abundant peptides within those groups based on median or mean",      
  nrow.block.min.integerItem = 1, label = "Minimum # Peptides", indent=10,
    tooltip = "If this is 1, it allows 1-hit wonders. Require at least this many peptides to be present in the block.",        
  nrow.block.max.integerItem = 5, label = "Maximum # Peptides",  indent=10,  
    tooltip = "Take the top N peptides within the protein for analysis.",          
      
  use.weight.trueFalseItem = FALSE, label = "Use Weighting",
    tooltip = "Add a weighting function to allow data variability to depend on the data value, useful for LC-MS data",
    signal = c("default", "toggle.sensitive", "weight.function", "weight.par"),
  weight.function.radiobuttonItem = c("exp(Y*", "Y^("), item.labels = c("var(y) ~ exp(W*y)", "var(y) ~ y^W"), label = "Weight Function", indent=10,
  weight.par.numericItem = 0.25, label = "Parameter W", indent=10,
        
#  use.glm.trueFalseItem = FALSE,  label = "Use General Linear Model", sensitive=F,
#    signal = c("default", "toggle.sensitive", "glm.link"),   
#  glm.link.choiceItem = strsplit('
#gaussian(link = "identity")  
#binomial(link = "logit")
#Gamma(link = "inverse")
#binomial(link = "logit")
#poisson(link = "log")
#quasi(link = "identity", variance = "constant")
#quasibinomial(link = "logit")
#quasipoisson(link = "log")', "\n")[[1]][-1], label = "Model Link Function", indent=10, sensitive=F,
  BREAK = TRUE,

  dummy.listItem = NULL, label = "Available Factors", show.arrows=FALSE,
    tooltip = "Select the factor that defines data categories you wish to compare",  
  column.metadata.fields.listItem = NULL, label = "Choose Factors",
    tooltip = "Select the factor that defines data categories you wish to compare",
   signal = c("add", "push.selection", "dummy"),
   signal = c("subtract", "pop.selection", "dummy"),  
   signal = c("default", function(item, formula.string, do.interactions, dataset, first.level){
     set.value(formula.string, make.formula.string(get.value(item), get.value(do.interactions)))
     obj_name <- get.value(dataset)
     dum_val <- get.value(item)     
     # Populate the first.level with the unique factors of the level
     if(object.exists(obj_name) && length(dum_val)) {
      obj <- safe.eval(obj_name)
      col.key <- get.column.metadata.key(obj)        
      cmt <- get.column.metadata.table(obj)
      ccx <- cmt[,dum_val[1]]
      if(is.factor(ccx))
        set.value(first.level, levels(ccx))
      else 
        set.value(first.level, na.omit(unique(ccx)))
    } else {
        set.value(first.level, character(0))    
    }
   }, "formula.string", "do.interactions", "dataset", "first.level"),
   
  first.level.choiceItem = NULL, label = "First Factor Reference Level",
    tooltip = "Choose category of data to serve as a control. To do this for more than one factor, use Edit Factors.",
#  first.level.contrasts.choiceItem = c("contr.treatment", "contr.sum"), item.labels = c("Treatment", "Sum"), label = "Contrasts", tooltip = "Choose way in which to compare data categories",  
#  BREAK = TRUE,   
  return.residuals.trueFalseItem = FALSE, label = "Include Fit and Residuals",
    tooltip = "Return model fits and residuals for the analysis. The residual of a sample is the difference between the observed and the value fit from the model.",
  do.interactions.trueFalseItem = FALSE, label = "Allow 2-Way Interactions",
   signal = c("default", function(item, formula.string, column.metadata.fields){
     set.value(formula.string, make.formula.string(get.value(column.metadata.fields), get.value(item)))
   }, "formula.string", "column.metadata.fields"),  
  formula.string.stringItem = "", label = "Formula: y ~ ...",
   tooltip = "The formula that is passed to the statistical analysis function"
)


  
# ProtInfo needs to have a Reference and MTID column
ANOVA <- function(
    dataset,
    split.by.row.metadata = FALSE, # for blocking peptides to proteins
    split.row.metadata.field = NULL,    
    column.metadata.fields = NULL, # factors to use
    do.interactions = FALSE,
    block.order.function.name = "median",
    nrow.block.min = 1,
    nrow.block.max = 5,
    useglm = "lm",
    use.weight = FALSE,
    weight.function = "NULL",
    weight.par = 0,
    formula.string = ".",
    do.residuals = FALSE,
    first.level = character(0), # explicitly 
    first.level.contrasts = character(0),
    progressbar = NULL,
    progresslabel = NULL,
    return.all.fits=FALSE,
    return.residuals = FALSE, # return resids and fits
    ...
    ){
  
  old_attr <- attributes(dataset)
  #print(column.metadata.fields)
      
  if(return.residuals){
    resids <- fitteds <- dataset
    resids[,] <- fitteds[,] <- NA
  }  
    
  column.metadata <- get.column.metadata.table(dataset)
  column.metadata.key <- get.column.metadata.key(dataset)
  if(is.null(column.metadata)) stop("Column metadata not specified")  
  if(!length(column.metadata.fields)) stop("\n\nNo factors chosen. You need to choose some factors from Available Factors.")    
             
  if(useglm == "lm") 
     theGlm <- lm
  else if(useglm == "rlm") 
     theGlm <- rlm
  else if(useglm == "poisson") 
     theGlm <- function(...) glm(..., family = poisson(link = "log"))
  else if(useglm == "quasipoisson") 
     theGlm <- function(...) glm(..., family = quasipoisson(link = "log"))     
  else if(useglm == "gamma") 
     theGlm <- function(...) glm(..., family = Gamma(link = "inverse"))
  else
    stop(paste("Model type", useglm, "not known"))
  
  # if(!column.metadata.fields%in%colnames(column.metadata)) stop("Column metadata: invalid field")
  
  column.key.col <- column.metadata[,column.metadata.key]
  rnf <- as.character(column.key.col[column.key.col%in%colnames(dataset)])
  if(!length(rnf)) stop(paste("Column names of data do not exist in", column.key.col, "of column metadata. Please check your factors are correct."))

  n.blocks <- nrow(dataset)
  if(is.null(rownames(dataset))) rownames(dataset) <- 1:dim(dataset)[1]
  block.names <- rownames(dataset)
  
  if(split.by.row.metadata && !is.null(split.row.metadata.field)){
    rnt <- rownames(dataset)  
    row.metadata.table <- get.row.metadata.table(dataset)
    row.metadata.key <- get.row.metadata.key(dataset)   
    row.field.col <- row.metadata.table[,split.row.metadata.field]    
    n.blocks <- length(unique(row.field.col))
    block.names <- unique(row.field.col)
    if(is.null(row.metadata.table)) stop("Row metadata not specified")  
    if(any(!rownames(dataset)%in%row.metadata.table[,row.metadata.key])) stop("Mismatch in data set rownames and row metadata")
    row.key.col <- row.metadata.table[,row.metadata.key]    
  }

#  factors <- column.metadata[column.key.col%in%colnames(dataset),column.metadata.fields,drop=F]
  factors <- column.metadata[column.key.col%in%colnames(dataset),,drop=F]
  #print(factors)
  first.factor.name <- column.metadata.fields[1]
  if(length(first.level) && first.factor.name%in%colnames(factors)){
    first.factor <- factors[,first.factor.name]
    if(!is.factor(first.factor)) first.factor <- as.factor(first.factor)
    if(first.level%in%levels(first.factor)) {
      first.factor <- factor(first.factor, levels=union(first.level, levels(first.factor)))
    }
    factors[,first.factor.name] <- first.factor
  }

  if(!all(rnf%in%colnames(dataset))) stop("Mismatch in data set columns and ANOVA factor table")
  
  #colnames(factors) <- paste(colnames(factors), "_", sep="")
  
  #formula.string <- "."
  
  if(use.weight && !is.null(weight.function))
    weight.function.expr <- parse(text=paste(weight.function, weight.par, ")", sep=""))    
    
    # Pre-fill the return array with names
  M = 1
  N = ncol(dataset)
  the.formula <- as.formula(paste("Y ~", formula.string))          
  names.df <- data.frame(Y = rnorm(M*N), factors)
  fit1 <- lm(the.formula, data=names.df)  
  nv <- names.vector(fit1, 1)
  return.arr <- array(NA, c(n.blocks, length(nv))) 
  rownames(return.arr) <- block.names
  colnames(return.arr) <- names(nv)
 
  block.order.function <- eval(parse(text=block.order.function.name))

  residuals.list <- all.fits <- list()
   
  for(ii in 1:n.blocks){
    tryCatch({         
      data.idx <- ii
      if(split.by.row.metadata)
        data.idx <- which(rnt%in%row.key.col[block.names[ii]==row.field.col])

      dat <- dataset[data.idx,rnf,drop=F]
                                                                       
      if(is.null(dat) || is.null(dim(dat)) || dim(dat)[1] < nrow.block.min) {
        next;
      }
      
      dat <- dat[order(apply(dat, 1, block.order.function), decreasing=T),,drop=F]
      if(nrow(dat) > nrow.block.max) dat <- dat[1:nrow.block.max,,drop=F]
    
      M <- nrow(dat)  

    my.df <- data.frame(Y=c(t(dat)), factors, row.names=NULL)
      # add a block for peptides if we need one
    if(M > 1){
      my.df <- data.frame(my.df, .DanteR_block_name = as.factor(rep(1:M, each=N)))
      formula.string <- paste(".DanteR_block_name", formula.string, sep = " + ")
    }
      
    the.formula <- as.formula(paste("Y ~", formula.string))
        
    if(use.weight)
      fit1 <- theGlm(the.formula, data=my.df, weights=eval(weight.function.expr))          
    else 
      fit1 <- theGlm(the.formula, data=my.df)
      
    if(ii == 1) {
      cat("First model fit:\n")
      print(fit1)
    }
    if(do.residuals)
      residuals.list[[block.names[ii]]] <- resid(fit1)
    if(return.all.fits) 
      all.fits[[block.names[ii]]] <- fit1
    if(return.residuals){
      Ymat <- t(dat)
      Ymat[which(!is.na(Ymat))] <- fit1$resid
      resids[data.idx[1:ncol(Ymat)],] <- t(Ymat)
      Ymat[which(!is.na(Ymat))] <- fit1$fit
      fitteds[data.idx[1:ncol(Ymat)],] <- t(Ymat)      
    }
          
      nv <- names.vector(fit1, M)
      return.arr[ii,names(nv)] <- nv
      #cat(ifelse(M > 9, 9, M))
    }, error = function(e) {#cat("!")
    if(ii == 1) print(e)
      }#,  
       #interrupt = function(...) stop("Stopped")
    )
    if(ii%%50==0) {
      #cat(paste(" ", ifelse(ii == 1, "", ii), "\n"))
      if(!missing(progressbar)) progressbar$setFraction(ii/n.blocks)
      if(!missing(progresslabel)) progresslabel$setText(paste("Status:", ii, "/", n.blocks))
    }    
  }
  #if(ii%%60!=0) cat("\n")

  if(split.by.row.metadata)
    attr(return.arr, "Row_Metadata") <- c(key = split.row.metadata.field, table = old_attr[["Row_Metadata"]][["table"]])
  else 
    attr(return.arr, "Row_Metadata") <- old_attr[["Row_Metadata"]]

if(do.residuals || return.all.fits)
  return(list(ANOVA=return.arr, residuals.list=residuals.list, all.fits=all.fits))
if(return.residuals)
  return(list(ANOVA=return.arr, residuals=resids, fitted=fitteds))
  
return(return.arr)
}