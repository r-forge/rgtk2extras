
DiffExpressed <- function(data, do.data2=FALSE, data2=NULL, effect.type = "log", log.thresh=0, unlog.thresh = 1, pval.thresh=0.05, do.qval = F){
  ll <- list()
  if(do.qval) require(qvalue)
  
  if(identical(effect.type, "log")) {
    effect.thresh <- log.thresh
    dlim <- -log.thresh
  } else {
    effect.thresh <- unlog.thresh
    dlim <- 1
    if(!identical(effect.thresh,0))
      dlim <- 1/effect.thresh    
  }
  if(dim(data)[2] < 2 || dim(data)[2]%%2 != 0) stop("Function got a table with the wrong number of columns")
  if(dim(data)[1] < 1) stop("Function got a table with no rows")  
  rv <- NULL
  for(jj in seq(1, ncol(data), by=2)){
    estimate <- data[,jj]
    pval <- data[,jj+1]
      # data checking
#    pp <- pval[is.finite(pval)]
#    if(length(pp)>0 && (any(pp < 0) || any (pp > 1))) stop("DiffExpressed found p-values that were outside (0, 1)")
    
    na.loc <- is.na(estimate) | is.na(pval)
    estimate.loc.upper <- estimate > effect.thresh
    estimate.loc.lower <- estimate < dlim
    if(do.qval){
      nn.idx <- !is.na(pval)
      qv <- qvalue(pval[nn.idx])$q
      pval[nn.idx] <- qv       
    }
    pval.loc <- pval < pval.thresh
    cn <- colnames(data)[jj]
    sig.up <- !na.loc & pval.loc & estimate.loc.upper
    sig.down <- !na.loc & pval.loc & estimate.loc.lower
    ll[[cn]]$up <- data[sig.up,,drop=F]
    ll[[cn]]$down <- data[sig.down,,drop=F]
  }
  #return(ll)
  return(ll)  
  # padded cbin
  
}

  # Specifically for statistical results having data as estimate, p-value columns.
DiffExp <- function(data, max.p=0.05, min.effect = 0, include.NA.effects=TRUE, make.summary=TRUE, order.by="Natural Order", summary.counts=TRUE, doingLog=TRUE, logBase=2){
  ff <- function(z, zn, dirn, doingLog) {
    rnz <- rownames(z)
    if(is.null(rnz)) rnz <- character(0)
    if(doingLog){                                 
      rv <- data.frame(rnz, z[,1,drop=F], logBase^(z[,1]), z[,2,drop=F], stringsAsFactors=FALSE)            
      colnames(rv) <- make.names(c(paste(dirn, zn, "Species"), "Log Effect Size", "Effect Size", "P-Value"))
    } else {
      rv <- data.frame(rnz, z[,1,drop=F], z[,2,drop=F], stringsAsFactors=FALSE)            
      colnames(rv) <- make.names(c(paste(dirn, zn, "Species"), "Effect", "P-Value"))        
    }
    return(rv)
  }
  doOrder <- function(x, order.by){
    if(identical(order.by, "Natural Order")) return(x)
    if(identical(order.by, "Effect Size")) return(x[order(abs(x[,1]), decreasing=TRUE),,drop=F])
    if(identical(order.by, "P-Value")) return(x[order(abs(x[,2]), decreasing=FALSE),,drop=F])
    stop("doOrder: unknown parameter")
  }
  if(!class(data)%in%c("data.frame", "matrix", "array")) stop("Function requires tabular data")
  cn <- colnames(data)
  rn <- rownames(data)
  retval <- list()
  if(length(cn) < 2 || length(cn)%%2 != 0) stop("Function requires data table with even # of columns")
  summ <- NULL
  for(jj in seq(1, length(cn), by=2)){
    ll <- list()
    cn.jj <- cn[jj]
    if(!is.numeric(data[,jj]) || !is.numeric(data[,jj+1]))
      stop("Function requires data set to be numeric")

#    if(rev(strsplit(cn.jj, ".", fixed=T)[[1]])[1] != "est" || nchar(cn.jj) < 5)
#      stop("Function requires all column names to be of form *.est, *.pval, ...")
    sig.p <- data[,jj+1] <= max.p & !is.na(data[,jj+1])
    sig.fx <- abs(data[,jj]) >= min.effect
    if(include.NA.effects) 
      sig.fx[is.na(sig.fx) & sig.p] <- TRUE
    else 
      sig.fx[is.na(sig.fx) & sig.p] <- FALSE
    sig.p <- sig.p & sig.fx      
    
    ll$up <- data[rn[data[,jj] > 0 & !is.na(data[,jj]) & sig.p],0:1+jj,drop=F]
    ll$up <- doOrder(ll$up, order.by)
    ll$down <- data[rn[data[,jj] < 0 & !is.na(data[,jj]) & sig.p],0:1+jj,drop=F]
    ll$down <- doOrder(ll$down, order.by)    
    nam <- substr(cn.jj, 1, nchar(cn.jj)-4)
    if(make.summary) {
      summ <- ragged.cbind(summ, ff(ll$up, nam, "up", doingLog), ff(ll$down, nam, "down", doingLog), Totals.Up.Down = c(dim(ll$up)[1], dim(ll$down)[1]))
    }
    retval[[nam]] <- ll   
  }
  return(list(Significant=retval, SummaryTable=summ))
}


DiffExp.dialog <- list(
  title = "Get Differentially Expressed",
  label = "Takes a table of effects and p-values from a \nstatistical test and split it into significant up and\ndown components",
  data.dataframeItem = "", label = "Table of Effects and P-values",    
  max.p.numericItem = 0.05, label = "Maximum P-value Threshold",
    tooltip = "Only accept p-values sizes below this value. Set to 1 to report all",  
  min.effect.numericItem = 0.0, label = "Minimum Effect Size Threshold", 
    tooltip = "Only accept effect sizes greater than this value. Leave at zero to report all",
  include.NA.effects.trueFalseItem = TRUE, label = "Accept NA Effects?", 
    tooltip = "Accept effects that are NA, if there is a p-value reported",  
  make.summary.trueFalseItem = TRUE, label = "Create Summary Table",
    tooltip = "Generate a table summarizing all significant findings",
  signal = c("default", "toggle.sensitive", "doingLog", "logBase"),  
  doingLog.trueFalseItem = TRUE, label = "Data Is In Log Scale", indent=10,
    tooltip = "Check this to include effect sizes in the Summary Table",
  signal = c("default", "toggle.sensitive", "logBase"),
    tooltip = "The log base your data is in",    
  BREAK=TRUE,
  order.by.radiobuttonItem = c("Natural Order", "Effect Size", "P-Value"), label = "Order Data By",
    tooltip = "Leave the significant species unsorted, or in decreasing effect size order, or increasing p-value order?",     
  logBase.radiobuttonItem = c(value=2, exp(1), 10), item.labels = c("2", "Natural Log", "10"), label = "Log Base"
)



