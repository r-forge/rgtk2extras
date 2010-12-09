update_plotxy_signal <- c("default", function(dummkopf, x, rowsOrCols, dummy, fields){
       obj.name <- get.value(x)
       if(object.exists(obj.name)){
         obj <- safe.eval(obj.name)
         dim.chosen <- ifelse(get.value(rowsOrCols)=="rows", 1, 2)
         vals <- dimnames(obj)[[dim.chosen]]
         if(is.null(vals)) vals <- 1:dim(obj)[dim.chosen]
         set.value(dummy, vals)         
       } else {
         set.value(dummy, NULL)
       }
       set.value(fields, NULL)
    }, "x", "rowsOrCols", "dummy", "fields")

DataPlot.dialog <- list(
  label = "Plot Rows or Columns from Data Frame or Matrix",
  x.dataframeItem="mtcars", label="Data Source",
    signal = update_plotxy_signal,
  rowsOrCols.radiobuttonItem = c("rows", value="cols"), 
    item.labels = c("Rows", "Columns"), label = "Plot",               
    signal = update_plotxy_signal,
  dummy.listItem = NULL, label = "Available Fields", suppress=T, show.arrows=F,
  fields.listItem = NULL, label = "Selected Fields",
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy"),
  BREAK=TRUE,
  transpose.trueFalseItem = FALSE, label = "Transpose Data",
  plot.type.radiobuttonItem = c("plot", "matplot", "pairs"), 
    item.labels = c("Scatter", "Matrix", "Pairs"), 
    label = "Plot Type",
  signal = c("default", function(plot.type, firstisX){
    firstisX$setSensitive(get.value(plot.type) == "matplot") 
  }, "firstisX"),
  firstisX.trueFalseItem = FALSE, label = "Plot Vs. First Item", indent=10,
    tooltip = "For matrix plots, this will set the X-axis to the first selection",
  type.choiceItem = c("p", "l", "s", "n"), item.labels = c("Point", "Line", "Step", "None"), label = "Plot Type",
  pch.choiceItem = c(-1, 18:0, 46), label = "Point Type",
  log.choiceItem = c("", "x", "y", "xy"), item.labels = c("None", "X", "Y", "X and Y"), label = "Log Scale Axes",
  do.labels.trueFalseItem = FALSE, label = "Data Labels",
  do.xlim.trueFalseItem = FALSE,  label = "Set x-axis?", tooltip = "Set the horizontal limit of all histograms",
    signal = c("default", "toggle.sensitive", "xmin", "xmax"),  
    xmin.numericItem = 0, label = "Minimum x", indent=10,
    xmax.numericItem = 1, label = "Maximum x", indent=10,
  do.ylim.trueFalseItem = FALSE,  label = "Set y-axis?", tooltip = "Set the vertical limit of all histograms",
    signal = c("default", "toggle.sensitive", "ymin", "ymax"),  
    ymin.numericItem = 0, label = "Minimum y", indent=10,
    ymax.numericItem = 1, label = "Maximum y", indent=10      
)



DataPlot <- function(x, rowsOrCols, fields, plot.type, type, pch, log, transpose=FALSE, firstisX = FALSE, 
                       do.xlim = F,
                      xmin=0,
                      xmax = 1,
                      do.ylim = F,
                      ymin=0,
                      ymax=1,...){
  xlim <- c(xmin, xmax)
  ylim <- c(ymin, ymax)
  #aa <- alist(x,,,drop=F)
  #aa[[ifelse(rowsOrCols=="rows", 2, 3)]] <- fields
  #x <- do.call(get("["), aa)
  tryCatch({
    if(rowsOrCols == "rows") 
      x <- x[fields,,drop=F]
    else 
      x <- x[,fields,drop=F]
    }, error = function(e){
      fields <- as.numeric(fields)
      if(rowsOrCols == "rows") 
        x <- x[fields,,drop=F]
      else 
        x <- x[,fields,drop=F]
    })
  if((transpose && rowsOrCols == "cols") ||
     (!transpose && rowsOrCols == "rows")) {
    x <- t(x)
    fields <- colnames(x)
  }


  if(plot.type == "matplot"){
    xlab <- "Order"
    ylab <- "Values"
    if(!firstisX && dim(x)[2] > 1) x <- cbind(Order=1:dim(x)[1], x)
    y <- data.matrix(x[,-1])
    colnames(y) <- colnames(x)[-1]
    x <- data.matrix(x[,1,drop=F])   

    ylab = ifelse(length(colnames(y)) == 1, colnames(y), "Value")

    if(!do.xlim) xlim <- range(x[is.finite(x)])
    if(!do.ylim) ylim <- range(y[is.finite(y)])

    if(pch < 0) pch <- 1:ncol(y)
    matplot(x, y, type=type, pch=pch, xlab = colnames(x), ylab = ylab, xlim=xlim, ylim=ylim)
    if(length(colnames(y))) 
      legend(xlim[1], ylim[2], colnames(y), pch=pch, col = 1:6)
  } else {  
    if(!do.xlim) xlim <- NULL
    if(!do.ylim) ylim <- NULL
    if(pch < 0) pch <- NULL
    if(plot.type == "pairs"){
      pairs(x, pch=pch, xlim=xlim, ylim=ylim)
    } 
    if(plot.type == "plot"){
      plot(x, pch=pch, xlim=xlim, ylim=ylim, log=log)
    }     
  }
  return(recordPlot())
}
