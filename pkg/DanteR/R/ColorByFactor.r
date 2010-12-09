# We want to consistently assign colors to our factors, even if the ordering or subset changes
# Get vector of colors based on factor information
# Gives color vector of colors and legend vector
# Names of color are column names and names of legend are factor levels
ColorByFactor <- function(x, color.key, color = "wheat2"){
  box_color <- rep(color,dim(x)[2])
  names(box_color) <- colnames(x)

  theFactors <- get.column.metadata.table(x)
  factor.key <- get.column.metadata.key(x)
  if(is.null(theFactors)
    || is.null(factor.key)
    || !factor.key%in%colnames(theFactors)
    || !color.key%in%colnames(theFactors)) return(list(color=box_color, legend=NULL))

  if (!is.null(color.key))
  {
       # assign colors before removing to keep them the same
     fcol <- theFactors[,color.key]
     fkey <- theFactors[,factor.key]
     if(is.factor(fcol)) fcol <- levels(fcol)[as.numeric(fcol)]
     if(is.factor(fkey)) fkey <- levels(fkey)[as.numeric(fkey)]
     uF <- unique(fcol)
     colStep <- length(uF)
     colorRange <- unique(hsv(h = seq(0,1, length = colStep+1), s=1, v=1))
     names(colorRange) <- uF

     cidx <- match(colnames(x), fkey)
     match.idx <- match(fcol[match(fcol[cidx], fcol)], uF)
     box_color <- colorRange[match.idx]
     names(box_color) <- colnames(x)
  }
  return(list(color = box_color, legend = colorRange))
}

  # Orders data frame x by the column metadata key order.key
  # ... passed to order()
OrderByFactor <- function(x, order.key, ...){
  def_order <- 1:ncol(x)

  theFactors = get.column.metadata.table(x)
  factor.key <- get.column.metadata.key(x)
  if(is.null(theFactors)
    || is.null(factor.key)
    || !factor.key%in%colnames(theFactors)
    || !order.key%in%colnames(theFactors)) return(x)

  if (!is.null(order.key))
  {
       # assign colors before removing to keep them the same
     ford <- theFactors[,order.key]
     fkey <- theFactors[,factor.key]
     if(is.factor(ford)) ford <- levels(ford)[as.numeric(ford)]
     if(is.factor(fkey)) fkey <- levels(fkey)[as.numeric(fkey)]
     uF <- unique(ford)
     cidx <- match(colnames(x), fkey)
     match_idx <- match(ford[match(ford[cidx], ford)], uF)
     x_new <- x[,order(match_idx, ...),drop=FALSE]
     attr(x_new, "Row_Metadata") <- attr(x, "Row_Metadata")
     attr(x_new, "Column_Metadata") <- attr(x, "Column_Metadata")
  }
  return(x_new)
}
