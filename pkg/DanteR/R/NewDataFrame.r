NewDataFrame <- function(nr, nc, klass){
  stopifnot(nr >= 0)
  stopifnot(nc >= 0)
  stopifnot(is.atomic(klass))
  do.factor <- F
  if(klass == "factor") {
    do.factor <- T
    klass <- "character"
  }
  rv <- matrix(vector(klass, nr*nc), c(nr, nc))
  rv[,] <- NA
  data.frame(rv, stringsAsFactors=do.factor)
}

NewDataFrame.dialog <- list(
  title = "Make New Data Frame",
  nr.integerItem = 5, label = "Number of Rows",
  nc.integerItem = 5, label = "Number of Columns",
  klass.radiobuttonItem = c("logical", "integer", "numeric", value="factor", "character"), label = "Data Type"
)