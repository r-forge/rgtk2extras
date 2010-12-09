  # Apply qvalue column-wise to matrix
QValue <- function(p.value.arr){
  p.value.df <- data.frame(na.omit(p.value.arr))
  sapply(p.value.df, function(x) qvalue(x)$q)
}

QValue.dialog <- list(
  p.value.arr.dataframeItem = "", label = "Array of P-values to Transform", tooltip = "Enter in the location of the p-values"
)