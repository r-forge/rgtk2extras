
  # Specifically for statistical results having data as estimate, p-value columns.
PAdj <- function(data, method){
  epsilon = 1E-6
  if(!method%in%c(p.adjust.methods, "qvalue")) stop("Method not recognized")

  if(!class(data)%in%c("data.frame", "matrix", "array")) stop("Function requires tabular data")  
  if(!(!is.null(dim(data)) && length(dim(data)) == 2 && dim(data)[2] > 0 && dim(data)[2]%%2 == 0))
    stop("Function requires data table with even # of columns")  

  p.idx <- seq(2, ncol(data), by=2)
  p.matrix <- data.matrix(data[,p.idx,drop=F])
  if(any((p.matrix < 0-epsilon) | (1+epsilon < p.matrix), na.rm=T)) 
    stop("P-values outside the range [0, 1] detected. Please check the data input.")
  
  if(method == "qvalue") {
    xx <- na.omit(as.vector(p.matrix))
    qvxx <- qvalue(xx)$q
    if(length(na.action(qvxx))) # Fix a bug where it fails if there are no NA values
      p.matrix[-na.action(qvxx)] <- qvxx
    else 
      p.matrix <- qvxx      
  } else {
    p.matrix <- p.adjust(p.matrix, method)
  }
  data[,p.idx] <- p.matrix

  attr(p.matrix, "Row_Metadata") <- attr(data, "Row_Metadata")
  return(data)
}

PAdj.dialog <- list(
  title = "Multiple Comparisons Adjustment",
  label = "Apply a p-value adjustment to\na table of effects and p-values from a \nstatistical test",
  data.dataframeItem = "", label = "Table of Effects and P-values",
  method.choiceItem = c("holm", "hochberg", "hommel", "bonferroni", value = "BH",  "BY", "none"), 
    item.labels = c("Holm", "Hochberg", "Hommel", "Bonferroni", "Benjamini & Hochberg", "Benjamini & Yekutieli", "none"), 
    label = "Choose Method"
)



