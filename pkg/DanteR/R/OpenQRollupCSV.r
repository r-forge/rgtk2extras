# Open QRollup output. Will drop duplicated MTID's from Eset.
open.MS.csv <- function(file, has.row.names, mtid.col, protein.col, dataset.cols) {
  if(nchar(file)){
    row.names.pos <- NULL
    if(has.row.names) row.names.pos <- 1
    use.choice <- function(get.choice){
       ProtInfo <- get.choice[,c(mtid.col, protein.col),drop=F]
       colnames(ProtInfo) <- c("Row_ID", "ProteinID")
       ProtInfo <<- ProtInfo

       if(length(dataset.cols)) {
         Eset <- get.choice[,setdiff(dataset.cols, c(mtid.col, protein.col)),drop=F]
         Eset <- Eset[!duplicated(ProtInfo[,1]),,drop=F]
         rownames(Eset) <- ProtInfo[!duplicated(ProtInfo[,1]),1]
         Eset <<- Eset
       }
    }
    tryCatch({
      get.choice <- read.csv(file, header=T, row.names=row.names.pos, stringsAsFactors=FALSE)
        use.choice(get.choice)
      }, error = function(e) { # try not reading row names
        get.choice <- read.csv(file, header=T, stringsAsFactors=FALSE)
        use.choice(get.choice)
      })
  }
  invisible(NULL)
}

open.MS.csv.dialog <- list(
  label = "Select the file you wish to load, then protein info table and dataset table",
  file.fileItem="", label = "Data Source", tooltip = "CSV file to open",
  has.row.names.trueFalseItem = FALSE, label = "Use row names?",
  mtid.col.fileColumnChoiceItem="iris", label = "Mass Tag ID Column",  tooltip = "Unique peptide identifier",
  protein.col.fileColumnChoiceItem="iris", label = "Protein ID Column",  tooltip = "Protein column",  
  dataset.cols.fileColumnSelectorItem="iris", label = "Data Set Columns", tooltip = "Columns with data set"  
)

