LoessNormalization.dialog = list(
  Data.dataframeItem="T_Data", label="Data Source",
    signal = c("default", "get.dataset.factors", "replicates"),
  replicates.choiceItem=NULL, label="Factor Denoting Replicates",
  reference.radiobuttonItem=c(1,value=2,3,4),
     item.labels = c("First Set in Replicate Category",
       "Create the Median Set in Replicate Category",
       "Dataset with Least Amount of Missing Data",
       "Mean of All Datasets"), label="Pick Reference",
  span.rangeItem=c(value=0.4, from=0, to=1, by=0.1), label="Span Value for LOESS",
  plotflag.trueFalseItem=FALSE, label="Save Diagnostic Plots (WARNING: Could be slow)",
   signal = c("default", "toggle.sensitive", "folder"),
  folder.fileItem="C:/temp/", label="Choose Folder", indent=10
)
                    
QuantileN.dialog <- list(
  label = "Quantile Normalization on Data",
  Data.dataframeItem = "", label = "Data Set",
  method.radiobuttonItem = c("median", "mean"), label = "Choose Method to Use", labels = c("Median", "Mean")
)

########################### MAD ##################################
MAD.dialog <- list(
  label = "Median Absolute Deviation",
  Data.dataframeItem = "", label = "Data Set",
    signal = c("default", "get.dataset.factors", "Factor"),        
  do.factors.trueFalseItem = FALSE, label = "Separate by Factors?",
    signal = c("default", "toggle.sensitive", "Factor"),
  Factor.choiceItem = NULL, label = "Choose Factor", indent=10, set.primary=FALSE,
  meanadjust.trueFalseItem = FALSE, label = "Adjust Dataset Means to Zero?"
)


CentralTendency.dialog = list(
  Data.dataframeItem="T_Data", label="Data Source",
  mode.radiobuttonItem = c(value="mean", "median"), label="Method", item.labels=c("Mean", "Median"),
  centerZero.trueFalseItem = FALSE, label="Recenter At Zero?"
)

LoadQRollupXLSX.dialog = list(
  filename.fileItem = "", label = "Choose File (*.xlsx) And First 3 Rows of Summary Deleted!!!",
  make.factors.trueFalseItem = FALSE, label = "Split Experiment Names Into Factors",
    signal = c("default", "toggle.sensitive", "experiment.name"),          
  experiment.name.regex.stringItem = "^Eco_45-(.*?)-?(M[0-9])_([0-9])(.)_(.*)$", label = "Optional Regex to Split Experiment Names", indent=10,
  factor.names.stringItem = 'c("tolerance", "mutant", "biol.rep", "tech.rep")', label = "Factor Names to Assign Split", indent=10
)

CorrScatterPlot.dialog <- list(  
  title = "Pairs Plot With Correlation", label = "Plot all pairs of data sets with correlation heatmap",
  Data.dataframeItem = "T_Data", label = "Choose Dataset",
  N.integerItem = 1000, label = "Choose Number Of Points To Sample",
  cMap.choiceItem = c("heat", "rainbow", "terrain", "topo", "cm"), label = "Colormap To Use"
)


CorrelationEllipses.dialog <- list(
  label = "Plot Correlation Ellipses",
  Data.dataframeItem="T_Data", label = "Data source",
    signal = c("default", "get.colnames", "Columns"),  
  Columns.variableSelectorItem=NULL, label="Choose Datasets"
)

PlotDataAgainstMean.dialog <- list(
  Data.dataframeItem = "T_Data", label = "Choose Dataset",
    signal = c("default", "get.colnames", "Columns"),  
  Columns.variableSelectorItem=substitute(colnames(T_Data)), label="Choose Datasets",  
  use.sampling.trueFalseItem=TRUE, label = "Thin Data With Sampling",
    signal = c("default", "toggle.sensitive", "sample.number"),        
    sample.number.integerItem=1000, label = "Number Of Sample Points", indent=10,
  DoPdf.trueFalseItem=FALSE, label = "Output To PDF",
  ShortenTitle.integerItem=c(value=10, from=1, to=26), label = "Shorten Title To N Letters"
)


SpectralCounting.dialog <- list(
  fileList.fileItem = "", multi=TRUE, label = "Select Files for Analysis",
  dataFolder.fileItem = "",type = "selectdir", label = "Analysis Folder", BREAK=TRUE,
  XCorr1Th.numericItem = 1.5, label = "Charge State 1 Xcorr",
  XCorr2Th.numericItem = 1.5, label = "Charge State 2 Xcorr",
  XCorr3Th.numericItem = 1.5, label = "Charge State 3 Xcorr",
  XCorrOTh.numericItem = 1.5, label = "Charge State 4+ Xcorr", BREAK=TRUE,
  XcRank.integerItem = 1, label = "Max Xcorr Rank",
  DelCn2Th.numericItem = 0.1, label = "DelCn2 Threshold",
  TrypStateNone.trueFalseItem = TRUE, label = "Tryptic State: None",
  TrypStatePartial.trueFalseItem = TRUE, label = "Tryptic State: Partial",
  TrypStateFull.trueFalseItem = TRUE, label = "Tryptic State: Full"
)       

save_tables.dialog <- list(
  title = "Export Selected Tables To File", label = "Select Tables",
  file_name.fileItem = "", extension = ".db*", label = "Choose File", filters = file.list.filters,
    signal = c("default", function(item, file_type) {
        fn <- get.value(item)
        file.type <- get.file.type(fn)
        set.value(file_type, file.type)
    }, "file_type"),      
  file_type.radiobuttonItem = names(file.list), label = "File Type",
  overwrite.trueFalseItem = TRUE, label = "Overwrite Existing Tables?", tooltip = "For Excel and Access, this will append the table with a new name",
  BREAK=TRUE,  
  dummy.listItem = substitute(get_all_tables()), suppress=T, show.arrows = FALSE, label = "All Tables",
  table_list.listItem = character(0), label = "Tables To Include",
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy")
)
                        
   
get_and_assign_obj <- function(f1, choice1, rb1){
  on.exit(w$destroy()) # block concurrent user calls with a modal window
  w <- gtkWindowNew("Loading...", show=F)
  w$setDecorated(FALSE)
  w$add(gtkLabelNew("Loading..."))
  w$setPosition(GtkWindowPosition["center"])
  #w$setTransientFor(getToolkitWidget(.win))    
  w$setModal(TRUE)        
  w$showAll()
  while(gtkEventsPending()) gtkMainIteration()

  if(!nchar(choice1)) return()
  obj.name <- make.names(sub("(.*)\\.[^.]*$", "\\1", basename(f1), perl=T))
  stopifnot(nchar(obj.name) > 0)
  tab <- get.table(f1, choice1, rb1)    
  assign(obj.name, tab, envir=.GlobalEnv)    
  return(obj.name)
}   
   
   
open.expression.dialog <- list(
  title = "Import Table, Step 1",
  f1.fileItem = "", extension = as.character(file.list.filters[1,2]), label = "Choose File", filters = file.list.filters,
    signal = c("default", function(item, rb1, choice1) {
        fn <- get.value(item)
        file.type <- get.file.type(fn)
        set.value(rb1, file.type)
        values <- get.table.names(fn, file.type)
        set.value(choice1, values)
    }, "rb1", "choice1"),      
  rb1.radiobuttonItem = names(file.list), label = "File Type", visible=FALSE,
  choice1.choiceItem = "", label = "Choose Table In File"
)

open.expression.2.dialog <- list(
  title = "Import Table, Step 2", label = "Settings For Importing A Table",
  rbAction.radiobuttonItem = c("Import Table As-Is", "Data Crosstab", "Row Metadata", "Column Metadata (Factors)"), label = "Table Type",
    signal = list("default", function(item, list2, id.col, dataset.cols, do.metadata, metadata.cols, dataset.name, row.metadata.name, do.factors, user.data){
      gvi <- get.value(item)      
      cn <- colnames(get(user.data$obj.name))        
      row.id.col <- rev(make.unique(c(cn, "Row Numbers")))[1]        
      sapply(c(list2, id.col, dataset.cols, do.metadata, metadata.cols, row.metadata.name, do.factors), gtkWidgetSetSensitive, FALSE)
      set.value(list2, cn)                
      set.value(dataset.cols, character(0))                
      set.value(metadata.cols, character(0))        
      set.value(id.col, c(row.id.col, cn))        
      if(gvi=="Data Crosstab"){    
        sapply(c(list2, id.col, dataset.cols, do.metadata, do.factors), gtkWidgetSetSensitive, TRUE) 
        if(get.value(do.metadata)) sapply(c(row.metadata.name, metadata.cols), gtkWidgetSetSensitive, TRUE) 
#          set.value(list2, cn)
      } else if (gvi%in%c("Row Metadata", "Column Metadata (Factors)")){
        sapply(c(list2, dataset.cols), gtkWidgetSetSensitive, TRUE)                      
#          cn <- colnames(get(obj.name))          
#         set.value(list2, cn)
#        set.value(id.col, c(row.id.col, cn))
      }
      set.value(dataset.name, list("Import Table As-Is"="T_Data", 
        "Data Crosstab"="T_Data", "Row Metadata"="T_Row_Metadata", 
        "Column Metadata (Factors)" = "T_Column_Metadata")[[gvi]])        
   }, "list2", "id.col", "dataset.cols", "do.metadata", "metadata.cols", "dataset.name", "row.metadata.name", "do.factors", user.data=list(object.name="Object name goes here")),
  #dataset.name.stringItem = obj.name, label = "Table Name",
    # Probably nicer to call this T_Data...
  BREAK=T,
  dataset.name.stringItem = "T_Data", label = "Table Name",
  list2.listItem = character(0), suppress=TRUE, label = "All Columns", show.arrows=FALSE,      
    
  BREAK=T,
  id.col.choiceItem = "", max.items=1, label = "Unique Row ID (Mass Tag or Probeset ID)",
    tooltip = "Choose the column that contains the unique identifier for your data rows. For example, the mass tag ID for proteomic data, or the probeset ID for genomic data.\n\n 'Row Numbers' will use the unique row ordering.",
  dataset.cols.listItem = character(0), label = "Columns To Include (Must Be Numeric)", show.arrows=TRUE,      
    tooltip = "Choose the numeric columns you want to put in the main data crosstab - usually correspond to experiment",    
    signal = c("add", "push.selection", "list2"),
    signal = c("subtract", "pop.selection", "list2"),       
  BREAK=T,
  do.factors.trueFalseItem = FALSE, label = "Guess Factor Table From Column Headers",  
    tooltip = "Create a table of factors linked to your data based on your data set names",
  do.metadata.trueFalseItem = FALSE, label = "Create Row Metadata Table",
    tooltip = "Create a linked table containing information about rows in your table - proteins, genes, pathways, etc",        
    signal = c("default", "toggle.sensitive", "row.metadata.name", "metadata.cols"),
  row.metadata.name.stringItem = "T_Row_Metadata", label = "Table Name", indent=10,      
    tooltip = "The name of the linked row metadata table",            
  metadata.cols.listItem = character(0), label = "Row Metadata Columns", indent=10,
    tooltip = "Choose the columns to include in the row metadata table - they don't have to be numeric",                
    signal = c("add", "push.selection", "list2"),
    signal = c("subtract", "pop.selection", "list2")
)
  
      
open.expression.wizard <- function(
   run.wizard=TRUE, 
   the.args = NULL,
   log.handler = NULL,
   envir = environment(),
   .win = NULL # main window its called from
)
{
  if(run.wizard){  # run the wizard


  d1 <- run.dialog(list, dlg.list=open.expression.dialog, auto.assign=F)$retval
  f1 <- d1$f1; choice1 <- d1$choice1; rb1 <- d1$rb1
  
  if(is.null(d1)) return()
  obj.name <- get_and_assign_obj(f1, choice1, rb1)
  
  open.expression.2.dialog[[5]]$user.data <- list(obj.name=obj.name)

  
  rv <- run.dialog(list, dlg.list=open.expression.2.dialog, auto.assign=F, do.logging=F)
  the.args <- rv$retval
    # we want to log this command so the function can work with it with run.wizard set to FALSE
  rbAction <- the.args$rbAction
  id.col <- the.args$id.col
  dataset.cols <- the.args$dataset.cols
  do.metadata <- the.args$do.metadata
  dataset.name <- the.args$dataset.name
  row.metadata.name <- the.args$row.metadata.name
  metadata.cols <- the.args$metadata.cols 
  do.factors <- the.args$do.factors
  # Create the command string
  if(!is.null(log.handler) && is.function(log.handler)) {
    cmd.args <- list(f1=f1, choice1=choice1, rb1=rb1, rbAction=rbAction, dataset.name=dataset.name, 
      id.col=id.col, dataset.cols=dataset.cols, do.metadata=do.metadata, row.metadata.name=row.metadata.name, 
      metadata.cols=metadata.cols, do.factors=do.factors)
    y1 <- lapply(cmd.args, function(x) paste(deparse(x), collapse=""))
    y2 <- paste("list(", paste(paste(names(y1), unlist(y1), sep="="), collapse=", "), ")", sep = "")
    cmd.string = paste("\nopen.expression.wizard(run.wizard=FALSE, the.args=", y2, ")\n", sep="")      
    log.handler(cmd.string)  
  }    
    
  } else {  # don't run the wizard, just put the args in
  # we need to get all the arguments into here via the args list
    attach(the.args, warn.conflicts=FALSE)   
    stopifnot(!any(sapply(list(f1, choice1, rb1, dataset.name, id.col, rbAction, dataset.cols, do.metadata, row.metadata.name, metadata.cols, do.factors), is.null)))
    obj.name <- get_and_assign_obj(f1, choice1, rb1)
    #return()
  
  } # end if run.wizard  

#return()
  if(!is.null(the.args)){
  get.choice <- get(obj.name)
  cn <- colnames(get.choice)
  
  # We've selected an undefined column; set it to the row names
  if(!id.col %in% cn){    
    id.col <- make.names(id.col)
    nrxx <- nrow(get.choice)
    if(nrxx) urxx <- 1:nrow(get.choice)
    else urxx <- integer(0)    
    get.choice <- data.frame(urxx, get.choice, check.names=FALSE, as.is=TRUE)
    colnames(get.choice)[1] <- id.col
  }  
          
  if(rbAction == "Data Crosstab") { 
     data_columns <- setdiff(dataset.cols, c(id.col, metadata.cols))
     dim_string <- function(x) paste(paste(dim(x), c("rows", "columns"), sep = " "), collapse=" x ")
     
     summary_message <- paste("Size of input data set", obj.name, ":\n    ",  dim_string(get.choice), "\n\n" )
     
       # Check the user's loaded only numeric columns
     turn_to_matrix_flag <- TRUE
     if(length(data_columns)){
       data.classes <- rep(NA, length(data_columns))
       for(jj in 1:length(data_columns)){
         data.classes[jj] <- paste(class(get.choice[,data_columns[jj]]), collapse = ", ")
       }
       data_non_numeric_idx <- !data.classes%in%c("integer", "numeric")
       if(any(data_non_numeric_idx)){
         summary_message <- 
           paste(summary_message, "Possible error in selecting data columns. The following are NOT numeric:\n    ",
             paste(data_columns[data_non_numeric_idx], collapse = ", "), "\n\n")
          turn_to_matrix_flag <- FALSE
       }         
     }
     T_Data <- get.choice[,data_columns,drop=F]
     if(turn_to_matrix_flag)
       T_Data <- data.matrix(T_Data)
          
     if(length(dataset.name)>0 && nchar(dataset.name) > 0)
        dataset.name <- make.names(dataset.name)
     else
        dataset.name <- "T_Data"     
          
     row.id <- get.choice[,id.col]
     row.id.dupes <- duplicated(row.id)
     data.dupes <- duplicated(T_Data)
     sum.nonidentical.dupes <- sum(!data.dupes[row.id.dupes])

     T_Data <- T_Data[!row.id.dupes,,drop=F]
     rownames(T_Data) <- row.id[!row.id.dupes]
         
     if(sum.nonidentical.dupes)
       summary_message <- paste(summary_message, "Possible error in selecting primary row ID!\n    Removed", 
         sum.nonidentical.dupes, paste("non-identical data row(s) to give ONE data row for each unique \"", id.col, "\"", sep=""), "\n\n")
     if(sum(row.id.dupes)) summary_message <- paste(summary_message, "Data crosstab: Removed ", sum(row.id.dupes), " row(s) with identical \"", id.col, "\" key from ", dataset.name, "\n\n", sep="") 
     summary_message <- paste(summary_message, "Final size of data crosstab",  dataset.name, ":\n    ", dim_string(T_Data), "\n\n")               
         
     if(do.metadata){
       metadata.cols <- setdiff(metadata.cols, id.col)      
       metadata <- get.choice[, c(id.col, metadata.cols), drop=F]
       duplicated_metadata <- duplicated(metadata)
              
       if(length(row.metadata.name)>0 && nchar(row.metadata.name) > 0)
          row.metadata.name <- make.names(row.metadata.name)
       else 
          row.metadata.name <- "T_Row_Metadata" 
                                                                        
       metadata <- metadata[!duplicated_metadata, , drop=F]
       char.idx <- which(apply(metadata, 2, function(x) !is.numeric(x)))
       for(jj in char.idx)
         metadata[,jj] <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", metadata[,jj], perl=TRUE)
       T_Row_Metadata <- metadata       
       colnames(T_Row_Metadata) <- c(id.col, metadata.cols)   
                        
       if(sum(duplicated_metadata)) summary_message <- paste(summary_message, "Row metadata table: removed", sum(duplicated_metadata), "duplicated row(s) from", row.metadata.name, "\n\n")
       summary_message <- paste(summary_message, "Final size of row metadata table", row.metadata.name, ":\n    ", dim_string(metadata), "\n\n")                         

       assign(row.metadata.name, T_Row_Metadata, envir=.GlobalEnv)       
       
       T_Data <- set.row.metadata.table(T_Data, row.metadata.name)
       T_Data <- set.row.metadata.key(T_Data, id.col)
     }
     
     if(do.factors){
       column.metadata.name <- "T_Column_Metadata"
       choice <- TRUE
       if(object.exists(column.metadata.name))
         choice <- gconfirm(paste("Warning!", column.metadata.name, "exists and will be replaced. OK?"))
        if(choice){
          metadata_rv <- Create.Metadata.Table(T_Data, 2, table.name = column.metadata.name, n.columns = 1, guess.factors=TRUE, boundary=c("\\s", "\\.", "\\-", "_"), letNumBd=TRUE)
          T_Data <- metadata_rv$dataset
          assign(column.metadata.name, metadata_rv$metadata, envir=.GlobalEnv)    
        }
     }

     #cat(summary_message)        
     quick_message(summary_message, caption = "Data Loading Information")
     assign(dataset.name, T_Data, envir=.GlobalEnv)       
     
     if (obj.name!=dataset.name) rm(list=obj.name, envir=.GlobalEnv)
    # open expression  
  } else if (rbAction == "Row Metadata"){
     T_Row_Metadata <- get.choice[,dataset.cols,drop=F]
     T_Row_Metadata <- T_Row_Metadata[!duplicated(T_Row_Metadata),,drop=F]       
     T_Row_Metadata <- array(sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", as.matrix(T_Row_Metadata), perl=TRUE), dim(T_Row_Metadata))
     
     colnames(T_Row_Metadata) <- dataset.cols
     
     if(length(dataset.name)>0 && nchar(dataset.name) > 0)
        dataset.name <- make.names(dataset.name)
     else
        dataset.name <- "T_Row_Metadata"
        
     assign(dataset.name, T_Row_Metadata, envir=.GlobalEnv)       

     rd <- run.dialog(Link_To_Metadata, envir=envir, auto.assign=F)

     if (obj.name!=dataset.name) rm(list=obj.name, envir=.GlobalEnv)      
  } else if (rbAction == "Column Metadata (Factors)"){
     
     T_Column_Metadata <- get.choice[,dataset.cols,drop=F]
     T_Column_Metadata <- T_Column_Metadata[!duplicated(T_Column_Metadata),,drop=F]         
     for(jj in 1:dim(T_Column_Metadata)[2]){
       theCol <- T_Column_Metadata[,jj]
       if(is.character(theCol)) {
         theCol <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", theCol, perl=TRUE)
         theCol <- factor(theCol, levels = unique(theCol))
         T_Column_Metadata[,jj] <- theCol
       } 
       #T_Column_Metadata[,jj] <- array(sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", as.matrix(T_Column_Metadata), perl=TRUE), dim(T_Column_Metadata))
     }
     colnames(T_Column_Metadata) <- dataset.cols  
     T_Column_Metadata <- as.data.frame(T_Column_Metadata, stringsAsFactors=F)
     
     if(length(dataset.name)>0 && nchar(dataset.name) > 0)
        dataset.name <- make.names(dataset.name)
     else
        dataset.name <- "T_Column_Metadata"
        
     assign(dataset.name, T_Column_Metadata, envir=.GlobalEnv) 

     tryCatch({
      rd <- run.dialog(Link_To_Metadata, dlg.list = Link_To_Metadata.dialog, auto.assign=F)
     }, error = function(e) print(e))
           
     if (obj.name!=dataset.name) rm(list=obj.name, envir=.GlobalEnv)    
  } else {
    # import as-is
  }
  } else { #is.null retval
    rm(list=obj.name, envir=.GlobalEnv) 
  }
}
  
  
