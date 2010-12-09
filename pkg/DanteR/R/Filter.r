
  # Filter a data set according to various criteria
  # Can select list of rows, columns, an expression,
  # row metadata or column metadata
Filter <- function(
  data,
  typ = "row.metadata",
  filter.type = "match",
  metadata.field = NULL,
  match.items = NULL,
  ...
)
{
  Data <- data
  old_attr <- attributes(Data)
  
  if(typ == "row"){
    Data <- Data[match.items,,drop=F]
    
  } else if(typ == "column") {
    Data <- Data[,match.items,drop=F]
    
  } else if (typ%in%c("row.metadata", "column.metadata")){
    if (typ == "row.metadata"){
      metadata.table <- get.row.metadata.table(Data)
      metadata.key <- get.row.metadata.key(Data)
    } else if (typ == "column.metadata"){
      metadata.table <- get.column.metadata.table(Data)
      metadata.key <- get.column.metadata.key(Data)
    }
    
    stopifnot(!is.null(metadata.key) && metadata.key%in%colnames(metadata.table))
    stopifnot(metadata.field%in%colnames(metadata.table))

    the.matches <- na.omit(unique(unlist(lapply(match.items,
      function(x) as.character(metadata.table[metadata.table[,metadata.field] == x, metadata.key])))))

    if (typ == "row.metadata"){
     Data <- Data[the.matches,,drop=F]
    } else if (typ == "column.metadata"){
     Data <- Data[,the.matches,drop=F]
    }

  } else {
    stop("Type not recognized")
  }
  
   # Copy the metadata over
  Data <- copy.metadata(Data, old_attr)
  return(Data)
}

Filter.dialog <- list(
  label = "Apply Filter To Data Table",
  typ.radiobuttonItem = c("column", value="column.metadata", "row", "row.metadata"), item.labels = c("Columns", "Column Metadata", "Rows", "Row Metadata"), label = "Filter Dataset Using...",
    signal = c("default", function(item, data, metadata.field, match.items, dummy){
      metadata.field$setSensitive(FALSE)
      set.value(match.items, NULL)
      dataset.name <- get.value(data)
      if(object.exists(dataset.name)){
        dataset <- safe.eval(dataset.name)
        typ.value <- get.value(item)
        metadata.table <- NULL

        new.items <- character(0)
        if(typ.value == "row.metadata"){
          metadata.field$setSensitive(TRUE)
          metadata.table <- get.row.metadata.table(dataset)
          metadata.key <- get.row.metadata.key(dataset)
          #new.items <- rownames(dataset)
        } else if(typ.value == "column.metadata"){
          metadata.field$setSensitive(TRUE)
          metadata.table <- get.column.metadata.table(dataset)
          metadata.key <- get.column.metadata.key(dataset)
          #new.items <- colnames(dataset)
        }
        if(typ.value %in%c("row.metadata", "column.metadata")){
          set.value(metadata.field, colnames(metadata.table)[!colnames(metadata.table)%in%metadata.key])
        } else {
          set.value(metadata.field, NULL)
        }
        if(typ.value == "row")
          new.items <- rownames(dataset)
        if(typ.value == "column")
          new.items <- colnames(dataset)
        set.value(dummy, new.items)
        RGtk2Extras:::signal(metadata.field)
        set.value(match.items, character(0))
      }

    }, "data", "metadata.field", "match.items", "dummy"),
    
  #filter.type.radiobuttonItem = c("Match A Subset", "Satisfy Expression"), label = "Type of Filtering", visible=FALSE,

  BREAK=TRUE,
  
  data.dataframeItem = "T_Data", label = "Select Data",
    signal = c("default", function(item, typ) RGtk2Extras:::signal(typ), "typ"),
  
  metadata.field.choiceItem= NULL, label = "Metadata Field",
    signal = c("default", function(item, data, typ, dummy, match.items){
      dataset <- safe.eval(get.value(data))
      typ.value <- get.value(typ)
      metadata.table <- NULL
      md.field <- get.value(item)
      if(typ.value == "row.metadata"){
        metadata.table <- get.row.metadata.table(dataset)
        metadata.key <- get.row.metadata.key(dataset)        
        data.id <- rownames(dataset)
        unique.candidates <- unique(metadata.table[metadata.table[,metadata.key]%in%data.id,md.field])            
      } else if(typ.value == "column.metadata"){
        metadata.table <- get.column.metadata.table(dataset)
        metadata.key <- get.column.metadata.key(dataset)        
        data.id <- colnames(dataset)        
        unique.candidates <- unique(metadata.table[metadata.table[,metadata.key]%in%data.id,md.field])            
      }  else {
        unique.candidates <- unique(metadata.table[,md.field])
      }   

      if(!is.null(metadata.table)){
        set.value(dummy, unique.candidates)
        set.value(match.items, character(0))
      } else {
      }
    }, "data", "typ", "dummy", "match.items"),

  dummy.listItem = character(0), label = "Exclude Items", suppress=TRUE, show.arrows=FALSE,  
  match.items.listItem =character(0), label="Include Items", 
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy"),
  
  expr.value.stringItem = "", label = "Expression", visible=FALSE
)

aggregate_functions_2 = list(
  FIRSTITEM = function(x) x[1], 
  LASTITEM = function(x) rev(x)[1],
  COUNT = length, 
  COUNTUNIQUE = function(x) length(unique(x)),
  COUNTDUPLICATES = function(x) sum(duplicated(x), na.rm=TRUE),
  COUNTBLANK = function(x) sum(is.na(x)),  
  COUNTNOTBLANK = function(x) sum(!is.na(x)),
  COUNTTRUE = function(x) sum(as.logical(x), na.rm=TRUE),
  COUNTFALSE = function(x) sum(!as.logical(x), na.rm=TRUE),
  ALLTRUE = function(x) all(as.logical(x), na.rm=TRUE), 
  ANYTRUE = function(x) any(as.logical(x), na.rm=TRUE),  
  SUM = function(x) sum(x, na.rm=TRUE),   
  MIN = function(x) min(x, na.rm=TRUE), 
  MAX = function(x) max(x, na.rm=TRUE), 
  AVG = function(x) mean(x, na.rm=TRUE), 
  MEDIAN = function(x) median(x, na.rm=TRUE), 
  STDEV = function(x) sd(x, na.rm=TRUE)  
)


Aggregate.dialog <- list(
  label = "Aggregate Data Table. Select row/column metadata and collapse within that factor.\n",  
  show.progress=TRUE,  
  x.dataframeItem = "T_Data", label = "Select Data",  
    signal = c("default", function(item, typ) RGtk2Extras:::signal(typ), "typ"),  
  typ.radiobuttonItem = c("column.metadata", value="row.metadata"), item.labels = c("Column Metadata", "Row Metadata"), label = "Using...",
    signal = c("default", function(item, x, metadata.field){  
      x <- get.value(x)
      typ.value <- get.value(item)
      if(object.exists(x)){
        x <- safe.eval(x)    
        if(typ.value == "row.metadata"){
          metadata.field$setSensitive(TRUE)
          metadata.table <- get.row.metadata.table(x)
          metadata.key <- get.row.metadata.key(x)
          #new.items <- rownames(dataset)
        } else if(typ.value == "column.metadata"){
          metadata.field$setSensitive(TRUE)
          metadata.table <- get.column.metadata.table(x)
          metadata.key <- get.column.metadata.key(x)
          #new.items <- colnames(dataset)
        }
        if(typ.value %in%c("row.metadata", "column.metadata")){
          set.value(metadata.field, colnames(metadata.table)[!colnames(metadata.table)%in%metadata.key])
        } else {
          set.value(metadata.field, NULL)
        }
      }
    }, "x", "metadata.field"),
  metadata.field.choiceItem = NULL, label = "Choose Field to Aggregate Over",
  fun.choiceItem = names(aggregate_functions_2), label = "Function"
)

Aggregate <- function(x, typ, metadata.field, fun="FIRSTITEM", do.totals=FALSE, progresslabel=NULL){
  cat1 <- function(txt) if(!is.null(progresslabel)) progresslabel$setText(txt)
  stopifnot(fun%in%names(aggregate_functions_2))
  x.attr <- attributes(x)
  if(typ == "row.metadata"){
    metadata.table <- get.row.metadata.table(x)
    metadata.key <- get.row.metadata.key(x)
  } else if(typ == "column.metadata"){
    metadata.table <- get.column.metadata.table(x)
    metadata.key <- get.column.metadata.key(x)
    x <- t(x)
  } else {
    stop("Invalid type")
  }
      
  PI <- metadata.table[match(rownames(x), metadata.table[,metadata.key]), c(metadata.key, metadata.field),drop=F]  
  zz <- as.character(PI[,2])
  
  DAT <- data.frame(PI, Total=1, x, check.names=F,stringsAsFactors =F)  

  if(any(is.na(PI[,2]))) # remove any NA elements in the metadata
    DAT <- DAT[!is.na(PI[,2]),,drop=F]

  id.vars <- colnames(PI)  
  cat1("Melting data...\n")
  cc.melt <- melt(DAT, id.vars = id.vars, variable_name = "Job")  
  cat1("Data melt done. Casting...\n")
  rvt <- cast(cc.melt, as.formula(paste(id.vars[2], "~Job")), function(x) aggregate_functions_2[[fun]](x))
  rownames(rvt) <- rvt[,1]
  totals <- rvt[,"Total",drop=F]  
  rvt <- rvt[,-(1:2),drop=F]
  if(typ == "column.metadata")
    rvt <- t(data.frame(rvt, check.names=F))
  else 
    rvt <- data.frame(rvt, check.names=F)
    
  attr(rvt, "Row_Metadata") <- x.attr[["Row_Metadata"]]
  attr(rvt, "Column_Metadata") <- x.attr[["Column_Metadata"]]
  if(typ == "column.metadata"){
    attr(rvt, "Column_Metadata")[["key"]] <- metadata.field
    attr(totals, "Row_Metadata") <- c(key = metadata.field, table = attr(rvt, "Column_Metadata")[["table"]])
  } else {
    attr(rvt, "Row_Metadata")[["key"]] <- metadata.field
    attr(totals, "Row_Metadata") <- c(key = metadata.field, table = attr(rvt, "Row_Metadata")[["table"]])
  } 
  if(do.totals) return(list(Aggregate = rvt, Totals=totals))
  return(rvt)
}

Smoosh.dialog <- list(
  title = "Bring Data Into Single Table", label = "Places a number of datasets alongside one another in the same table, regardless of shape.\nUseful for generating summary tables.",
  output_name.stringItem = "T_Output", label = "Output Name", 
  dummy.listItem = substitute(get_all_tables()), suppress=T, show.arrows = FALSE, label = "All Tables",
  table_list.listItem = character(0), label = "Tables To Include",
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy")
)

Smoosh <- function(output_name, table_list){
  assign(output_name, do.call(ragged.cbind, lapply(table_list, safe.eval)), envir=.GlobalEnv)
}
