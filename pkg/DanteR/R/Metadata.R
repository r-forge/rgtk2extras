  # Get and set metadata functions
get.metadata.obj <- function(Data, typ, obj){
  stopifnot(obj%in%c("key", "table"))
  if(!is.null(attr(Data, typ)) && obj%in%names(attr(Data, typ)) && nchar(attr(Data, typ)[obj]) > 0){
    obj.name <- attr(Data, typ)[obj]
    return(obj.name)
  }
  return(NULL)
}

set.metadata.obj <- function(Data, typ, obj, x){
  if(is.null(attr(Data, typ)))
    attr(Data, typ) <- c()
  if(!length(x)) x <- ""
  attr(Data, typ)[obj] <- x
  return(Data)
}

get.row.metadata.key        <-  function(Data) get.metadata.obj(Data, "Row_Metadata", "key")
get.column.metadata.key     <-  function(Data) get.metadata.obj(Data, "Column_Metadata", "key")
get.row.metadata.table.name      <-  function(Data) get.metadata.obj(Data, "Row_Metadata", "table")
get.column.metadata.table.name   <-  function(Data) get.metadata.obj(Data,"Column_Metadata", "table")
get.row.metadata.table      <-  function(Data) {
  tn <- get.metadata.obj(Data, "Row_Metadata", "table")
  if(!is.null(tn) && is.character(tn) && nchar(tn) > 0 ** exists(tn)) return(get(tn))
  return(NULL)
}
get.column.metadata.table   <-  function(Data) {
  tn <- get.metadata.obj(Data,"Column_Metadata", "table")
  if(!is.null(tn) && is.character(tn) && nchar(tn) > 0 && exists(tn)) return(get(tn))
  return(NULL)
}


set.row.metadata.key     <-  function(Data, x) set.metadata.obj(Data, "Row_Metadata", "key", x)
set.column.metadata.key  <-  function(Data, x) set.metadata.obj(Data, "Column_Metadata", "key", x)
set.row.metadata.table   <-  function(Data, x) set.metadata.obj(Data, "Row_Metadata", "table", x)
set.column.metadata.table<-  function(Data, x) set.metadata.obj(Data,"Column_Metadata", "table", x)

row.metadata.colnames      <-  function(Data) colnames(get.metadata.obj(Data, "Row_Metadata", "table"))
column.metadata.colnames   <-  function(Data) colnames(get.metadata.obj(Data,"Column_Metadata", "table"))
row.metadata.rownames      <-  function(Data) rownames(get.metadata.obj(Data, "Row_Metadata", "table"))
column.metadata.rownames   <-  function(Data) rownames(get.metadata.obj(Data,"Column_Metadata", "table"))

copy.metadata <- function(Data, old_attr) {
  for(an in c("Row_Metadata", "Column_Metadata")) if(an %in% names(old_attr)) attr(Data, an) <- old_attr[[an]]
  return(Data)
}

  # Utility functions to get ProtInfo and Factors tables
get.ProtInfo <- function(dataset){
  row.metadata.table <- get.row.metadata.table(dataset)
  row.metadata.key <- get.row.metadata.key(dataset)

  if(is.null(row.metadata.table) || is.null(row.metadata.key))
    quick_message("Table has no row metadata (factors) defined.\n Use Metadata -> Define Row Metadata to do this.")
  if(!row.metadata.key%in%colnames(row.metadata.table)) {
    quick_message("Row Metadata is incorrectly linked.\n\nUse Metadata->Row Metadata to load.")
  }       


  ProtInfo <- row.metadata.table[
    match(rownames(dataset), row.metadata.table[,row.metadata.key]),
    c(row.metadata.key, setdiff(colnames(row.metadata.table), row.metadata.key)),drop=F]

  stopifnot(all(ProtInfo[,1] == rownames(dataset)))
  return(ProtInfo)
}

  # Gives a table with the row names equal to the columns of the dataset
get.factors <- function(dataset){
  column.metadata.table <- get.column.metadata.table(dataset)
  column.metadata.key <- get.column.metadata.key(dataset)

  if(is.null(column.metadata.table) || is.null(column.metadata.key)) {
    quick_message("Table has no column metadata (factors) defined.\n Use Metadata -> Define Column Metadata to do this.")
    return()    
  } else if(!column.metadata.key%in%colnames(column.metadata.table)) {
    quick_message("Column Metadata is incorrectly linked.\n\nUse Metadata->Column Metadata to load.")
    return()    
  }

  the.factors <- column.metadata.table[
    match(colnames(dataset), column.metadata.table[,column.metadata.key]),
    c(column.metadata.key, setdiff(colnames(column.metadata.table), column.metadata.key)),drop=F]
  rownames(the.factors) <- the.factors[,1]
  #the.factors <- the.factors[,-1,drop=F]
  return(the.factors)
}

  # Given a metadata table name, return the names of all tables that are linked to it
get_all_linked_table_names <- function(metadata_name, attr_name = "Row_Metadata"){
  all_linked_table_names <- get_all_tables()[sapply(get_all_tables(), function(x, md_name){
    xx <- safe.eval(x)
    identical(attr(xx, attr_name)[["table"]], md_name)
  }, metadata_name)]
  stopifnot(!metadata_name%in%all_linked_table_names)
  return(all_linked_table_names)
}



  # Go thru all tables in Global
  # If their column names are linked to this table via primary.field, then
  #  update their column names to alias.field with a join on primary.field
  # Finally update the alias field to primary.field
  # 
update.aliases <- function(cm.table.name, primary.field, alias.field){

  column.metadata.table <- safe.eval(cm.table.name)  
  if(any(!c(primary.field, alias.field)%in%colnames(column.metadata.table)))
    stop("Column names incorrectly specified in column metadata")
  
  column.names <- column.metadata.table[,primary.field]
  if(any(duplicated(column.names))) stop("Column names are duplicated, this isn't allowed")  
  new.names <- column.metadata.table[,alias.field]    
  if(any(duplicated(new.names))) stop("New names are duplicated, this isn't allowed")
  
  all_tables <- get_all_tables()

  for(dataset in all_tables){
    obj <- safe.eval(dataset)
    column.metadata.table.name <- get.column.metadata.table.name(obj)  
    if(is.null(column.metadata.table.name) || column.metadata.table.name != cm.table.name)
      next;
    column.metadata.table <- get.column.metadata.table(obj)
    column.metadata.key <- get.column.metadata.key(obj)  
      # maybe warn here?
    if(is.null(column.metadata.table) || is.null(column.metadata.key)){
      warning(paste("1: Column metadata is incorrectly linked to", dataset))    
      next;
    }
    if(!all(colnames(obj)%in%column.names)){ 
      warning(paste("2: Column metadata is incorrectly linked to", dataset))
      next;
    }
    
    #print(dataset)
    #print(length(colnames(obj)))
    #print(length(new.names[match(colnames(obj),column.names)]))
    #print(match(colnames(obj),column.names))
    #print(colnames(obj))
    colnames(obj) <- new.names[match(colnames(obj),column.names)]
    #print(colnames(obj))    
    cmd.str <- paste(paste(".GlobalEnv", dataset, sep = "$"), "<- obj")
    #print(cmd.str)
    eval(parse(text = cmd.str))
  }
    
  column.metadata.table[,primary.field] <- new.names
  cmd.str <- paste(paste(".GlobalEnv", cm.table.name, sep = "$"), "<- column.metadata.table")  
  #print( column.metadata.table[1:5,])
  #print(cmd.str)  
  eval(parse(text = cmd.str))
  NULL
}


update.aliases.dialog = list(
  label = "Update Data Column Names\nThis updates the column names of all datasets \nlinked to the given Column Metadata table",
  cm.table.name.dataframeItem = "T_Column_Metadata", label = "Column Metadata/Factors Table", as.character=T,
    signal = c("default", "get.colnames", "primary.field", "alias.field"),  
  primary.field.choiceItem = NULL, label = "Update Everything Linked Via Field",
  alias.field.choiceItem = NULL, label = "Replace Column Names With This Field"
)

  # Create two additional columns in the associated column metadata table:
  # Old_<primary_key>: original dataset names
  # Alias_<primary_key>: Updated dataset names
  # Then update...
create.aliases <- function(dataset, do.abbr = TRUE, abbr.length = 12){
  dataset_name <- deparse(substitute(dataset))
  cm.table.name <- get.column.metadata.table.name(dataset)
  if(is.null(cm.table.name) || !object.exists(cm.table.name))  
    stop("Table has no column metadata (factors) defined.\nUse Metadata -> Define Column Metadata to do this.")
  column.metadata.table <- get.column.metadata.table(dataset)
  column.metadata.key <- get.column.metadata.key(dataset)  

  if(is.null(column.metadata.table) || is.null(column.metadata.key))
    stop("Table has no column metadata (factors) defined.\nUse Metadata -> Define Column Metadata to do this.")
  if(!column.metadata.key %in% colnames(column.metadata.table) ||
     !all(colnames(dataset)%in%column.metadata.table[,column.metadata.key]))
    stop("Column Metadata is incorrectly linked.\nUse Metadata->Column Metadata to load.")
  
  dataset.names <- column.metadata.table[,column.metadata.key]
  new.names <- abbreviate(dataset.names, abbr.length)
  old.field.name <- paste("Old", column.metadata.key, sep="_")
  new.field.name <- paste("Alias", column.metadata.key, sep="_")
  #print(c(old.field.name, new.field.name))
  if(any(c(old.field.name, new.field.name)%in%colnames(column.metadata.table))) 
    stop("Associated column metadata already has aliases created")

  cn <- colnames(column.metadata.table)
  column.metadata.table <- data.frame(column.metadata.table, dataset.names, new.names)
  colnames(column.metadata.table) <- c(cn, old.field.name, new.field.name)  
  #print(column.metadata.table[1:2,])


  cmd.str <- paste(paste(".GlobalEnv", cm.table.name, sep = "$"), "<- column.metadata.table")  
  #print(cmd.str)  
  eval(parse(text = cmd.str))
  NULL  
  #assign(get.column.metadata.table.name(dataset), column.metadata.table)
    
}

create.aliases.dialog = list(
  title = "Data Column Aliases", label = "This function creates aliases for data sets.\nChoose a data set with linked column metadata",
  dataset.dataframeItem = "T_Data", label = "Choose Data Set", tooltip = "Data set must have linked column metadata/factors",
  do.abbr.trueFalseItem = TRUE, label = "Shorten Names?", tooltip = "Abbreviate names, preserving uniqueness",
    signal = c("default", "toggle.sensitive", "abbr.length"),          
  abbr.length.integerItem = c(value=12, from=4, to=256), label = "Length to Shorten", indent=10
)

  # Create a new metadata table or link a supplied one with the given key
  # this function returns the original table with the links
Create.Metadata.Table <- function(dataset, typ, table.name = "T_Metadata", n.columns=0, create.new = TRUE, metadata.table = NULL, key.name = NULL, guess.factors=FALSE, boundary="_", letNumBd=FALSE){
             
  stopifnot(is.data.frame(dataset) || is.matrix(dataset))

  stopifnot(typ %in%c(1,2))
  if(typ == 1){
    if(create.new){
      key.name = "Data_Row"
      #table.name = "T_Row_Metadata"
    }      
    err.typ = "rows"    
    metadata.typ = "Row_Metadata"
  } else if (typ == 2){
    if(create.new){  
      key.name = "Data_Column"
      #table.name = "T_Column_Metadata"              
    }
    err.typ = "columns"        
    metadata.typ = "Column_Metadata"    
  } 
  
  if(!is.null(metadata.table)) 
    table.name = deparse(substitute(metadata.table))
  
  dimn <- dimnames(dataset)[[typ]]  
  stopifnot(!is.null(dimn) && length(dimn) > 0)  
  
  if(create.new){
    stopifnot(0 < n.columns && n.columns <= 26)
    if(guess.factors && !is.null(dimnames(dataset))){ # Guess factor names
      xx <- dimnames(dataset)[[typ]]
      if(!identical(boundary, "none")){
        split.xx <- strsplit(xx, paste("[", paste(boundary, collapse=""), "]", sep=""), perl=T)      
        n.columns <- max(sapply(split.xx, length))
        arr.xx <- array("", c(length(xx), n.columns))
        for(ii in 1:length(xx)) arr.xx[ii, 1:length(split.xx[[ii]])] <- split.xx[[ii]]
        isBlank <- arr.xx==""      
        if(any(isBlank)) arr.xx[isBlank] <- NA
        arr.xx <- data.frame(arr.xx[,apply(arr.xx, 2, function(yy) length(unique(yy))!=1),drop=F])
      } else {
        arr.xx <- NULL
      }
      if(letNumBd) { # Form another factor frame based on letter-number boundaries

        gextract <- function(txt, gg){
          #gg <- gregexpr(re, txt)[[1]]
          lgg <- length(gg)
          mVec <- vector("character", lgg)
          names(mVec) <- gg
          if(lgg) for(jj in 1:lgg) 
              mVec[jj] <- substr(txt, gg[jj], gg[jj] + attr(gg, "match.length")[jj]-1)
          return(mVec)
        }
        
        mM <- gregexpr("[a-zA-Z]+|[0-9.]+", xx)
        n.columns2 <- max(sapply(mM, length))        
        arr.xx2 <- array("", c(length(xx), n.columns2))                
        for(ii in 1:length(mM)) {
          gex <- gextract(xx[ii], mM[[ii]])      
          arr.xx2[ii, 1:length(gex)] <- gex
        }
        isBlank <- arr.xx2==""      
        if(any(isBlank)) arr.xx2[isBlank] <- NA        
        arr.xx2 <- data.frame(arr.xx2[,apply(arr.xx2, 2, function(yy) length(unique(yy))!=1),drop=F])
        # Remove factors we've got already
        if(!is.null(arr.xx)){
          sim.vec <- rep(FALSE, ncol(arr.xx2))
          for(ii in 1:ncol(arr.xx)) 
            for(jj in 1:ncol(arr.xx2)) 
              if(identical(all.equal(as.character(arr.xx[,ii]), as.character(arr.xx2[,jj])), TRUE)) {
                sim.vec[jj] <- TRUE
                break;
              }
          arr.xx2 <- arr.xx2[,!sim.vec, drop=F]
        }
        if(!is.null(arr.xx)) arr.xx <- data.frame(arr.xx, arr.xx2)
        else arr.xx <- arr.xx2
      }
      if(is.null(arr.xx))  
        metadata.table <- data.frame(array(NA_character_, c(length(dimn), n.columns)))
      else 
        metadata.table <- data.frame(arr.xx)
      colnames(metadata.table) <- LETTERS[1:ncol(metadata.table)]
      metadata.table <- data.frame(dimn, metadata.table, stringsAsFactors=F)
      colnames(metadata.table)[1] <- key.name
    } else {
      metadata.table <- data.frame(array(NA_character_, c(length(dimn), n.columns)))            
      metadata.table <- data.frame(dimn, metadata.table, stringsAsFactors=F)
      colnames(metadata.table) <- c(key.name, LETTERS[1:n.columns])
    }
#    assign(table.name, metadata.table, envir=.GlobalEnv) # might want to warn here      
  } else { # attempt to link the supplied table in
    stopifnot(!is.null(metadata.table))
    stopifnot(is.data.frame(metadata.table) || is.matrix(metadata.table))    
    stopifnot(!is.null(key.name))
    stopifnot(length(key.name)==1)    
    stopifnot(key.name%in%colnames(metadata.table))    
    if(!all(dimn%in%metadata.table[,key.name])) # check integrity
      stop(paste("Can't link tables together, data set contains", err.typ, "not seen in metadata table"))     
  }

  dataset <- set.metadata.obj(dataset, metadata.typ, "key", key.name)          
  dataset <- set.metadata.obj(dataset, metadata.typ, "table", table.name)          
  
  return(list(dataset=dataset, metadata=metadata.table))
}

# Create a new factors table
Column_Metadata <- function(dataset,column.metadata.name, guess.factors, boundary, letNumBd, n.fields){
  dataset.name <- deparse(substitute(dataset))
  choice <- TRUE
  if(object.exists(column.metadata.name))
    choice <- gconfirm(paste("Warning!", column.metadata.name, "exists and will be replaced. OK?"))
  if(choice){
    metadata_rv <- Create.Metadata.Table(dataset, 2, table.name = column.metadata.name, n.columns = n.fields, guess.factors=guess.factors, boundary=boundary, letNumBd=letNumBd)    
    assign(dataset.name, metadata_rv$dataset, envir=.GlobalEnv)  
    assign(column.metadata.name, metadata_rv$metadata, envir=.GlobalEnv)    
  }
  invisible(NULL)
}

# Create a new factors table
Row_Metadata <- function(dataset, row.metadata.name, n.fields){
  dataset.name <- deparse(substitute(dataset))
  choice <- TRUE
  if(object.exists(row.metadata.name))  
    choice <- gconfirm(paste("Warning!", column.metadata.name, "exists and will be replaced. OK?"))
  if(choice){  
    metadata_rv <- Create.Metadata.Table(dataset, 1, table.name = row.metadata.name, n.columns = n.fields)
    assign(dataset.name, metadata_rv$dataset, envir=.GlobalEnv)  
    assign(row.metadata.name, metadata_rv$metadata, envir=.GlobalEnv)      
  }
  invisible(NULL)
}

Row_Metadata.dialog = list(
  title = "Define Row Metadata",
  dataset.dataframeItem = "", label = "Use Columns In This Data Set",
  row.metadata.name.stringItem = "T_Row_Metadata", label = "Row Metadata Table Name",
    tooltip = "Choose the name of the row metadata table",    
  n.fields.integerItem = c(value=1, from=1, to=26), label = "Number of Fields"
)

Column_Metadata.dialog = list(
  title = "Create Factor Table",
  dataset.dataframeItem = "", label = "Use This Data Set",
    tooltip = "The data set you want to define a set of factors for",
  column.metadata.name.stringItem = "T_Column_Metadata", label = "Factor Table Name",    
    tooltip = "Choose the name of the factors table",  
  guess.factors.trueFalseItem = TRUE, label = "Guess Factors From Dataset Names",
    tooltip = "Look at the column names of the data sets to get factors.",
    signal = c("default", "toggle.sensitive", "boundary", "letNumBd"),
    letNumBd.trueFalseItem = TRUE, label = "Split Into Letters/Number", indent=10,
    tooltip = "For example, split 'TxD75L1' into 'TxD', '75', 'L', '1'", 
    boundary.choiceItem = c("none", "_", "\\-", "\\s", "\\."), item.labels = c("Don't Split", "Underscore '_'", "Hyphen '-'", "Whitespace","Period '.'"), label = "Separators in Column Name", indent=10,
    tooltip = "Character to split your column titles (eg 'L1_B2_R2' splits with '_' to give 'L1', 'B2', 'R2' factors)",    
    n.fields.integerItem = c(value=1, from=1, to=26), label = "Number of Factors (Treatment, Tech.Rep, etc)",
    tooltip = "Select the number of different factors you want to link to your dataset. For example, if you want factors for Treatment and Bio.Replicate, choose 2."
)

Link_To_Metadata <- function(dataset, row_metadata_exists, row_metadata_table, row_metadata_key, column_metadata_exists, column_metadata_table, column_metadata_key){  
                         
  get.dataset <- safe.eval(dataset)
  doMeta <- function(dataset1, metadata_exists, metadata_table, metadata_key, an){
    if(metadata_exists && nchar(metadata_key) > 0 && nchar(metadata_table) > 0){
       attr(dataset1, an)["key"] <-  metadata_key
       attr(dataset1, an)["table"] <-  metadata_table
    } else {
      quick_message(paste("Warning: for dataset", dataset, "no link for", an, "has been set!"))
      attr(dataset1, an) <- NULL
    }
    return(dataset1)
  }
  
  get.dataset <- doMeta(get.dataset, row_metadata_exists, row_metadata_table, row_metadata_key, "Row_Metadata")
  get.dataset <- doMeta(get.dataset, column_metadata_exists, column_metadata_table, column_metadata_key, "Column_Metadata")  

  cmd.string <- paste(".GlobalEnv$", dataset, " <- get.dataset", sep="")
  eval(parse(text=cmd.string))
  return(NULL)
}

Link_To_Metadata.dialog <- list(
  dataset.dataframeItem = "", label = "Choose Data Set", as.character=T, 
    signal = c("default", function(item, row_metadata_exists, 
      column_metadata_exists, row_metadata_table, column_metadata_table, 
      row_metadata_key, column_metadata_key){
      obj.name <- get.value(item)
      if(object.exists(obj.name)) {
        obj <- safe.eval(obj.name)
        rmt <- get.row.metadata.table.name(obj)
        set.value(row_metadata_exists, !is.null(rmt))
          set.value(row_metadata_key, colnames(rmt))                  
        #if(!is.null(rmt))        
          set.value(row_metadata_table, rmt, propagate=FALSE)
        cmt <- get.column.metadata.table.name(obj)
        set.value(column_metadata_exists, !is.null(cmt))        
        #if(!is.null(cmt))
          set.value(column_metadata_table, cmt, propagate=FALSE)
          set.value(column_metadata_key, colnames(cmt))                            
      }      
    }, "row_metadata_exists", "column_metadata_exists", "row_metadata_table", "column_metadata_table", "row_metadata_key", "column_metadata_key"), 
  BREAK=T,
  row_metadata_exists.trueFalseItem = FALSE, label = "Link Row Metadata",
    signal = c("default", "toggle.sensitive", "row_metadata_table", "row_metadata_key"),
  row_metadata_table.dataframeItem = NULL, label = "Link Row Names To Table", as.character=T, indent=10,
    signal = c("default", function(item, dataset, row_metadata_exists, row_metadata_key){
        # Set the choice 
      row_metadata_key$setSensitive(FALSE)        
      if(get.value(row_metadata_exists) && object.exists(get.value(item)) && object.exists(get.value(dataset))){
        obj <- safe.eval(get.value(item))
        ds <- safe.eval(get.value(dataset))
        if(is.null(colnames(obj))) colnames(obj) <- 1:dim(obj)[2]
        cn <- colnames(obj)[apply(obj, 2, function(x) all(rownames(ds)%in%x))]        
        if(!length(cn)) {
          quick_message(paste("No columns in", get.value(item), "match ALL row names of", get.value(dataset)))
        } else {
          row_metadata_key$setSensitive(TRUE)        
        }
        set.value(row_metadata_key, cn)
      }
    }, "dataset", "row_metadata_exists", "row_metadata_key"),        
  row_metadata_key.choiceItem = NULL, label = "Row Metadata Key", indent=10, 
  column_metadata_exists.trueFalseItem = FALSE, label = "Link Column Metadata",
    signal = c("default", "toggle.sensitive", "column_metadata_table", "column_metadata_key"),  
  column_metadata_table.dataframeItem = NULL, label = "Link Column Names To Table", as.character=T, indent=10,
    signal = c("default", function(item, dataset, column_metadata_exists, column_metadata_key){
        # Set the choice 
      column_metadata_key$setSensitive(FALSE)                          
      if(get.value(column_metadata_exists) && object.exists(get.value(item)) && object.exists(get.value(dataset))){
        obj <- safe.eval(get.value(item))
        ds <- safe.eval(get.value(dataset))
        if(is.null(colnames(obj))) colnames(obj) <- 1:dim(obj)[2]
        cn <- colnames(obj)[apply(obj, 2, function(x) all(colnames(ds)%in%x))]

        if(!length(cn)){ 
          quick_message(paste("No columns in", get.value(item), "match ALL column names of", get.value(dataset)))
        } else {
          column_metadata_key$setSensitive(TRUE)                                  
        }
        set.value(column_metadata_key, cn)
      }
    }, "dataset", "column_metadata_exists", "column_metadata_key"), 
  column_metadata_key.choiceItem = NULL, label = "Column Metadata Key", indent=10
)

GetLinksToMetadata <- function(metadata_name, metadata_type)
#  metadata_type = "Row_Metadata"
#  metadata_name = "T_Row_Metadata"
  unlist(sapply(get_all_tables(), function(x)
     if(identical(attr(safe.eval(x), metadata_type)[["table"]], metadata_name)) attr(safe.eval(x), metadata_type)[["key"]]))

Plot_All_Links <- function(row_metadata_name = "T_Row_Metadata", col_metadata_name = "T_Column_Metadata"){
  library(diagram)
  if(!object.exists(row_metadata_name) || !object.exists(col_metadata_name)){
    quick_message("Can't find row or column metadata tables!")
    return()
  }
  row_metadata <- safe.eval(row_metadata_name)
  col_metadata <- safe.eval(col_metadata_name)  
        
    # Get all tables that are linked to the metadata
    
#  all_table_names <- get_all_tables()
#  row_linked_tables <- all_table_names[sapply(all_table_names, function(x) !is.null(attr(safe.eval(x), "Row_Metadata")))]
#  row_link_keys <- sapply(row_linked_tables, function(x) attr(safe.eval(x), "Row_Metadata")[["key"]])
  row_link_keys <- GetLinksToMetadata(row_metadata_name, "Row_Metadata")
  row_linked_tables <- names(row_link_keys)

  col_link_keys <- GetLinksToMetadata(col_metadata_name, "Column_Metadata")
  col_linked_tables <- names(col_link_keys)

#  col_linked_tables <- all_table_names[sapply(all_table_names, function(x) !is.null(attr(safe.eval(x), "Column_Metadata")))]
#  col_link_keys <- sapply(col_linked_tables, function(x) attr(safe.eval(x), "Column_Metadata")[["key"]])
#  col_link_keys <- col_link_keys[sapply(col_linked_tables, function(x) attr(safe.eval(x), "Column_Metadata")["table"]==col_metadata_name)]  
#
  diagram_tables <- c(row_metadata_name, c(setdiff(row_linked_tables, col_linked_tables), intersect(row_linked_tables, col_linked_tables), setdiff(col_linked_tables, row_linked_tables)), col_metadata_name)
  N <- length(diagram_tables)
  M = matrix(nrow = N, ncol = N, byrow = TRUE, data=0)

  row_pos <- which(diagram_tables%in%row_metadata_name)
  col_pos <- which(diagram_tables%in%col_metadata_name)
  box_colors <- rep("white", N)
  
  for(item in row_linked_tables) {
    item.idx <- which(diagram_tables%in%item)
    M[item.idx, row_pos] <- row_link_keys[[item]]
    if(!row_link_keys[[item]]%in%colnames(row_metadata)){
      diagram_tables[item.idx] <- paste(diagram_tables[item.idx], "(Bad Row Key!)", sep="")
      box_colors[item.idx] <- "red"
    } else if(!all(rownames(safe.eval(item))%in%row_metadata[,row_link_keys[[item]]])){
      diagram_tables[item.idx] <- paste(diagram_tables[item.idx], "(Row Name Mismatch!)", sep="")
      box_colors[item.idx] <- "red"      
    }
  }
  
  for(item in col_linked_tables) {
    item.idx <- which(diagram_tables%in%item)
    M[item.idx, col_pos] <- col_link_keys[[item]]
    if(!col_link_keys[[item]]%in%colnames(col_metadata)){
      diagram_tables[item.idx] <- paste(diagram_tables[item.idx], "(Bad Column Key!)", sep="")
      box_colors[item.idx] <- "red"      
    } else if(!all(colnames(safe.eval(item))%in%col_metadata[,col_link_keys[[item]]])){
      diagram_tables[item.idx] <- paste(diagram_tables[item.idx], "(Column Name Mismatch!)", sep="")    
      box_colors[item.idx] <- "red"      
    }
  }

  if(N > 3)
    pos = c(2, rep(1, N-4), 2)
  else 
    pos = N

   pp<-plotmat(M,pos=pos,name=diagram_tables,lwd=1,box.lwd=2,cex.txt=0.8, box.size=0.1,box.type="square",box.prop=0.25, box.col=box_colors)
   return(recordPlot())
}

Plot_All_Links.dialog <- list(
  label = "Select Row and Column Metadata Tables to Plot",
  row_metadata_name.dataframeItem = "T_Row_Metadata", as.character=TRUE, label = "Row Metadata", take.hint = FALSE,
  col_metadata_name.dataframeItem = "T_Column_Metadata", as.character=TRUE, label = "Column Metadata"
)