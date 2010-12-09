# To go into aaaStart.r

  # This function binds its arguments together into a ragged column-wise array
ragged.cbind <- function(...){
  items <- list(...)
  #if(length(items) == 1) items <- items[[1]]
  for(item in items)
    stopifnot(all(sapply(item, is.atomic)))
  dd <- function(x){
    if(!is.null(dim(x))) return(dim(x))
    if(is.list(x)) return(c(max(sapply(x, length)), length(x)))
    return(c(length(x), 1))
  }
  num.rows <- max(sapply(items, function(x) dd(x)[1]))
  rv <- NULL
  for(ii in 1:length(items)){
    item <- items[[ii]]
    if(is.null(item)) next;
    tmp <- data.frame(array(NA, c(num.rows, dd(item)[2])))
    colnames(tmp) <- 1:dim(tmp)[2]
    if(!is.null(dim(item))){
      if(dd(item)[1]) for(jj in 1:dd(item)[2]) tmp[1:dd(item)[1],jj] <- item[,jj]
      colnames(tmp) <- colnames(item)
    } else {
      if(is.list(item)){
        for(jj in 1:length(item)){
          if(length(item[[jj]]))
            tmp[1:length(item[[jj]]),jj] <- item[[jj]]
          colnames(tmp)[jj] <- names(item)[jj]
        }      
      } else if(dd(item)[1] > 0){
        tmp[1:dd(item)[1],] <- unlist(item)
        colnames(tmp) <- names(items)[[ii]]          
      }
    }                                
    if(is.null(rv))
      rv <- cbind(tmp)
    else
      rv <- data.frame(rv, tmp, check.names=FALSE, stringsAsFactors=FALSE)
  }
  return(rv)
}

# Get the data object name from b and the column from item and report
# the column objects as their unique factor levels
# Character coercion may mess up push and pop
get.factors.column <- function(item, b, c, user.data=NULL) {
  colname <- get.value(item)[1]
  #obj <- get(get.value(b))
  obj <- safe.eval(get.value(b))
  set.value(c, as.character(unique(obj[,colname])))
}

get.metadata.fields <- function(obj.name, metadata.type, include.primary=TRUE){
  cn <- NULL
  if(object.exists(obj.name)){
    obj <- safe.eval(obj.name)
    md <- attr(obj, metadata.type)
    if(is.null(md) || !object.exists(md[["table"]])){
      message(paste("Data has no", metadata.type, "set.\nUse Metadata->Define", metadata.type, "or Import Tables... to do this."))
    } else                                     {
      md_table <- safe.eval(md[["table"]])
      md_key <- md[["key"]]
      cn <- colnames(md_table)
      if(identical(include.primary, FALSE))  cn <- rev(rev(setdiff(colnames(md_table), md_key)))
    }
  }  
  return(cn)
}
  # Fill b with column metadata fields
  # user data sets whether to include the primary key or not
#get.dataset.column.metadata.fields <- function(item, ..., user.data=list(include.primary=FALSE)) {
get.dataset.column.metadata.fields <- function(item, ..., user.data=list(include.primary=FALSE)) {
  cmd <- get.metadata.fields(get.value(item), "Column_Metadata", user.data$include.primary)
  sapply(list(...), function(x) set.value(x, cmd))
}

get.dataset.factors <- get.dataset.column.metadata.fields

  # return all row metadata fields that aren't in the key
get.dataset.row.metadata.fields <- function(item, ..., user.data=list(include.primary=FALSE)){
  rmd <- get.metadata.fields(get.value(item), "Row_Metadata", user.data$include.primary)
  sapply(list(...), function(x) set.value(x, rmd))
}

toggle.dataset.factors <- function(item, Data, factor.column){
    factor.column$setSensitive(get.value(item))
    if( get.value(item) ) get.dataset.factors(Data, factor.column)
}
get.file.type <- function(fn){
  stopifnot(length(fn)>0 && is.character(fn) && nchar(fn)>0)
  emap <- sapply(file.list, function(x) x$extensions)
  extension <- rev(strsplit(fn, "\\.")[[1]])[1]
  file.type <- names(emap)[sapply(emap, function(x) extension%in%x)]
  if(length(file.type)>0) return(file.type)
  return("default")
}

save.ODBC.table <- function(fn, table, table.name, connect, overwrite=FALSE){

  library(RODBC)
  conn <- connect(fn, readOnly = FALSE)
  on.exit(tryCatch(odbcClose(conn), error = function(e) message("Couldn't close connection")))

  nchar_lim <- 31
  all_tables <- gsub("(.*)[$]$", "\\1", sqlTables(conn)$TABLE_NAME)
  table.name <- gsub(".", "_", substr(table.name, 1, nchar_lim), fixed=T)
  if(!is.data.frame(table)) table <- as.data.frame(table)
  stopifnot(is.data.frame(table))

    # Annoyingly, we can't delete tables reliably...
    # Next best thing is to make a reasonable name.
  if(overwrite && table.name%in%all_tables) {
     all_tables <- gsub("_", ".", all_tables, fixed=T)
     new.name <- rev(abbreviate(make.names(c(all_tables, table.name), unique=T), nchar_lim))[1]
     table.name <- gsub(".", "_", new.name, fixed=T)
  }

  print(table.name)
  sqlSave(conn, table, tablename=table.name, rownames=TRUE)
}


get.ODBC.table.names <- function(choice, connect){
  values <- NULL
  if(file.exists(choice)){
    require(RODBC)
    cc <- connect(choice, rows_at_time=1)
    on.exit({odbcClose(cc)})
    values <- sqlTables(cc)$TABLE_NAME
      # sqlFetch seems to like putting "$" on the end of table names,
      # which we really don't want  
    values_return <- c()
    for(val in values) {
      if(length(grep(" ", val)))
        values_return <- c( values_return,  gsub("^[']?(.*?)[$]?[']?$", "\\1", val, perl=T))
      else 
        values_return <- c(values_return,  val)
    }
    values <- unique(values_return)
    values_return <- values_return[nchar(values_return) > 0]
  }
  flush.console()
	return(values)
}

  # Returns the table names in some kind of file...
get.ODBC.table <- function(fn, table.name, connect){
  values <- NULL
  if(file.exists(fn)){
    require(RODBC)
    cc <- connect(fn, rows_at_time=1)
    on.exit({odbcClose(cc)})
    stopifnot(table.name%in%get.ODBC.table.names(fn, connect))
    values <- sqlFetch(cc, table.name, colnames=FALSE, dec=options("OutDec")[[1]])
    row.values <- rowSums(!is.na(values))
    which.ok <- which(row.values > 0)
    if(length(which.ok) && which.ok[length(which.ok)] < nrow(values)){
      last.idx <- which.ok[length(which.ok)]:nrow(values)
      values <- values[-last.idx,,drop=F]
      message(paste("Removed", length(last.idx), "completely blank rows from the end of", table.name))      
    }    
    
  } else {
    cat("File not found")
  }
  flush.console()
	return(values)
}


get.table.names <- function(fn, file.type="default"){
  values <- NULL
  if(file.exists(fn)){
    if(file.type%in%names(file.list))
      values <- file.list[[file.type]]$get.table.names(fn)
    else
      values <- fn
  }
  return(values)
}

get.table <- function(fn, table.name, file.type="default", ...){
  values <- NULL
  if(file.exists(fn) && file.type%in%names(file.list))
      values <- file.list[[file.type]]$get.table(fn, table.name, ...)
  else
    stop("get.table can't get table: doesn't exist or unknown type")
  return(values)
}

  # Returns the column names of a file
get.csv.colnames <- function(choice){
	if(file.exists(choice)){ # Selected a data frame
		get.choice <- read.csv(choice, header=T, nrows=5)
  	values <- colnames(get.choice)
	} else { # no valid file specified
    values <- character(0)
	}
	return(values)
}

file.list <- list(
  SQLite = list(
    get.table.names = function(fn){
      values <- NULL
        require(RSQLite)
        driver<-dbDriver("SQLite")
        connect<-dbConnect(driver, dbname = fn)
        on.exit({
          sqliteCloseConnection(connect)
          sqliteCloseDriver(driver)
        })
       dbListTables(connect)            
    },
    get.table = function(fn, tableChoice) {
      values <- NULL
      if(file.exists(fn)){
        require(RSQLite)
        driver<-dbDriver("SQLite")
        connect<-dbConnect(driver, dbname =fn)
        on.exit({sqliteCloseConnection(connect); sqliteCloseDriver(driver)})
        tables <- dbListTables(connect)
        if(tableChoice%in%tables){
        res <- dbSendQuery(connect, paste("SELECT * from", tableChoice))        
        values <- fetch(res, n = -1)
        dbClearResult(res)
        }
      }
      return(values)
    },
    save.table = function(fn, table, table.name, overwrite=F){
      library(RSQLite)
      driver<-dbDriver("SQLite")
      conn<-dbConnect(driver, dbname =fn)
      if(!is.data.frame(table)) table <- as.data.frame(table)
        stopifnot(is.data.frame(table))
      on.exit({sqliteCloseConnection(conn);sqliteCloseDriver(driver);})
      if(overwrite && table.name%in%dbListTables(conn))
        dbRemoveTable(conn, table.name)
      dbWriteTable(conn, table.name, table)
    },
    extensions = c("db3", "db")
  ),
  Excel2007 = list(
    get.table.names = function(choice) get.ODBC.table.names(choice, odbcConnectExcel2007),
    get.table = function(...) get.ODBC.table(..., connect=odbcConnectExcel2007),
    save.table = function(...) save.ODBC.table(..., connect=odbcConnectExcel2007),
    extensions = "xlsx"
  ),
  Excel = list(
    get.table.names = function(choice) get.ODBC.table.names(choice, odbcConnectExcel),
    get.table = function(...) get.ODBC.table(..., connect=odbcConnectExcel),
    save.table = function(...) save.ODBC.table(..., connect=odbcConnectExcel),
    extensions = "xls"
  ),
  csv = list(
    get.table.names = function(fn) fn,
    get.table = function(fn, ..., sep=",") {
      dec <- options("OutDec")[[1]]    
      if(missing(sep)) sep <- ifelse (identical(dec, ","), ";", ",")    
      read.table(fn, sep=sep, dec=dec, header=T, stringsAsFactors=F, row.names=NULL, fill=T, comment.char="", quote="")
    },
    save.table = function(fn, table, table.name, ..., sep=",") {
      dec <- options("OutDec")[[1]]
      if(missing(sep)) sep <- ifelse (identical(dec, ","), ";", ",")
      if(!is.null(row.names(table))) table <- cbind(row_names = row.names(table), table)
      write.table(table, fn, sep=sep, dec=dec, na="", row.names=!is.null(row.names(table)), quote=F)
    },
    extensions = "csv"
  ),
  txt = list(
    get.table.names = function(fn) fn,
    get.table = function(fn, ...) {
      dec <- options("OutDec")[[1]] 
      read.table(fn, header=T, stringsAsFactors=F, row.names=NULL, sep="\t", dec=dec, fill=T, comment.char="", quote="")
      },
    save.table = function(fn, table, table.name, ...) {
      dec <- options("OutDec")[[1]] 
      if(!is.null(row.names(table))) table <- cbind(row_names = row.names(table), table)
      write.table(table, fn, na="", dec=dec, row.names=FALSE, quote=F, sep="\t")
    },
    extensions = "txt"
  ),
  Access = list(
    get.table.names = function(choice) get.ODBC.table.names(choice, odbcConnectAccess),
    get.table = function(...) get.ODBC.table(..., connect=odbcConnectAccess),
    save.table = function(...) save.ODBC.table(..., connect=odbcConnectAccess),
    extensions = "mdb"
  )
)

   # Create a filter based on data file types
file.list.filters <- NULL
if(.Platform$OS.type == "windows"){
  file.list.filters <- Filters[c(1, rep(1, length(file.list)), dim(Filters)[1]),]
  rownames(file.list.filters)[1] <- "All Formats"  
  file.list.filters[1,1] <- paste(rownames(file.list.filters)[1], " (", paste(sapply(file.list, function(x) paste(paste("*.", x$extension, sep=""), collapse=", ")), collapse = ", "), ")", sep="")
  file.list.filters[1,2] <- paste(paste("*", unlist(sapply(file.list, function(x) x$extension)), sep=".", collapse = ";"))  
  rownames(file.list.filters)[1+1:length(file.list)] <- names(file.list)
  file.list.filters[1+1:length(file.list), 1] <- paste(names(file.list), "files", paste("(", sapply(file.list, function(x) paste(paste("*.", x$extension, sep=""), collapse=", ")), ")", sep=""))
  file.list.filters[1+1:length(file.list), 2] <- sapply(sapply(file.list, function(x) x$extension), function(x) paste(paste("*.", x, sep=""), collapse=";"))
}


## Define some changes in gWidgets
#
#getMethod(".update", 
#  signature(toolkit="guiWidgetsToolkitRGtk2",object="gTreeRGtk"))
#
