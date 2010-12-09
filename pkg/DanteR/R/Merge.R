merge_all_tables <- function(...){
  tabs <- list(...)
  if(length(tabs) < 2) return(tabs)
  rv <- tabs[[1]]
  for(tab in tabs[-1]){
    rv <- merge_tables(rv, tab)
  }
  return(rv)
}

  # merge x1 and x2; if columns are repeated take x1's.
merge_tables <- function(x1, x2){
  m1 <- merge(x1, x2, by.x=0, by.y=0, all=TRUE, suffix="")
  rnc <- m1[,1]%in%rownames(x2)
  cc <- intersect(colnames(x1), colnames(x2))
  if(length(cc)){
    for(nn in cc) {
      m1.c <- m1[is.na(m1[,nn]) & rnc,1]
      m1[m1[,1]%in%m1.c,nn] <- as.vector(x2[m1.c,nn])
    }
    m1 <- m1[,!colnames(m1)%in%paste(cc, "NA", sep=""),drop=F]
  }
  rownames(m1) <- m1[,1]
  m1 <- m1[,-1,drop=F]
  return(m1)
}

Merge <- function(setop1="union", setop2="union", setop3="union", tabs1="", tabs2="", include="Both Sets"){

  list.ops <- list(
  union = function(...){
    ll <- list(...)
    rv <- NULL
    for(item in ll) rv <- union(rv, item)
    rv
  },
  intersect = function(...){
    ll <- list(...)
    rv <- NULL
    if(length(ll)){
      rv <- ll[[1]]
      for(item in ll[-1]) rv <- intersect(rv, item)
    }
    rv
  },
  setdiff= setdiff
  )
  
  stopifnot(all(c(setop1)%in%c("union", "setdiff", "intersect")))
  stopifnot(all(c(setop2, setop3)%in%c("union", "intersect")))

  setop1 <- list.ops[[setop1]]
  setop2 <- list.ops[[setop2]]
  setop3 <- list.ops[[setop3]]


  tables.1 <- lapply(c(tabs1), function(tab) if(object.exists(tab)) safe.eval(tab))
  tables.2 <- lapply(c(tabs2), function(tab) if(object.exists(tab)) safe.eval(tab))
    
#  items1 <- lapply(tabs1, function(tab) if(object.exists(tab)) rownames(safe.eval(tab)))
#  items2 <- lapply(tabs2, function(tab) if(object.exists(tab)) rownames(safe.eval(tab)))
  items1 <- lapply(tables.1, rownames)
  items2 <- lapply(tables.2, rownames)

    # Get the row names of our tables...
  list1 <- list(do.call(setop2, items1), do.call(setop3, items2))
  rows1 <- do.call(setop1, list1)

  all_tables <- c(tables.1, tables.2)
  big_merge <- do.call(merge_all_tables, all_tables)
  
  rv <- big_merge[rows1,,drop=F]
  
  if(include != "Both Sets"){
    include.list <- NULL
    if(include == "Set #1") include.list <- union(include.list, unlist(lapply(tables.1, colnames)))
    if(include == "Set #2") include.list <- union(include.list, unlist(lapply(tables.2, colnames)))
    stopifnot(all(include.list%in%colnames(rv)))
    rv <- rv[,include.list,drop=F]
  }

  if(all(sapply(all_tables, is.numeric))) rv <- data.matrix(rv)

  return(rv)
}

Merge.dialog <- list(
  label = "Merge And Filter Data Sets: Return a combined table with selected row elements.\n\nReturns the outer join on all tables with rows from Operation#3( Operation#1(Set#1), Operation#2(Set#2) ).\n\nAll columns in the tables will be included by default.",
  dummy.listItem = substitute(get_all_tables()), suppress=T, show.arrows = FALSE, label = "All Tables",
  BREAK = T,

  setop2.choiceItem = c("union", "intersect"), label = "Set Operation #1",
  tabs1.listItem = character(0), label = "Tables In Set #1...",
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy"),
  BREAK = T,
  setop1.choiceItem = c("union", "intersect", "setdiff"), label = "Set Operation #3",
  include.radiobuttonItem = c("Both Sets", "Set #1", "Set #2"), label = "Include Columns In",
  BREAK = T,
  setop3.choiceItem = c("union", "intersect"), label = "Set Operation #2",
  tabs2.listItem = character(0), label = "Tables In Set #2...",
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy")
)

Merge.dialog <- list(
  label = "Merge two data frames by common columns or row names, or do other versions of database join operations.",
  x.choiceItem = substitute(get_all_tables()), label = "Data Frame x",
    signal = list("default", "get.colnames", "dummy.x", user.data=list(extra.names="row.names")),
  y.choiceItem = substitute(get_all_tables()), label = "Data Frame y",  
    signal = list("default", "get.colnames", "dummy.y", user.data=list(extra.names="row.names")),  
  sort.trueFalseItem = TRUE, label = "Sort", tooltip =  "Should the results be sorted on the by columns?",  
  BREAK = T,
  dummy.x.listItem = NULL, label = "Fields in x", show.arrows=F, suppress=T,                                                
  by.x.listItem = NULL,
    signal = c("add", "push.selection", "dummy.x"),
    signal = c("subtract", "pop.selection", "dummy.x"),  
  all.x.trueFalseItem = TRUE, label = "Include All Rows in x", tooltip = "If TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y",      
  BREAK=T,
  dummy.y.listItem = NULL, label = "Fields in y", show.arrows=F, suppress=T,
  by.y.listItem = NULL,
    signal = c("add", "push.selection", "dummy.y"),
    signal = c("subtract", "pop.selection", "dummy.y"),
  all.y.trueFalseItem = TRUE, label = "Include All Rows in y", tooltip = "Analogous to all.x"    
)

Merge <- function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
      sort = TRUE, suffixes = c(".x",".y"), incomparables = NULL, ...){
    merge(x=safe.eval(x), y=safe.eval(y), by.x=by.x, by.y=by.y, all.x=all.x, all.y=all.y,
      sort=sort, suffixes=suffixes, incomparables = incomparables, ...)
}
      
