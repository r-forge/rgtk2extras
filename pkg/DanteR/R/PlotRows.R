# Written by Tom Taverner
# for the Department of Energy (PNNL, Richland, WA)
# Copyright 2010, Battelle Memorial Institute
# Website: http://omics.pnl.gov/software
# -------------------------------------------------------------------------
#
# Licensed under the Apache License, Version 2.0; you may not use this file except
# in compliance with the License.  You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#
# R Plotting functions used in DAnTE
# -------------------------------------------------------------------------



get_all_linked_table_names <- function(metadata_name){
  all_linked_table_names <- get_all_tables()[sapply(get_all_tables(), function(x, md_name){
    xx <- safe.eval(x)
    if("table"%in%names(attr(xx, "Row_Metadata")) && identical(attr(xx, "Row_Metadata")[["table"]], md_name)) return(TRUE)
    return(FALSE)
  }, metadata_name)]
  stopifnot(!metadata_name%in%all_linked_table_names)
  return(all_linked_table_names)
}

  # split the first vector into groups defined by the second vector
  # then perform the aggregate function on those groups and return a
  # vector containing the aggregate function's value for each group 1.
  #   aggregate_on_group(iris$Species, iris$Petal.Width, mean)
  #    == mean(iris[iris$Species=="virginica", ]$Petal.Width)
  # returns the mean value for Petal.Width within each species group.
aggregate_on_group <- function(x.group1, x.group2, aggregate.function){
  stopifnot(length(x.group1) == length(x.group2))
  stopifnot(is.function(aggregate.function))  
  if(is.factor(x.group1)) x.group1 <- levels(x.group1)[as.integer(x.group1)]
  if(is.factor(x.group2)) x.group2 <- levels(x.group2)[as.integer(x.group2)]
  x.new <- unlist(lapply(split(x.group2, x.group1), aggregate.function))
  x.aggregate <- x.new[match(x.group1, names(x.new))]
  return(x.aggregate)
}

    
# Take the merged table and return the equivalent of 
# SELECT select.cols FROM * GROUP BY group.field ORDER BY sort.field decreasing
# Sort is done on aggregate.function(sort.field)
resort_table <- function(merged_table, group.field, sort.field, aggregate.function=mean, decreasing=FALSE){

  stopifnot(!is.null(merged_table) && !is.null(group.field))
  on.exit(w$destroy()) # block concurrent user calls with a modal window
   w <- gtkWindowNew("Loading...", show=F)
  w$setDecorated(FALSE)
  w$add(gtkLabelNew("Sorting merged data, please wait..."))
  w$setPosition(GtkWindowPosition["center"])              
  w$setModal(TRUE)        
  w$showAll()  
      
  tryCatch({
  if(!is.null(sort.field)){
    x.group <- merged_table[,group.field]
    x.sort <- merged_table[,sort.field]                                                    
    if(is.factor(x.group)) x.group <- levels(x.group)[as.numeric(x.group)]
    x.new <- unlist(lapply(split(x.sort, x.group), aggregate.function))
    sort.vec <- x.new[match(x.group, names(x.new))]
      # Sort the merged table by the sort vector
    merged_table <- merged_table[order(sort.vec, decreasing=decreasing),,drop=F]
  }
  }, error = function(e) quick_message(as.character(e)))

    # Now remove the duplicates... so we report the first element of each group.
    # Actually, we want the table reported with the selected dataset's key.
  #merged_table <- merged_table[!duplicated(merged_table[,metadata]),,drop=F]
  #rownames(merged_table) <- merged_table[,group.field]

  return(list(merged_table=merged_table)) 
}

aggregate_functions = list(Value = function(x) x[1], 
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

#record.linked.table.values <- function(){
#  linked.table.choice$setData("group.by", get.value(group.by))                 
#  linked.table.choice$setData("sort.by", get.value(sort.by))           
#  linked.table.choice$setData("decreasing", get.value(decreasing))                 
#  linked.table.choice$setData("aggregate.method", get.value(aggregate.method))
#}
#
#  # Have our values changed?
#check.linked.table.values <- function()  
#  get.value(group.by) == linked.table.choice$getData("group.by")  && get.value(sort.by) == linked.table.choice$getData("sort.by")  && get.value(decreasing) == linked.table.choice$getData("decreasing")  && get.value(aggregate.method) == linked.table.choice$getData("aggregate.method")
#  

refresh_all_fields <- function(dummy, linked.table.choice, dataset, group.by, sort.by, filter.by, decreasing, title.label.1, title.label.2, row.label.1, plot.items, choose.fields){
  #stopifnot(!is.null(merged_table) && !is.null(group.field))
  on.exit(w$destroy()) # block concurrent user calls with a modal window
   w <- gtkWindowNew("Loading...", show=F)
  w$setDecorated(FALSE)
  w$add(gtkLabelNew("Merging tables, please wait..."))
  w$setPosition(GtkWindowPosition["center"])              
  w$setModal(TRUE)        
  w$showAll()  

  selected_table_names <- get.value(linked.table.choice) # the selected tables
  obj.name <- get.value(dataset)
  obj <- safe.eval(obj.name)
  
  row_metadata_table_name <- attr(obj, "Row_Metadata")[["table"]]  # get the row metadata table
  row_metadata_key <- attr(obj, "Row_Metadata")[["key"]]  # get the row metadata table      
  stopifnot(!is.na(row_metadata_table_name))
  row_metadata_table <- safe.eval(row_metadata_table_name)
    # Get all the fields of the linked tables and the row metadata table

  selected_table <- safe.eval(obj.name)
  selected_tables <- lapply(selected_table_names, safe.eval)
  selected_table_names_abbr <- abbreviate(selected_table_names, 6)
  names(selected_tables) <- selected_table_names
    # Check for duplicate field names
    
  # This is where the merge takes place. We first merge the row metadata with the
  # selected data so the user can select by any piece of row metadata.
  # When other tables are selected, two things can happen.
  # First they might contain other metadata (p-values or cluster information) 
  # the user also wants to sort by. In this case they don't share column names
  # so we can merge them using a left join.
  # Second they might contain other information the user wants to plot on the same
  # plot, in which case they will have (some of) the same column names. In this 
  # case, we add these tables as new rows, with a fictitious displayed row name
  # coming from [row name + table name]. For example the user may want to plot
  # protein summary information in the same plot as peptide information.

  row_metadata_table <- row_metadata_table[!duplicated(row_metadata_table),,drop=F]
  selected_key <- attr(selected_table, "Row_Metadata")[["key"]]
  merged_table <- merge(row_metadata_table, selected_table, by.x = selected_key, by.y = 0)
  if(length(selected_tables))
    for(ii in 1:length(selected_tables)){
      tab <- selected_tables[[ii]]
      tab_name <- names(selected_tables)[ii]
      key <- attr(tab, "Row_Metadata")[["key"]]
      
      if(key%in%colnames(row_metadata_table) && all(rownames(tab)%in%row_metadata_table[,key])){
         # Second case above      
        if( any(colnames(tab)%in%colnames(merged_table))) { # merge similar tables with outer         
          m1 <- merge(row_metadata_table, tab, by.x = key, by.y = 0)
            # Create a "virtual row" with the selected key indicating its real origin
          m1[,selected_key] <- paste(m1[,key], "@", selected_table_names_abbr[ii], sep="")
          merged_table <- merge(merged_table, m1, all=T)
            # Probably not needed if we did the merge right
          merged_table <- merged_table[,!duplicated(colnames(merged_table)),drop=F]      
        } else {
            # First case above
          merged_table <- merge(merged_table, tab, by.x = key, by.y = 0)
        }
      } else
        quick_message("Table has incorrectly specified row metadata")
    }
    # use the merged table colnames that don't appear in the original table
  #new_fields <- setdiff(colnames(merged_table), colnames(obj))
  new_fields <- colnames(merged_table)
  sapply(list(group.by, sort.by, filter.by), function(x) set.value(x, new_fields))
  set.value(choose.fields, new_fields, initial=new_fields%in%colnames(selected_table))
  new_fields <- c("None", new_fields)
  sapply(list(title.label.1, title.label.2, row.label.1), function(x) set.value(x, new_fields))  
    # Perform the query on the merged table 
#  rv <- resort_table(
#    merged_table,
#    group.field = get.value(group.by), 
#    sort.field = get.value(sort.by),
#    aggregate.function = aggregate_functions[["Value"]],
#    decreasing = get.value(decreasing) 
#  )
  
#  linked.table.choice$setData("merged_table", rv$merged_table)

  linked.table.choice$setData("merged_table", merged_table)
  linked.table.choice$setData("select.field", colnames(obj))
  linked.table.choice$setData("select.key", row_metadata_key)     
   
  #record.linked.table.values()        
  #new.items <- unique(as.character(rv$merged_table[,get.value(group.by)]))
  sort.vec <- order(merged_table[,get.value(sort.by)], decreasing=get.value(decreasing)) 
  new.items <- merged_table[sort.vec,get.value(group.by)]
  new.items <- as.character(unique(new.items))
  set.value(plot.items, new.items)    
  plot.items$getParent()$getParent()$getParent()$setLabel(paste(length(new.items), "Groups Available (click to select)"))
}
    
refresh_plot_items <- c("default", function(dummy, dataset, linked.table.choice, group.by, sort.by, decreasing, do.filter, filter.by, include.NA, plot.items){

  if(dummy$getData("name") == do.filter$getData("name") && !is.logical(linked.table.choice$getData("merged_table")[,get.value(filter.by)])) return()

#  rv <- resort_table(
#    linked.table.choice$getData("merged_table"),
#    group.field = get.value(group.by), 
#    sort.field = get.value(sort.by),
#    aggregate.function = aggregate_functions[["Value"]],
#    decreasing = get.value(decreasing) 
#  )
#  
#  linked.table.choice$setData("merged_table", rv$merged_table)

  merged_table <- linked.table.choice$getData("merged_table")
  
  new_items <- merged_table[,get.value(group.by)]
  sort_vec <- order(merged_table[,get.value(sort.by)], decreasing=get.value(decreasing))  
    
  if(get.value(do.filter) && !get.value(include.NA) && is.logical(merged_table[,get.value(filter.by)])){
    filter.idx <- merged_table[,get.value(filter.by)]
      # Remove where filter.idx is NA ?
    new_items <- new_items[sort_vec & filter.idx]
  }  else {  
    new_items <- new_items[sort_vec]
  }
  
  new_items <- as.character(unique(new_items))
  set.value(plot.items, new_items)
    # Put indicated number of items on frame label
  plot.items$getParent()$getParent()$getParent()$setLabel(paste(length(new_items), "items available, click to select"))
#}
  
}, "dataset", "linked.table.choice","group.by", "sort.by", "decreasing", "do.filter", "filter.by", "include.NA", "plot.items")

#
    
PlotRows.dialog <- list(
  label = "Linked Data Set Plotting.\nPlots data as a matrix or heatmap with row groupings from other linked tables.\nGroups can be ordered using other linked columns.",  title = "Crosstab Row Plot",
#  ok.button = FALSE,   
  dataset.dataframeItem = "", label = "Choose Data Crosstab",
  tooltip = "Choose a data set",
    signal = c("default", function(dataset, linked.table.choice){
      obj.name <- get.value(dataset)
      if(object.exists(obj.name)) {
        obj <- safe.eval(obj.name)
        row_metadata_table_name <- attr(obj, "Row_Metadata")[["table"]]
        obj_row_metadata_key <- attr(obj, "Row_Metadata")[["key"]]
        if(!length(row_metadata_table_name) || is.na(row_metadata_table_name)) {
          quick_message("Data table has no row metadata set")
          return()
        }
        all_linked_tables <- get_all_linked_table_names(row_metadata_table_name)
        all_linked_tables <- setdiff(all_linked_tables, obj.name) 
        set.value(linked.table.choice, all_linked_tables, initial=FALSE)
#        obj <- get(obj.name)
#        row.key <- get.row.metadata.key(obj)
#        rmt <- get.row.metadata.table(obj)
#        print(colnames(rmt))
#        set.value(split.row.metadata.field, colnames(rmt))#[!colnames(rmt)%in%row.key])        
##        col.key <- get.column.metadata.key(obj)        
##        cmt <- get.column.metadata.table(obj)
##        if(is.null(cmt))
##          quick_message("Data Has No Column Metadata Set.\n\nUse Metadata->Column Metadata or File->Load Column Metadata to load factors")
##        set.value(dummy, colnames(cmt)[!colnames(cmt)%in%col.key])
      }
    }, "linked.table.choice"),
    
#  do.group.by.trueFalseItem = TRUE, label = "Group Rows Using Metadata", 
#    signal = c("default", "toggle.sensitive", "linked.table.choice"),
  linked.table.choice.variableSelectorItem = NULL, label = "Crosslink Tables To Data", 
  tooltip = "Select another linked data set to include columns from it",  
    signal =  c("default", refresh_all_fields, "linked.table.choice", "dataset", "group.by", "sort.by", "filter.by", "decreasing", "title.label.1", "title.label.2", "row.label.1", "plot.items", "choose.fields"), signal.on.starting=FALSE,
  group.by.choiceItem = NULL, label = "Column To Form Groups From", signal = refresh_plot_items, signal.on.starting=FALSE,
      
  new.field.buttonItem = "  Create New Column...  ",
    signal = c("clicked", function(x, dataset, group.by, sort.by, do.filter, filter.by, title.label.1, title.label.2, row.label.1, linked.table.choice){
    
        # suggest a reasonable field name from the new parameters
       # suggest a reasonable field name from the new parameters
      refresh_signal <- c("default", function(dummy, field, new_field_name, aggregate_columns_method, do.aggregate, aggregate_field, aggregate_method, do.expression, expression, parameter){
        nfn <- paste(get.value(field), collapse = ",")
        if(is.null(get.value(field))) {
          set.value(new_field_name, "")
          return()
        }
        if(get.value(aggregate_columns_method) != "Value")
            nfn <- paste(get.value(aggregate_columns_method), "(", nfn, ")", sep="")

        if(get.value(do.aggregate)) {
          if(get.value(aggregate_method) != "Value")
            nfn <- paste(get.value(aggregate_method), "(", nfn, ")", sep="")
          if(length(get.value(field)) > 1 || get.value(field) != get.value(aggregate_field))
            nfn <- paste(nfn, " by ", get.value(aggregate_field), sep = "")
        }
        if(get.value(do.expression)) nfn <- paste(nfn, get.value(expression), get.value(parameter), sep="")
          set.value(new_field_name, nfn)
      }, "field", "new_field_name", "aggregate_columns_method", "do.aggregate", "aggregate_field", "aggregate_method", "do.expression", "expression", "parameter")
      
    new_field.dialog <- list(
      label = "Create A New Column", title = "Create Column",
      dummy.listItem = get.value(group.by, selected=F), label = "Available Columns", signal = refresh_signal, suppress=TRUE, show.arrows=FALSE,
        tooltip = "Available columns to use to create a new column",
      field.listItem = character(0), label = "Use Values In Column(s)", signal = refresh_signal,
        tooltip = "Make a selection from the 'Available Columns' list item and use the arrows to move column in and out",
        signal = c("add", "push.selection", "dummy"),
        signal = c("subtract", "pop.selection", "dummy"),
        signal = c("add", function(item, aggregate_columns_method){
          if(length(get.value(item)) < 2) aggregate_columns_method$setSensitive(FALSE)
          else aggregate_columns_method$setSensitive(TRUE)                    
        }, "aggregate_columns_method"),
        signal = c("subtract", function(item, aggregate_columns_method){
          if(length(get.value(item)) < 2) aggregate_columns_method$setSensitive(FALSE)
          else aggregate_columns_method$setSensitive(TRUE)          
        }, "aggregate_columns_method"),
      BREAK=TRUE,
      
      new_field_name.stringItem = "New_Column", label = "New Column Name",
          tooltip = "What should the new column be called?",
        aggregate_columns_method.choiceItem = names(aggregate_functions), label = "Aggregate Multiple Columns With", signal = refresh_signal, sensitive=F,
          tooltip = "What function to apply to all of the selected columns to generate a single new column.\nThe default Value will take the first element.",
        do.aggregate.trueFalseItem = FALSE, label = "Aggregate Rows",
          tooltip = "Generate a new column by bringing together (aggregating) rows that have some column in common",
          signal = c("default", "toggle.sensitive", "aggregate_field", "aggregate_method", "filterThenAggregate"), signal = refresh_signal,
          aggregate_field.choiceItem = get.value(group.by, selected=F), label = "Grouping By", indent=10, signal = refresh_signal,
          tooltip = "The common column in the rows to aggregate",
          aggregate_method.choiceItem = names(aggregate_functions), label = "Using", indent=10, signal = refresh_signal,
          tooltip = "The function to replace each aggregated group with a single value.\nThe default Value will take the first element.",
          filterThenAggregate.radiobuttonItem = c(FALSE, TRUE), item.labels = c("Aggregate Then Filter", "Filter Then Aggregate"), label = character(0), indent=10,
          tooltip = "Should the new column be the result of row aggregation followed by applying a true/false filter, or vice versa",
          do.expression.trueFalseItem = FALSE, label = "True/False Filter",
          tooltip = "Should the new column contain the TRUE/FALSE values from applying a test?",
            signal = c("default", "toggle.sensitive", "expression", "parameter"), signal = refresh_signal,
          expression.choiceItem = c("<", ">", "==", "!=", "<=", ">="), label = "Expression", indent=10, signal = refresh_signal,
          tooltip = "Which mathematical expression should the test contain?",
          parameter.stringItem = "0", label = "Value", indent=10, signal = refresh_signal,
          tooltip = "What parameter should the test take?"
      )
      
      dlg1 <- run.dialog(list, dlg.list = new_field.dialog)
      if(!is.null(dlg1$retval)){
        theArgs <- dlg1$args        
        
          # inner joined table
        merged_table <- linked.table.choice$getData("merged_table") 
                
          # Aggregate over fields first
        new_field <- apply(merged_table[,theArgs$field,drop=F], 1, aggregate_functions[[theArgs$aggregate_columns_method]])
        
          # aggregate or create filter group over row groupings
        if(!theArgs$filterThenAggregate && theArgs$do.aggregate)          
          new_field <- aggregate_on_group(merged_table[,theArgs$aggregate_field], new_field, aggregate_functions[[theArgs$aggregate_method]])
        
        if(theArgs$do.expression) {
              # "field < 3"
           if(nchar(theArgs$parameter) < 1) {
             quick_message("No parameter specified")
             return()
           }
           x.group <- new_field
           if(is.factor(x.group)) x.group <- levels(x.group)[as.numeric(x.group)]
           parameter <- as(theArgs$parameter, class(x.group))
           new_field <- get(theArgs$expression)(x.group, parameter)
           new_field[is.na(new_field)] <- FALSE # turn incomparables into FALSE
        }
        
        if(theArgs$filterThenAggregate && theArgs$do.aggregate)          
          new_field <- aggregate_on_group(merged_table[,theArgs$aggregate_field], new_field, aggregate_functions[[theArgs$aggregate_method]])
        
                
          # add field to merged_table
        new_field_name <- theArgs$new_field_name
        if(nchar(new_field_name) > 0 && !new_field_name%in%colnames(merged_table) && !is.null(new_field)){
          merged_table <- cbind(merged_table, new_field)
          colnames(merged_table)[dim(merged_table)[2]] <- theArgs$new_field_name        
          linked.table.choice$setData("merged_table", merged_table)
          obj.name <- get.value(dataset)
          obj <- safe.eval(obj.name)
          #sapply(list(group.by, sort.by, filter.by), function(x) set.value(x, setdiff(colnames(merged_table), colnames(obj)) ))          
          new_fields <- colnames(merged_table)
          sapply(list(group.by, sort.by, filter.by), function(x) {
              old.value <- get.value(x)
              set.value(x, new_fields)              
              ww <- which(new_fields%in%old.value)
              if(length(ww)==1) x$setActive(ww-1)
          })          
          new_fields <- c("None", new_fields)
          sapply(list(title.label.1, title.label.2, row.label.1), function(x) {
              old.value <- get.value(x)
              set.value(x, new_fields)              
              ww <- which(new_fields%in%old.value)
              if(length(ww)==1) x$setActive(ww-1)          
          })
        } else { 
          quick_message("Badly specified field name")
        }
      } # is.null dlg$retval 
    }, "dataset", "group.by", "sort.by", "do.filter", "filter.by", "title.label.1", "title.label.2", "row.label.1", "linked.table.choice"),  # end signal from button
  reset.fields.buttonItem = "         Reset Columns         ", 
    signal =  c("clicked", refresh_all_fields, "linked.table.choice", "dataset", "group.by", "sort.by", "filter.by", "decreasing", "title.label.1", "title.label.2", "row.label.1", "plot.items", "choose.fields"),      
  sort.by.choiceItem = NULL, label = "Sort Groups By", signal = refresh_plot_items,signal.on.starting=FALSE,
  decreasing.radiobuttonItem = c("FALSE", "TRUE"), item.labels = c("Ascending", "Descending"), label = "Sort Order", indent=10, signal = refresh_plot_items,signal.on.starting=FALSE,
  do.filter.trueFalseItem = FALSE, label = "Do Filtering", signal = refresh_plot_items, signal.on.starting=FALSE,
  signal = c("default", "toggle.sensitive", "filter.by", "include.NA"), signal = refresh_plot_items,signal.on.starting=FALSE,
  filter.by.choiceItem = NULL, label = "Filter Column", indent=10, signal = refresh_plot_items,signal.on.starting=FALSE,
  include.NA.trueFalseItem = FALSE, label = "Show Excluded Groups", indent=10, signal = refresh_plot_items,signal.on.starting=FALSE,

  BREAK=T,
  
  choose.fields.variableSelectorItem = NULL, label = "Choose Columns",
    signal = c("default", function(item, linked.table.choice){
      linked.table.choice$setData("select.field", get.value(item))
      }, "linked.table.choice"),
  plot.items.listItem = NULL, label = "Choose Rows or Groups", show.arrows = F,
    signal = c("default", function(plot.items, plot.type, group.by, sort.by, do.filter, filter.by, linked.table.choice, group.in.title, title.label.1, title.label.2, do.row.labels, row.label.1, export.name){ 
      merged_table <- linked.table.choice$getData("merged_table")
      select.field <- linked.table.choice$getData("select.field")
      select.key <- linked.table.choice$getData("select.key")
      sel <- get.value(plot.items, selected=TRUE)      
      group.field <- get.value(group.by)
      filter.key <- get.value(filter.by)  
      pt <- get.value(plot.type)
      sort.field <- get.value(sort.by)      
            
      if(!is.null(merged_table) && length(sel) > 0){      
        row.idx <- merged_table[,group.field]%in%sel
        dat <- merged_table[row.idx,,drop=F]
        #rownames(dat) <- make.names(merged_table[row.idx,select.key], unique=TRUE)      

        best.width <- function(v) 2 + max(0.3*nchar(v)) 
        if(dim(dat)[1] > 0){
          the.title <- ""
          if(get.value(group.in.title))
            the.title <- paste(group.field, ": ", sel, sep = "")
          x.group2 <- dat[,group.field]
          
          dupe.idx <- duplicated(dat[,select.key]) & !is.na(dat[,select.key])           
            # returning by groups
          #dat <- dat[!dupe.idx,,drop=F]
          
          rxx <- as.character(dat[,select.key])
          rxx[is.na(rxx)] <- "[No label]"
          rownames(dat) <- make.unique(rxx)
          
          additional.row.label <- get.value(row.label.1)
          if(additional.row.label != "None") {
            rr <- dat[,additional.row.label]
            if(is.numeric(rr)) rr <- signif(rr, 3)
            rownames(dat) <- paste(rownames(dat), rr, sep = "; ")
          }
          
            # filter out data
          if(get.value(do.filter) && is.logical(dat[,filter.key])){
            dat <- dat[dat[,filter.key],,drop=F]
            #dupe.idx <- dupe.idx[filter.key]
          }
                      
            # get within-group values for title fields
          title.field <- sort.field
          
          title_fields <- c(get.value(title.label.1),get.value(title.label.2))
          title_fields <- title_fields[title_fields != "None"]
          if(length(title_fields)){
            x.group2 <- dat[,group.field]
            if(is.factor(x.group2)) x.group2 <- levels(x.group2)[as.integer(x.group2)]          
            .e <- new.env()
            .e$the.title <- the.title
            sapply(title_fields, function(title){
              x.group1 <- dat[,title]
              if(is.factor(x.group1)) x.group1 <- levels(x.group1)[as.integer(x.group1)]
              txt <- sapply(split(x.group1, x.group2), function(x) x[[1]])
              if(is.numeric(txt)) txt <- signif(txt, 3)
              .e$the.title <- paste(.e$the.title, "; ", title, ": ", txt, sep="")
            })
            the.title <- .e$the.title
          }          

            # return columns in original data crosstab
            
          dat <- dat[,select.field,drop=F]
          dat <- dat[!duplicated(dat),,drop=F]
          colStep.legend <- max(1, nrow(dat))
          colorRange.legend <- hsv(h = seq(0,1,1/colStep.legend), s=1, v=1)          
          legend.names <- rownames(dat)
          #xx <- t(dat[-dupe.idx,,drop=F])
          xx <- t(dat)
          xx1 <- xx
          #xx1 <- t(dat[!duplicated(dat),,drop=F])
          ncol.xx <- ncol(xx)
          if(ncol(xx)==0) xx <- cbind(xx, "<No Data>"=NA)            

          if(pt == "Plot Matrix") {
            par("mar" = c(best.width(rownames(xx1)),4,1.5, 4))          
              # This should deal with tricky cases like ALL of xx being nonexistent or NA
            the.ylim <- tryCatch(range(xx1, na.rm=T), error = function(e) c(-1, 1), warning = function(w) c(-1,1))
            if(NA%in%the.ylim) the.ylim <- c(-1, 1)

              # if the rowname starts in "[" (because it's from a crosslinked data crosstab
              # then make the line thickness change
            my.lwd = ifelse(regexpr("(@)", colnames(xx1)) > -1, 2, 1)
            colStep <- max(1, ncol(xx1))
            colorRange <- hsv(h = seq(0,1,1/colStep), s=1, v=1)
            if(length(the.title) > 1) the.title <- paste("(", length(the.title), "items selected)", sep="")
            matplot(xx1, type="l", main=the.title, xlab = "", ylab = "Data Value", ylim = the.ylim, col = colorRange, xaxt="n", lwd=my.lwd )
            matpoints(xx1, type = "p", pch=18, axes=F,  ylim = the.ylim, col = colorRange)
            axis(1, 1:nrow(xx1), labels = rownames(xx1), las = 2, line = -0.5, tick = 0)            
            if(get.value(do.row.labels)) {             
              a <- legend("topleft","what", legend.names, cex=0.7, col=colorRange.legend,pch=19, bg="transparent", plot=F) 
              rect(a$rect$left+a$rect$w, a$rect$top-a$rect$h, a$rect$left, a$rect$top, col="white")
              legend("topleft","what", legend.names, cex=0.7, col=colorRange.legend,pch=19, bg="transparent")               
            }
#            bringToTop()            
          }
          else if(pt == "Plot Heatmap") {                      
            par("mar" = c(best.width(rownames(xx)),0,1.5, best.width(colnames(xx))))
            image(1:nrow(xx), 1:ncol(xx),xx,main=the.title, axes = FALSE, xlab = "", ylab = "", )
            if(get.value(do.row.labels)) axis(4, 1:ncol(xx), labels = colnames(xx), las = 1, line = -0.5, tick = 0)
            axis(1, 1:nrow(xx), labels = rownames(xx), las = 2, line = -0.5, tick = 0)
#            bringToTop()
          } else if (pt == "Create Table") {
            eval(parse(text = paste(".GlobalEnv$", get.value(export.name), " <- dat", sep="")))
          } else if (pt == "Pass To A Function"){
          }
        } else {
          plot(1, type="n")            
          text(1, 1, "Nothing found to plot!")
        }
      } else {
        #quick_message("No data available to plot.items")
      }
    }, "plot.type", "group.by", "sort.by", "do.filter", "filter.by", "linked.table.choice", "group.in.title", "title.label.1", "title.label.2", "do.row.labels", "row.label.1", "export.name"),

  BREAK=TRUE,    
  plot.type.radiobuttonItem = c("Plot Matrix", "Plot Heatmap", "Create Table"), label = "Choose Action Type",  
    tooltip = "Choose to either plot the selected data or to export it to the main workspace",
#    signal = c("default", function(plot.type, plot.items) do.call(plot.items), "plot.items"),

  group.in.title.trueFalseItem = TRUE, label = "Include Group In Title",
  title.label.1.choiceItem = "None", label = "Title Label 1",
  title.label.2.choiceItem = "None", label = "Title Label 2",

  do.row.labels.trueFalseItem = TRUE, label = "Show Row Labels",
    row.label.1.choiceItem = "None", label = "Additional Row Labels", indent=10,
  export.name.stringItem = "T_Output", label = "Exported Table Name"#,
#  choose.function.objectItem = "", label = "Choose Function", tooltip="", data.types="function"
)  


PlotRows <- function(Data.Columns, idx=NULL, file="deleteme.png", bkground="white")
{
    #png(filename=file,width=1152,height=864,pointsize=12,bg=bkground,
    #        res=600)
    #require(Cairo)
    #CairoPNG(filename=file,width=IMGwidth,height=IMGheight,pointsize=FNTsize,bg=bkground,res=600)
    corners <- par('usr')
    par(mar=c(10,4,4,2))
    x <- Data.Columns
    if(is.null(idx)) idx <- 1:nrow(Data.Columns)
    #browser()
    tryCatch(
    {
      xlabels <- colnames(x)
      idx <- rev(idx)
      plot.labels <- rownames(x[idx,,drop=FALSE])
      X <- x[idx,]

      if (length(idx) == 1)
      {
        plot(x[idx,],type="o",pch=19,main=plot.labels,
              ylab="", xlab="", col="blue", xaxt="n")
        axis(1,at=1:length(xlabels),labels=xlabels, las = 2, cex.axis=.7)
        grid()
      }
      else
      {
          X <- X[rowSums(!is.na(X)) > 1,,drop=FALSE]
          plot.labels <- rownames(X)
          require(Hmisc)
          N <- dim(X)[1]
          curves <- vector('list', N)
          matplot(t(X),type="n",main="",ylab="",
              xlab="", xaxt="n")
          for(i in 1:N) {
            x <- 1:dim(X)[2]
            y <- X[i,]
            lines(x,y, type='o', col=i, pch=19)
            curves[[i]] <- list(x=x,y=y)
          }
          labcurve(curves, plot.labels, tilt=FALSE, type='l', col=1:N, cex=.7)
          axis(1,at=1:length(xlabels),labels=xlabels, las = 2, cex.axis=.7)
          grid()
      }
    },
  interrupt = function(ex)
  {
    cat("An interrupt was detected.\n");
    print(ex);
  },
  error = function(ex)
  {
    plot(c(1,1),type="n",axes=F,xlab="",ylab="")
    text(1.5,1,paste("Error:", ex),cex=.7)
    cat("An error was detected.\n");
    print(ex);
  },
  finally =
  {
    cat("Releasing tempfile...");
    par(mfrow=c(1,1),pch=1)
    ##dev.off()
    cat("done\n");
  }) # tryCatch()
}
