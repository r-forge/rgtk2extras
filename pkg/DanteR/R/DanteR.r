# Written by Tom Taverner <Thomas.Taverner@pnl.gov>
# for the U.S. Department of Energy (PNNL, Richland, WA, USA)
# Website: http://omics.pnl.gov/software
#
# Notice: This computer software was prepared by Battelle Memorial Institute,
# hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with the
# Department of Energy (DOE).  All rights in the computer software are reserved
# by DOE on behalf of the United States Government and the Contractor as
# provided in the Contract.
#
# NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR
# IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
#
# This notice including this sentence must appear on any copies of this computer
# software.


# To do: report formatting, workflows.
# Wizard.
# Work flow automation: Probably another window a la MDART.
# 
# "Loading" window

dante <- function(){

DanteR_author_string <- gsub("", "", "
# Written by Tom Taverner <Thomas.Taverner@pnl.gov> and Ashoka Polpitiya
# for the U.S. Department of Energy (PNNL, Richland, WA, USA)
# Website: http://omics.pnl.gov/software
#
# Notice: This computer software was prepared by Battelle Memorial Institute,
# hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with the
# Department of Energy (DOE).  All rights in the computer software are reserved
# by DOE on behalf of the United States Government and the Contractor as
# provided in the Contract.
#
# NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY, 
# EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF 
# THIS SOFTWARE.
#
# This notice including this sentence must appear on any copies of this computer
# software. ")      

DanteR_main_env <- new.env()
DanteR_main_env$list.of.tables <- list(History=TRUE)

GetTabNames <- function()
  names(.nb)
  
  # which session are we working in?
DanteR_main_env$current.session <- "DanteRSession" 
DanteR_main_env$starting.dir <- getwd()
DanteR_main_env$do.logging <- TRUE
DanteR_main_env$script.location <- "C:/R/addons"

# Set pretty print and global options
.GlobalEnv$.DanteROptions <- list(pretty_print=FALSE, limitRows=TRUE, maxRowsN = 10000, sprintf_format = "%.6G")

options(guiToolkit="RGtk2")
library(MASS)

if(.Platform$OS.type != "windows"){
  Filters <- NULL
}

make_history_panel <- function(cont){
  a = ggroup(cont=cont, horizontal=F)
  a1 = ggroup(cont=a, expand=F)
  b_open = gbutton("Open...", cont=a1, expand=F, handler = function(h, b) { 
    ff <- Filters[c("R", "All"),]
    fn <- file.path(getwd(), "*.R")
    f <- my_choose_files(fn, multi = F, filters=ff)
    if(length(f) && nchar(f) > 0) {
      txt <- paste(readLines(f, warn=F), collapse = "\n")
      if(nchar(txt) > 0) svalue(workflow_editor) <- txt
    }
  })
  b_save = gbutton("Save As...", cont=a1, expand=F, handler=  function(...){
    ff <- Filters[c("R", "All"),]
    fn <- file.path(getwd(), "*.R")
    f <- my_choose_files(fn, multi = F, filters=ff)
    if(length(f) && nchar(f) > 0) {    
      txt <- svalue(workflow_editor)
      write(txt, file = f)  
    }
  })
  b_rec = gbutton("Logging", cont=a1, expand=F, handler = function(h, b) { 
    if(b$getLabel() == "Logging"){
      b$setLabel("Paused")
      DanteR_main_env$do.logging <- FALSE
    } else {
      b$setLabel("Logging")  
      DanteR_main_env$do.logging <- TRUE
    }
    })                                     
  b_runsel = gbutton("Run Selected", cont=a1, expand=F, handler = function(...){
    tryCatch({
      pt <- parse(text=svalue(workflow_editor, drop=T))  
      names.before <- ls(); names.after <- NULL;
      tryCatch(eval(pt), error = function(e) quick_message(paste("Error in evaluating:", e)))        
      names.after <- ls()
      for(nam in setdiff(names.after, c(names.before, "names.after", "names.before"))){
        assign(nam, get(nam), envir=.GlobalEnv)
      }
      
    }, error = function(e) quick_message(paste("Couldn't parse text:", e)))
  })         
  b_run = gbutton("Run All", cont=a1, expand=F, handler = function(...){   
    tryCatch({
      pt <- parse(text=svalue(workflow_editor, drop=F))  
      names.before <- ls()    
      tryCatch(eval(pt), error = function(e) quick_message(paste("Error in evaluating:", e)))        
      names.after <- ls()
      for(nam in setdiff(names.after, c(names.before, "names.after", "names.before"))){
        assign(nam, get(nam), envir=.GlobalEnv)
      }
    }, error = function(e) quick_message(paste("Couldn't parse text:", e)))
  })         
  sapply(c(b_open, b_save, b_run, b_rec), function(x) getToolkitWidget(x)$setSizeRequest(100, -1))
  workflow_editor <- gtext(cont=a, expand=T)
  tv <- getToolkitWidget(workflow_editor) 
  tv$getParent()$setShadowType(as.integer(1))  
  tv$setIndent(25)
  tv$modifyBase(as.integer(0), as.GdkColor(c(198, 214, 253)*256))
  
  return(list(workflow_editor=workflow_editor, group=a))
}
  
log.handler <- function(cmd.string) if(DanteR_main_env$do.logging) insert(workflow_editor, cmd.string)

  # Local wrapper around run.dialog, we pass the log handler in, and the main window.
runDialog <- function(dlg, ...) {
  run.dialog(deparse(substitute(dlg)), 
    envir = environment(),
    #dlg.list = get(paste(deparse(substitute(dlg)), "dialog", sep=".")),
    var.browser=.vb, 
    parent.window=getToolkitWidget(.win), 
    do.logging=TRUE, log.handler=log.handler, ...)
  #update(.vb)
}
    
my.getwd <- function() gsub("(.*)/$", "\\1", getwd()) # for "C:/" we want to remove the end "/"

# reset wd

  # Load directory options from persistent file
.options.file.name <- file.path(getwd(), "DanteR_options.r")
if(file.exists(.options.file.name)){
  tryCatch({
    #source(.options.file.name, echo=TRUE) # in global        
    source(.options.file.name, local=TRUE)
    setwd(getwd()) # remove old
   }, error = function(e) {
    cat("Cannot source options file\n")
    print(e)
    setwd(getwd()) # remove old
  })
  #tryCatch(unlink(.options.file.name), error = function(e) cat("Failed to remove options file\n"))  
} else {
  cat("Options file does not exist\n")
}
          
#init.dialogs()
.STATUSBAR_DEFAULT_STRING <- paste("DanteR, v", installed.packages()["DanteR","Version"])
cat(paste(.STATUSBAR_DEFAULT_STRING, "\n", sep=""))
.win <- gwindow(.STATUSBAR_DEFAULT_STRING, width = 1000, height=600, visible=F, parent=c(100, 0))
parent.window <- getToolkitWidget(.win)
parent.window$setTitle(paste(DanteR_main_env$current.session, "- DanteR"))
#.win <<- .win   # globally putting .win and .vb in place

gSignalConnect(getToolkitWidget(.win), "destroy", function(...){
    # We've run the session console-less, so we just want to quit R
  if("no-console=TRUE"%in%commandArgs()){ 
    quit(save="no")
  } else {
    dialog <- gtkDialog("Quit R session?", NULL, NULL, "Save And Quit", 2, "Just Quit", 1, "gtk-no", 0, show = FALSE)   
    dialog$setPosition(GtkWindowPosition["center"])
     tryCatch({
       cmd.string <- paste("\nsetwd(",deparse(getwd()),
         "); DanteR_main_env$script.location <- ", 
         deparse(DanteR_main_env$script.location), ";", sep="")
       write(cmd.string, file=.options.file.name, append=FALSE)
         # Set the dir back to the starting dir so we write our .RData somewhere useful
       setwd(DanteR_main_env$starting.dir)
     }, error = function(e) print(e))
    hbox <- gtkVBoxNew()
    hbox2 <- gtkHBoxNew()
    hbox$packStart(gtkLabelNew("Do you want to quit from R?"), TRUE, TRUE, 10)
    hbox$packEnd(hbox2, FALSE, FALSE)
    dialog[["vbox"]]$packStart(hbox, TRUE, TRUE,0)
    dialog$setModal(TRUE)        

    user.choice <- dialog$run()
    if(user.choice==2){
      quit(save="yes")
    } else if(user.choice==1) {
      quit(save="no")    
    }
  }
  dialog$destroy()
})
.mainBigGroup <- ggroup(container=.win, expand=TRUE, horizontal=FALSE)

.mainGroup <- gpanedgroup(container=.mainBigGroup, expand=TRUE)
.vbfr <- gframe("Object browser", container=.mainGroup, expand=TRUE)

.rightGroup <- ggroup(horizontal=FALSE, container=.mainGroup, expand=TRUE)
#.nb <- gnotebook(container = .rightGroup, expand=TRUE, closebuttons=FALSE, dontCloseThese=1)
.nb <- gnotebook(container = .rightGroup, expand=TRUE, closebuttons=FALSE)
#.nb <<- .nb


hp <- make_history_panel(cont=NULL)

workflow_editor <- hp$workflow_editor
#workflow_editor <<- workflow_editor
workflow_tab_name <- "History"
add(.nb, hp$group, label = workflow_tab_name)
  # remove the first "x" in the notebook
nb <- getToolkitWidget(.nb)
#nb$getTabLabel(nb$getNthPage(0))$getChildren()[[2]]$getChildren()[[1]]$destroy()
log.handler(DanteR_author_string)
log.handler("\n")                              
gSignalConnect(getToolkitWidget(.nb), "page-removed", after=T, function(...){
  gtknb <- getToolkitWidget(.nb)
  if(!(is.null(gtknb))){
    idx <- svalue(.nb)
    DanteR_main_env$list.of.tables <- DanteR_main_env$list.of.tables[names(DanteR_main_env$list.of.tables)%in%GetTabNames()]
      
    #print(DanteR_main_env$list.of.tables)
    if(idx > 1){
      theDim <- DanteR_main_env$list.of.tables[[idx]]$getDimension()
      svalue(.sb) <- paste(paste(theDim[1], "R", sep=""), "x", paste(theDim[2], "C", sep="")) 
    } else {
      svalue(.sb) <- .STATUSBAR_DEFAULT_STRING
    }
  }
  FALSE
})

# Configure the table to ellipsize labels it adds, and put a popup menu on them

gSignalConnect(nb, "page-added", after=T, function(obj, child, page_num, user_data=NULL){
  lab <- nb$getTabLabel(nb$getNthPage(page_num))$getChildren()[[1]]$getChildren()[[1]]
  lab["ellipsize"] <- PangoEllipsizeMode["end"]
  lab$setWidthChars(12)  
  page_title = lab$getText()  
  lab$setTooltipText(page_title) 

  eb <- nb$getTabLabel(nb$getNthPage(page_num))$getChildren()[[1]]
  gSignalConnect(eb, "button-press-event", function(obj, event, user.data=list(nb=nb, page_title = page_title, page_num=page_num)){
    if (event[["button"]] == as.integer(3)){
      page_names <- c()
      for(jj in 0:(nb$getNPages()-1)){
        theLab = nb$getTabLabel(nb$getNthPage(jj))$getChildren()[[1]]$getChildren()[[1]]
        if(inherits(theLab, "GtkLabel"))
          page_names <- c(page_names, theLab$getText())
      }
      page_num <- as.integer(which(page_names%in%user.data$page_title) - 1)
      stopifnot(length(page_num) == 1 && is.integer(page_num) && !is.na(page_num))      
      m <- gtkMenu()    
      deleteItem <- gtkMenuItem("Remove From View")
      m$append(deleteItem)
      gSignalConnect(deleteItem, "activate", function(...) {
         user.data$nb$removePage(page_num)
      })
      gWidgetsRGtk2:::gtkMenuPopupHack(m, button = event$GetButton(), 
        activate.time = gdkEventGetTime(event))
      return(TRUE)
    } else {
      return(FALSE)
    }
  })      
 FALSE
})


if(0) gSignalConnect(getToolkitWidget(.nb), "switch-page", after=T, function(...){
  if(!(is.null(getToolkitWidget(.nb)))){
    idx <- svalue(.nb)
    if(idx > 1){
      theDim <- DanteR_main_env$list.of.tables[[idx]]$getDimension()
      svalue(.sb) <- paste(paste(theDim[1], "R", sep=""), "x", paste(theDim[2], "C", sep="")) 
    } else {
      svalue(.sb) <- .STATUSBAR_DEFAULT_STRING
    }
  }
  FALSE             
})
                 
.sb <- gstatusbar(.STATUSBAR_DEFAULT_STRING, cont=.win)
  # Called when we select something from .vb, the var browser
.putItInDisplay <- function(h, action){

  if(!nchar(h)) return()
  
  dataset.class <- NULL
  get.h <- safe.eval(h) # this fails for names with spaces
  dataset.class <- class(get.h)
  if(dataset.class == "recordedplot") {
    replayPlot(get.h)
  } else if(any(dataset.class %in% c("data.frame", "matrix"))) {
    if(h %in% GetTabNames()) { # it's in the notebook already, refresh it
      svalue(.nb) <- which(GetTabNames()%in%h)
      dispose(.nb)
    }
    if(0%in%dim(get.h)) {
      quick_message(paste("Can't display table of dimension", paste(paste(dim(get.h), collapse = " rows x "), " columns", sep="")))
    } else {
    
      on.exit(w$destroy()) # block concurrent user calls with a modal window
      w <- gtkWindowNew("Loading...", show=F)
      w$setDecorated(FALSE)
      w$add(gtkLabelNew("Loading..."))
      w$setPosition(GtkWindowPosition["center-on-parent"])
      w$setTransientFor(getToolkitWidget(.win))    
      w$setModal(TRUE)        
      w$showAll()        

#       maxRowsN <- .GlobalEnv$.DanteROptions$maxRowsN
#       nrowsh = dim(get.h)[1]
#        # give the user a choice to display row ranges
#       if(identical(.GlobalEnv$.DanteROptions$limitRows, TRUE) 
#         && is.numeric(maxRowsN) && is.numeric(nrowsh) 
#         && length(maxRowsN) == 1 && length(nrowsh) == 1 
#         && maxRowsN > 1 && nrowsh > maxRowsN){
#         rowsOpts <- unique(c(seq(from=0, to=nrowsh, by=maxRowsN), nrowsh))
#         rowsOpts <- cbind(rev(rev(rowsOpts)[-1])+1, rowsOpts[-1])
#         rowsOptsStr <- apply(rowsOpts, 1, paste, collapse="-")
#         user_row_choice <- menu(rowsOptsStr, TRUE, "Choose Help File")
#       }
       
      obj <- gtkDfEdit(h, dataset.name=h, pretty_print=.GlobalEnv$.DanteROptions$pretty_print, dataset.class = dataset.class, col.width=80, sprintf_format = .GlobalEnv$.DanteROptions$sprintf_format)
      DanteR_main_env$list.of.tables[[h]] <- obj
      #print(DanteR_main_env$list.of.tables)
      if(1) obj$setActionHandler("Selection", data=list(action=action), 
        handler=function(obj, row.idx, col.idx, data) {
           if(length(row.idx) > 1 || length(col.idx) > 1)
             svalue(data$action) <- 
               paste(paste(length(row.idx), "R", sep=""), "x", paste(length(col.idx), "C", sep="")) 
           else 
             svalue(data$action) <- paste(paste(obj$getDimension()[1], "R", sep=""), "x", paste(obj$getDimension()[2], "C", sep="")) 
           })
      a = as.gWidgetsRGtk2(obj)
      add(.nb, a, label=h)  
    }

  } else {
       # open it up with the editor, unless it's a list - users might click lists accidentally
     if(is.list(get.h)){
       quick_message("To edit a list, you can expand it to edit its components, or right-click and select Edit in R", caption = "List Selected")
     } else {
       x <- edit(get.h)
       cmd.string <- paste(".GlobalEnv$", h, " <- x", sep="")      
       eval(parse(text=cmd.string))
     }
    #warning("Can't load this object")
  }
}
                       
suppressWarnings(
  .vb <- gvarbrowser(container = .vbfr, expand=T, handler=function(h, data=NULL, ...) {
    .putItInDisplay(svalue(.vb), action=.sb)})) 

    # Our own version of .update to get around the intermittent problem where it stops updating
    # This is a hack till John updates gvarbrowser
setMethod(".update", 
  signature(toolkit="guiWidgetsToolkitRGtk2",object="gTreeRGtk"), where =.GlobalEnv,
  function (object, toolkit, ...)
{
  tryCatch({
  getWithDefault <- function (x, default)
  {
    if (is.null(x) || (is.atomic(x) && length(x) == 1 && is.na(x)))
        default           
     else x            
  }
    theArgs = list(...)
    offspring.data = theArgs$offspring.data
    if (is.null(offspring.data) && length(theArgs))
        offspring.data = theArgs[[1]]
    obj = object
    newchildren <- tag(obj, "offspring")(c(), offspring.data)
    newvalues <- newchildren
    stillThere <- c()
    isStillThere <- function(old, new)
        identical(old[1] %in% new[, 1, drop = TRUE] && old[2] %in% new[, 3, drop = TRUE], TRUE)
    i = 0
    remove.these = c()
    iter = tag(obj, "store")$GetIterFromString(i)
    while (iter$retval) { 
      treeValue = tag(obj, "store")$GetValue(iter$iter, 0 +
          tag(obj, "iconFudge"))$value
      treeValueType = tag(obj, "store")$GetValue(iter$iter,
          0 + 1 + tag(obj, "iconFudge"))$value
      if (isStillThere(c(treeValue, treeValueType), newvalues)) {
          stillThere <- c(stillThere, treeValue)
      }                 
      else {
          remove.these = c(remove.these, i)
      }
      i = i + 1
      iter = tag(obj, "store")$GetIterFromString(i)
    }
    if (length(remove.these) > 0) {
      for (i in rev(sort(remove.these))) {
          iter = tag(obj, "store")$GetIterFromString(i)
          tag(obj, "store")$Remove(iter$iter)
      }
    }
    didThese = newvalues[, 1, drop = TRUE] %in% stillThere
    newchildren = newchildren[!didThese, , drop = FALSE]
    if (nrow(newchildren) > 0) {
        getOffSpringIcons <- gWidgetsRGtk2:::getOffSpringIcons
        addChildren <- gWidgetsRGtk2:::addChildren
        lst = getOffSpringIcons(newchildren, tag(obj, "hasOffspring"),
            tag(obj, "icon.FUN"))
        newchildren = lst$children
        doExpand = lst$doExpand
        addChildren(tag(obj, "store"), newchildren, doExpand,
            tag(obj, "iconFudge"))
    }
  }, error = function(e){
    print("Error in update")
  })
})    

# Expand and collapse
if(0){
tv <- getToolkitWidget(.vb)
gSignalConnect(tv, "row-activated", function(tree.view, path, column, ...){
  model <- tree.view$getModel()
  iter <- model$getIter(path)
  if(iter$retval) {
    iter <- iter$iter
    the.item = model$get(iter,1)[[1]]
    if(exists(the.item, envir=.GlobalEnv)){
      get.item <- get(the.item, envir=.GlobalEnv)
        # only expand a list, not a data frame
      if(is.list(get.item) && is.null(dim(get.item))){
        if(tree.view$rowExpanded(path)){
          tree.view$collapseRow(path)
        } else {
          tree.view$expandRow(path, FALSE)
        }
      }
    }
  }
  FALSE
})
}

.LoadWorkSpace <- function(...) {
  

  ff <- Filters[c("RData", "All"),]
  fn <- my_choose_files(file.path(my.getwd(), "*.RData"), multi = F, filters = ff) 
  if(!length(fn)) return()
    
  current.session <- paste(rev(rev(strsplit(basename(fn), ".", fixed=T)[[1]])[-1]), collapse=".")

    if(nchar(current.session)>0) {                                                                 
    on.exit(w$destroy()) # block concurrent user calls with a modal window
    w <- gtkWindowNew("Loading...", show=F)
    w$setDecorated(FALSE)
    w$add(gtkLabelNew("Loading..."))
    w$setPosition(GtkWindowPosition["center-on-parent"])
    w$setTransientFor(getToolkitWidget(.win))    
    w$setModal(TRUE)        
    w$showAll()
    while(gtkEventsPending()) gtkMainIteration()  
  
    setwd(dirname(fn))
    load(fn, envir=.GlobalEnv)
    if(nchar(current.session) > 0) {
      getToolkitWidget(.win)$setTitle(paste(current.session, "- DanteR"))
      DanteR_main_env$current.session <- current.session
    }
    cmd.string <- paste("load(",deparse(fn),")", sep="")      
    log.handler(cmd.string)
  }
}

.LoadDNT <- function(...) {
    ff <- Filters[c("zip", "All"),]
    row.names(ff)[1] <- "dnt"
    ff[1,] <- c("Dante files (*.dnt)", "*.dnt")
    fn <- my_choose_files(file.path(my.getwd(), "*.dnt"), multi = F, filters = ff) 
    if(!length(fn)) return()    
    if(nchar(fn)>0) {
    
      setwd(dirname(fn))
      load(fn, envir=.GlobalEnv)
      local({
         # Need to set metadata tables
                               
        if(exists("factors")) { 
          tmp <-  as.data.frame(t(get("factors")))
          T_Column_Metadata <- data.frame(Data_Column = rownames(tmp), tmp, row.names=NULL, stringsAsFactors=FALSE)
          remove("factors")          
        }
        if(exists("ProtInfo")) {
          T_Row_Metadata <- get("ProtInfo")
          remove("ProtInfo")
        }
        if(exists("Eset")) {
          T_Data <- get("Eset")
          remove("Eset")
          if(exists("T_Column_Metadata"))
            attr(T_Data, "Column_Metadata") <- c(key = "Data_Column", table = "T_Column_Metadata")            
          if(exists("T_Row_Metadata"))
            attr(T_Data, "Row_Metadata") <- c(key = colnames(T_Row_Metadata)[1], table = "T_Row_Metadata")            
        }
                
      }, envir=.GlobalEnv)

       win.title <- strsplit(basename(fn), ".", fixed=T)[[1]][1]
       if(nchar(win.title) > 0) getToolkitWidget(.win)$setTitle(paste(win.title, "session in DanteR"))
       cmd.string <- paste("load(",deparse(fn),")", sep="")
       log.handler(cmd.string)
    }
    #if(nchar(fn)) .Execute(list("load", fn))
}

.CloseWorkSpace <- function(...) {
  rm(list=ls(envir=.GlobalEnv), envir=.GlobalEnv)  
  for(item in setdiff(GetTabNames(), workflow_tab_name)){
    svalue(.nb) <- 2
    dispose(.nb)
  }
  getToolkitWidget(.win)$setTitle("DanteR")  
}

.LoadCSV <- function(...) {
  ff <- Filters[c("All"),,drop=F]
  row.names(ff)[1] <- "csv"
  ff[1,] <- c("CSV files (*.csv)", "*.csv")
  fn <- my_choose_files(file.path(my.getwd(), "*.*"), multi = F, filters = ff)  
  if(!length(fn)) return()  
  aa <- basename(fn[[1]])
  newname <- strsplit(aa[length(aa)], "[.]")[[1]][1]
  assign(newname, read.csv(fn[[1]]), envir=.GlobalEnv)
}

.SaveWorkSpace <- function(...) {
    fn <- my_choose_files(file.path(my.getwd(), paste(DanteR_main_env$current.session, "Rdata", sep=".")), multi=F, type = "save", caption = "Save File (Must End in \".Rdata\")")
    if(!length(fn)) return()
    new.session <- paste(rev(rev(strsplit(basename(fn), ".", fixed=T)[[1]])[-1]), collapse=".")
    if(nchar(new.session) >0) {
      setwd(dirname(fn))
      save(list = ls(), file=fn)
      if(nchar(new.session) > 0) {
        getToolkitWidget(.win)$setTitle(paste(new.session, "- DanteR"))
        DanteR_main_env$current.session <- new.session
      }
      cmd.string <- paste("save(list = ls(), file=",deparse(fn),")", sep="")      
      log.handler(cmd.string)
    }    
}

.SaveItem <- function(...) {
  tryCatch({
    h = svalue(.vb)
    get.h <- eval(parse(text=h))
    if (class(get.h) %in% c("data.frame", "matrix", "table")){
        xx <- my_choose_files(file.path(my.getwd(), paste(h, "csv", sep=".")), multi=F)
        if(length(xx) && nchar(xx) > 0) write.csv(get.h, quote=F, file=xx)
      }
  }, error = function(e) {})
}

.removeIt <- function(h, ...){
  if(!nchar(h)) return()
  
   cmd.string <- NULL
   if( length(h) && nchar(h) ){
     if(exists(h, envir = .GlobalEnv)) {
       remove(list=h, envir = .GlobalEnv)
       cmd.string <- paste("remove(", deparse(h), ")", sep="")  
     } else {
       cmd.string <- paste(h, "<- NULL")
       eval(parse(text = paste(h, "<- NULL")), envir=.GlobalEnv)
     }
       # See if anything's been deleted by the command in the notebook
     for(item in setdiff(GetTabNames(), workflow_tab_name)){
       if(!object.exists(item)){
         svalue(.nb) <- which(GetTabNames()%in%item)
         dispose(.nb)
       }
     }
   }
   #update(.vb) # no, that's annoying
   log.handler(cmd.string)
}

# need to update this...
.renameIt <- function(h, ...){
  if(!nchar(h)) return()
  
   dlgHandler <- function(g, ...) {   
       newname <- svalue(dlg)   
       if(newname != h){
         tryCatch({
         cmd.string <- paste(".GlobalEnv$", newname, " <- eval(parse(text=", deparse(h), "))", sep="")
         eval(parse(text=cmd.string))              
         log.handler(cmd.string)                    
             # Handle name changes to metadata
         for(metadata_type in c("Row_Metadata", "Column_Metadata"))
           for (n in names(GetLinksToMetadata(h, metadata_type)))
             eval(parse(text=paste("attr(.GlobalEnv$", n, ", '", metadata_type, "')[['table']] <- '", newname, "'", sep="")))
                                                       
           .removeIt(h)
         }, error = function(e) print(e))
       }
         # remove the table if you renamed it
       if(exists(".nb") && h%in%setdiff(GetTabNames(), workflow_tab_name)){
         svalue(.nb) <- which(GetTabNames()%in%h)
         dispose(.nb)
       }
      dispose(dlg)
      #update(.vb)
   }
   dlg <- gedit(h, cont=gwindow(paste("Input new name of", h), height=10, parent=.win), expand=T, handler=dlgHandler)
   focus(dlg)
}

# need to update this...
.copyIt <- function(h, ...){
  if(!nchar(h)) return()
  
   dlg <- gedit(h, cont=gwindow(paste("Input name of copy", h), height=10, parent=.win), expand=T, 
     handler = function(g, ...) {
       if(svalue(dlg) != h){
         cmd.string <- paste(".GlobalEnv$", svalue(dlg), " <- eval(parse(text=", deparse(h), "))", sep="")
         eval(parse(text=cmd.string))   
         #update(.vb)            
         log.handler(cmd.string)
       }
       dispose(dlg)})
   focus(dlg)
}

.lst <- list()
.lst[['Show Item']]$handler <- function(h,...) .putItInDisplay(svalue(.vb), ...)
.lst$sep$separator <- 1 
.lst[['Save Item...']]$handler <- function(h,...) .SaveItem()
.lst$sep1$separator <- 1 
.lst$"Rename..."$handler <- function(h,...) .renameIt(svalue(.vb), ...)
.lst$Copy$handler <- function(h,...) .copyIt(svalue(.vb), ...)
.lst$sep15$separator <- 1
.lst$Delete$handler <- function(h,...) .removeIt(svalue(.vb), ...)
.lst$sep2$separator <- 1
.lst$"Properties..."$handler <- function(...) {
  h <- svalue(.vb)
  if(nchar(h)){
    h_obj <- safe.eval(h)
    theDim <- dim(h_obj)
    class_string <- paste("Object of class", paste(class(h_obj), collapse=", "))
    length_string <- paste("length(object) = ", length(h_obj))      
    class_string <- paste(class_string, length_string, sep="\n\n")    
    if(!is.null(theDim)){
      dim_string <- paste(paste(theDim[1], "rows", sep=" "), "x", paste(theDim[2], "columns", sep=" "))  
      class_string <- paste(class_string, dim_string, sep="\n\n")
      if(is.list(h_obj))
        cct = table(sapply(h_obj, class))
      else 
        cct = table(apply(h_obj, 2, class))
        
      cnames = paste("Columns:", paste(paste(names(cct), cct, sep=":"), collapse = "; "))
      class_string <- paste(class_string, cnames, sep="\n\n")            
    }
    quick_message(class_string, caption = "Object properties")
  }
}
.lst$"Summary..."$handler <- function(...) {
   h <- svalue(.vb)  
   if(nchar(h)) 
     tryCatch({
      x <- as.data.frame(summary(safe.eval(h)))
      dfedit(x)
      }, error = function(e) message("Couldn't place summary in a data frame"))
 }

.lst$sep3$separator <- 1   
.lst$"Use R edit"$handler <- function(...) {
   h <- svalue(.vb)  
   if(nchar(h)) {
     x <- safe.eval(h)   
     x <- edit(x)
     cmd.string <- paste(".GlobalEnv$", h, " <- x", sep="")      
     eval(parse(text=cmd.string))
   }
 }
.lst$sep4$separator <- 1   
.lst$"Refresh View"$handler <- function(h,...) {
  update(.vb)
}

.MenuBarHelpItems <- function(){
  .menubarlist <- DanteR_main_env$menubarlist
  .menubarlist$Help$"Open Manual"$handler <- function(...) {
    help.urls <- c("http://omics.pnl.gov/tutorials/DanteR_Help.pdf", 
    "http://prismwiki.pnl.gov/images/DAnTE_HelpDoc.pdf", 
    "http://omics.pnl.gov/tutorials/RGtk2Extras-manual.pdf", 
    "http://omics.pnl.gov/tutorials/RGtk2Extras-manual.pdf")
    help.choice <- menu(c("DanteR Help", "Previous DAnTE Help", "Dialog Maker Help", "Table Viewer Help"), TRUE, "Choose Help File")
    if(help.choice > 0)
      system(paste(.Options$pdf, help.urls[help.choice]), wait=FALSE)    
  }
  .menubarlist$Help$sep1$separator <- 1
  #.menubarlist$Help$"Live Help @ MeetingWords"$handler <- function(...) 
  #  system(paste("open", "http://meetingwords.com/DanteRHelp"), wait=FALSE)  
  #.menubarlist$Help$sep2$separator <- 1
  .menubarlist$Help$About$handler <- function(...){
	  dlg <- gtkAboutDialogNew(show=F)
    dlg["authors"] <- DanteR_author_string
          
    dlg["program-name"] <- "DanteR"
    dlg["comments"] <- "A proteomics front-end to R"
    dlg["copyright"] <- "GPLv2"
    dlg["version"] <- installed.packages()["DanteR","Version"]
    gtkDialogRun(dlg)
    gtkWidgetDestroy(dlg)
  }
  DanteR_main_env$menubarlist <- .menubarlist
}


.MenuBarFileItems <- function(){
  .menubarlist <- DanteR_main_env$menubarlist
  .menubarlist$File <- list()
  .menubarlist$File$"Import Tables" <- list()
  .menubarlist$File$"Import Tables"$"Excel/SQLite/CSV/TXT files..."$handler <- function(...) open.expression.wizard(log.handler=log.handler, .win=.win)
  .menubarlist$File$"Import Tables"$"Nonstandard Text Files..."$handler <- function(...) read_in_table(parent.window = getToolkitWidget(.win)) 

  .menubarlist$File$sep0$separator <- 1 
  .menubarlist$File[['Close Table']]$handler <- function(...) .removeIt(svalue(.vb), ...)
  .menubarlist$File[['Save Table...']]$handler <- function(...) .SaveItem()
  .menubarlist$File$sep1$separator <- 1 
  .menubarlist$File[['Load Workflow/Script...']]$handler  <- function(...) {
      ff <- Filters[c("R", "All"),]
      fn <- my_choose_files(file.path(my.getwd(), "*.R"), multi = F, filters = ff)
      if(!length(fn)) return()      
      tryCatch({
        source(fn, local=TRUE)
        }, error = function(e) quick_message(paste("An error occured:\n", e)))
   }
  .menubarlist$File[['View Workflow']]$handler <- function(...) {
#    shell(paste(.Options$edit, .log.file), wait=FALSE)      
  }
#  .menubarlist$File[['Clear Workflow']]$handler <- function(...) write("", file=.log.file, append=F)
#  .menubarlist$File[['Save Workflow/Script...']]$handler <- function(...) .removeIt(svalue(.vb), ...)
    
  .menubarlist$File$sep2$separator <- 1     
  .menubarlist$File[['Open Session']][['Open DanteR/R Session...']]$handler <- .LoadWorkSpace
  .menubarlist$File[['Open Session']]$sep1$separator <- 1
  .menubarlist$File[['Open Session']][['Open Old Dante Session...']]$handler <- .LoadDNT
  .menubarlist$File[['Save Session']]$handler <- function(...){
    current.session <- DanteR_main_env$current.session
    if(nchar(current.session)) save.image(paste(current.session, "RData", sep="."))
  }
  
  .menubarlist$File[['Save Session As...']]$handler <- .SaveWorkSpace    
  .menubarlist$File[['Close Session']]$handler <- .CloseWorkSpace    
  .menubarlist$File$sep3$separator <- 1            
  .menubarlist$File[['Export Tables...']]$handler <- function(...) runDialog(save_tables)
  #.menubarlist$File[['Open Qrollup (CSV)']]$handler <- function(...) runDialog(open.MS.csv)  
  #.menubarlist$File[['Open QRollup (Excel 2007)']]$handler <- function(...){
  #  runDialog(LoadQRollupXLSX)
  #  }    
  .menubarlist$File$sep5$separator <- 1        
  .menubarlist$File$Quit$handler <- function(...) {
    dialog <- gtkDialog("Quit?", NULL, "destroy-with-parent", "gtk-yes", 1, "gtk-no", 0, show = F)   
    hbox <- gtkHBoxNew()
    hbox$packStart(gtkLabelNew("Really quit DanteR?"), TRUE, TRUE, 10)
    dialog$setPosition(GtkWindowPosition["center"])    
    dialog[["vbox"]]$packStart(hbox, TRUE, TRUE,0)
    if(dialog$run() == 1)
      dispose(.win)
    dialog$destroy()  
  }
  .menubarlist$File$Quit$icon <- "quit"
    DanteR_main_env$menubarlist <- .menubarlist
}

.MenuBarDataItems <- function(){
  .menubarlist <- DanteR_main_env$menubarlist
  .menubarlist$Data$"Table Display Options..."$handler <- function(...) {    
    curr_values <- strsplit(.GlobalEnv$.DanteROptions$sprintf_format, "")[[1]]
    
    rv <- runDialog(list, dlg.list=list(
      title = "Table Display Options",
      label = "Set options for displaying data in tables.",
      pretty_print.trueFalseItem = .GlobalEnv$.DanteROptions$pretty_print, label = "Apply formatting? (Faster loading if false)",
      signal = c("default", "toggle.sensitive", "floatingPoint", "dp"),
      dp.integerItem = c(value=as.integer(curr_values[3]), from=0, to=9), label = "Decimal Places",
      floatingPoint.trueFalseItem = identical("G", curr_values[4]), label = "Floating point display?"#,
#      limitRows.trueFalseItem = TRUE, label = "Limit Maximum Rows to Display?",
#      signal = c("default", "toggle.sensitive", "maxRowsN"),
#        maxRowsN.integerItem = c(from=1, value=.GlobalEnv$.DanteROptions$maxRowsN, to=1E6, by=1), label = "Maximum Rows", indent=10
    ), auto.assign=FALSE)
    if(!is.null(rv)){
       .GlobalEnv$.DanteROptions$pretty_print <- rv$args$pretty_print   
#       .GlobalEnv$.DanteROptions$limitRows <- rv$args$limitRows
#       .GlobalEnv$.DanteROptions$maxRowsN <- rv$args$maxRowsN     
         # Create the format string    
       fmtLtr <- "f"
       if (identical(rv$args$floatingPoint, TRUE)) fmtLtr <- "G"
       .GlobalEnv$.DanteROptions$DanteR_sprintf_format <- paste("%.", rv$args$dp, fmtLtr, sep="")
    }
  }
  .menubarlist$Data$sep0$separator = TRUE    
  .menubarlist$Data$"Merge..."$handler <- function(...) runDialog(Merge)          
  .menubarlist$Data$"Merge..."$icon <- "merge"  
  .menubarlist$Data$"Filter..."$handler <- function(...) runDialog(Filter)  
  .menubarlist$Data$"Filter..."$icon <- "filter"
  
  .menubarlist$Data$"Aggregate..."$handler <- function(...) runDialog(Aggregate)    
  .menubarlist$Data$"Aggregate..."$icon <- "aggregate"  

  .menubarlist$Data$"Smoosh..."$handler <- function(...) runDialog(Smoosh, auto.assign=FALSE)    
  .menubarlist$Data$"Smoosh..."$icon <- "smoosh"
  
  .menubarlist$Data$sep1$separator = TRUE
  .menubarlist$Data[["Sort..."]]$handler <- function(...) 
    if(svalue(.nb) > 1) 
      DanteR_main_env$list.of.tables[[GetTabNames()[svalue(.nb)]]]$sort()
  .menubarlist$Data[["Edit Factors..."]]$handler <- function(...) 
    if(svalue(.nb) > 1) 
       DanteR_main_env$list.of.tables[[GetTabNames()[svalue(.nb)]]]$editFactors()
  .menubarlist$Data$sep1$separator <- 1     
  .menubarlist$Data[["Insert Column"]]$handler <- function(...) {
    if(svalue(.nb) > 1) {
      obj <- DanteR_main_env$list.of.tables[[GetTabNames()[svalue(.nb)]]]
      idx <- obj$getSelection()$columns
      if(length(idx))
        obj$doTask(list(list(func="InsertNAColumns", args=list(idx=idx[1]+1))))
    }
  }
  .menubarlist$Data[["Insert Row"]]$handler <- function(...) {
    if(svalue(.nb) > 1) {
      obj <- DanteR_main_env$list.of.tables[[svalue(.nb)]]
      idx <- obj$getSelection()$rows
      if(length(idx))
        obj$doTask(list(list(func="InsertNARows", args=list(idx=idx[1]))))
    }
  }
  .menubarlist$Data$sep2$separator <- 1       
  .menubarlist$Data[["Apply Command To Selection..."]]$handler <- function(...) 
    if(svalue(.nb) > 1) DanteR_main_env$list.of.tables[[svalue(.nb)]]$commandData()
  .menubarlist$Data[["Undo..."]]$handler <- function(...) 
    if(svalue(.nb) > 1) DanteR_main_env$list.of.tables[[svalue(.nb)]]$undo()
  .menubarlist$Data$sep3$separator <- 1       
  .menubarlist$Data[["Summary Table"]]$handler <- function(h, ...) {
    if(svalue(.nb) > 1){
      obj <- DanteR_main_env$list.of.tables[[svalue(.nb)]]
      obj.summary <- as.matrix(summary(obj$getDataFrame()))
      class(obj.summary) <- "matrix"
      obj.summary.name <- paste(obj$getDatasetName(), "summary", sep=".")
      assign(obj.summary.name, obj.summary, envir=.GlobalEnv)
      .putItInDisplay(obj.summary.name, action=.sb)
    }
  }  
    DanteR_main_env$menubarlist <- .menubarlist
}

# Assume that we're putting a .png into doc/
init.icons <- function(icon.file.path = file.path(.Library, "DanteR/doc"), loadDanteIcon=TRUE){  
  all.files <- dir(icon.file.path)  
  all.paths <- dir(icon.file.path, full.names = TRUE)  
  if(length(all.files) > 0){
  
    icon.idx <- gregexpr("[.](png|PNG)$", all.files) > -1
    
    icon.paths <- all.paths[icon.idx]  
    icon.files <- all.files[icon.idx]    
    function.names <- substr(icon.files, 1, nchar(icon.files)-4)
    addStockIcons(function.names, icon.paths)
              
    if(loadDanteIcon)
    tryCatch({ # fail silently
      pb <- gdkPixbuf(50, 50, file.path(icon.file.path, "dante.png"))$retval
      if(!is.null(pb)) getToolkitWidget(.win)$setIcon(pb)
    }, error = function(e) {})              
  } else {
    #cat("DanteR can't read your icons directory. Icons may not display correctly\n")
  }

}


.MenuBarRScripts <- function(){
  .menubarlist <- DanteR_main_env$menubarlist

  .menubarlist$"Metadata"$"Define Factors (Column Metadata)"$handler <- function(...) {
    rv <- runDialog(Column_Metadata) 
     
    if(!is.null(rv) && object.exists(as.character(rv$args$dataset))) {  
      ds <- safe.eval(as.character(rv$args$dataset)) 
      cmd <- attr(ds, "Column_Metadata")[["table"]]
      if(object.exists(cmd)){
        .putItInDisplay(cmd, action=.sb)
      }
    }
  }
  .menubarlist$"Metadata"$"Define Factors (Column Metadata)"$icon <- "define-factors" 

  .menubarlist$"Metadata"$"Define Row Metadata"$handler <- function(...) {
    if(exists("T_Row_Metadata")) {
      .putItInDisplay("T_Row_Metadata", action=.sb)
    } else {
      # create factors
      runDialog(Row_Metadata)
    }
  } 
  .menubarlist$Metadata$sep0$separator <- 1       
  .menubarlist$"Metadata"$"Link To Metadata"$handler <- function(...)  runDialog(Link_To_Metadata, auto.assign=F)
  .menubarlist$"Metadata"$"Plot All Links"$handler <- function(...)  runDialog(Plot_All_Links)
   
  .menubarlist$Metadata$sep1$separator <- 1
  .menubarlist$Metadata$"Create Data Aliases"$handler <- function(...)  runDialog(create.aliases)  
  .menubarlist$Metadata$"Create Data Aliases"$icon <- "alias"
  .menubarlist$Metadata$"Apply Aliases"$handler <- function(...)  runDialog(update.aliases)
  
  .menubarlist$"Pre-Process"$"Transform Data"$handler <- function(...) runDialog(LogT)
  .menubarlist$"Pre-Process"$"Transform Data"$icon <- "LogTransform"
  .menubarlist$"Pre-Process"$sep1$separator <- 1    
  .menubarlist$"Pre-Process"$Normalization <- list()    
  .menubarlist$"Pre-Process"$Normalization$"Using Linear Regression"$handler <- function(...) runDialog(LR)  
  .menubarlist$"Pre-Process"$Normalization$"Using Eigenvalues (EigenMS)"$handler <- function(...) runDialog(eigen_norm)  
  .menubarlist$"Pre-Process"$Normalization$"Using LOESS"$handler <- function(...) runDialog(LoessNormalization)
  .menubarlist$"Pre-Process"$Normalization$"Using LOESS"$icon <- "LoessNormalization"
  .menubarlist$"Pre-Process"$Normalization$"Using Data Quantiles"$handler <- function(...) runDialog(QuantileN)        
  .menubarlist$"Pre-Process"$sep2$separator <- 1      
  .menubarlist$"Pre-Process"$"Median Absolute Deviation"$handler <- function(...) runDialog(MAD)          
  .menubarlist$"Pre-Process"$"Central Tendency"$handler <- function(...) runDialog(CentralTendency)    
  .menubarlist$"Pre-Process"$sep3$separator <- 1        
  .menubarlist$"Pre-Process"$Imputation <- list()  
  .menubarlist$"Pre-Process"$Imputation$"Impute Missing Values"$handler <- function(...) runDialog(ImputeData)  
  .menubarlist$"Pre-Process"$Imputation$"Model Based Filter/Impute/ANOVA"$handler <- function(...) runDialog(CensorANOVA)  
#  .menubarlist$"Pre-Process"$"Tamu Imputation"$handler <- function(...) runDialog(TamuQ)  
  
  .menubarlist$"Rollup"$"Using a Reference Peptide(RRollup)"$handler <- function(...) runDialog(RRollup)
  .menubarlist$"Rollup"$"Using a Reference Peptide(RRollup)"$icon <- "RRollup"      
  .menubarlist$"Rollup"$"Top 2/3 Peptides (QRollup)"$handler <- function(...) runDialog(QRollup)  
  .menubarlist$"Rollup"$"Top 2/3 Peptides (QRollup)"$icon <- "QRollup"        
  .menubarlist$"Rollup"$"Median Centered and Scaled (ZRollup)"$handler <- function(...) runDialog(ZRollup)    
  .menubarlist$"Rollup"$"Median Centered and Scaled (ZRollup)"$icon <- "ZRollup"          
#  .menubarlist$"Rollup"$"Using Top Peptides (QRollup)"$handler <- function(...) runDialog(QRollup)    
#  .menubarlist$"Rollup"$"Using Top Peptides (QRollup)"$icon <- "QRollup"        
#  .menubarlist$"Rollup"$"Median/StDev Scaling (ZRollup)"$handler <- function(...) runDialog(ZRollup)    
#  .menubarlist$"Rollup"$"Median/StDev Scaling (ZRollup)"$icon <- "ZRollup"          
# 
  .menubarlist$"Statistics"$"ANOVA"$handler <- function(...) runDialog(ANOVA)
  .menubarlist$"Statistics"$"ANOVA"$icon <- "anova"
  .menubarlist$"Statistics"$"Model Based Filter/Impute/ANOVA"$handler <- function(...) runDialog(CensorANOVA)
  .menubarlist$"Statistics"$"Calculate Fold Changes"$handler <- function(...) runDialog(FoldChanges)  
  .menubarlist$"Statistics"$sep4$separator <- 1  
  .menubarlist$"Statistics"$"Shapiro-Wilks Test"$handler <- function(...) runDialog(ShapiroWilks)  
  .menubarlist$"Statistics"$"Kruskal-Walis Test"$handler <- function(...) runDialog(KruskalWalis)  

#  .menubarlist$"Statistics"$sep4$separator <- 1
#  .menubarlist$"Statistics"$"Peptide ANOVA"$handler <- function(...) runDialog(PepAnova)  
#  .menubarlist$"Statistics"$"Peptide ANOVA"$icon <- "nova"
  .menubarlist$"Statistics"$"Count Fisher Test"$handler <- function(...) runDialog(Fisher)    
  .menubarlist$"Statistics"$"Count Fisher Test"$icon <- "fisher"
  .menubarlist$"Statistics"$sep1$separator <- 1      
  .menubarlist$"Statistics"$"P-Value Adjustment"$handler <- function(...) runDialog(PAdj)       
  .menubarlist$"Statistics"$sep2$separator <- 1    
  .menubarlist$"Statistics"$"Split Significant Up/Down"$handler <- function(...) runDialog(DiffExp)
  .menubarlist$"Statistics"$"Split Significant Up/Down"$icon <- "summarize"
    

  .menubarlist$"Plot"$"New Plot Window"$handler <- function(...) X11()
  .menubarlist$"Plot"$"Switch Active Window"$handler <- function(...) dev.set()
  .menubarlist$"Plot"$"Plot Options"$handler <- function(...) runDialog(setpar, auto.assign=FALSE)
  .menubarlist$"Plot"$NewWin$separator <- 1

  .menubarlist$"Plot"$"Matrix/Scatter Plot"$handler <- function(...) runDialog(DataPlot)
  .menubarlist$"Plot"$"Matrix/Scatter Plot"$icon <- "scatter"
  .menubarlist$"Plot"$"Plot 3D"$handler <- function(...) runDialog(Plot3D)  
  .menubarlist$"Plot"$sepXY$separator <- 1

  .menubarlist$"Plot"$"Histograms"$handler <- function(...) runDialog(Histogram)    
  .menubarlist$"Plot"$"Histograms"$icon <- "hist"
  .menubarlist$"Plot"$"Boxplots"$handler <- function(...) runDialog(Boxplots)  
  .menubarlist$"Plot"$"Boxplots"$icon <- "Boxplots"

  .menubarlist$"Plot"$"Q-Q Plot"$handler <- function(...) runDialog(QQplot)  
  .menubarlist$"Plot"$"Q-Q Plot"$icon <- "QQ"
  .menubarlist$"Plot"$sepQQ$separator <- 1

  .menubarlist$"Plot"$"Correlation Heatmap"$handler <- function(...) runDialog(CorrelationHeatmap)
  .menubarlist$"Plot"$"Correlation Heatmap"$icon <- "CorrelationPlot"

  .menubarlist$"Plot"$"Correlation Ellipses"$handler <- function(...) runDialog(CorrelationEllipses)  
  .menubarlist$"Plot"$"Correlation Ellipses"$icon <- "ellipse"
  .menubarlist$"Plot"$sepCorr$separator <- 1
  
  .menubarlist$"Plot"$"Venn Diagrams"$handler <- function(...) runDialog(VennPlot)
  .menubarlist$"Plot"$"Venn Diagrams"$icon="venn"

  .menubarlist$"Plot"$sep1$separator <- 1           
  .menubarlist$"Plot"$"Volcano Plot"$handler <- function(...) runDialog(VolcanoPlot)
  .menubarlist$"Plot"$"Volcano Plot"$icon <- "volcanoplot1"
  
  .menubarlist$"Plot"$"Plot Against Row Means"$handler <- function(...) runDialog(PlotDataAgainstMean)    
  .menubarlist$"Plot"$"Plot Against Row Means"$icon <- "plot_against_row_means"

#  .menubarlist$"Plot"$sep3$separator <- 1
#  .menubarlist$"Plot"$"Merge Tables"$handler <- function(...) runDialog(Merge)
  
  .menubarlist$Explore$"Clustering..."$icon <- "HeatmapClusters"
  .menubarlist$Explore$"Pattern Search..."$handler <- function(...) {
     if(!is.null(runDialog(PatternScore)$retval))
       runDialog(list, dlg.list=PlotRows.dialog, auto.assign=F)
  }
  .menubarlist$Explore$"Pattern Search..."$icon <- "PatternScore"
  .menubarlist$Explore$sep1$separator=T
  .menubarlist$Explore$"PCA/PLS Plot"$handler <- function(...) runDialog(PCAplot)    
  .menubarlist$Explore$"PCA/PLS Plot"$icon <- "PCAplot"  
  .menubarlist$Explore$"Clustering..."$handler <- function(...) runDialog(HeatmapClusters)        
  .menubarlist$Explore$"Dynamic Row Plot"$handler <- function(...) runDialog(list, dlg.list=PlotRows.dialog, auto.assign=F)      
  .menubarlist$Explore$"Dynamic Row Plot"$icon <- "crosslink_icon"
  DanteR_main_env$menubarlist <- .menubarlist
}

script.location <- DanteR_main_env$script.location

install_addons <- function(script.location = script.location){
  .menubarlist <- DanteR_main_env$menubarlist
  .menubarlist$"Addons"$"Change Addons Location..."$handler <- function(...) {
    script.location <- my_choose_files(script.location, type = "selectdir")
    if(!is.na(script.location)) {
      DanteR_main_env$menubarlist$"Addons" <- list()
      install_addons(script.location)
      DanteR_main_env$script.location <- script.location
      delete(.win, DanteR_main_env$.gm)
      DanteR_main_env$.gm <- gmenu(DanteR_main_env$menubarlist, cont = .win)      
    }
  }
  .menubarlist$"Addons"$"Change Addons Location..."$icon <- "open-file"
  .menubarlist$"Addons"$"Refresh"$handler <- function(...) {
    DanteR_main_env$menubarlist$"Addons" <- list()
    install_addons(script.location)
    delete(.win, DanteR_main_env$.gm)
    DanteR_main_env$.gm <- gmenu(DanteR_main_env$menubarlist, cont = .win)
  }
  .menubarlist$"Addons"$"Refresh"$icon <- "refresh"  
  .menubarlist$"Addons"$sep1$separator <- 1   
  
  if(!is.null(script.location) && file.exists(script.location)){    
    Rfiles.full.names <- dir(path = script.location, pattern="[rR]$", recursive=TRUE, full.names=TRUE)
    Rfiles.names <- dir(path = script.location, pattern="[rR]$", recursive=TRUE, full.names=FALSE)    
    if(length(Rfiles.full.names)){
        # source files locally and look for "<func.name>.dialog"
      for (i in 1:length(Rfiles.full.names)){
        path_here <- Rfiles.names[i]
        full_path_here <- Rfiles.full.names[i]
        cat(paste("Loading addons from ", full_path_here, ": ", sep=""))
        tryCatch({
          curr_objects <- ls()
          source(full_path_here, local=TRUE, verbose=FALSE) # for finding functions          
          source(full_path_here, verbose=FALSE) # for finding dialogs
          addons_name <- ".menubarlist$Addons"
          path_dirs <- rev(rev(strsplit(path_here, .Platform$file.sep)[[1]])[-1])
          if(length(path_dirs)) addons_name <- paste(addons_name, paste(path_dirs, collapse="$"), sep="$")            
              # new code
          new_objects <- setdiff(ls(), curr_objects)
          for(fn in new_objects){
            fn.dlg <- paste(fn, "dialog", sep=".")  # use name
            silent.fn.dlg <- paste("", fn.dlg, sep=".")  # use silent name        
            if(is.function(get(fn))){
              if(exists(silent.fn.dlg) && is.list(get(silent.fn.dlg))) fn.dlg <- silent.fn.dlg                    
              if(exists(fn.dlg) && is.list(get(fn.dlg))){
                cat(paste(fn, ""))
                title <- get(fn.dlg)$title # menu title
                if (is.null(title)) title <- fn                
                ts <- paste(paste(addons_name, deparse(title), "handler", sep="$"), 
                  " <- function(...) runDialog(", fn, ")", sep="")
                eval(parse(text=ts), envir=environment())
                  # add an icon of the form <func.name>.png
                if(fn%in%names(getStockIcons())){
                  ts <- paste(paste(addons_name, deparse(title), "icon", sep="$"), 
                    " <- ", deparse(fn), sep="")
                  eval(parse(text=ts), envir=environment())                      
                  }
          }}}                              
          cat("\n")
          cmd.string <- paste("source(",deparse(path_here),")", sep="")
          log.handler(cmd.string)                
        }, error = function(e) print(e))
      }        
      init.icons(script.location, loadDanteIcon=FALSE)
        # Now add menu items
#      for(fn in ls()) {    
#        fn.dlg <- paste(fn, "dialog", sep=".")  # use name
#        silent.fn.dlg <- paste("", fn.dlg, sep=".")  # use silent name        
#        if(is.function(get(fn))){
#          if(exists(silent.fn.dlg) && is.list(get(silent.fn.dlg))) fn.dlg <- silent.fn.dlg                    
#          if(exists(fn.dlg) && is.list(get(fn.dlg))){
#            title <- get(fn.dlg)$title # menu title
#            if(is.null(title)) title <- fn
#            
#            ts <- paste(paste(".menubarlist$Addons", deparse(title), "handler", sep="$"), 
#              " <- function(...) runDialog(", fn, ")", sep="")
#            eval(parse(text=ts), envir=environment())
#              # add an icon of the form <func.name>.png
#            if(fn%in%names(getStockIcons())){
#              ts <- paste(paste(".menubarlist$Addons", deparse(title), "icon", sep="$"), 
#                " <- ", deparse(fn), sep="")
#              eval(parse(text=ts), envir=environment())        
#            }
#          }
#        }
#      }
    } else {
      cat("Updating addons: not found.\n")
    }
  } #
  DanteR_main_env$menubarlist  <- .menubarlist
  
}

.MenuBarAddons <- function(script.location){
  install_addons(script.location)
}

.create.menubarlist <- function(){
  DanteR_main_env$menubarlist <- list()
  .MenuBarFileItems()
  #.menubarlist <- LoadRFiles(.menubarlist)
  .MenuBarDataItems()
  .MenuBarRScripts()  
  .MenuBarAddons(DanteR_main_env$script.location)    
  .MenuBarHelpItems()

  DanteR_main_env$menubarlist
}

create.toolbar <- function(){
  tbl <- list()
  tbl$Open <- list(label="Open Session", icon="new-file",handler = .LoadWorkSpace)  
  tbl$Dir <- list(label="Save Session", icon="save-file-1",handler = .SaveWorkSpace)        
  tbl$sep0$separator <- T  
  tbl$Save <- list(label="Open Table", icon="open-file",handler = function(...) open.expression.wizard(log.handler=log.handler, .win=.win))        
  #tbl$Save <- list(label="Open Table", icon="open-file",handler = function(...) read_in_table(parent.window=getToolkitWidget(.win)))    
  tbl$New <- list(label="Save Dataframe", icon="save-file-2",handler = function(...) .SaveItem())
  tbl$New2 <- list(label="New Dataframe", icon="matrix",handler = function(...) runDialog(NewDataFrame))  
  tbl$Quit <- list(label="Delete Dataframe", icon="bigredx",handler = function(...) .removeIt(svalue(.vb), ...))

  tbl$sep0.5$separator <- T
  tbl$factors <- list(label="Define Factors", icon="define-factors", handler = function(...) {
    if(exists("T_Column_Metadata")) {
      .putItInDisplay("T_Column_Metadata", action=.sb)
    } else {
      # create factors
      runDialog(Column_Metadata)
    }    
  })
      
  tbl$sep1$separator <- T
  tbl$Transform <- list(label="Log Transform", icon="LogTransform", handler = function(...) runDialog(LogT))  
  tbl$LOESS <-  list(label="LoessNormalization", icon="LoessNormalization", handler = function(...) runDialog(LoessNormalization))
  tbl$sep2$separator <- T  
  tbl$RRollup <-  list(label="RRollup", icon="RRollup2", handler = function(...) runDialog(RRollup))    
#  tbl$ZRollup <-  list(label="ZRollup", icon="ZRollup", handler = function(...) runDialog(QRollup))    
#  tbl$QRollup <-  list(label="QRollup", icon="QRollup", handler = function(...) runDialog(ZRollup))       
  tbl$sep3$separator <- T  
  tbl$anova <- list(label="Do ANOVA", icon="anova", handler = function(...) runDialog(ANOVA))
  tbl$sep3.5$separator <- T 
  tbl$Scatter <- list(label="Scatter Plot", icon="scatter", handler = function(...) runDialog(DataPlot))            
  tbl$Histogram <-  list(label="Histograms", icon="hist", handler = function(...) runDialog(Histogram))            
  tbl$Boxplot <- list(label="Boxplots", icon="Boxplots", handler = function(...) runDialog(Boxplots))                  
  tbl$QQ <-  list(label = "Q-Q Plots", icon="QQ", handler = function(...) runDialog(QQplot))              
  tbl$sep3.6$separator <- T   
  tbl$CorrelationPlot <- list(label="CorrelationPlot", icon="CorrelationPlot", handler = function(...) runDialog(CorrelationHeatmap))              
  tbl$Venn <- list(label="Venn Diagrams", icon="venn", handler = function(...) runDialog(VennPlot))           
  tbl$sep4$separator <- T    
  
  tbl$HeatmapClusters <- list(label="Heatmap Clustering", icon="HeatmapClusters", handler = function(...) runDialog(HeatmapClusters))                
  tbl$PCA <- list(label="Plot Principal Components", icon="PCAplot", handler = function(...) runDialog(PCAplot))
  tbl$PlotRows <- list(label="Dynamic Row Plot", icon="crosslink_icon", handler = function(...) runDialog(list, dlg.list=PlotRows.dialog, auto.assign=F))
  tbl$sep5$separator <- T      
  tbl$VolcanoPlot <- list(label="Volcano Plot", icon="volcanoplot1", handler = function(...) runDialog(VolcanoPlot))  
  tbl$SplitSignificant <- list(label="Summarize Up/Down Expressed", icon="summarize", handler = function(...) runDialog(DiffExp))
    
  tb <- gtoolbar(tbl)
  tb.label <- lapply(tbl, "[[", "label")
  gg <- getToolkitWidget(tb)
  for(jj in 1:gg$getNItems()) {
    gg[[jj]]$setTooltipText(tb.label[[jj]])
  }
  gg$setStyle(as.integer(0))  
  return(tb)
}                                                                   

#menu.names <- setdiff(names(svalue(gm)), c("File", "Help"))

          
for (path in .libPaths()) init.icons(file.path(path, "DanteR/doc"))
#init.icons(file.path(.Library, "DanteR/doc"))
#init.icons(file.path(Sys.getenv("R_LIBS_USER"), "DanteR/doc"))
#
.menubarlist <- .create.menubarlist()
DanteR_main_env$.gm <- gmenu(.menubarlist, cont = .win)
#.gm <<- DanteR_main_env$.gm

tb <- create.toolbar()
add(.win, tb)

tryCatch(add3rdmousepopupmenu(.vb, .lst), error= function(e){})

gtkWindowPresent(getToolkitWidget(.win))
svalue(.mainBigGroup) <- 0.8
getToolkitWidget(.win)$present()
# end of env statement
invisible(list(win=.win, env=DanteR_main_env))
}