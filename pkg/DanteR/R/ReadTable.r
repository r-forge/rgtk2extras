# Dialog for reading in tables
#

# Options dialog menu

dfedit.options <- function(...){}

dfedit.options.dialog <- list(
   title = "Options", label = "Options for Data Frame Editor",
   dfedit.column.width.integerItem = c(value=64, from=20, to=256), label = "Column Width",
   dfedit.rownames.width.integerItem = c(value=64, from=20, to=256), label = "Row Names Column Width",
   dfedit.copy.col.names.trueFalseItem = FALSE, label = "Copy Column Names",
   dfedit.copy.row.names.trueFalseItem = FALSE, label = "Copy Row Names",
   dfedit.paste.col.names.trueFalseItem = FALSE, label = "Paste Column Names",
   dfedit.paste.row.names.trueFalseItem = FALSE, label = "Paste Row Names"
)

read_in_table <- function(handler=NULL, user.data=NULL, parent.window=NULL){
## read table dialog
  dialog <- gtkDialog("Open Table", NULL, c("destroy-with-parent"), "gtk-ok", 1, "gtk-cancel", 0, show = F)
  dialog$setPosition(GtkWindowPosition["center-on-parent"])              
  dialog$setTransientFor(parent.window)          
  
  #dialog$setDecorated(FALSE)
  box0 <- gtkVBoxNew(FALSE, 5)
  
  fr1 <- gtkFrameNew(label="Choose File")
  box1 <- gtkHBoxNew(FALSE, 5)
  
  entry1 <- gtkEntryNew()
  label1 <- gtkLabelNew("Lines To Preview:")
  entry1$setAlignment(0)
  button1 <- gtkButtonNewWithLabel("Select File...")
  box1$packStart(entry1, TRUE, TRUE, 0)
  box1$packStart(button1, FALSE, FALSE, 0)

  gSignalConnect(button1, "clicked", data=entry1, function(obj, label){
    tryCatch({
      f <- my_choose_files(file.path(getwd(), "*.*"), multi=F)
      if(nchar(f) > 0){
        setwd(dirname(f))
        # modify settings
        ext <- strsplit(f, "[.]")[[1]]
        ext <- ext[length(ext)]
        if(ext == "csv"){
          sep1$setActive(2)
        } else if (ext == "txt") {
          sep1$setActive(3)
        } else if (ext == "prn") {
          sep1$setActive(1)
        } else {
          sep1$setActive(0)
        }
        
        entry1$setText(f)
        preview.txt <- paste(readLines(con = f, n = nlines1$getValue()), collapse="\n")
        tb$getBuffer()$setText(preview.txt)
      }
    }, error = function(e){print(e)})
  })
  
  nlines1_adj <- gtkAdjustment(5, 1, 1000, 1, 1)
  nlines1 <- gtkSpinButton(nlines1_adj, 1.0, 0)
  nlines1$setValue(5)
  box1$packEnd(nlines1, FALSE, FALSE, 0)
  box1$packEnd(label1, FALSE, FALSE, 0)
  
  fr1$add(box1)


  box1 <- gtkHBoxNew(FALSE, 5)

  fr0 <- gtkFrameNew(label="File Preview")
  scroll <- gtkScrolledWindowNew()
  scroll$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
  
  tb <- gtkTextViewNew()
  tb$setEditable(FALSE)
  tb$setSizeRequest(400, 100)
  scroll$add(tb)
  fr0$add(scroll)

  tb$getBuffer()$setText("No File Loaded")

  fr2 <- gtkFrameNew(label="Table Read Options")
  
  box2 <- gtkVBoxNew(FALSE, 5)

  sep.types = list("Whitespace" = "", "Space (\" \")" = " ", "Comma (\",\")" = ",", "Tab (\"\t\")" = "\t", "Semicolon (\";\")" = ";")

  sep1 <- gtkComboBoxNewText()
  for (ll in names(sep.types)) sep1$appendText(ll)
  if (.Options$dec[[1]]  == ".") sep1$setActive(2)
  else sep1$setActive(4)
  
  hbox <- gtkHBoxNew(FALSE, 0)
  hbox$add(gtkLabelNew("File Separator Type"))
  hbox$add(sep1)
#  box2$packStart(hbox, FALSE, FALSE, 0)

  dec1 <- gtkComboBoxNewText()
  dec1$setSizeRequest(50, -1)
  for (ll in c(".", ",")) dec1$appendText(ll)
  if (.Options$dec[[1]]  == ".") dec1$setActive(0)
  else dec1$setActive(1)  
#  hbox <- gtkHBoxNew(FALSE, 0)
  hbox$add(gtkLabelNew("Decimal Point"))
  hbox$add(dec1)
  box2$packStart(hbox, FALSE, FALSE, 0)


  opt.box <- gtkHBoxNew(FALSE, 0)
  opt.box1 <- gtkVBoxNew(FALSE, 0)
  opt.box2 <- gtkVBoxNew(FALSE, 0)
  opt.box$add(opt.box1)
  #opt.box$add(opt.box2)
  box2$add(opt.box)
  
  opt.table <- gtkTableNew(4, 2, homogeneous=FALSE)
  opt.table$setRowSpacings(5)
  opt.box$packEnd(opt.table, TRUE, TRUE, 5)


  row.names0 <- gtkCheckButtonNewWithLabel(label="File Contains Row Labels")

#  row.names0$setActive(TRUE)
  opt.box1$packStart(row.names0, FALSE, FALSE, 0)
  
  header1 <- gtkCheckButtonNewWithLabel(label="File Contains Header Row")
  header1$setActive(TRUE)
  opt.box1$packStart(header1, FALSE, FALSE, 0)
  
  # Stolen from Peter Dalgaard
  ASCII <- c("Any White Space", sapply(1:255, function(i) parse(text=paste("\"\\",
                    structure(i,class="octmode"), "\"", sep=""))[[1]]))

  row.names1 <- gtkSpinButton(gtkAdjustment(1, 1, 1000, 1, 1), 1.0, 0)
  row.names1$setValue(1)

  row.names1$setSensitive(row.names0$getActive())
  gSignalConnect(row.names0, "toggled", function(widget){
    row.names1$setSensitive(widget$getActive())
  })
    
  #hbox <- gtkHBoxNew(FALSE, 0)
  label = gtkLabelNew("Row Labels In Column")
  label$setAlignment(1, 0.5)                
  opt.table$attach(label, 0, 1, 0, 1, xpad=10)
  opt.table$attach(row.names1, 1, 2, 0, 1)

  skip1 <- gtkSpinButton(gtkAdjustment(0, 0, 1000, 1, 1), 1.0, 0)
  skip1$setValue(0)
  label = gtkLabelNew("Number Of Rows To Skip")
  label$setAlignment(1, 0.5)
  opt.table$attach(label, 0, 1, 1, 2, xpad=10)
  opt.table$attach(skip1, 1, 2, 1, 2)
  
  colClasses1 <- gtkComboBoxNewText()
  for (ll in c("Default", "logical", "integer", "numeric", "complex", "character", "raw", "factor", "Date", "POSIXct")) colClasses1$appendText(ll)
  colClasses1$setActive(0)
  #hbox <- gtkHBoxNew(FALSE, 0)
  label = gtkLabelNew("Column Classes")
  label$setAlignment(1, 0.5)  
  opt.table$attach(label, 0, 1, 2, 3, xpad=10)
  opt.table$attach(colClasses1, 1, 2, 2, 3)
  
  na.strings1 <- gtkEntryNew()
  na.strings1$setText("NA")
  hbox <- gtkHBoxNew(FALSE, 0)
  label = gtkLabelNew("NA String")
  label$setAlignment(1, 0.5)  
  opt.table$attach(label, 0, 1, 3, 4, xpad=10)
  opt.table$attach(na.strings1, 1, 2, 3, 4)
  
  fill1 <- gtkCheckButtonNewWithLabel(label="Fill Unequal Length Rows")
  fill1$setActive(TRUE)
  opt.box1$packStart(fill1, FALSE, FALSE, 0)
  
  stringsAsFactors1 <- gtkCheckButtonNewWithLabel(label="Read Strings As Factors")
  stringsAsFactors1$setActive(FALSE)
  opt.box1$packStart(stringsAsFactors1, FALSE, FALSE, 0)

  fr2$add(box2)
  
  box0$packStart(fr1, FALSE, FALSE, 0)
  box0$packStart(fr0, FALSE, FALSE, 0)
  box0$packStart(fr2, FALSE, FALSE, 0)
  
  fr3 <- gtkFrameNew(label="Table Preview")
  box3 <- gtkVBoxNew(FALSE, 5)
  preview.button <- gtkButtonNewWithLabel("  Preview Table  ")
  hbox <- gtkHBoxNew(FALSE, 0)
  hbox$packEnd(preview.button, FALSE, FALSE, 5)
  box3$packStart(hbox, FALSE, FALSE, 0)
  
  #box3$packStart(preview.button, FALSE, FALSE, 0)
  tb2 <- gtkTextViewNew()
  tb2$setSizeRequest(400, 200)
  tb2$getBuffer()$setText("No Table loaded")
  box3$packStart(tb2)

  read_table <- function(nrows=-1, doMsg=FALSE){
    if(nchar(entry1$getText()) == 0) stop("No file selected")
    row.names.arg <- NULL
    if(row.names0$getActive()) row.names.arg <- as.integer(row.names1$getText())
    
    col.classes.arg <- NA
    if(colClasses1$getActiveText() != "Default")  {
      col.classes.arg <- colClasses1$getActiveText() # preview to get ncols
      if(col.classes.arg != "character" && !is.null(row.names.arg)){
        xx <- read.table(file=entry1$getText(), sep=sep.types[[sep1$getActiveText()]],
              dec = dec1$getActiveText(), header=header1$getActive(), row.names=row.names.arg, nrows=5)
        NN <- dim(xx)[2]
        col.classes.arg <- c("character", rep(col.classes.arg, NN))
      }
    }
    
    cmd_list = list(file=entry1$getText(),
      header= header1$getActive(),
      sep = sep.types[[sep1$getActiveText()]],
      skip= as.integer(skip1$getText()),
      #row.names = row.names.arg,
      nrows = nrows,
      colClasses= col.classes.arg,
      stringsAsFactors = stringsAsFactors1$getActive(),
      dec=dec1$getActiveText(),
      fill = fill1$getActive()
      )
    xx <- do.call(read.table, cmd_list) # read it with row names NULL

    if(!is.null(row.names.arg) && length(dim(xx))==2 && 0 < row.names.arg && row.names.arg <= ncol(xx)){
      dupes <- duplicated(xx[,row.names.arg])
      xx <- xx[!dupes,,drop=F]
      if(doMsg) message("Dropping ", sum(dupes), " row(s) from table to create unique row names")
      rownames(xx) <- xx[,row.names.arg]
      xx <- xx[,-row.names.arg,drop=F]
    }            
    return(list(xx=xx, cmd_list=cmd_list))
  }
        
  gSignalConnect(preview.button, "clicked", function(obj, label){
    tryCatch({
    
      preview <- read_table(nrows=nlines1$getValue())$xx
        
      obj <- gtkDfEdit(preview, size.request = c(400, 200), col.width=100)
      if(!is.null(box3$getChildren()[[2]])) box3$getChildren()[[2]]$destroy()
      box3$packEnd(obj)
    }, error = function(e) {
      tb3 <- gtkTextViewNew()
      tb3$setWrapMode(GtkWrapMode['word_char'])
      tb3$setSizeRequest(400, 200)
      tb3$getBuffer()$setText(as.character(e))
      if(!is.null(box3$getChildren()[[2]])) box3$getChildren()[[2]]$destroy()
      box3$packEnd(tb3)
    })
  })
  fr3$add(box3)
  box0$packStart(fr3)
  
  dialog[["vbox"]]$packStart(box0, TRUE, TRUE, 10)
  dialog$showAll()

  xx <- NULL
  if(dialog$run() == 1 && nchar(entry1$getText())){
    on.exit(w$destroy()) # block concurrent user calls with a modal window
    w <- gtkWindowNew("Loading...", show=F)
    w$setDecorated(FALSE)
    w$add(gtkLabelNew("Loading..."))
    w$setPosition(GtkWindowPosition["center-on-parent"])              
    w$setTransientFor(parent.window)    
    w$setModal(TRUE)        
    w$showAll()
  tryCatch({               
   rv <- read_table(doMsg=TRUE)
   xx <- rv$xx
   cmd_list = rv$cmd_list
          
   aa <- basename(entry1$getText())
   newname <- make.names(strsplit(aa, "[.]")[[1]][1])
   
   assign(newname, xx, envir=.GlobalEnv)         
   
   if(exists(".log.file")){
       sub.values <- as.character(substitute(cmd_list))
       cidx <- sapply(cmd_list, is.character)
       sub.values[cidx] <- sapply(as.character(cmd_list[cidx]), deparse)       
   
       cmd.string <- paste(newname, " <- ", "read.table", "(",
          paste(paste(names(cmd_list),sub.values, sep="="), collapse=", "),
          ")\n", sep="")
       print(cmd.string)
       write(cmd.string, file=get(".log.file"), append=T)
   }       
  }, error = function(e) {quick_message(e$message, caption = "Error Loading Table")})
   dialog$destroy()
 } else {
   dialog$destroy()
 }
  return(xx)
}
