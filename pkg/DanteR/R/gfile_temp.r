gfile_temp <- function(text="",
                   type=c("open","save","selectdir"),
                   initialfilename = NULL,
                   filter =  list(
                     "All files"=list(
                       patterns=c("*")
                       ),
                     "R files"=list(
                       patterns=c("*.R","*.Rdata")
                       ),
                     "text files"=list(
                       mime.types=c("text/plain")
                       )
                     ),
                   multi = FALSE,
                   handler = NULL,
                   action = NULL,                     #
                   ...
                   ) {
            args = list(...)

            type = match.arg(type)
            availTypes = c(
              "open"="open",
              "save"="save",
              "selectdir"="select-folder",
              "createdir"="create-folder"
              )

            actiontype = GtkFileChooserAction[availTypes[type]]


            buttonWithId = list(
              "ok"= c("gtk-ok",GtkResponseType["ok"]),
              "cancel" = c("gtk-cancel",GtkResponseType["cancel"])
              )

            whichButtons = switch(type,
              "save"=c("ok","cancel"),
              "open"=c("ok","cancel"),
              "selectdir"=c("ok","cancel")
              )


            okhandler.default = function(h,...) {
              if(gWidgets:::is.gComponent(h$action)) {
                if(!is.null(args$quote))
                  svalue(h$action) <- gWidgets:::Paste("'",h$file,"'")
                else
                  svalue(h$action) <- h$file
              } else {
                do.call(h$action,list(h$file))
              }
            }
            ## give a default of printing.
            if(is.null(handler)) {
              #handler = okhandler.default
              if(is.null(action))
                action="print"
            }

            cancelhandler = function(h,...) {
              dispose(h$obj)
              return(NA)
            }

            filechooser = gtkFileChooserDialogNew(title=text, action=actiontype)

            filechooser$setSelectMultiple(multi)

            for(i in whichButtons)
              filechooser$AddButton(buttonWithId[[i]][1],buttonWithId[[i]][2])

            ## add a filter
            if(!is.null(filter) && type == "open") {
              for(i in names(filter)) {
                filefilter = gtkFileFilterNew()
                filefilter$SetName(i)
                if(!is.null(filter[[i]]$patterns)) {
                  for(pattern in filter[[i]]$patterns)
                    filefilter$AddPattern(pattern)
                }
                if(!is.null(filter[[i]]$mime.types)) {
                  for(mime.type in filter[[i]]$mime.types)
                    filefilter$AddMimeType(mime.type)
                }
                filechooser$AddFilter(filefilter)
              }
            }


            ## initialize
            if(!is.null(initialfilename)) {
              filechooser$SetFilename(gWidgets:::Paste(getwd(),"/",initialfilename))
            }

            ## this makes it modal
            response = filechooser$Run()
              # return a vector of chars for multi select - TT
            file=unlist(filechooser$GetFilenames())
            
            h = list(obj=filechooser,action=action,file=file)
            if(response == GtkResponseType["cancel"]) {
              ## just close
              filechooser$Destroy()
              return(NA)
            } else if(response == GtkResponseType["ok"]) {
              filechooser$Destroy()
              if(!is.null(handler)) handler(h)
              if(!is.null(args$quote) && as.logical(args$quote))
                return(paste("'",file,"'",sep=""))
              else
                return(file)
            } else {
              filechooser$Destroy()
              return(NA)
            }
}