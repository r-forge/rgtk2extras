

  # if append is true and the file exists, add tables to existing file
save_tables <- function(file_name, file_type, table_list, overwrite = FALSE){  

    # Kludge: Excel 2007 has trouble creating new files called xlsx, but is fine with xls
  if(file_type == "Excel2007")
    file_name <- substr(file_name, 1, nchar(file_name)-1)
  
  stopifnot(nchar(file_name) > 0 && length(table_list) > 0)  
  
  if(file_type%in%c("csv", "txt") && length(table_list) > 1)
    quick_message(paste("Creating the following files:\n", paste(paste(table_list, file_name, sep = "_"), collapse = "\n")))
  
  for(dataset in table_list){
    obj <- safe.eval(dataset)
    curr_file_name <- file_name
    table_name <- sub(".", "_", make.names(dataset), fixed=T)
    
    if(file_type%in%c("csv", "txt") && length(table_list) > 1)
      curr_file_name <- paste(table_name, file_name, sep = "_")
    file.list[[file_type]]$save.table(curr_file_name, obj, table_name, overwrite)
  }
NULL
}

