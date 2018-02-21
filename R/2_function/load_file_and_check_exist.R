load_file_and_check_exist_f <- function(folder_of_file, file, file_type, sheet_name = NULL, column, open = T) {
  writeLines(paste('Getting data from',folder_of_file,'/',file))
  writeLines('Please wait...')
  
  # save run_date
  run_time <- gsub('-','',Sys.Date())
  run_time <- gsub(' ','',run_time)
  run_time <- gsub(':','',run_time)
  
  # if file does not exist prompt the user to provide the file
  if (!file.exists(paste0(folder_of_file,'/',file))) {
    
    tkmessageBox(title = 'Missing file!', message = paste(file,'in', folder_of_file ,'DOES NOT EXIST! \n
Please provide this file in this folder before running the code.'), type = 'ok')
  } else {    
    
    # fetch the file with read_csv or read.xlsx depending on file_type
    if (file_type == 'csv') {file_output <- read_csv(paste0(folder_of_file,'/',file))
    
    } else if (file_type == 'excel' && open == T) {
      
      # if there is an error fetching file's data then try opening, saving and close the file before fetching data
      file_output <- tryCatch({file_output <- read.xlsx(paste0(folder_of_file,'/',file), sheet = sheet_name)
                              return(file_output)},
               
                              error = function(cond){writeLines(paste('Opening',file))
                              xl.workbook.open(paste0(folder_of_file,'/',file))
                              writeLines(paste('Saving',file))
                              Sys.sleep(1)
                              xl.workbook.save(paste0(folder_of_file,'/',file))
                              Sys.sleep(3)
                              writeLines(paste('Closing',file))
                              xl.workbook.close(file)
                              Sys.sleep(3)
                              writeLines(paste('Importing data from',file))
                              file_output <- read.xlsx(paste0(folder_of_file,'/',file), sheet = sheet_name)
                              return(file_output)})
      
    } else if (file_type == 'excel' && open == F) {
      writeLines(paste('Importing data from',file))
      file_output <- read.xlsx(paste0(folder_of_file,'/',file), sheet = sheet_name)}
    
    # if the file is empty, prompt the user to rectify the issue
    if (is.null(file_output)) {
      
      tkmessageBox(title = 'File empty!', message = paste(file,'in', folder_of_file ,'IS EMPTY! \n 
Please make sure there is data in this file. \n
You can find the previous version of this file in archive'), type = 'ok')
      } else if (missing(column)) {
        # else if the argument 'column' was not filled out 
        # then archive file and return file_output
        writeLines(paste('Archiving',file))
        
        archive_f(folder_of_file, file, remove = F)
        
        return(file_output)
        
      } else { i <- 0
    # else if the argument 'column' was filled out
    # then if there are missing columns, prompt the user 
    # and return but not archive
      for (column_name in column) {
        if (!(column_name %in% colnames(file_output))) {
          tkmessageBox(title = 'Column missing!', message = paste('Column',column_name,'in',folder_of_file,'/',file,'IS MISSING! \n
Please provide this column.'), type = 'ok')
          i <- i + 1}}
        
        # if no column is missing the archive and return file_output
        if (i == 0) {
        writeLines(paste('Archiving',file))
          
        archive_f(folder_of_file, file, remove = F)
        
        return(file_output)}
        
        
      }
    } 
}