archive_f <- function(folder_of_file, file_to_archive, remove = T) {
  
  # close all Excel workbooks to avoid errors
  for (wb in xl.workbooks()) {xl.workbook.close(wb)}
  
  # save run_date
  run_time <- gsub('-','',Sys.Date())
  run_time <- gsub(' ','',run_time)
  run_time <- gsub(':','',run_time)
  run_time <- paste(run_time,hour(Sys.time()),round(minute(Sys.time())/100,1)*10, sep = '_')

  
  # create archive folder
  dir.create(file.path(getwd(),paste0(folder_of_file,'/archive')),showWarnings = FALSE)
  # create archive_run_time folder
  dir.create(file.path(getwd(),paste0(folder_of_file,'/archive/archive_',run_time)),showWarnings = FALSE)
  # copy file to archive
  file.copy(paste0(folder_of_file,'/',file_to_archive),paste0(folder_of_file,'/archive/archive_',run_time,'/',file_to_archive), overwrite = T )
  
  if (remove == T) {
  # remove file from 4_temp_input
  file.remove(paste0(folder_of_file,'/',file_to_archive))}
  
  
}