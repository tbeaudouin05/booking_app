write_excel_w_error_handling_f <- function (workbook_path,data_header,data_detail = NULL){


file_output <- tryCatch({
  
  # create workbook_path.xlsx
  wb <- loadWorkbook(workbook_path, create = TRUE)
  # create the sheet Header in workbook_path.xlsx
  createSheet(wb, name = 'Header')
  # write data in the sheet Header
  writeWorksheet(wb, data_header, sheet = 'Header', startRow = 1, startCol = 1)
  
  # if data_detail is a dataframe then create detail sheet
  if (is.data.frame(data_detail)) {
  createSheet(wb, name = 'Detail')
  writeWorksheet(wb, data_detail, sheet = 'Detail', startRow = 1, startCol = 1)}
  
  #save workbook_path.xlsx on the computer
  saveWorkbook(wb)},

error = function(cond){
  
  # prompts user on the console
  writeLines(paste('Too much data in',workbook_path,'! Try running automation for fewer SC transaction dates'))
  
  # prompts user with box
  tkmessageBox(title = paste('Too much data!')
               , message = paste('Too much data in',workbook_path,'!\n
Try running automation for fewer SC transaction dates\n
The automation will now break - you have to re-run with less data')
               , type = 'ok')
  
  })

}