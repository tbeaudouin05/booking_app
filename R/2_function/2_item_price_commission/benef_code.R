benef_code_f <- function(df_to_map, df_to_map_name,loop_number){
  
  i <- 0
  df_with_benef_code <- NULL
  
  while (i < 2) {

    # close all Excel workbooks to avoid errors
    for (wb in xl.workbooks()) {xl.workbook.close(wb)}
  
    # fetch benef_code_map ------------------------------------------------------------------------------
    benef_code_map <- load_file_and_check_exist_f('2_input'
                                                  ,'benef_code_map.xlsm'
                                                  ,'excel'
                                                  ,sheet_name = 'do_not_change_name'
                                                  ,open = F)
    benef_code_map <- data.frame('short_code' = benef_code_map$short_code
                                  ,'benef_code' = benef_code_map$benef_code)
    
    # to check there is no blanck in short_code
    check_blank_short_code_map <- benef_code_map[is.na(benef_code_map[,'short_code']),]
    
    # to check there is no blanck in benef_code
    check_blank_benef_code_map <- benef_code_map[is.na(benef_code_map[,'benef_code']),]
    
    # to check there are no exact duplicated short_code
    duplicated_short_code <- data.frame('short_code' = benef_code_map$short_code, 'is_duplicate' = duplicated(benef_code_map$short_code))
    duplicated_short_code <- duplicated_short_code[duplicated_short_code[,'is_duplicate'] == 'TRUE',]

    # to check benef_code formatting = length 10 + integer (not decimal)
    # length(benef_code) = 10
    is_not_10_length_benef_code <- data.frame(benef_code_map, 'benef_code_length' = nchar(benef_code_map$benef_code))
    is_not_10_length_benef_code <- is_not_10_length_benef_code[is_not_10_length_benef_code[,'benef_code_length'] != 10,]
    # benef_code = integer
    is_not_int_benef_code <- data.frame(benef_code_map, 'diff_round_benef_code' = round(as.numeric(benef_code_map$benef_code))- as.numeric(benef_code_map$benef_code))
    is_not_int_benef_code <- is_not_int_benef_code[is_not_int_benef_code[,'diff_round_benef_code']!= 0,] 
    
    # if there are blank short_code then re_run the loop and prompt user to rectify
    
    # if there are blank benef_code then re_run the loop and prompt user to rectify
    if (nrow(check_blank_short_code_map) >0 ) {
      
      df_with_benef_code <- 're_run'
      
      shell.exec(paste0(getwd(),'/2_input/benef_code_map.xlsm'))
      
      tkmessageBox(title = paste('Blank short_code!') 
                   , message = 'There are blank short_codes in benef_code map - provide all short_code THEN click OK'
                   , type = 'ok')
      
      # restart checking if there was any error in input
      i <- 0
      
    } else if (nrow(check_blank_benef_code_map) >0 ){
      
      df_with_benef_code <- 're_run'
      
      shell.exec(paste0(getwd(),'/2_input/benef_code_map.xlsm'))
      
      tkmessageBox(title = paste('Blank benef_code!') 
                   , message = 'There are blank benef_codes in benef_code map - provide all benef_code THEN click OK'
                   , type = 'ok')
      
      # restart checking if there was any error in input
      i <- 0
      
    } else if (nrow(duplicated_short_code) > 0) {
      
      df_with_benef_code <- 're_run'
      
      write_excel_csv(duplicated_short_code,'4_temp_input/duplicate_short_code_map_missing.csv')
      
      shell.exec(paste0(getwd(),'/2_input/benef_code_map.xlsm'))
      
      shell.exec(paste0(getwd(),'/4_temp_input/duplicate_short_code_map_missing.csv'))
      
      tkmessageBox(title = paste('Duplicated short_code!') 
                   , message = 'There are duplicated short_code in benef_code map - remove all duplicates THEN click OK'
                   , type = 'ok')
      
      # restart checking if there was any error in input
      i <- 0
      
    } else if (nrow(is_not_10_length_benef_code) > 0 | nrow(is_not_int_benef_code) > 0) {
      
      df_with_benef_code <- 're_run'
      
      shell.exec(paste0(getwd(),'/2_input/benef_code_map.xlsm'))
      
      if (nrow(is_not_10_length_benef_code) > 0) {info_m <- 'Some benef_code do not have 10 digits'
        } else {info_m <- 'Some benef_code are not integers'}
      
      tkmessageBox(title = paste('Wrong format benef_code!') 
                   , message = paste(info_m,'- rectify benef_code format THEN click OK')
                   , type = 'ok')
      
      # restart checking if there was any error in input
      i <- 0
      
    } else {
    
    # map beneficiary codes -------------------------------------------------------------------------------
    df_with_benef_code <- user_interaction_update_input_f(df_to_map
                                                ,df_to_map_name
                                                ,df_map = benef_code_map
                                                ,df_map_name = 'benef_code_map'
                                                ,column_map = 'short_code'
                                                ,column_check = 'benef_code'
                                                ,missing_field_name = 'benef_code'
                                                ,loop_number)}
    
    
      if (is.data.frame(df_with_benef_code)) { 
      
      # break the loop after two fully successful run - runs twice to check that user interaction did not input crap into the benef_map
      i <- i + 1
      
      # reorder columns
      df_with_benef_code <- df_with_benef_code[,c(ncol(df_with_benef_code),1:ncol(df_with_benef_code)-1)]
      
      return(df_with_benef_code)}
  
  }
  
}