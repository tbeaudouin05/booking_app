user_interaction_new_input_f <- function(missing_field_df
                                         ,missing_field_name
                                         ,missing_field_df_name
                                         ,no_missing_field_df
                                         ,loop_number
                                         ,check_input = F) {
  
  writeLines(paste('Checking there is no missing',missing_field_name, 'in', missing_field_df_name))
  writeLines('Please wait...')
  
  file_path <- paste0('4_temp_input/',missing_field_df_name,loop_number,'.xlsx')
  file_name <- paste0(missing_field_df_name,loop_number,'.xlsx')
  
  # if there are no missing field then return no_missing_field_df   
  if (nrow(missing_field_df) == 0) {return(no_missing_field_df)} else {
    
    i <- 0
    while (i == 0) {
      # close all Excel workbooks to avoid errors
      for (wb in xl.workbooks()) {xl.workbook.close(wb)}
      
      # else if missing_field_df.xlsx does not exist
      # then create the file 'missing_field_df.xlsx' and ask the user to fill it out
      if (!file.exists(file_path)){
        
        # save missing_field_df.xlsx to 4_temp_input folder without error
        write_excel_w_error_handling_f(file_path,missing_field_df)
        
        # open missing_field.xlsx for user to modify
        shell.exec(paste0(getwd(),'/',file_path))
        
        # if check_input = T then open cond_missing_field_name.xlsx for user to know what are accepted input
        if (check_input == T) {shell.exec(paste0(getwd(),'/2_input/input_conditional_format/cond_',missing_field_name,'.csv'))}
        
        # prompts user to modify missing_field.xlsx
        tkmessageBox(title = paste('Missing', missing_field_name,'!')
                     , message = paste0('Please, fill out missing order information under ',file_path,': ', missing_field_name)
                     , type = 'ok')
        
        
      } else {
        
        # else if missing_field.xlsx does exist 
        # NB: there ARE rows with missing fields otherwise the function has already returned no_missing_field_df at the beginning
        # then append missing_field_df to no_missing_field_df
        ## WARNING: user should have modified missing_field.xlsx at this point
        
        # load new input to R
        to_append <- load_file_and_check_exist_f('4_temp_input'
                                                 , file_name
                                                 , 'excel'
                                                 , sheet_name = 1
                                                 , open = T)
        
        check_flag <- 0
        # if check_input = true check that all new input match with possible values in database
        if (check_input == T) {        
          
          # load check_format_input to R
          check_format_input <- load_file_and_check_exist_f('2_input/input_conditional_format'
                                                            , paste0('cond_',missing_field_name,'.csv')
                                                            , 'csv')
          
          
          # merge and flag
          check_format_input$flag <- 1
          to_append_checked <- merge(to_append, check_format_input, by = missing_field_name, all.x = TRUE)
          
          # if there are input which do not match the format of required input then prompt user and re-run the loop
          if (nrow(to_append_checked[is.na(to_append_checked$flag) == T,]) > 0) {
            
            # open missing_field.xlsx for user to modify
            shell.exec(paste0(getwd(),'/',file_path))
            
            # pen cond_missing_field_name.xlsx for user to know what are accepted input
            shell.exec(paste0(getwd(),'/2_input/input_conditional_format/cond_',missing_field_name,'.csv'))
            
            # prompts user to modify missing_field.xlsx
            tkmessageBox(title = paste('Incorrect format:', missing_field_name,'!')
                         , message = paste0('Input format is incorrect! Please check formatting of missing field in ',file_path,': ', missing_field_name)
                         , type = 'ok')
            
            
            check_flag <- 1
            
          }
          
        }
        
        if (check_input == 'numeric') {        
          
          
          # check that the field in to_append has only numeric
          to_append_not_numeric <- to_append[is.na(as.numeric(to_append[,missing_field_name])) == T,]
          
          # if there are input which are not numbers then prompt user and re-run the loop
          if (nrow(to_append_not_numeric) > 0) {
            
            
            # open missing_field.xlsx for user to modify
            shell.exec(paste0(getwd(),'/',file_path))
            
            # prompts user to modify missing_field.xlsx
            tkmessageBox(title = paste('Incorrect format:', missing_field_name,'!')
                         , message = paste0('Input format is incorrect! All inputs in should be NUMBERS in missing fields ',file_path,': ', missing_field_name)
                         , type = 'ok')

            
            check_flag <- 1
            
          }
          
        }

        # only continue if all inputs match the correct conditional formatting
        if (check_flag == 0) {
          
        
        no_missing_field_df_2 <- rbind(no_missing_field_df,to_append)
        
        
        # to verify that no_missing_field_df actually does not have missing payment method
        missing_field_df_2 <- no_missing_field_df_2[is.na(no_missing_field_df_2[,missing_field_name]) == T,]
        
        # if no_missing_field_df still has missing payment method
        # then prompt user to solve issue
        # else continue the program
        if (nrow(missing_field_df_2) != 0){
          
          # open missing_field_df.xlsx for user to modify
          shell.exec(paste0(getwd(),'/',file_path))
          
          # prompts user to modify missing_field_df.xlsx
          tkmessageBox(title = paste('Missing', missing_field_name,'!')
                       , message = paste0('ALL TRANSACTIONS IN ', file_name,' SHOULD HAVE A VALUE in the field ', missing_field_name), type = 'ok')
          
          # else return no_missing field df and continue program
        } else {
          return(no_missing_field_df_2)
          i <- 1} 
        }
        
      }
    }
  }
}
