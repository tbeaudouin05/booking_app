format_final_pivot_f <- function(df_final_pivot,df_name, is_final) {
  
  df_pivot_formatted <- df_final_pivot
  
  # is final ------------------------------------------------------------------------------------
  if (is_final){

    # get field list in proper order
    field_list <- load_file_and_check_exist_f('2_input/item_price_commission_format/'
                                              ,paste0(df_name,'_format.xlsm')
                                              ,file_type = 'excel'
                                              ,sheet_name = 'do_not_change_name'
                                              ,open = F)
    
    # change item_price_credit and item_price to ipc and ipt
    field_list$transaction_type <- gsub('item_price_credit','ipc',field_list$transaction_type)
    field_list$transaction_type <- gsub('item_price','ipt',field_list$transaction_type)
    field_list$transaction_type <- gsub('commission','c',field_list$transaction_type)
    # format field_list so that each value resembles the format of df_final_pivot column names
    field_list$ledger_list <- paste(field_list$ledger_order, field_list$subledger_order, field_list$transaction_type, sep = '_')
    # erase unnecessary fields
    field_list$ledger_order <- NULL
    field_list$subledger_order <- NULL
    field_list$transaction_type <- NULL
    # erase the _NA from the field name
    field_list$ledger_list <- gsub('_NA','',field_list$ledger_list)
    
    # get field list without total
    field_list_no_total <- field_list[!(field_list$ledger_list %like% 'total'),]
    
    # calculate total 60002
    df_pivot_formatted$'62002_total' <- df_pivot_formatted$`62002_ipc`- df_pivot_formatted$`62002_ipt`
    
    # calculate total 31002
    # initialize at 0
    df_pivot_formatted$'31002_total' <- 0
    # add or subtract value of ledger depending on the column 'calculation_total' of field list
    for (m in c(1:nrow(field_list_no_total))){
    df_pivot_formatted$'31002_total' <- df_pivot_formatted$'31002_total' + field_list_no_total$total_calculation[m] *df_pivot_formatted[,field_list_no_total$ledger_list[m]]
    }
   
    # get length of field list
    n <- nrow(field_list)
    # define ledger_list to call the name of each ledger in next loop
    ledger_list <- field_list$ledger_list
    
    # initialize data frame with seller_name column
    df_formatted_final <- data.frame(df_pivot_formatted$seller_name)
    
    # add all columns in data frame given field list ORDER ------------------------------
    for (k in c(1:n)) { 
      # add each ledger to data frame
      df_formatted_final <-  data.frame(df_formatted_final
                                        ,df_pivot_formatted[,ledger_list[k]]) }
    
    # rename seller_name column                  
    names(df_formatted_final) <- 'seller_name'
    
    # rename all columns of df_formatted final given field list ORDER -----------------------------------
    for (l in c(1:n)) {
      names(df_formatted_final)[l+1] <- ledger_list[l]}
    
    # erase the _ipc, _ipt, _c and total from the field names
    for (i in c(2:ncol(df_formatted_final))) { 
      names(df_formatted_final)[i] <- gsub('_ipc','',names(df_formatted_final)[i])
      names(df_formatted_final)[i] <- gsub('_ipt','',names(df_formatted_final)[i])
      names(df_formatted_final)[i] <- gsub('_total','',names(df_formatted_final)[i])
      names(df_formatted_final)[i] <- gsub('_c','',names(df_formatted_final)[i])}
    
    
    
    # is not final ---------------------------------------------------------------------------------------------
    } else {
      
  # erase the _NA from the field names
  for (i in c(2:ncol(df_pivot_formatted))) { 
    names(df_pivot_formatted)[i] <- gsub('_NA','',names(df_pivot_formatted)[i])}
      
      # get field list in proper order
      field_list <- load_file_and_check_exist_f('2_input/item_price_commission_format/'
                                                ,paste0(df_name,'_format.xlsm')
                                                ,file_type = 'excel'
                                                ,sheet_name = 'do_not_change_name'
                                                ,open = F)
      
      # format field_list so that each value resembles the format of df_pivot_formatted column names
      field_list$ledger_list <- paste(field_list$ledger_order, field_list$subledger_order, sep = '_')
      # erase unnecessary fields
      field_list$ledger_order <- NULL
      field_list$subledger_order <- NULL
      
      # erase the _NA from the field name
      field_list$ledger_list <- gsub('_NA','',field_list$ledger_list)
      
      # create vector made of 0s with same number of rows as df_pivot_formatted 
      y <- nrow(df_pivot_formatted)
      zero_vector <- vector(mode = 'numeric', length = y)
      # change zero_vector to data frame to be able to use names() function in loop
      zero_vector <- data.frame(zero_vector)
      
      # check that each field in field list exists in df_pivot_formatted
      # otherwise add the field with 0 values
      for (field_name in field_list[[1]]) {
        if(field_name %in% colnames(df_pivot_formatted)) {
          # if field name exists in df_pivot_formatted: do nothing
        } else {
          # if field name does not exist in df_pivot_formatted
          # then add column of 0s with 'field_name' as column name to df_pivot_formatted
          names(zero_vector)[1] <- field_name
          df_pivot_formatted <- cbind(df_pivot_formatted,zero_vector)}}
      
      # get length of field list
      n <- nrow(field_list)
      # define ledger_list to call the name of each ledger in next loop
      ledger_list <- field_list$ledger_list
      
      # initialize data frame with seller_name column
      df_formatted_final <- data.frame(df_pivot_formatted$seller_name)
      
      # add all columns in data frame given field list ORDER ------------------------------
      for (k in c(1:n)) { 
        # add each ledger to data frame
        df_formatted_final <-  data.frame(df_formatted_final
                                          ,df_pivot_formatted[,ledger_list[k]]) }
      
      
      # rename seller_name column                  
      names(df_formatted_final) <- 'seller_name'
      
      # rename all columns of df_formatted final given field list ORDER -------------------------------
      # concatenate the name of the data frame (ipt or ipc) to each ledger in order to differentiate them between ipc vs. ipt
      for (l in c(1:n)) {
        names(df_formatted_final)[l+1] <- paste(ledger_list[l],df_name, sep ='_') }
      
      
      }
  
  
  return(df_formatted_final)
}