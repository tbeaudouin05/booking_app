ledger_map_check_f <- function(oms_id_sales_order_item){
  
  writeLines('Checking ledger map is complete...')
  transaction_type <- data.frame('transaction_type' = c('Item Price', 'Item Price Credit'))
  
  # to filter only the id of ipc_ship_oms_id_soi and ipt_rcc_oms_id_soi needed
  oms_filter <- paste0('\'',gsub(',','\',\'',paste(unlist(oms_id_sales_order_item),collapse=",")),'\'')
  
  # fetch the text of ledger_check.txt
  unformatted_ledger_check <- readLines('SQL/ledger_check.txt')
  
  # format the text of unformatted_oms_query
  #NB: THERE SHOULD BE NO COMMENT IN THE SQL or this will not work
  formatted_ledger_check <- gsub("\t","", paste(unformatted_ledger_check, collapse=" "))
  
  # create ledger_check_query
  ledger_check_query <- paste(formatted_ledger_check
                          ,'AND isoi.id_sales_order_item IN (',oms_filter,') ) tt')
  
  # format the text of ledger_check_query
  #NB: THERE SHOULD BE NO COMMENT IN THE SQL or this will not work
  formatted_ledger_check_query <- gsub("\t","", paste(ledger_check_query, collapse=" "))
  
  writeLines('Getting all combination of item_status, payment_method and shipment_provider during booked period from OMS database...')
  ledger_check_r <- run_query_wo_error_f(formatted_ledger_check_query, is_sc_query = F)
  
  
  # all possible combinations
  combination <- expand.grid.df(transaction_type
                         ,data.frame('item_status' = ledger_check_r$item_status)
                         ,data.frame('payment_method' = ledger_check_r$payment_method)
                         ,data.frame('shipment_provider_name' = ledger_check_r$shipment_provider_name))
  
  
  i <- 1
  while (i != 0) {
    
    # close all Excel workbooks to avoid errors
    for (wb in xl.workbooks()) {xl.workbook.close(wb)}
    
    # fetch ledger map --------------------------------------------------------------------------
    ledger_map <- load_file_and_check_exist_f('2_input'
                                              ,'ledger_map.xlsm'
                                              ,file_type = 'excel'
                                              , sheet = 'do_not_change_name'
                                              , column = c('transaction_type'
                                                           ,'item_status'
                                                           ,'payment_method'	
                                                           ,'shipment_provider_name'
                                                           ,'ledger'
                                                           ,'subledger')
                                              , open = F)
  
  # keys
  ledger_map$key <- paste0(ledger_map$item_status , ledger_map$payment_method , ledger_map$shipment_provider_name , ledger_map$transaction_type)
  combination$key <- paste0(combination$item_status , combination$payment_method , combination$shipment_provider_name , combination$transaction_type)
  
  # check
  check <- data.frame('key' = ledger_map$key
                      ,'flag' = 1)
  
  # merge combination and check
  merged <- merge(combination, check, by = 'key', all.x = T)
  
  # filter missing ledgers
  missing_ledger_df <- merged[is.na(merged[,'flag']),]
  missing_ledger_df$flag <- NULL
  missing_ledger_df$key <- NULL
  
  # to check ledger format
  ledger_map_validation <- load_file_and_check_exist_f('2_input'
                                            ,'ledger_map.xlsm'
                                            ,file_type = 'excel'
                                            , sheet = 'validation'
                                            , open = T)
  transaction_type_format <- data.frame('transaction_type' = ledger_map_validation[1][!is.na(ledger_map_validation[1])])
  item_status_format <- data.frame('item_status' = ledger_map_validation[2][!is.na(ledger_map_validation[2])])

  # if there are unmapped ledgers, prompt user to give information
  if (nrow(missing_ledger_df)>0) {
    
  # write missing fields
  write_excel_csv(missing_ledger_df,'4_temp_input/ledger_map_missing_field.csv')
  
  # open ledger_map.xlsm and ledger_map_missing_field.csv for user to modify
  shell.exec(paste0(getwd(),'/2_input/ledger_map.xlsm'))
  shell.exec(paste0(getwd(),'/4_temp_input/ledger_map_missing_field.csv'))
  
  Sys.sleep(1)
  
  # prompts user to modify df_map_name.xlsm
  tkmessageBox(title = 'Missing ledgers!' 
               ,message = 'Please add missing ledgers to ledger_map.xslm', type = 'ok')

  } else if ( nrow(ledger_map[is.na(ledger_map[,'ledger']),]) >0){
    # if all ledgers are mapped
    # then if there are NA ledgers, prompt the user to rectify
    
    shell.exec(paste0(getwd(),'/2_input/ledger_map.xlsm'))
    
    Sys.sleep(1)
    
    # prompts user to modify df_map_name.xlsm
    tkmessageBox(title = 'Blank ledgers!' 
                 ,message = 'Please, there should be NO BLANK LEDGERS in ledger_map.xslm', type = 'ok')
    
    
  } else {
    
    i <- 0
    
  }
  
  }
}