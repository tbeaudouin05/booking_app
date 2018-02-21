ledger_f <- function(df_to_map, df_to_map_name, loop_number) {
  
  i <- 0
  
  while (i == 0) {
    
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
  
  
  # II_check_if_missing_ledger -----------------------------------------------------------
  
  source('R/2_function/2_item_price_commission/user_interaction_update_input.R')
  df_with_ledger <- user_interaction_update_input_f (df_to_map
                                                    ,df_to_map_name
                                                    ,ledger_map
                                                    ,'ledger_map'
                                                    ,column_map =  c('transaction_type'
                                                       ,'item_status'
                                                       ,'payment_method'	
                                                       ,'shipment_provider_name')
                                                    ,column_check =  c('ledger'
                                                       ,'subledger')
                                                    ,missing_field_name =  'ledger'
                                                    ,loop_number)
  
  
  if (is.data.frame(df_with_ledger)) { 
    i <- 1
    return(df_with_ledger)}
    
  }
  
}