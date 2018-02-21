prepare_ledger_f <- function() {
  
  # fetch ledger map
  ledger_map <- load_file_and_check_exist_f('2_input'
                                            ,'ledger_map.xlsm'
                                            ,file_type = 'excel'
                                            , sheet = 'do_not_change_name'
                                            , column = c('Transaction.Type'
                                                         ,'Item.Status'
                                                         ,'Payment.Method'	
                                                         ,'Customer.Service.-.Refund.Complete.Creator'	
                                                         ,'Delivery.Company'
                                                         ,'ledger'
                                                         ,'subledger')
                                            , open = F)
  
  # create ledger_key in ledger_map
  ledger_map$ledger_key <- paste0(ledger_map$Transaction.Type
                                  ,ledger_map$Delivery.Company
                                  ,ledger_map$Payment.Method
                                  ,ledger_map$Item.Status
                                  ,ledger_map$`Customer.Service.-.Refund.Complete.Creator`)
  # erase unnecessary columns
  ledger_map$Transaction.Type <- NULL
  ledger_map$Delivery.Company <- NULL
  ledger_map$Payment.Method <- NULL
  ledger_map$Item.Status <- NULL
  ledger_map$`Customer.Service.-.Refund.Complete.Creator` <- NULL
  
  return(ledger_map)
  
  
  
  
}