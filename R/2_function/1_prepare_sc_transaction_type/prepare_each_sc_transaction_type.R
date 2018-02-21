prepare_each_sc_transaction_type_f <- function (sc_transaction_wo_retail) {
  
  
  # define filters for each output
  transaction_type_list <- list('Cancellation Penalty  (After 24 Hrs)'
                                ,' Cancellation Penalty - (Within 24 Hrs)'
                                ,c('Commission Fee (No Discount)','Commission Fee (Discounted)','Commission')
                                ,'Commission Credit'
                                ,'Consigned Order Items handling Fee'
                                ,'Down Payment Credit'
                                ,'Item Price'
                                ,'Item Price Credit'
                                ,'Shipping Fee (Order Level)'
                                ,'Shipping Fee (Order Level) Credit'
                                ,'Storage Fee'
                                ,'Lost or Damaged (Product Level) Credit')
  
  
  # list to store data frames of each transaction type without and with comment
  transaction_type_df_list <- list()
  transaction_type_df_list_w_comment <- list()
  
  # define name for each element
  transaction_name_list <- list('Cancellation Penalty  (After 24 Hrs)' = 0
                                ,' Cancellation Penalty - (Within 24 Hrs)'= 0
                                , 'Commission' = 0
                                , 'Commission Credit' = 0
                                ,'Handling Fee' = 0
                                ,'Down Payment Credit' = 0
                                ,'Item Price' = 0
                                ,'Item Price Credit' = 0
                                ,'Shipping Fee (Order Level)' = 0
                                ,'Shipping Fee (Order Level) Credit' = 0
                                ,'Storage Fee' = 0
                                ,'Lost or Damaged (Product Level) Credit' = 0)
  
  for (i in c(1: length(transaction_name_list))) {
    
    # create transaction type
    transaction_type <- sc_transaction_wo_retail[sc_transaction_wo_retail$transaction_type %in% transaction_type_list[[i]],]
    # create transaction type without comment
    transaction_type_wo_comment <- transaction_type[transaction_type$comment == '',]
    # create transaction type with comment
    transaction_type_w_comment <- transaction_type[transaction_type$comment != '',]
    
    # save transaction type without comment into transaction_type_df_list
    transaction_type_df_list[[i]] <- transaction_type_wo_comment
    # save transaction type without comment into transaction_type_df_list_w_comment
    transaction_type_df_list_w_comment[[i]] <- transaction_type_w_comment
    
  }
  
  # assign names to the elements of the lists of data frames
  names(transaction_type_df_list) <- names(transaction_name_list)
  names(transaction_type_df_list_w_comment) <- names(transaction_name_list)
  
  transaction_type_prepared <- list(transaction_type_df_list,transaction_type_df_list_w_comment)
  
  return(transaction_type_prepared)
  
}