prepare_transform_sc_data_f <- function(sc_transaction) {
  
  
  # I_prepare_sc_transaction_data ------------------------------------------------------------------

  
  #to filter retail suppliers
  retail_seller <- load_file_and_check_exist_f('2_input'
                                               ,'retail_seller.xlsm'
                                               ,'excel'
                                               ,sheet_name = 'do_not_change_name'
                                               ,open = F)
  
  # to flag retail suppliers
  retail_seller$flag <- 1
  # enable merge on Seller.Name with sc_transaction
  names(retail_seller)[1] <- 'seller_name'
  
  
  
  # II_sc_transaction_data_cleaning_transformation -----------------------------------------------
  
  # filter for retail
  sc_transaction_to_filter_retail <- sc_transaction
  # flag retail sellers
  sc_transaction_to_filter_retail <- merge(sc_transaction_to_filter_retail,retail_seller, by = 'seller_name', all.x = TRUE)
  sc_transaction_to_filter_retail$flag[is.na(sc_transaction_to_filter_retail$flag)] <- 0
  
  # create sc_transaction without retail
  sc_transaction_wo_retail <- sc_transaction_to_filter_retail[sc_transaction_to_filter_retail$flag == 0,]
  # erase flag
  sc_transaction_wo_retail$flag <- NULL


  return(sc_transaction_wo_retail)
}