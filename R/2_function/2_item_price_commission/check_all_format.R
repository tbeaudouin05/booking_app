check_all_format_f <- function(data_frame) {
  
  k <- 1
  
  payment_method_check <- prepare_missing_field_df_f(data_frame,'payment_method')
  item_status_check <- prepare_missing_field_df_f(data_frame,'item_status')
  shipment_provider_check <- prepare_missing_field_df_f(data_frame,'shipment_provider_name')
  paid_price_check <- data_frame[is.na(as.numeric(data_frame[,'paid_price'])),]
  
  if (nrow(payment_method_check[[1]])>0 | nrow(item_status_check[[1]])>0 | nrow(shipment_provider_check[[1]])>0 | nrow(paid_price_check)>0) {
    k <- 0
  }
  
  return(k)
  
  
}