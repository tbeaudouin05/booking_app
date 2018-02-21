merge_sc_oms_f <- function (oms_df, sc_df) {
  
  
  # filter out order numbers which are not numbers from oms_df [this should not be placed in data cleaning because Shirin showed the process as such]
  # might be useless since anyway sc data is joined with oms data on sales order item id
  oms_df <- oms_df[is.na(as.numeric(oms_df$order_nr)) == F,]
  
  # only fetch necessary columns of OMS update report | update <> ship because update --> item status
  oms_df <- data.frame('oms_id_sales_order_item' = oms_df$id_sales_order_item
                           ,'payment_method' = oms_df$payment_method
                           ,'shipment_provider_name' = oms_df$shipment_provider_name
                           ,'billing_city' = oms_df$billing_city
                           ,'paid_price' = oms_df$paid_price
                           ,'coupon_code'= oms_df$coupon_code
                           ,'item_status' = oms_df$item_status
                           ,'refund_complete_creator'= oms_df$refund_complete_creator)
  
  # merge Item Price data frame to OMS update report
  sc_w_oms <- merge(sc_df,oms_df, by = 'oms_id_sales_order_item', all.x = TRUE)
  
  return(sc_w_oms)
  
  
}