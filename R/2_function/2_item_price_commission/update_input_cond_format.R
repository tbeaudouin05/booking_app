update_input_cond_format_f <- function() {
  
  # close all Excel workbooks to avoid errors
  for (wb in xl.workbooks()) {xl.workbook.close(wb)}
  
  cond_item_status <- run_query_wo_error_f('SELECT DISTINCT isois.name item_status FROM ims_sales_order_item isoi LEFT JOIN ims_sales_order_item_status isois ON isoi.fk_sales_order_item_status = isois.id_sales_order_item_status'
                                           , is_sc_query = F)
  
  cond_payment_method <- run_query_wo_error_f('SELECT DISTINCT iso.payment_method FROM ims_sales_order iso'
                                              , is_sc_query = F)
  
  cond_shipment_provider_name <- run_query_wo_error_f('SELECT DISTINCT osp.shipment_provider_name from oms_shipment_provider osp'
                                                      , is_sc_query = F)
  
  write_excel_csv(cond_item_status,'2_input/input_conditional_format/cond_item_status.csv')
  write_excel_csv(cond_payment_method,'2_input/input_conditional_format/cond_payment_method.csv')
  write_excel_csv(cond_shipment_provider_name,'2_input/input_conditional_format/cond_shipment_provider_name.csv')
  
}