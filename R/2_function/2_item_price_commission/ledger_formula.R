ledger_formula_f <- function(df_to_map, df_to_map_name, loop_number) {
  
  
  # close all Excel workbooks to avoid errors
  for (wb in xl.workbooks()) {xl.workbook.close(wb)}
  
  df_to_map$ledger <- NA
  df_to_map$subledger <- NA
  
  df_to_map <- data.table( df_to_map)
  
  # item_price_credit ID = 17 ------------------------------------------------
  
  # item_price_credit COD ledger
  setDT(df_to_map)[id_transaction_type == 17 & payment_method =='CashOnDelivery', ledger := '13004']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method =='CashOnDelivery' & shipment_provider_name == 'Pishtaz', ledger := '84005']
  # item_price_credit COD subledger
  setDT(df_to_map)[id_transaction_type == 17 & payment_method =='CashOnDelivery' & shipment_provider_name == 'Tipax', subledger := '4000000006']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method =='CashOnDelivery' & shipment_provider_name == 'Tpx', subledger := '4000000006']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method =='CashOnDelivery' & shipment_provider_name == 'Bamilo Transportation System', subledger := '4000000003']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method =='CashOnDelivery' & shipment_provider_name == 'Sent by Seller', subledger := '4000000003']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method =='CashOnDelivery' & shipment_provider_name == 'Chaapaar', subledger := '4000000002']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method =='CashOnDelivery' & shipment_provider_name == 'Post', subledger := '4000000001']
  
  
  # item_price_credit NOT COD ledger
  setDT(df_to_map)[id_transaction_type == 17 & payment_method !='CashOnDelivery', ledger := '33001']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method == 'BankDeposit', ledger := '33002']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method == 'NoPayment', ledger := '1']
  # item_price_credit NOT COD subledger
  setDT(df_to_map)[id_transaction_type == 17 & payment_method == 'PEC', subledger := '4000000062']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method == 'SEP', subledger := '4000000061']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method == 'Irankish', subledger := '4000000209']
  setDT(df_to_map)[id_transaction_type == 17 & payment_method == 'AsanPardakht', subledger := '4000000196']
  
  
  # item_price ID = 18 -----------------------------------------------------------------------
  
  # item_price NOT COD ledger
  setDT(df_to_map)[id_transaction_type == 18 & payment_method !='CashOnDelivery', ledger := '94001']
  setDT(df_to_map)[id_transaction_type == 18 & payment_method !='NoPayment', ledger := '1']
  
  # item_price COD and NOT canceled and NOT closed ledger
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' & item_status != 'closed' & item_status != 'canceled' , ledger := '94001']
  
  
  # item_price COD and (canceled OR closed) ledger
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' 
                   & (item_status != 'closed' | item_status != 'canceled') 
                   , ledger := '13004']
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' 
                   & (item_status != 'closed' | item_status != 'canceled') 
                   & shipment_provider_name == 'Pishtaz'
                   , ledger := '84005']
  
  # item_price COD and (canceled OR closed) subledger
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' 
                   & (item_status != 'closed' | item_status != 'canceled') 
                   & shipment_provider_name == 'Tipax', subledger := '4000000006']
  
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' 
                   & (item_status != 'closed' | item_status != 'canceled') 
                   & shipment_provider_name == 'Tpx', subledger := '4000000006']
  
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' 
                   & (item_status != 'closed' | item_status != 'canceled') 
                   & shipment_provider_name == 'Bamilo Transportation System', subledger := '4000000003']
  
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' 
                   & (item_status != 'closed' | item_status != 'canceled') 
                   & shipment_provider_name == 'Sent by Seller', subledger := '4000000003']
  
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' 
                   & (item_status != 'closed' | item_status != 'canceled') 
                   & shipment_provider_name == 'Chaapaar', subledger := '4000000002']
  
  setDT(df_to_map)[id_transaction_type == 18 & payment_method =='CashOnDelivery' 
                   & (item_status != 'closed' | item_status != 'canceled') 
                   & shipment_provider_name == 'Post', subledger := '4000000001']
  
  
  df_to_map <- data.frame(df_to_map)
  
  # II_return_missing_ledger_for_checking -----------------------------------------------------------
  
  df_to_check <- data.frame('transaction_type' = df_to_map$transaction_type
                            ,'item_status'	= df_to_map$item_status
                            ,'payment_method'	= df_to_map$payment_method
                            ,'shipment_provider_name' = 	df_to_map$shipment_provider_name
                            ,'ledger' = df_to_map$ledger
                            ,'subledger' = df_to_map$subledger)
  
  
  
  
  return(df_with_ledger)
  
  
}