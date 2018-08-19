SELECT 
  t.id_transaction
  ,soi.src_id 'oms_id_sales_order_item'
  ,so.order_nr
  ,soi.src_created_at 'oms_soi_created_at'
  ,s.id_seller
  ,s.short_code
  ,s.name AS  'seller_name'
  ,tasg.id_tre2_account_statement_group 'id_transaction_type'
  ,tasg.name AS  'transaction_type'
  ,t.value AS  'transaction_value'
  ,ts.id_transaction_statement
  ,ts.start_date AS 'statement_start_date'
  ,ts.end_date AS 'statement_end_date'
  ,t.description AS 'comment'
  

  FROM transaction t 

  LEFT JOIN tre2_account_statement_group tasg
  ON t.fk_tre2_account_statement_group = tasg.id_tre2_account_statement_group

  LEFT JOIN seller s
  ON t.fk_seller = s.id_seller
  
  LEFT JOIN sales_order_item soi
  ON soi.id_sales_order_item = t.ref

  LEFT JOIN sales_order so
  ON so.id_sales_order = soi.fk_sales_order

  LEFT JOIN shipment_provider sp
  ON sp.id_shipment_provider = soi.fk_shipment_provider

  LEFT JOIN transaction_statement ts
  ON ts.id_transaction_statement = t.fk_transaction_statement
  
WHERE t.created_at BETWEEN '2018-7-16 00:00:00' AND '2018-7-22 23:59:59'