  SELECT DISTINCT

  CONCAT(isois.name, iso.payment_method, osp.shipment_provider_name) 'ledger_map_key'

  FROM ims_sales_order_item isoi

  LEFT JOIN ims_sales_order iso
  ON isoi.fk_sales_order = iso.id_sales_order

  LEFT JOIN oms_package_item opi
  ON opi.fk_sales_order_item = isoi.id_sales_order_item

  LEFT JOIN oms_package_dispatching opd
  ON opi.fk_package = opd.fk_package

  LEFT JOIN oms_shipment_provider osp
  ON opd.fk_shipment_provider = osp.id_shipment_provider

  LEFT JOIN ims_sales_order_item_status isois
  ON isois.id_sales_order_item_status = isoi.fk_sales_order_item_status

  LEFT JOIN 
  ims_sales_order_item_status_history AS shipped_at
  ON isoi.id_sales_order_item = shipped_at.fk_sales_order_item
  AND shipped_at.fk_sales_order_item_status='5'

  WHERE shipped_at.created_at IS NOT NULL

  


