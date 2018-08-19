SELECT
  isoi.id_sales_order_item
  ,isois.name 'item_status'
  ,iso.payment_method 'payment_method'
  ,osp.shipment_provider_name 'shipment_provider_name'
  ,isoi.paid_price 'paid_price'

  FROM ims_sales_order_item isoi

  LEFT JOIN ims_sales_order iso
  ON iso.id_sales_order = isoi.fk_sales_order

  LEFT JOIN oms_package_item opi
  ON opi.fk_sales_order_item = isoi.id_sales_order_item

  LEFT JOIN oms_package_dispatching opd
  ON opi.fk_package = opd.fk_package

  LEFT JOIN oms_shipment_provider osp
  ON opd.fk_shipment_provider = osp.id_shipment_provider

  LEFT JOIN ims_sales_order_item_status isois
  ON isois.id_sales_order_item_status = isoi.fk_sales_order_item_status

  GROUP BY isoi.id_sales_order_item