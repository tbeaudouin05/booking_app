SELECT
  isoi.id_sales_order_item
  ,iso.order_nr
  ,iso.payment_method 'payment_method'
  ,osp.shipment_provider_name 'shipment_provider_name'
  ,isoa.city 'billing_city'
  ,isoi.paid_price 'paid_price'
  ,iso.coupon_code 'coupon_code'
  ,isois.name 'item_status'
  ,refund_complete_creator.username AS refund_complete_creator

  FROM ims_sales_order_item isoi

  LEFT JOIN ims_sales_order iso
  ON isoi.fk_sales_order = iso.id_sales_order

  LEFT JOIN oms_package_item opi
  ON opi.fk_sales_order_item = isoi.id_sales_order_item

  LEFT JOIN oms_package_dispatching opd
  ON opi.fk_package = opd.fk_package

  LEFT JOIN oms_shipment_provider osp
  ON opd.fk_shipment_provider = osp.id_shipment_provider

  LEFT JOIN ims_sales_order_address isoa
  ON isoa.id_sales_order_address = iso.fk_sales_order_address_billing

  LEFT JOIN ims_sales_order_item_status isois
  ON isois.id_sales_order_item_status = isoi.fk_sales_order_item_status

  LEFT JOIN 
  ims_sales_order_item_status_history AS shipped_at
  ON isoi.id_sales_order_item = shipped_at.fk_sales_order_item
  AND shipped_at.fk_sales_order_item_status='5'

  LEFT JOIN ims_sales_order_item_status_history AS refund_complete
  ON isoi.id_sales_order_item = refund_complete.fk_sales_order_item
  AND refund_complete.fk_sales_order_item_status='56'

  LEFT JOIN ims_user AS refund_complete_creator
  ON refund_complete.fk_user = refund_complete_creator.id_user

  WHERE shipped_at.created_at IS NOT NULL

  GROUP BY isoi.id_sales_order_item

  

