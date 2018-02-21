map_item_price_to_commission_f <- function(ipc_final_pivot, ipt_final_pivot, commission_final_pivot) {
  
  
  ipc_ipt_final_pivot <- merge(ipt_final_pivot,ipc_final_pivot, by = 'seller_name', all = TRUE)
  ipc_ipt_commission_final_pivot <- merge(ipc_ipt_final_pivot,commission_final_pivot, by = 'seller_name', all = TRUE)
  
  ipc_ipt_commission_final_pivot[is.na(ipc_ipt_commission_final_pivot)] <- 0
  
  
  return(ipc_ipt_commission_final_pivot)

}