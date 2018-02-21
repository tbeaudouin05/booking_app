run_oms_query_f <- function(ipc_ship_oms_id_soi, ipt_rcc_oms_id_soi){
  
  start <- Sys.time()
  
  # create filters to filter only the id of ipc_ship_oms_id_soi and ipt_rcc_oms_id_soi needed
  ipc_filter <- paste0('\'',gsub(',','\',\'',paste(unlist(ipc_ship_oms_id_soi),collapse=",")),'\'')
  ipt_filter <- paste0('\'',gsub(',','\',\'',paste(unlist(ipt_rcc_oms_id_soi),collapse=",")),'\'')
  
  # fetch the text of oms_query.txt
  unformatted_oms_query <- readLines('SQL/oms_query.txt')
  
  # format the text of unformatted_oms_query
  #NB: THERE SHOULD BE NO COMMENT IN THE SQL or this will not work
  formatted_oms_query <- gsub("\t","", paste(unformatted_oms_query, collapse=" "))
  
  # create oms_ship_query and oms_rcc_query
  oms_ship_query <- paste(formatted_oms_query
                          , 'AND isoi.id_sales_order_item IN (',ipc_filter,')'
                          ,'GROUP BY isoi.id_sales_order_item')

  oms_rcc_query <- paste(formatted_oms_query
                         ,'AND isoi.id_sales_order_item IN (',ipt_filter,')'
                        ,'GROUP BY isoi.id_sales_order_item')
  
                    
  # format the text of oms_ship_query and oms_rcc_query
  #NB: THERE SHOULD BE NO COMMENT IN THE SQL or this will not work
  formatted_oms_ship_query <- gsub("\t","", paste(oms_ship_query, collapse=" "))
  formatted_oms_rcc_query <- gsub("\t","", paste(oms_rcc_query, collapse=" "))
  
  writeLines('Fetching oms_ship from OMS database:')
  oms_ship <- run_query_wo_error_f(formatted_oms_ship_query, is_sc_query = F)
  writeLines('Fetching oms_rrc from OMS database:')
  oms_rcc <- run_query_wo_error_f(formatted_oms_rcc_query, is_sc_query = F)
  
  Encoding(oms_ship$billing_city) <- 'UTF-8'
  Encoding(oms_rcc$billing_city) <- 'UTF-8'
  
  end <- Sys.time()
  
  oms_df_list <- list(oms_ship,oms_rcc)
  
  writeLines(paste('OMS query time:',as.character(round(difftime(end,start,units = 'mins' ),digits = 0)),'minute(s)'))
  
  return(oms_df_list)
  
  
}