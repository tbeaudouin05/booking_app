run_sc_query_f <- function() {
  
  start <- Sys.time()

  # load sc_date_filter_df
  sc_date_filter_df <- load_file_and_check_exist_f('2_input','sc_date_filter.xlsm','excel',sheet_name = 'do_not_change_name', open = T)
  
  # extract each criteria from sc_date_filter_df
  year_from <- sc_date_filter_df[[1]]
  month_from <- sc_date_filter_df[[2]]
  day_from <- sc_date_filter_df[[3]]
  year_to <- sc_date_filter_df[[4]]
  month_to<- sc_date_filter_df[[5]]
  day_to <- sc_date_filter_df[[6]]
  
  # prompt user about what timeframe is being booked
  writeLines(paste0('You are booking revenue for SC transactions between dates from ',day_from,'/',month_from,'/',year_from,' to ',day_to,'/',month_to,'/',year_to))
  
  tkmessageBox(title = 'Booking timeframe'
               , message = paste0('You are booking revenue for SC transactions between dates:\n from  ',day_from,'/',month_from,'/',year_from,'  to  ',day_to,'/',month_to,'/',year_to)
               ,type = 'ok')
    

  # create sc_date_filter
  sc_date_filter <- paste0('WHERE t.created_at BETWEEN'
                           ,' \'',year_from,'-',month_from,'-',day_from,' 00:00:00\' '
                           ,'AND'
                           ,' \'',year_to,'-',month_to,'-',day_to,' 23:59:59\'')
  
  
  # fetch the text of sc_query.txt
  unformatted_sc_query <- readLines('SQL/sc_query.txt')
  
  # format the text of unformatted_sc_query and formatted_sc_date_filter
  #NB: THERE SHOULD BE NO COMMENT IN THE SQL or this will not work
  formatted_sc_date_filter <- gsub("\t","", paste(sc_date_filter, collapse=" "))
  formatted_sc_query <- gsub("\t","", paste(unformatted_sc_query, collapse=" "))
  
  
  # create sc_query
  sc_query <- paste(formatted_sc_query
                    ,formatted_sc_date_filter)
  
  # format the text of sc_query to be readable by R
  #NB: THERE SHOULD BE NO COMMENT IN THE SQL or this will not work
  formatted_sc_query <- gsub("\t","", paste(sc_query, collapse=" "))
  
  
  sc_transaction <- run_query_wo_error_f(formatted_sc_query, is_sc_query = T)
  
  Encoding(sc_transaction$seller_name) <- "UTF-8"
  
  end <- Sys.time()
  
  writeLines(paste('Seller Center query time:',as.character(round(difftime(end,start,units = 'mins' ),digits = 0)),'minute(s)'))
  
  return(sc_transaction)
  
  

}