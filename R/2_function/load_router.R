load_router_f <- function (sc_transaction, seller_id_already_booked){
  
  
  writeLines('Determining remaining transactions to be booked and the maximum number of transactions to be booked in next batch...')
  
  # if this is the first loop, 
  # then take rows_not_booked_yet = all the rows returned by the query
  # sc_transaction_not_booked_yet = sc_transaction
  if (length(seller_id_already_booked) == 0) {
    
    rows_not_booked_yet <- nrow(sc_transaction)
    sc_transaction_not_booked_yet <- sc_transaction
    
  
  # else sc_transaction_not_booked_yet = sc_transaction - rows already booked
  # rows_not_booked_yet = nrow(sc_transaction_not_booked_yet)
  } else {
    
    # enable flagging sellers already booked
    seller_id_already_booked_flag <- data.frame ('id_seller' = seller_id_already_booked, 'flag' = 1)
    # flag sellers already booked in sc_transaction_not_booked_yet: sellers not booked yet will have flag = NA
    sc_transaction_not_booked_yet <-  merge(sc_transaction, seller_id_already_booked_flag, by = 'id_seller', all.x = TRUE)
    # only keep sellers which have not been booked yet ie sellers with flag = NA
    sc_transaction_not_booked_yet <- sc_transaction_not_booked_yet[is.na(sc_transaction_not_booked_yet[,'flag']),]
    # erase the flag from sc_transaction_not_booked_yet
    sc_transaction_not_booked_yet$flag <- NULL
    # count the number of rows not booked yet
    rows_not_booked_yet <- nrow(sc_transaction_not_booked_yet)
    }
  
  # if all the rows have been booked already then return "stop"
  if (rows_not_booked_yet == 0) { 
    
    return(NULL)
  
  # else if thenumber of rows which have not been booked yet are less than 500,000
  # then do the booking for all these remaining rows
  } else if (rows_not_booked_yet <= 1000000) {
  
  return(sc_transaction_not_booked_yet)
  
  # else only book transactions for a few sellers with the some of their transactions <= 450000 rows
} else {
  
    # identify sellers which have not been booked yet
    seller_id_not_booked_yet <- unique(sc_transaction_not_booked_yet$id_seller)
    
    # initialize head and rows_to_book
    head <- head(seller_id_not_booked_yet)
    rows_to_book <- 0
    
    # initialize sc_transaction_to_book with the transaction of the first sellers which has not been booked yet
    sc_transaction_to_book <- sc_transaction[sc_transaction[,'id_seller'] == seller_id_not_booked_yet[[1]],]
    
    # add the transactions of sellers which have not been boooked yet 
    # until the number of transactions reaches at least 450,000
    # or until there is no seller to book anymore
    while (rows_to_book <= 950000 & length(head) != 0){
      
      # Take 100 first sellers
      head <- head(seller_id_not_booked_yet,150)
      
      # erase first seller of the head list since sc_transaction_to_book already initialized with it
      head <- head[-1]
      # create head_flag
      head_flag <- data.frame('id_seller' = head, 'flag' = 1)
      
      # only keep transactions related to sellers in head
      # flag sc_transaction
      sc_transaction_flagged <- merge(sc_transaction, head_flag, by = 'id_seller', all.x =T)
      # filter sc_transaction only for flagged sellers
      sc_transaction_filtered <- sc_transaction_flagged[!is.na(sc_transaction_flagged$flag),]
      # erase flag
      sc_transaction_filtered$flag <- NULL
      
      # append sc_transaction_filtered to sc_transaction_to_book until rows to book are enough or length(head) = 0 ie no sellers anymore
      sc_transaction_to_book <- rbind(sc_transaction_to_book
                                      ,sc_transaction_filtered)
      
      # erase 100 first sellers from seller_id_not_booked_yet so because they have been taken into account now - and should not appear in head anymore (otherwise head length never equals 0)
      # when head
      seller_id_not_booked_yet <- seller_id_not_booked_yet[-1:-50]
      # calculate number of rows taken into account for now - when reaching the set limit, the loop ends
      rows_to_book <- nrow(sc_transaction_to_book)
      
      writeLines(paste0('Number of transactions to be processed: ',rows_to_book,'...'))
    }
  
    return(sc_transaction_to_book)
    
    # ALL THAT IS LEFT TO CODE IS TO HAVE A LOOP IN BOOKING WHICH RECORDS SELLER ID ALREADY BOOKED 
    # AND MAKE THE LOOP STOP WHEN THIS FUNCTION RETURNS STOP
    # AND MAKE SURE THAT THE STUFF PRINTED TO COMPUTER ARE DIFFERENT DEPENDING ON THE LOOP NUMBER
  
}
  
}