commission_f <- function(commission_df, commission_credit_df) {
  
  # group commission transaction_value by short_code
  commission_df_grouped <- data.table(commission_df)
  commission_df_grouped <- commission_df_grouped[, .(sum(transaction_value)), by=list(short_code)]
  commission_df_grouped <- data.frame(commission_df_grouped)
  names(commission_df_grouped)[2] <- 'transaction_value'
  
  # group commission credit transaction_value by short_code
  commission_credit_df_grouped <- data.table(commission_credit_df)
  commission_credit_df_grouped <- commission_credit_df_grouped[, .(sum(transaction_value)), by=list(short_code)]
  commission_credit_df_grouped <- data.frame(commission_credit_df_grouped)
  names(commission_credit_df_grouped)[2] <- 'transaction_value'
  
  commission_merged <- merge(commission_df_grouped,commission_credit_df_grouped, by = 'short_code', all = TRUE)
  commission_merged[is.na(commission_merged)] <- 0
  
  commission_merged$'62001_c' <- -(commission_merged$transaction_value.x + commission_merged$transaction_value.y)*100/109
  commission_merged$'32021_c' <- commission_merged$`62001` *0.09
  
  commission_merged$transaction_value.x <- NULL
  commission_merged$transaction_value.y <- NULL
  
  return(commission_merged)
  
}