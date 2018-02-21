commission_append_f <- function(commission_df, commission_credit_df) {
  
  # group commission transaction_value by Seller Name
  commission_df_grouped <- data.table(commission_df)
  commission_df_grouped <- commission_df_grouped[, .(sum(transaction_value)), by=list(seller_name)]
  commission_df_grouped <- data.frame(commission_df_grouped)
  names(commission_df_grouped)[2] <- 'transaction_value'
  
  # group commission credit transaction_value by Seller Name
  commission_credit_df_grouped <- data.table(commission_credit_df)
  commission_credit_df_grouped <- commission_credit_df_grouped[, .(sum(transaction_value)), by=list(seller_name)]
  commission_credit_df_grouped <- data.frame(commission_credit_df_grouped)
  names(commission_credit_df_grouped)[2] <- 'transaction_value'
  
  commission_merged <- merge(commission_df_grouped,commission_credit_df_grouped, by = 'seller_name', all = TRUE)
  commission_merged[is.na(commission_merged)] <- 0
  
  commission_merged_1 <- commission_merged
  commission_merged_2 <- commission_merged
  
  # define the two data frames to append
  # commission revenue and commission VAT are considered 2 different "transaction type" ie two different ledgers
  
  # commission revenue
  commission_merged_1$transaction_value <- -(commission_merged_1$transaction_value.x + commission_merged_1$transaction_value.y)*100/109
  commission_merged_1$ledger <- '62001_c'
  commission_merged_1$transaction_value.x <- NULL
  commission_merged_1$transaction_value.y <- NULL
  # commission VAT
  commission_merged_2$transaction_value <- (-(commission_merged_2$transaction_value.x + commission_merged_2$transaction_value.y)*100/109)*0.09
  commission_merged_2$ledger <- '32021_c'
  commission_merged_2$transaction_value.x <- NULL
  commission_merged_2$transaction_value.y <- NULL
  
  commission_appended <- rbind(commission_merged_1,commission_merged_2)
  commission_appended$subledger <- NA
  
  return(commission_appended)
  
}