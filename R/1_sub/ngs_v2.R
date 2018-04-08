ngs_v2_f <- function(ipc_final,ipt_final, commission_revenue_and_vat,loop_number) {
  
  # NB: the only ledgers booked at seller level are: 1. commission revenue and 2. total payout to seller - nothing else!
  
  # voucher value SIGN is correct already in ipc and ipt
  # however, transaction value SIGN is correct in ipc and commission but incorrect in ipt
  # hence we change transaction_value sign in ipt and commission
  ipt_final$paid_price <- ipt_final$paid_price * (-1)
  commission_revenue_and_vat$transaction_value <- commission_revenue_and_vat$transaction_value * (-1)
  
  # I added a _c at some point for ledgers in commission, just makes sure I get rid of this here
  commission_revenue_and_vat$ledger <- gsub('_c','',commission_revenue_and_vat$ledger)
  
  # store ledgers which are not booked at seller levels 
  # (all ledgers except commission revenue and total payout ie except 62001 and 31002)
  ledger_type_13004 <- c('13004','33001','31006','33002','84006','32021','94001')
  
  
  filter_group_f <- function(df_to_format,filter_list
                             ,group_list,sum_field,change_amount_sign = F) {
    
    # filter by ledger_type_v
    df_formatted <- df_to_format[df_to_format[,'ledger'] %in% filter_list,]
    # group by group_list
    df_formatted <- data.table(df_formatted)
    if (sum_field == 'voucher') {df_formatted <- df_formatted[, .(sum(voucher)), by=group_list]}
    else if (sum_field == 'paid_price') {df_formatted <- df_formatted[, .(sum(paid_price)), by=group_list]}
    else if (sum_field == 'transaction_value') {df_formatted <- df_formatted[, .(sum(transaction_value)), by=group_list]}
    else {df_formatted <- df_formatted[, .(sum(amount)), by=group_list]}
    df_formatted <- data.frame(df_formatted)
    
    if (change_amount_sign == T) {df_formatted$V1 <- df_formatted$V1 * (-1)}
    
    return(df_formatted)
  }
  
  map_benef_code_f <- function(df_to_map){
  
    # map with benef_code, rename benef_code to subledger and erase short_code
    df_formatted <- benef_code_f(df_to_map,'random',0)
    names(df_formatted)[1] <- 'subledger'
    df_formatted$short_code <- NULL
    
    return(df_formatted)
    
  }
  
  # NB: you cannot simply reproduce the logic of total 31002 here
  # BECAUSE it is more complexe than this: here, you cannot append everything
  # because for ipt vs. ipc amounts you want to see BOTH credit and debit sides
  # not the sum of credit + debit! 
  # (whereas in the total you precisely want to see this: the complete sum of debit + credit)
  
  # voucher_62002 ------------------------------------------------------------------------------------------
  # append ipc_final and ipt_final
  ipc_ipt_a <- rbind(ipc_final,ipt_final)
  
  
  # NO should be included: this amount is correct if you include all
  # but the blanks should be corrected from the 31002 amounts for everything except voucher
  # not in voucher
  # ipc_ipt_a <- ipc_ipt_a[ipc_ipt_a[,'ledger'] != 'blank',] 
  
  # change all ledgers to 62002 
  # and erase subledgers to aggregate on ledger and get the total sum of voucher
  ipc_ipt_a$ledger <- '62002'
  ipc_ipt_a$subledger <- ''
  
  voucher_62002 <- filter_group_f(df_to_format = ipc_ipt_a
                                  ,filter_list = '62002'
                                  ,group_list = c('ledger','subledger')
                                  ,sum_field = 'voucher')
  
  # deal with 13004 ledger type (booked at ledger / subledger level) ---------------------------------------------------------------------------------------------------
  # item price credit
  ipc_final_13004 <- filter_group_f(df_to_format = ipc_final
                                    ,filter_list = ledger_type_13004
                                    ,group_list = c('ledger','subledger')
                                    ,sum_field = 'paid_price')
  
  # item price
  ipt_final_13004 <- filter_group_f(df_to_format = ipt_final
                                    ,filter_list = ledger_type_13004
                                    ,group_list = c('ledger','subledger')
                                    ,sum_field = 'paid_price')
  
  # commission vat
  c_final_13004 <- filter_group_f(df_to_format = commission_revenue_and_vat
                                    ,filter_list = ledger_type_13004
                                    ,group_list = c('ledger','subledger')
                                    ,sum_field = 'transaction_value')
  
  # deal with commission revenue = ledger 62001 -----------------------------------------------------------------------------------------------
  # along with the total payout to sellers (ledger 31002), it is the only ledger booked for every seller
  c_final_62001 <- filter_group_f(df_to_format = commission_revenue_and_vat
                                  ,filter_list = '62001'
                                  ,group_list = c('ledger','short_code')
                                  ,sum_field = 'transaction_value')
  c_final_62001 <- map_benef_code_f(c_final_62001)
  
  # calculate 31002 total ---------------------------------------------------------------------------------------
  
  # since voucher, ipt, ipc and commission all have correct sign
  # we can just keep short_code and transaction_value columns for ipc, ipt and commission
  # short_code and voucher columns for voucher, 
  # rename all amounts (voucher and transaction value) to amount
  # append ipc, ipt, commission and voucher together
  # then add the ledger 31002 and group by sellers!  and finally map benef_code
  
  # keep appropriate columns for ipc, ipt, commission and voucher - and rename all amounts to 'amount'
  ipc_s <- data.frame('short_code' = ipc_final$short_code
                      ,'ledger' = ipc_final$ledger
                      ,'amount' = ipc_final$paid_price)
  ipt_s <- data.frame('short_code' = ipt_final$short_code
                      ,'ledger' = ipt_final$ledger
                      ,'amount' = ipt_final$paid_price)
  c_revenue_and_vat_s <- data.frame('short_code' = commission_revenue_and_vat$short_code
                                    ,'ledger' = commission_revenue_and_vat$ledger
                                    ,'amount' = commission_revenue_and_vat$transaction_value)
  voucher_s <- data.frame('short_code' = ipc_ipt_a$short_code
                          ,'ledger' = ipc_ipt_a$ledger
                          ,'amount' = ipc_ipt_a$voucher)
  
  # append ipc_s, ipt_s, c_revenue_and_vat_s and voucher_s
  final_31002 <- rbind(ipc_s, ipt_s)
  
  # exclude blank ledgers only from ipc_s and ipt_s cause blank appear during mapping related only to these guys
  final_31002 <- final_31002[final_31002[,'ledger'] != 'blank',]
  
  final_31002 <- rbind(voucher_s,final_31002,c_revenue_and_vat_s)
  
  # add ledger 31002
  final_31002$ledger <- '31002'
  
  # group by seller and ledger 31002 - and map benef_code
  # since everything is booked with a certain sign, 
  # then the total should be booked with the oppostive sign in order to balance to 0
  # hence change_amount_sign = T
  final_31002 <- filter_group_f(df_to_format = final_31002
                                  ,filter_list = '31002'
                                  ,group_list = c('ledger','short_code')
                                  ,sum_field = 'amount'
                                  ,change_amount_sign = T)
  final_31002 <- map_benef_code_f(final_31002)
  
  
  
  
  # create ledger_subledger_amount: the basis of ngs_template -----------------------------------------------
  # we just append all the different amounts created
  ledger_subledger_amount <- rbind(voucher_62002
                                   ,ipc_final_13004
                                   ,ipt_final_13004
                                   ,c_final_13004
                                   ,c_final_62001
                                   ,final_31002)
  
  
  
  # format into the final ngs_template --------------------------------------
  ngs_format_and_output_f(ledger_subledger_amount_df = ledger_subledger_amount
                          ,map_name = 'ipc_ipt_c'
                          ,process_name = 'ipc_ipt_c'
                          ,loop_number)
  
  
  
  
}