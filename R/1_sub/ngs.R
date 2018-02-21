ngs_f <- function(ipc_final,ipt_final, commission_revenue_and_vat,loop_number) {

  # prepare format file -----------------------------------------------------------------------------
  
  # get ipc_ipt_commission_format.xlsm
  ipc_ipt_commission_format <- load_file_and_check_exist_f('2_input/item_price_commission_format'
                                                          ,'ipc_ipt_commission_format.xlsm'
                                                          ,'excel'
                                                          ,sheet_name = 'do_not_change_name'
                                                          ,open = T)
  
  # check there is no null ledger in ipc_ipt_commission_format
  format_with_null_ledger <- ipc_ipt_commission_format[is.na(ipc_ipt_commission_format$ledger_order),]
  if (nrow(format_with_null_ledger) != 0) {tkmessageBox(title = paste('Empty ledgers!')
                                                        , message = 'There are empty ledgers in 2_input/item_price_commission_format/ipc_ipt_commission_format.xlsm.
This is not allowed and the program will break.', type = 'ok')}
  
  # change item_price_credit, item_price and commission to ipc, ipt and c
  ipc_ipt_commission_format$transaction_type <- gsub('item_price_credit','ipc',ipc_ipt_commission_format$transaction_type)
  ipc_ipt_commission_format$transaction_type <- gsub('item_price','ipt',ipc_ipt_commission_format$transaction_type)
  ipc_ipt_commission_format$transaction_type <- gsub('commission','c',ipc_ipt_commission_format$transaction_type)
  # format ipc_ipt_commission_format ledger and subledger the same way as ipc and ipt grouped
  ipc_ipt_commission_format$ledger_order <- paste(ipc_ipt_commission_format$ledger_order,ipc_ipt_commission_format$transaction_type, sep = '_' )
  ipc_ipt_commission_format$subledger_order <- paste(ipc_ipt_commission_format$subledger_order,ipc_ipt_commission_format$transaction_type, sep = '_' )
  # create merge key
  ipc_ipt_commission_format$merge_key <- paste0(ipc_ipt_commission_format$ledger_order,ipc_ipt_commission_format$subledger_order)
  # format key to exact same format as ipc_ipt_c_a_w_benef_code
  ipc_ipt_commission_format$merge_key <- gsub('NA_c','NA',ipc_ipt_commission_format$merge_key)
  ipc_ipt_commission_format$merge_key <- gsub('NA_ipt','NA',ipc_ipt_commission_format$merge_key)
  ipc_ipt_commission_format$merge_key <- gsub('NA_ipc','NA',ipc_ipt_commission_format$merge_key)
  # store ledger_order in a vector called 'ledger'
  ledger <- ipc_ipt_commission_format$ledger_order
  # erase unnecessary columns
  ipc_ipt_commission_format$ledger_order <- NULL
  ipc_ipt_commission_format$subledger_order <- NULL
  ipc_ipt_commission_format$transaction_type <- NULL
  
  
  # prepare ipc and ipt grouped -------------------------------------------------------------------------------------------
  
  # group ipc_final by seller_name, ledger and subledger
  ipc_grouped <- data.table(ipc_final)
  ipc_grouped <- ipc_grouped[, .(sum(transaction_value)), by=list(seller_name,ledger,subledger)]
  ipc_grouped <- data.frame(ipc_grouped)
  names(ipc_grouped)[4] <- 'transaction_value'
  # format ipt ledger and subledger
  ipc_grouped$ledger <- paste(ipc_grouped$ledger,'ipc', sep = '_')
  ipc_grouped$subledger <- paste(ipc_grouped$subledger,'ipc', sep = '_')
  
  # group ipt_final by seller_name, ledger and subledger
  ipt_grouped <- data.table(ipt_final)
  ipt_grouped <- ipt_grouped[, .(sum(transaction_value)), by=list(seller_name,ledger,subledger)]
  ipt_grouped <- data.frame(ipt_grouped)
  names(ipt_grouped)[4] <- 'transaction_value'
  # format ipt ledger and subledger
  ipt_grouped$ledger <- paste(ipt_grouped$ledger,'ipt', sep = '_')
  ipt_grouped$subledger <- paste(ipt_grouped$subledger,'ipt', sep = '_')

  # append ipc_grouped, ipt_grouped and commission_revenue_and_vat together ---------------------------------------------------------
  ipc_ipt_c_appended <- rbind(ipc_grouped, commission_revenue_and_vat, ipt_grouped)
  
  # source and use benef_code_f --------------------------------------------------------------------------------
  
  source('R/2_function/2_item_price_commission/benef_code.R')
  ipc_ipt_c_a_w_benef_code <- benef_code_f(ipc_ipt_c_appended,'ipc_ipt_c_a', loop_number)
  # erase seller name
  ipc_ipt_c_a_w_benef_code$seller_name <- NULL
  # create merge key for ipc_ipt_c_a_w_benef_code
  ipc_ipt_c_a_w_benef_code$merge_key <- paste0(ipc_ipt_c_a_w_benef_code$ledger,ipc_ipt_c_a_w_benef_code$subledger)
  
  # add format information to ipc_ipt_c_a_w_benef_code ----------------------------------------------------------------------
  
  # merge ipc_ipt_c_a_w_benef_code with ipc_ipt_commission_format on merge_key
  # NB: it is an inner join because we only want to keep ledgers which are BOTH in ipc_ipt_c_a_w_benef_code and ipc_ipt_commission_format
  # from what I understand unmatched ledgers should only correspond to amounts which were already paid by bamilo
  ipc_ipt_c_w_format <- merge(ipc_ipt_c_a_w_benef_code,ipc_ipt_commission_format, by = 'merge_key')
  # erase merge key
  ipc_ipt_c_w_format$merge_key <- NULL
  
  # calculate total_31002 at benef_code level ----------------------------------------------------------------------------------
  
  # create total_31002
  total_31002 <- ipc_ipt_c_w_format
  # add amount_total column to total_31002
  total_31002$amount_total <- total_31002$transaction_value * total_31002$total_calculation
  # erase total_31002$total_calculation
  total_31002$total_calculation <- NULL
  

  # change all ledgers to 31002
  total_31002$ledger <- '31002_total'
  
  # group total_31002 by ledger 31002 and benef_code
  total_31002  <- data.table(total_31002)
  total_31002  <- total_31002 [, .(sum(amount_total)), by=list(ledger,benef_code)]
  total_31002  <- data.frame(total_31002)
  names(total_31002 )[3] <- 'transaction_value'
  # change benef_code name to "subledger"
  names(total_31002 )[2] <- 'subledger'
  
  # calculate 62001 total at benef_code level ----------------------------------------------------------------------------------
  # create total_62001
  total_62001 <- ipc_ipt_c_w_format
  
  # filter total_62001 on ledger = 62001_c
  total_62001 <- total_62001[total_62001$ledger == '62001_c',]
  
  # group total_62001 by ledger 62001 and benef_code
  total_62001  <- data.table(total_62001)
  total_62001  <- total_62001 [, .(sum(transaction_value)), by=list(ledger,benef_code)]
  total_62001  <- data.frame(total_62001)
  names(total_62001 )[3] <- 'transaction_value'
  # change benef_code name to "subledger"
  names(total_62001 )[2] <- 'subledger'
  
  # calculate other totals at ledger / subledger level -------------------------------------------------------------------------
  # create total_other
  total_other <- ipc_ipt_c_w_format
  # filter out totals at benef_code level
  total_other <- total_other[total_other$is_booked_at_seller_level == 0,]
  
  # group total_other by ledger and subledger
  total_other  <- data.table(total_other)
  total_other  <- total_other [, .(sum(transaction_value)), by=list(ledger,subledger)]
  total_other  <- data.frame(total_other)
  names(total_other )[3] <- 'transaction_value'
  
  # create ngs_template wrong signs
  ngs_template_wrong_sign <- rbind(total_other, total_62001, total_31002)
  
  # re-add 'ledger' to ipc_ipt_commission_format  
  ipc_ipt_commission_format <- data.frame(ipc_ipt_commission_format,ledger)
  # erase unnecessary columns
  ipc_ipt_commission_format$total_calculation <- NULL
  ipc_ipt_commission_format$is_booked_at_seller_level <- NULL
  ipc_ipt_commission_format$merge_key <- NULL
  
  # add correct signs to ngs_template_wrong_sign
  ngs_template_right_sign <- merge(ngs_template_wrong_sign,ipc_ipt_commission_format, by = 'ledger')
  ngs_template_right_sign$transaction_value <- ngs_template_right_sign$transaction_value * ngs_template_right_sign$final_ngs_value
  ngs_template_right_sign$final_ngs_value <- NULL
  
  # erase _c, _ipt, _ipc from ledger and subledger
  ngs_template_right_sign$ledger <- gsub('_c','',ngs_template_right_sign$ledger)
  ngs_template_right_sign$ledger <- gsub('_ipc','',ngs_template_right_sign$ledger)
  ngs_template_right_sign$ledger <- gsub('_ipt','',ngs_template_right_sign$ledger)
  ngs_template_right_sign$ledger <- gsub('_total','',ngs_template_right_sign$ledger)
  ngs_template_right_sign$subledger <- gsub('_c','',ngs_template_right_sign$subledger)
  ngs_template_right_sign$subledger <- gsub('_ipc','',ngs_template_right_sign$subledger)
  ngs_template_right_sign$subledger <- gsub('_ipt','',ngs_template_right_sign$subledger)
  ngs_template_right_sign$subledger <- gsub('_total','',ngs_template_right_sign$subledger)
  
  # if account code = 62001 and transaction_value < 0 then account code is changed to 62003
  ngs_template_right_sign$ledger[ngs_template_right_sign$transaction_value < 0 & ngs_template_right_sign$ledger == '62001'] <- 62003
  
  
  ngs_format_and_output_f(ngs_template_right_sign,'ipc_ipt_c','ipc_ipt_c',loop_number)
  


  
  
}
