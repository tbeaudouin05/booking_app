ngs_other_f <- function(transaction_type_df
                        ,revenue_ledger
                        ,negative_revenue_ledger = revenue_ledger
                        ,map_name
                        ,process_name
                        ,is_vat_process = T
                        ,loop_number){
  
  file_path <- paste0('3_output/t_type_pivot_check/',process_name,'_pivot_check',loop_number,'.csv')
  folder <- '3_output/t_type_pivot_check'
  file <- paste0(process_name,'_pivot_check',loop_number,'.csv')
  
  if (nrow(transaction_type_df) > 0){
  if (is_vat_process) {
  
  # group transaction_type_df by short_code, sum(transaction_value)
  transaction_type_df <- data.table(transaction_type_df) 
  transaction_type_df <- transaction_type_df[, .(sum(transaction_value)), by=list(short_code)]
  transaction_type_df <- data.frame(transaction_type_df)
  names(transaction_type_df)[2] <- 'transaction_value'
  
  # add revenue and VAT
  transaction_type_df$revenue <- transaction_type_df$transaction_value * 100/109
  transaction_type_df$vat <- transaction_type_df$revenue * 0.09
  
  # map benef_code
  transaction_type_df <- benef_code_f(transaction_type_df, process_name, loop_number)
  
  # if pivot table to check exists then archive it
  # save pivot table to check
  if (file.exists(file_path)){
    archive_f(folder,file)}
  
  dir.create(file.path(getwd(),folder),showWarnings = FALSE)
  write_excel_csv(transaction_type_df, file_path)
  
  # transaction_value ledger at seller level
  transaction_value <- data.frame(31002,transaction_type_df$benef_code,transaction_type_df$transaction_value)
  names(transaction_value)[1] <- 'Account Code'
  names(transaction_value)[2] <- 'Account Free'
  names(transaction_value)[3] <- 'Amount'
  # for 31002: take amount opposite sign
  transaction_value$Amount <- -transaction_value$Amount
  
  # revenue ledger at seller level
  revenue <- data.frame(revenue_ledger,transaction_type_df$benef_code,transaction_type_df$revenue)
  names(revenue)[1] <- 'Account Code'
  names(revenue)[2] <- 'Account Free'
  names(revenue)[3] <- 'Amount'

  
  # vat ledger at ledger level
  vat <- data.frame(32021,'',transaction_type_df$vat)
  names(vat)[1] <- 'Account.Code'
  names(vat)[2] <- 'Account.Free'
  names(vat)[3] <- 'Amount'
  # group vat by Account.Code, sum(Amount) (Account Free is null for all ligns at this point)
  vat <- data.table(vat) 
  vat <- vat[, .(sum(Amount)), by=list(Account.Code, Account.Free)]
  vat <- data.frame(vat)
  names(vat)[1] <- 'Account Code'
  names(vat)[2] <- 'Account Free'
  names(vat)[3] <- 'Amount'
  
  # append vat, transaction_value and revenue
  ledger_subledger_amount_df <- rbind(transaction_value,revenue,vat)
  
  # erase lines with Amount = 0
  ledger_subledger_amount_df <- ledger_subledger_amount_df[ledger_subledger_amount_df$Amount != 0,]
  
  # if account code = revenue_ledger and Amount < 0 then account code is changed to negative_revenue_ledger
  ledger_subledger_amount_df$`Account Code`[ledger_subledger_amount_df$Amount < 0 & ledger_subledger_amount_df$`Account Code` == revenue_ledger] <- negative_revenue_ledger
  
  
  # format and output ngs template
  ngs_format_and_output_f(ledger_subledger_amount_df,map_name,process_name,loop_number)
  
  } else {
    
    # group transaction_type_df by short_code, sum(Amount)
    transaction_type_df <- data.table(transaction_type_df) 
    transaction_type_df <- transaction_type_df[, .(sum(transaction_value)), by=list(short_code)]
    transaction_type_df <- data.frame(transaction_type_df)
    names(transaction_type_df)[2] <- 'transaction_value'
    
    # map benef_code
    transaction_type_df <- benef_code_f(transaction_type_df, process_name, loop_number)
    
    # if pivot table to check exists then archive it
    # save pivot table to check
    if (file.exists(file_path)){
      archive_f(folder,file)}
    
    dir.create(file.path(getwd(),folder),showWarnings = FALSE)
    write_excel_csv(transaction_type_df, file_path)
    
    # transaction_value ledger at seller level (opposite sign of transaction_value)
    transaction_value <- data.frame('Account Code' = 31002
                                    ,'Account Free' = transaction_type_df$benef_code
                                    ,'Amount' = - transaction_type_df$transaction_value)
    
    # revenue_cost ledger at ledger level
    revenue_cost <- data.frame('Account.Code' = revenue_ledger
                               ,'Account.Free' = ''
                               ,'Amount' = transaction_type_df$Amount)
    
    # group revenue_cost by Account.Code, sum(transaction_value) (Account Free is null for all ligns at this point)
    revenue_cost <- data.table(revenue_cost) 
    revenue_cost <- revenue_cost[, .(sum(transaction_value)), by=list(Account.Code, Account.Free)]
    revenue_cost <- data.frame(revenue_cost)
    names(revenue_cost)[1] <- 'Account Code'
    names(revenue_cost)[2] <- 'Account Free'
    names(revenue_cost)[3] <- 'Amount'
    
    # append amount and revenue_cost
    ledger_subledger_amount_df <- rbind(revenue_cost,transaction_value)
    
    # erase lines with Amount = 0
    ledger_subledger_amount_df <- ledger_subledger_amount_df[ledger_subledger_amount_df$Amount != 0,]
    
    # format and output ngs template
    ngs_format_and_output_f(ledger_subledger_amount_df,map_name,process_name,loop_number)
    
  }
  }
  
}