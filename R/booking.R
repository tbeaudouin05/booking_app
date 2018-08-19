# I_program_environment_setting -------------------------------------------------------------------

# turn off warnings
options(warn=-1)

# shows working_directory
print(getwd())

# clear previous environment
rm(list=ls())

# record start time of program
start_t <- Sys.time()

writeLines(paste('Start time:', as.character(Sys.time())))

# call libraries 
library(openxlsx)
library(RODBC)
library(class)
library(XLConnect)
library(arules)
library(data.table)
library(tidyverse)
library(readr)
library(tcltk2)
library(excel.link)
library(reshape)
library(svMisc)
library(RMySQL)
library(lubridate)
library(yaml)

# set system options 
Sys.setlocale(locale = "persian")
options(scipen=999)

# create output folder
dir.create(file.path(getwd(), '3_output'),showWarnings = FALSE)
# create temp_input folder
dir.create(file.path(getwd(), '4_temp_input'),showWarnings = FALSE)

# close all Excel workbooks to avoid errors
for (wb in xl.workbooks()) {xl.workbook.close(wb)}

# source global environment functions
source('R/2_function/start_fresh.R')
source('R/2_function/load_router.R')
source('R/2_function/archive.R')
source('R/2_function/write_excel_w_error_handling.R')
source('R/2_function/load_file_and_check_exist.R')
source('R/2_function/run_query_without_error.R')
source('R/2_function/ngs_format_and_output.R')
source('R/2_function/1_prepare_sc_transaction_type/prepare_transform_sc_data.R')
source('R/2_function/1_prepare_sc_transaction_type/prepare_each_sc_transaction_type.R')
source('R/2_function/1_prepare_sc_transaction_type/save_transaction_type.R')
source('R/2_function/2_item_price_commission/benef_code.R')
source('R/1_sub/run_sc_query.R')
source('R/1_sub/run_oms_query.R')
source('R/2_function/2_item_price_commission/merge_sc_oms.R')
source('R/2_function/2_item_price_commission/user_interaction_new_input.R')
source('R/2_function/2_item_price_commission/user_interaction_update_input.R')
source('R/1_sub/ngs_v2.R')
source('R/1_sub/ngs_other.R')
source('R/2_function/2_item_price_commission/benef_code.R')
source('R/2_function/2_item_price_commission/ledger_map_check.R')
source('R/2_function/2_item_price_commission/update_input_cond_format.R')
source('R/2_function/2_item_price_commission/prepare_missing_field_df.R')
source('R/2_function/2_item_price_commission/check_all_format.R')
source('R/2_function/2_item_price_commission/format_final_pivot.R')
source('R/1_sub/item_price_credit.R')
source('R/1_sub/item_price.R')
source('R/2_function/2_item_price_commission/pivot_ledger_seller_voucher.R')

Sys.sleep(0.5)

# asks user if she wants to erase existing temporary inputs or re-run the same session
start_fresh_f()

# II_prepare_sc_transaction_data ------------------------------------------------------------------
writeLines('PREPARE SC TRANSACTION DATA')

# get sc_transaction data from Seller Center database
sc_transaction <- run_sc_query_f()

# update conditional formatting of input from OMS database
writeLines('Updating conditional formatting of input from OMS database')
update_input_cond_format_f()

rows_t <- nrow(sc_transaction)
writeLines(paste('Number of rows / transactions to be processed: ', rows_t))

seller_id_already_booked <- 0
sc_transaction_to_book <- data.frame()
loop <- 1

# check at very beginning there are no missing ledgers
ledger_map_check_f(sc_transaction$oms_id_sales_order_item)

# check all necessary benef_code are here
temp <- benef_code_f(sc_transaction,'sc_transaction',0)
temp <- NULL

## while there are still transactions to book run, the process again and again
while (is.data.frame(sc_transaction_to_book)) {
  
  sc_transaction_to_book <- load_router_f(sc_transaction, seller_id_already_booked)
  
if (is.data.frame(sc_transaction_to_book)) {
  
  seller_id_to_book <- unique(sc_transaction_to_book$id_seller)
  rows <- nrow(sc_transaction_to_book)
  
  writeLines(paste('Number of rows / transactions to be processed in batch number',loop,':', rows))


  # keep seller_name / short_code mapping
  short_code_map <- data.table(sc_transaction_to_book)
  short_code_map <- short_code_map[, .(sum(transaction_value)), by=list(seller_name, short_code)]
  short_code_map <- data.frame(short_code_map)
  short_code_map$V1 <- NULL

## run prepare_transform_sc_data_f
sc_transaction_wo_retail <- prepare_transform_sc_data_f(sc_transaction_to_book)

## run prepare_each_sc_transaction_type_f
transaction_type_prepared <- prepare_each_sc_transaction_type_f(sc_transaction_wo_retail)

transaction_type_df_list <- transaction_type_prepared[[1]]
transaction_type_df_list_w_comment <- transaction_type_prepared[[2]]

transaction_type_prepared <- NULL

# save each transaction type into csv for Finance to check
save_transaction_type_f(transaction_type_df_list,transaction_type_df_list_w_comment,loop)


# III_prepare_oms_ship_and_oms_rcc_data -----------------------------------------------------------

# get oms_ship and oms_rcc data from OMS database based on filtering found in sc_transaction item_price_credit and item_price
oms_df_list <- run_oms_query_f(
                ipc_ship_oms_id_soi = transaction_type_df_list$`Item Price Credit`$oms_id_sales_order_item
                ,ipt_rcc_oms_id_soi = transaction_type_df_list$`Item Price`$oms_id_sales_order_item)

oms_ship <- oms_df_list[[1]]
oms_rcc <- oms_df_list[[2]]

oms_df_list <- NULL

# IV_item_price_credit_ipc --------------------------------------------------------------------
writeLines('LAUNCH ITEM PRICE CREDIT PROCESS')

# get and run item_price_f function
ipc_final <- item_price_credit_f(transaction_type_df_list$`Item Price Credit`
                                 ,oms_ship
                                 ,loop)


# run pivot_ledger_seller_voucher_f function
ipc_final_pivot <- pivot_ledger_seller_voucher_f(ipc_final)


# get and run format_final_pivot_f function
ipc_pivot_formatted <- format_final_pivot_f(ipc_final_pivot,'ipc', is_final = F)


# V_item_price_transaction_ipt -------------------------------------------------------------------
writeLines('LAUNCH ITEM PRICE PROCESS')

# only execute if item price credit process was successfully executed
if (is.data.frame(ipc_pivot_formatted)) {

# get and run item_price_f function

ipt_final <- item_price_f(transaction_type_df_list$`Item Price`
                          ,oms_rcc
                          ,loop)

# source and run pivot_ledger_seller_voucher_f function
source('R/2_function/2_item_price_commission/pivot_ledger_seller_voucher.R')
ipt_final_pivot <- pivot_ledger_seller_voucher_f(ipt_final)

# run format_final_pivot_f function again
ipt_pivot_formatted <- format_final_pivot_f(ipt_final_pivot,'ipt', is_final = F)


# VI_merge_commission_item_price_benef_code -------------------------------------------------------------------
writeLines('LAUNCH COMBINED COMMISSION, ITEM PRICE CREDIT AND ITEM PRICE PROCESS')

# only execute if item price credit process was successfully executed
if (is.data.frame(ipt_pivot_formatted)) {
  
  # get and run commission_f function
  source('R/1_sub/commission.R')
  commission_final_pivot <- commission_f(transaction_type_df_list$Commission
                                         ,transaction_type_df_list$`Commission Credit`)
  
  # get and run map_item_price_to_commission_f function
  source('R/1_sub/map_item_price_to_commission.R')
  ipc_ipt_commission_final_pivot <- map_item_price_to_commission_f(ipc_pivot_formatted
                                                                   ,ipt_pivot_formatted
                                                                   ,commission_final_pivot)
  
# VII_format_ipc_ipt_commission_check_pivot --------------------------------------------------------------------------------------
  writeLines('CREATE IPT_IPC_COMMISSION PIVOT CHECK')
  
  # run format_ipt_pivot_f function
  ipc_ipt_commission_pivot_formatted <- format_final_pivot_f(ipc_ipt_commission_final_pivot,'ipc_ipt_commission', is_final = T)
  
  # run benef_code_f
  ipc_ipt_c_w_benef_code <- benef_code_f(ipc_ipt_commission_pivot_formatted
                                         ,'ipc_ipt_c_formatted'
                                         ,loop_number = loop)
  
  # add seller_name
  ipc_ipt_c_w_benef_code <- merge(short_code_map,ipc_ipt_c_w_benef_code, by = 'short_code', all.y = TRUE)
  

  # output ipc_ipt_c_w_benef_code.csv into ipc_ipt_c_pivot_check.csv for user to check relevance
  # if ipc_ipt_c_pivot_check.csv already exists, archive it
  dir.create(file.path(getwd(),'3_output/t_type_pivot_check'),showWarnings = FALSE)
  file_path_1 <- paste0('3_output/t_type_pivot_check/ipc_ipt_c_pivot_check',loop,'.csv')
  if (file.exists(file_path_1)) {archive_f('3_output/t_type_pivot_check',paste0('ipc_ipt_c_pivot_check',loop,'.csv'))}
  write_excel_csv(ipc_ipt_c_w_benef_code,file_path_1)
  
  
# VIII_format_ngs_upload_template ----------------------------------------------------------------------------------------------------
  writeLines('CREATE IPT_IPC_C NGS TEMPLATE')
  
  # get and run commission_append_f function
  # commission has to be appended not merged if we want to directly get ngs template in a "SQL way"
  source('R/1_sub/commission_append.R')
  commission_revenue_and_vat <- commission_append_f(transaction_type_df_list$Commission
                                                    ,transaction_type_df_list$`Commission Credit`)

  # get and run ngs_f sub: output ngs template in order to upload to NGS
  ngs_v2_f(ipc_final
        ,ipt_final
        ,commission_revenue_and_vat
        ,loop)
  

  
  sc_transaction_wo_retail <- NULL
  #ipc_final <- NULL
  #ipt_final <- NULL
  ipc_ipt_commission_final_pivot<- NULL
  ipc_ipt_commission_pivot_formatted <- NULL
  ipc_ipt_c_w_benef_code <- NULL
  commission_revenue_and_vat <- NULL
  commission_final_pivot <- NULL
  ipc_pivot_formatted <- NULL
  ipt_pivot_formatted <- NULL
  ipt_final_pivot <- NULL
  ipc_final_pivot <- NULL
  

# IX_cancellation_penalty_within_24hrs ----------------------------------------------------------------------
  writeLines('CREATE cancel_wi_24 NGS TEMPLATE')
  
  # run ngs_other_f sub
  ngs_other_f(transaction_type_df_list$` Cancellation Penalty - (Within 24 Hrs)`
              ,revenue_ledger = '62009'
              ,negative_revenue_ledger = '62010'
              ,map_name = 'cancel_wi_24'
              ,process_name = 'cancel_wi_24'
              ,loop_number = loop)
  
# X_cancellation_penalty_after_24hrs ----------------------------------------------------------------------
  writeLines('CREATE cancel_after_24 NGS TEMPLATE')

  # run ngs_other_f sub
  ngs_other_f(transaction_type_df_list$`Cancellation Penalty  (After 24 Hrs)`
              ,revenue_ledger = '62009'
              ,negative_revenue_ledger = '62010'
              ,map_name = 'cancel_after_24'
              ,process_name = 'cancel_after_24'
              ,loop_number = loop)
  
# XI_storage_fee ----------------------------------------------------------------------
  writeLines('CREATE storage_fee NGS TEMPLATE')
  
  # run ngs_other_f sub
  ngs_other_f(transaction_type_df_list$`Storage Fee`
              ,revenue_ledger = '62005'
              ,negative_revenue_ledger = '62011'
              ,map_name = 'storage_fee'
              ,process_name = 'storage_fee'
              ,loop_number = loop)
  
  
# XII_handling_fee ----------------------------------------------------------------------
  writeLines('CREATE handling_fee NGS TEMPLATE')
  
  # run ngs_other_f sub
  ngs_other_f(transaction_type_df_list$`Handling Fee`
              ,revenue_ledger = '62005'
              ,negative_revenue_ledger = '62011'
              ,map_name = 'handling_fee'
              ,process_name = 'handling_fee'
              ,loop_number = loop)
  
  
  # XIII_shipping_fee ----------------------------------------------------------------------
  writeLines('CREATE shipping_fee NGS TEMPLATE')
  
  # append shipping fee and shipping fee credit
  shipping_fee_df <- rbind(transaction_type_df_list$`Shipping Fee (Order Level)`,transaction_type_df_list$`Shipping Fee (Order Level) Credit`)
  
  
  # run ngs_other_f sub
  ngs_other_f(shipping_fee_df
              ,revenue_ledger = '62004'
              ,negative_revenue_ledger = '62008'
              ,map_name = 'shipping_fee'
              ,process_name = 'shipping_fee'
              ,loop_number = loop)
  
  
  # XIV_lost_or_damage ----------------------------------------------------------------------
  writeLines('CREATE lost_damage NGS TEMPLATE')
  
  
  # run ngs_other_f sub without VAT process
  ngs_other_f(transaction_type_df_list$'Lost or Damaged (Product Level) Credit'
              ,revenue_ledger = '84005'
              ,negative_revenue_ledger = '84005'
              ,map_name = 'lost_damage'
              ,process_name = 'lost_damage'
              ,is_vat_process = F
              ,loop_number = loop)
  

  # update seller_id_already_booked to make the while loop stop at some point
  seller_id_already_booked <- c(seller_id_already_booked, seller_id_to_book)
  
  # update loop_number
  loop <- loop + 1 
  

}

}

}
  
}

# remove map_missing temporary files from 4_temp_input folder
file_to_erase <- list.files('4_temp_input/',pattern = "_map_missing")
if (length(file_to_erase) > 0){
  for (i in c(1:length(file_to_erase))) {
    archive_f('4_temp_input/',file_to_erase[i])}}

writeLines('END OF PROGRAMME')
writeLines('----------------------------------------------------------------------------------------------------')
writeLines('CHECK OUT THE NGS FILES IN THE FOLDER 3_OUTPUT :)')

# record end time of program
end_t <- Sys.time()

writeLines(paste('Total process time:',as.character(round(difftime(end_t,start_t,units = 'mins' ),digits = 0)),'minute(s)'))

tkmessageBox(title = 'END OF PROGRAMME'
             , message = paste('Check out the NGS files in the folder 3_output :)\n
                               Total process time:', as.character(round(difftime(end_t,start_t,units = 'mins' ),digits = 0)),'minute(s)\n
                               Number of transactions processed:', rows_t), type = 'ok')