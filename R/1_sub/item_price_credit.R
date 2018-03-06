
item_price_credit_f <- function (item_price_credit_df, oms_ship, loop_number) {

# I_ipc_data_cleaning_transformation --------------------------------------------------------------
  
  
  # get and run merge_sc_oms_f
  source('R/2_function/2_item_price_commission/merge_sc_oms.R')
  ipc_w_oms <- merge_sc_oms_f(oms_ship, item_price_credit_df)

  # this loop allows the final format check which should make it impossible for the user to make format mistake - it will re-run until no mistake is found
  k <- 0
  while (k == 0) {
# III_make_sure_all_rows_have_payment_method_[user_interaction] ----------------------------------------------------------------

# run prepare_missing_field_df_f
list_missing_payment_method <- prepare_missing_field_df_f(ipc_w_oms,'payment_method')

# run user_interaction_new_input_f
ipc_no_missing_payment_method <- user_interaction_new_input_f(list_missing_payment_method[[1]]
                                                              ,'payment_method'
                                                              ,'ipc_missing_payment_method'
                                                              ,list_missing_payment_method[[2]]
                                                              ,loop_number
                                                              ,check_input = T)


# only proceed if there are no missing payment method anymore
if (is.data.frame(ipc_no_missing_payment_method)) {
 
# IV_make_sure_all_rows_have_shipment_provider_[user_interaction] ---------------------------------------------------------------- 
  
  # run prepare_missing_field_df_f
  list_missing_shipment_provider <- prepare_missing_field_df_f(ipc_no_missing_payment_method,'shipment_provider_name')
  
  # run user_interaction_new_input_f
  ipc_no_missing_shipment_provider <- user_interaction_new_input_f(list_missing_shipment_provider[[1]]
                                                                ,'shipment_provider_name'
                                                                ,'ipc_missing_shipment_provider'
                                                                ,list_missing_shipment_provider[[2]]
                                                                ,loop_number
                                                                ,check_input = T)
  
# only proceed if there are no missing shipment provider anymore  
if (is.data.frame(ipc_no_missing_shipment_provider)) {
  
  # V_make_sure_all_rows_have_paid_price_[user_interaction] ---------------------------------------------------
  
  # erase rows without paid_amount OR with paid_amount <> number from ipc_no_missing_shipment_provider 
  # goal: allow user to manually give the information for these missing rows
  ipc_no_missing_paid_price <- ipc_no_missing_shipment_provider[is.na(as.numeric(ipc_no_missing_shipment_provider$paid_price)) == F,]
  
  # identify rows without paid amount OR with paid_amount <> number even after merging OMS report
  ## for mor details about the logic: cf. item_price_credit.R > III_rectify_sc_oms_mismatch_[user_interaction] 
  ipc_missing_paid_amount <- ipc_no_missing_shipment_provider[is.na(as.numeric(ipc_no_missing_shipment_provider$paid_price)) == T,]
  
  # run the function user_interaction_new_input_f
  # the function returns a string if issues have to be resolved by the user, it returns a data frame if all issues have been solved
  ipc_no_missing_paid_price <- user_interaction_new_input_f(ipc_missing_paid_amount
                                                            ,'paid_price'
                                                            ,'ipc_missing_paid_amount'
                                                            ,ipc_no_missing_paid_price
                                                            ,loop_number
                                                            ,check_input = 'numeric')                                                             
  
  
  
  # only continue code if missing paid amount has been solved by the user
  if (is.data.frame(ipc_no_missing_paid_price)) {
  
  
  # V_make_sure_all_rows_have_item_status_[user_interaction] ---------------------------------------------------------------- 
  
  # run prepare_missing_field_df_f
  list_missing_item_status <- prepare_missing_field_df_f(ipc_no_missing_paid_price,'item_status')
  
  # run user_interaction_new_input_f
  ipc_no_missing_item_status <- user_interaction_new_input_f(list_missing_item_status[[1]]
                                                             ,'item_status'
                                                             ,'ipc_missing_item_status'
                                                             ,list_missing_item_status[[2]]
                                                             ,loop_number
                                                             ,check_input = T)
  
  
  
  # only proceed if there are no missing item_status anymore  
  if (is.data.frame(ipc_no_missing_item_status)) {
    
    # run check_all_format_f - will determine if user made a mistake - if so then the whole loop should be processed again
    k <- check_all_format_f(ipc_no_missing_item_status)
    
    # if check has failed then prompt user and archive inputs
    if (k == 0) {
      # prompts user
      tkmessageBox(title = 'Incorrect format!'
                   , message = 'Sorry! Despite all the conditional formatting, you made a mistake somewhere - please be very careful about the formatting of your inputs! Loop will restart when you press OK.'
                   , type = 'ok')
      # define present temp inputs
      temp_input <- list.files('4_temp_input/',pattern = "_missing_")
      
      # archive previous temp inputs
      for (i in c(1:length(temp_input))) {
        
        archive_f('4_temp_input',temp_input[i]) 
      }}
    
    # only process if check was successful ie k = 1
    if (k == 1) {
  

# VI_ipc_data_cleaning_transformation ----------------------------------------
  
  # add voucher to ipc_no_missing_item_status
  ipc_no_missing_item_status$voucher <- ipc_no_missing_item_status$transaction_value - ipc_no_missing_item_status$paid_price
  

# VII_map_ledger --------------------------------------------------------------------- 
  
  # source and run ledger_f function
  source('R/2_function/2_item_price_commission/ledger.R')
  ipc_no_missing_ledger <- ledger_f(ipc_no_missing_item_status, 'ipc',loop_number)
  
  # only continue code if missing ledger has been solved by the user
  if (is.data.frame(ipc_no_missing_ledger)) {
    
    return(ipc_no_missing_ledger)
    
  }
}
}
}
}
}
  }
}