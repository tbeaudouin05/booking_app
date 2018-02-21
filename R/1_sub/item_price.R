item_price_f <- function(item_price_df, oms_rcc, loop_number) {


  
  # I_ipt_data_cleaning_transformation --------------------------------------------------------------
  
  # run merge_sc_oms_f
  ipt_w_oms <- merge_sc_oms_f(oms_rcc, item_price_df)
  
  # this loop allows the final format check which should make it impossible for the user to make format mistake - it will re-run until no mistake is found
  k <- 0
  while (k == 0) {
  
  # II_make_sure_all_rows_have_paid_price_[user_interaction] ---------------------------------------------------
  
  # erase rows without paid_price OR with paid_price <> number from ipt_w_oms 
  # goal: allow user to manually give the information for these missing rows
  ipt_no_missing_paid_price <- ipt_w_oms[is.na(as.numeric(ipt_w_oms$paid_price)) == F,]
  
  # identify rows without paid amount OR with paid_price <> number even after merging OMS report
  ## for mor details about the logic: cf. item_price_credit.R > III_rectify_sc_oms_mismatch_[user_interaction] 
  ipt_missing_paid_price <- ipt_w_oms[is.na(as.numeric(ipt_w_oms$paid_price)) == T,]
  
  # run the function user_interaction_new_input_f
  # the function returns a string if issues have to be resolved by the user, it returns a data frame if all issues have been solved
  ipt_no_missing_paid_price <- user_interaction_new_input_f(ipt_missing_paid_price
                                                             ,'paid_price'
                                                             ,'ipt_missing_paid_price'
                                                             ,ipt_no_missing_paid_price
                                                             ,loop_number
                                                             ,check_input = 'numeric')                                                             
  
  
  
  # only continue code if missing paid amount has been solved by the user
  if (is.data.frame(ipt_no_missing_paid_price)) {
    
    # III_make_sure_all_rows_have_payment_method_[user_interaction] ----------------------------------------------------------------
    
    
    # run prepare_missing_field_df_f
    list_missing_payment_method <- prepare_missing_field_df_f(ipt_no_missing_paid_price,'payment_method')
    
    # run user_interaction_new_input_f
    ipt_no_missing_payment_method <- user_interaction_new_input_f(list_missing_payment_method[[1]]
                                                                  ,'payment_method'
                                                                  ,'ipt_missing_payment_method'
                                                                  ,list_missing_payment_method[[2]]
                                                                  ,loop_number
                                                                  ,check_input = T)
    
    
    # only proceed if there are no missing payment method anymore
    if (is.data.frame(ipt_no_missing_payment_method)) {
  
    
  # V_rectify_missing_shipment_provider_[user_interaction] ---------------------------------------------------  
    
    
    # run prepare_missing_field_df_f
    list_missing_shipment_provider <- prepare_missing_field_df_f(ipt_no_missing_payment_method,'shipment_provider_name')
    
    # run user_interaction_new_input_f
    ipt_no_missing_shipment_provider <- user_interaction_new_input_f(list_missing_shipment_provider[[1]]
                                                                     ,'shipment_provider_name'
                                                                     ,'ipt_missing_shipment_provider'
                                                                     ,list_missing_shipment_provider[[2]]
                                                                     ,loop_number
                                                                     ,check_input = T)
    
    
    
    # only proceed if there are no missing shipment provider anymore  
    if (is.data.frame(ipt_no_missing_shipment_provider)) {
      
      # VI_make_sure_all_rows_have_item_status_[user_interaction] ---------------------------------------------------------------- 
      
      # run prepare_missing_field_df_f
      list_missing_item_status <- prepare_missing_field_df_f(ipt_no_missing_shipment_provider,'item_status')
      
      # run user_interaction_new_input_f
      ipt_no_missing_item_status <- user_interaction_new_input_f(list_missing_item_status[[1]]
                                                                 ,'item_status'
                                                                 ,'ipt_missing_item_status'
                                                                 ,list_missing_item_status[[2]]
                                                                 ,loop_number
                                                                 ,check_input = T)
      
      
      
      # only proceed if there are no missing item_status anymore  
      if (is.data.frame(ipt_no_missing_item_status)) {
      
      # run check_all_format_f - will determine if user made a mistake - if so then the whole loop should be processed again
      k <- check_all_format_f(ipt_no_missing_item_status)
      
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
      
      # only process if check was successful ie k = 0
      if (k == 1) {
        
      
  # VII_ipt_data_cleaning_transformation --------------------------------------------------------------------------------------
  
  # replace the delivery company "0" by "Bamilo Transportation System" for CashOnDelivery
  setDT(ipt_no_missing_item_status)[payment_method=='CashOnDelivery' & shipment_provider_name == 0, shipment_provider_name := 'Bamilo Transportation System']
  ipt_no_missing_item_status <- data.frame(ipt_no_missing_item_status)
  
  # add voucher to ipt_no_missing_item_status
  ipt_no_missing_item_status$voucher <- ipt_no_missing_item_status$transaction_value - ipt_no_missing_item_status$paid_price
  
  
  # VIII_map_ledger ---------------------------------------------------------------------------------------------------------------
  
  # source and run ledger_f function
  source('R/2_function/2_item_price_commission/ledger.R')
  ipt_no_missing_ledger <- ledger_f(ipt_no_missing_item_status, 'ipt', loop_number)
  
  # only continue code if missing ledger has been solved by the user
  if (is.data.frame(ipt_no_missing_ledger)) {
  

    
    return(ipt_no_missing_ledger)
    
    
  }
  }
  }  
  }
  }
  }
  }
  }
