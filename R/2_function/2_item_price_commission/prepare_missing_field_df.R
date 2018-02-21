prepare_missing_field_df_f <- function(data_frame,missing_field_name){
  
  # load check_format_input to R
  check_format_input <- load_file_and_check_exist_f('2_input/input_conditional_format'
                                                    , paste0('cond_',missing_field_name,'.csv')
                                                    , 'csv')
  
  check_format_input[is.na(check_format_input)] <- 'I_am_dumb_and_stupid'
  
  # create check_format_flag
  check_format_flag <- data.frame(missing_field_name = check_format_input
                                  ,'flag' = 1)
  
  # flag data frame
  flagged_df <- merge(data_frame
                              ,check_format_flag
                              ,by = missing_field_name
                              ,all.x = T)
  # missing_field_df
  missing_field_df <- flagged_df[is.na(flagged_df$flag),]
  missing_field_df$flag <- NULL
  
  # no_missing_field_df
  no_missing_field_df <- flagged_df[!is.na(flagged_df$flag),]
  no_missing_field_df$flag <- NULL
  
  return(list(missing_field_df,no_missing_field_df))
  
  
  
}