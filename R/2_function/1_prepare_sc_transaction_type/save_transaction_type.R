save_transaction_type_f <- function(transaction_type_df_list
                                    ,transaction_type_df_list_w_comment
                                    ,loop_number){
  
  
  # create transaction_type folder
  dir.create(file.path(getwd(), '3_output','transaction_type'),showWarnings = FALSE)
  # create t_type folder
  dir.create(file.path(getwd(), '3_output','transaction_type','t_type'),showWarnings = FALSE)
  # create t_type_w_comment folder
  dir.create(file.path(getwd(), '3_output','transaction_type','t_type_w_comment'),showWarnings = FALSE)
  
  # if exists, archive previous transaction type csv
  # write each transaction type csv into t_type folder
  for (i in c(1:length(transaction_type_df_list))) {
    
    if (file.exists(paste0('3_output/transaction_type/t_type/'
                           ,names(transaction_type_df_list)[i],'.csv'))) {archive_f('3_output/transaction_type/t_type/'
                                                                                    ,paste0(names(transaction_type_df_list)[i],loop_number,'.csv'))}
    
    write_excel_csv(transaction_type_df_list[[i]]
                    , paste0('3_output/transaction_type/t_type/'
                             ,names(transaction_type_df_list)[i],loop_number,'.csv'))
      
      }
  
  # if exists, archive previous transaction type with comments csv
  # write each transaction type with comments csv into t_type_w_comment folder
  for (i in c(1:length(transaction_type_df_list_w_comment))) {
    
    if (file.exists(paste0('3_output/transaction_type/t_type_w_comment/'
                           ,names(transaction_type_df_list_w_comment)[i],'.csv'))) {archive_f('3_output/transaction_type/t_type/'
                                                                                    ,paste0(names(transaction_type_df_list)[i],loop_number,'.csv'))}
    
    write_excel_csv(transaction_type_df_list_w_comment[[i]]
                    , paste0('3_output/transaction_type/t_type_w_comment/'
                             ,names(transaction_type_df_list_w_comment)[i],loop_number,'.csv'))
    }
  
  
  
}