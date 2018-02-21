user_interaction_update_input_f <- function(df_to_map
                                            ,df_to_map_name
                                            ,df_map
                                            ,df_map_name
                                            ,column_map
                                            ,column_check
                                            ,missing_field_name
                                            ,loop_number) {
  
  file_path <- paste0('4_temp_input/',df_to_map_name,'_',df_map_name,'_missing',loop_number,'.xlsx')
  
 
  # create key in df_map
  df_map$key <- df_map[,column_map[1]]
  df_map[,column_map[1]] <- NULL
  if (length(column_map) >1) {
  for (column_name in column_map[2:length(column_map)]) {
    df_map$key <- paste0(df_map$key,df_map[,column_name])
    df_map[,column_name] <- NULL}}
  
  # create key in df_to_map
  df_to_map$key <- df_to_map[,column_map[1]]
  if (length(column_map) >1) {
    for (column_name in column_map[2:length(column_map)]) {
      df_to_map$key <- paste0(df_to_map$key,df_to_map[,column_name])}}
  
  # merge df_to_map with df_map on key
  merged_df <- merge(df_to_map,df_map, by = 'key', all.x = TRUE)
  # erase key
  merged_df$key <- NULL
  
  #create df_missing_field to check if there are missing fields
  df_missing_field <- merged_df
  for (column_name in column_check) {
    df_missing_field <- df_missing_field[is.na(df_missing_field[,column_name]),]}
  
  # if there are missing fields
  # then ask user to rectify
  if (nrow(df_missing_field)!=0){
    
    # format df_missing_field for easy rectification
    
    # only keep columns used for mapping & check
    df_missing_field_map <- data.frame(matrix(0, ncol = 0, nrow = nrow(df_missing_field)))
    i <- 1
    for (column_name in c(column_map,column_check)) {
      df_missing_field_map <- data.frame(df_missing_field_map,df_missing_field[,column_name])
      names(df_missing_field_map)[i] <- column_name
      i <- i+1
    }
    
    # group df_missing_field_map by column_map
    df_missing_field_map <- data.table(df_missing_field_map) 
    df_missing_field_map <- df_missing_field_map[, .(mean(column_check[[1]])), by=column_map]
    df_missing_field_map <- data.frame(df_missing_field_map)
    
    # add empty columns of column check
    names(df_missing_field_map)[length(column_map)+1] <- column_check[[1]]
    if (length(column_check)>1) {
      for (i in c(2: length(column_check))) {
        df_missing_field_map$new_column <- data.frame(matrix(0, ncol = 1, nrow = nrow(df_missing_field_map)))
        df_missing_field_map$new_column <- NA
        names(df_missing_field_map)[length(column_map)+i] <- column_check[[i]]}}
    
    
    # create df_missing_field_map.xlsx without error
    write_excel_w_error_handling_f(file_path,df_missing_field_map)
    
    
    # open df_map.xlsm and df_missing_field_map.xlsx for user to modify
    shell.exec(paste0(getwd(),'/2_input/',df_map_name,'.xlsm'))
    shell.exec(paste0(getwd(),'/',file_path))
    
    Sys.sleep(2)
    
    # prompts user to modify df_map_name.xlsm
    tkmessageBox(title = paste('Missing',missing_field_name,'!') 
                 ,message = paste('Please add missing',missing_field_name
                                  ,'in 2_input/',df_map_name,'.xlsm THEN click ok. \n
Missing',missing_field_name, 'are in', file_path), type = 'ok')
    
    
    # else if user rectified issue then return merged_df
  } else {return(merged_df)}
}