ngs_format_and_output_f <- function(ledger_subledger_amount_df, map_name, process_name,loop_number){
  
  
  
  
  # get ngs_voucher_map.xlsm
  ngs_voucher_map <- load_file_and_check_exist_f('2_input/ngs_map/'
                                                 ,paste0(map_name,'_ngs_map.xlsm')
                                                 ,'excel'
                                                 ,sheet_name = 'do_not_change_name'
                                                 ,open = T)
  # format ngs_voucher_map_detail
  ngs_voucher_map_detail <- data.frame(ngs_voucher_map$relation,ngs_voucher_map$transaction_remark)
  names(ngs_voucher_map_detail)[1] <- 'VoucherRelation'
  names(ngs_voucher_map_detail)[2] <- 'Remark'
  ngs_voucher_map_detail$voucher_key <- 1
  
  # initialize detail tab with ledger_subledger_amount_df
  ngs_detail_tab <- ledger_subledger_amount_df
  
  # rename columns 1, 2 and 3 to Account Code, Account Free and Amount
  names(ngs_detail_tab)[1] <- 'Account Code'
  names(ngs_detail_tab)[2] <- 'Account Free'
  names(ngs_detail_tab)[3] <- 'Amount'
  
  # add VoucherRelation and Remark from voucher_map_detail
  ngs_detail_tab$voucher_key <-1
  ngs_detail_tab <- merge(ngs_detail_tab, ngs_voucher_map_detail, by = 'voucher_key', all.x = T )
  ngs_detail_tab$voucher_key <- NULL
  
  # add index RowSeq
  ngs_detail_tab <- tibble::rowid_to_column(ngs_detail_tab, "RowSeq")	
  
  # add Account Free Group Code
  ngs_detail_tab$'Account Free Group Code' <- substring(ngs_detail_tab$`Account Free`,1,1)
  
  # add empty columns
  ngs_detail_tab$'Currency Code' <- ''
  ngs_detail_tab$'Currency Amount' <- ''
  ngs_detail_tab$'Effective Date' <- ''
  ngs_detail_tab$'Mark' <- ''
  ngs_detail_tab$'Reference' <- ''
  
  # re-order columns according to ngs template
  ngs_detail_tab_o <- ngs_detail_tab[,c(5,1:3,7:10,4,11:12,6)]
  
  # round up Amount
  ngs_detail_tab_o$Amount <- round(ngs_detail_tab$Amount, digits = 0)
  
  # convert all NAs to ""
  ngs_detail_tab_o[is.na(ngs_detail_tab_o)] <- ''
  
  # format all columns to text
  ngs_detail_tab_o <- data.frame(lapply(ngs_detail_tab_o,function(x) as.character(x)))
  
  # make sure that headers do have spaces in there names
  for (i in c(1:length(names(ngs_detail_tab_o)))) {
  names(ngs_detail_tab_o)[i] <- gsub('\\.',' ',names(ngs_detail_tab_o)[i])
  }
  
  # create ngs_header_tab
  ngs_header_tab <- ngs_voucher_map[,c(1:7)]
  # convert all NAs to ""
  ngs_header_tab[is.na(ngs_header_tab)] <- ''
  # format all columns to text
  ngs_header_tab <- data.frame(lapply(ngs_header_tab,function(x) as.character(x)))
  
  # create and save ngs_template.xlsx ----------------------------------------------------------------------------
  
  # create ngs_upload folder and loop folder
  dir.create(file.path(getwd(),"3_output/ngs_upload/"),showWarnings = FALSE)
  dir.create(file.path(getwd(),paste0("3_output/ngs_upload/loop_",loop_number)),showWarnings = FALSE)
  
  if (file.exists(paste0("3_output/ngs_upload/loop_",loop_number,'/',process_name, "_ngs_template.xlsx"))) {
    archive_f(paste0('3_output/ngs_upload/loop_',loop_number),paste0(process_name,'_ngs_template.xlsx'))}
  
  # create workbook ngs_template.xlsx without error
  write_excel_w_error_handling_f(paste0("3_output/ngs_upload/loop_",loop_number,'/',process_name, "_ngs_template.xlsx")
                                 ,data_header = ngs_header_tab
                                 ,data_detail = ngs_detail_tab_o)
  
  
}