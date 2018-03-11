
run_query_wo_error_f <- function(formatted_query, is_sc_query = T){
  
  db_access <- yaml.load_file('SQL/db_access.yaml')
  j <- 0
  k <- 0
  conn <- NULL
  while (is.null(conn) & j < 10) {
    
  if (is_sc_query) {
    
    
    writeLines('Connecting to Seller Center database...')
  # connect to sc database
  tryCatch({conn = dbConnect(MySQL(), user= db_access$sc_access[[1]][[1]]
                                       , password= db_access$sc_access[[2]][[1]]
                                       , dbname= db_access$sc_access[[3]][[1]]
                                       , host= db_access$sc_access[[4]][[1]])}
           ,error = function(cond){conn <- NULL})
    
  
  } else {
    writeLines('Connecting to OMS database...')
    # connect to oms database
    
    tryCatch({conn = dbConnect(MySQL(), user= db_access$oms_access[[1]][[1]]
                               , password= db_access$oms_access[[2]][[1]]
                               , dbname= db_access$oms_access[[3]][[1]]
                               , host= db_access$oms_access[[4]][[1]])}
             ,error = function(cond){conn <- NULL})
  }
    
    j <- j + 1
  }
  
  i <- 1
  j <- 0
  
  while (i == 1 & j < 10) {
    
  # define fetch_row_f function
  fetch_row_f <- function(rs,conn1) {
    # run formatted_sc_query on MySQL database
    rs = dbSendQuery(conn1, formatted_query)
    # fetch rows returned by the query into R
    output = dbFetch(rs, n=-1)
  return(output)}

  writeLines('Running query and handling potential errors, please wait...')
  query_output <- withCallingHandlers(fetch_row_f(rs,conn),
                                      warning = function(w){
                                        if(grepl("error while fetching rows", w$message)){
                                          k <- 0
                                          writeLines(paste('Error while fetching rows',w$message))
                                        } else if (grepl("MySQL server has gone away", w$message)) {
                                          k <- 0
                                          writeLines(paste('MySQL server has gone away: ',w$message))
                                        } else {
                                          writeLines('All good on my side - moving on and retrieving data! :)')
                                          k <- 1
                                        }
                                      })
  
# break the loop when all known errors have been handled 
if(is.data.frame(query_output) & k <- 1){i <- 2} else {i <- 1}
  
  lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  
  }
  
  return(query_output)
  
}