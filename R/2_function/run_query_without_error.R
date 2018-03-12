
run_query_wo_error_f <- function(formatted_query, is_sc_query = T){
  
  db_access <- yaml.load_file('SQL/db_access.yaml')
  j <- 1
  i <- 1
  conn <- NULL
  rs <- NULL
  list <- list(i,data.frame())
  
  # fetch correct credential for SC or OMS
  if (is_sc_query) {
    user1 <- db_access$sc_access[[1]][[1]]
    password1 <- db_access$sc_access[[2]][[1]]
    dbname1 <- db_access$sc_access[[3]][[1]]
    host1 <- db_access$sc_access[[4]][[1]] 
  } else {
    user1 <- db_access$oms_access[[1]][[1]]
    password1 <- db_access$oms_access[[2]][[1]]
    dbname1 <- db_access$oms_access[[3]][[1]]
    host1 <- db_access$oms_access[[4]][[1]]
  }
  
  # re-run the query until all errors /  warnings have been handled
  while (i == 1 & j < 10) {
    
    writeLines('Querying database and handling potential errors, please wait...')
    writeLines(paste('Try:',j))
    
    # tryCatch handles errors and warnings
    # NB: the result of tryCatch is applied to i: 
    # if there is an error / warning, then this tryCatch returns 1 (which continues the loop)
    # if there is no error / warning, then the entire function run_query_wo_error_f returns query_output :) because tryCatch contains "return" hence we do not care about the value of i anymore anyway :)
    i <- tryCatch({
      
    # establish connection to database
    conn = dbConnect(MySQL(), user= user1, password= password1, dbname= dbname1, host= host1)
    
    
    # run formatted_query on MySQL database
    # if warnings do NOT contain "lost connection" AND do NOT contain "row" THEN delete warnings
    # i.e. show warnings if and ONLY if warnings contain "lost connection" or "row"
    query_output <- withCallingHandlers(dbGetQuery(conn, formatted_query, n = -1)
                                        ,warning = function(w){if (!grepl('lost connection',w$message) & !grepl('row',w$message))
                                          {invokeRestart('muffleWarning')}})
    
    # disconnect all database connections in case of success
    lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)
    
    # if everything runs fine: break the loop and return query output
    i <- 2
    return(query_output)
    
    }
    
    # handle blocking warnings: re-run the loop 
    # NB: all non blocking warnings should be suppressed thanks to withCallingHandlers function
    ,warning = function(w){
                          writeLines(paste('WARNING:',w$message))
                          return(1)
    }
      # handle errors: re-run the loop
    ,error = function(e){
                          writeLines(paste('ERROR:',e$message))
                          return(1)})


  # disconnect all database connections in case of errors / warnings
  lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)
  
  # make sure it is not an infinite loop
  j <- j + 1
  # wait 3 seconds before re-running the loop
  Sys.sleep(10)
  
  }
  
}