start_fresh_f <- function () {
  
  # define potential present temp inputs
  temp_input <- list.files('4_temp_input/',pattern = "_missing_")
  
  # only display temp inputs which actually exists
  temp_input_2 <- vector()
  for (i in c(1:length(temp_input))) {
    if (file.exists(paste0("4_temp_input/",temp_input[i]))){temp_input_2 <- c(temp_input_2,temp_input[i])}}


  # format text to display to user
  text <-'Start a NEW booking?\n\n\nThese files will be removed from 4_temp_input: '
  for (variable in temp_input_2) {text <- paste(text, variable, sep = '\n')}
  
  # record user's answer
  answer <- tkmessageBox(title = 'Start fresh?'
               , message = text, type = "yesno")
  answer <- tclvalue(answer)
  
  # if user wants to start fresh then archive existing temp. inputs
  if (answer == 'yes') {
    for (i in c(1:length(temp_input_2))) {
      
      archive_f('4_temp_input',temp_input_2[i])
      
    }
  }
  
  
  
}