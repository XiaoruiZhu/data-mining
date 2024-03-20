#' educateusgpt  
#' The function to create question html file that can be 
#' added to the qmd content file. It also updates the "openai-api.html" file
#' to insert a id to track the chatHistory for the new question. 
#'
#' @param id the id of the question
#' @param question a string to 
#'
#' @return a file name of the question html file. The question html file will be automaticly 
#' created and updated. The function will insert a id in the  "openai-api.html" file to 
#' track chatHistory. 
#'
#' @examples
educateusgpt <- function(id, question) {
  if (!is.character(question)) {
    stop ()
  } 
  # id <- 333
  # question <- "What is your name?"
    
  html_text <- paste(
    '<div class="card bg-light mb-3" style="max-width: 1200px;">
  <div class="card-header">Ask Educate Us GPT</div>
  <div class="card-body">
    <h4 class="card-title">',
    question, 
    '</h4>
    <div class="chat-container" id="chat-container-', id, 
    '">
        <div id="chat-messages-', id, 
    '"></div>
        <input type="text" id="user-input-', id, 
    '" class="chat-input" placeholder="Ask your question here ..." value="', question, 
    '">
        <button onclick="sendMessage(', id, ')" class="chat-button">Ask</button>
    </div>
  </div>
</div>', sep = ""
  )
  # html_text
  
  filename <- paste("questions/Q-",id,"-",abbreviate(question), ".html", sep = "")
  # Save the question file 
  write.table(html_text, 
              file=filename, 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Read the openai-api.html HTML file into a character vector
  html_openai <- readLines("questions/openai-api.html")
  # Insert the new line for the chatHistory in the 5th line (may need change in the future)
  new_line <- paste("            ", id, ": [],", sep = "")
  
  # Check if the 5th line is added already, 
  # if the question needs to add is already in the 6th row, then no need to add again
  if (!(new_line %in% html_openai)) {
    html_openai <- c(html_openai[1:5], 
                     new_line, 
                     html_openai[(5 + 1):length(html_openai)])
  }
  
  writeLines(html_openai, "questions/openai-api.html")
  
  return(filename)
}