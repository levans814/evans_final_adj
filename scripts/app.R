library(shiny)
library(rsconnect)

ui <- fluidPage(
  
  titlePanel("Missouri Senate Race")
  
)

server <- function(input, output){
  
  
}

shinyApp(ui = ui , server = server)
