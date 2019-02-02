library(shiny)

if (interactive()) {
ui <- fluidPage(
     sidebarLayout(
       sidebarPanel(
         fileInput("data", "Choose File",
                   accept = c(
                     "text/plain",
                     "text/comma-separated-values,text/plain",
                     ".txt")
         ),
         tags$hr(),
         checkboxInput("header", "Header", TRUE)
       ),
       mainPanel(
         tableOutput("contents")
       )
     )
)
}

server <- function(input, output) {
  
  output$contents <- renderTable({
    #inFile <- renderText({input$data}) 
    inFile <- input$data
    
    if (is.null(inFile))
      return(NULL)
    
    file_data <- read.delim(inFile$datapath)
    head(file_data)

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

