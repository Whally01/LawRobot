library(shiny)

shinyUI(fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      fileInput("txtFile", "txt",
                accept = c(
                  "text/plain",
                  "text/comma-separated-values,text/plain",
                  ".txt")
      ),
      
      fileInput('pdfFile', "PDF", accept = c('.pdf'))
      ),
    mainPanel(
      #uiOutput("pdfview")
      tableOutput("contents")
    )
  )
  )
)
