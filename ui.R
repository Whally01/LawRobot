library(shiny)

shinyUI(fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      fileInput('pdfFile', "PDF", accept = c('.pdf')) 
      ),
    
    mainPanel(
      h3("Результат"),
      htmlOutput("result")
    )
  )
  )
)
