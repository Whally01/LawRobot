library(shiny)
library(tm)
library(SnowballC)
source("judge_robot.R")

shinyServer(
function(input, output) {
    observe({
      req(input$pdfFile)
      file.copy(input$pdfFile$datapath, "www.pdf", overwrite = T)
      result <- go("www.pdf")
      print(result)
     
      output$result <- renderText({
          result
       })
   })
}
)