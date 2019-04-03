library(shiny)
library(tm)
library(SnowballC)
source("judge_robot.R")

shinyServer(
function(input, output) {
    observe({
      req(input$pdfFile)
      file.copy(input$pdfFile$datapath, "copy.pdf", overwrite = T)
      result <- go("copy.pdf")
      print(result)
     
      output$result <- renderText({
          result
       })
   })
}
)