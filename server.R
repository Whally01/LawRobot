library(shiny)
library(tm)
library(SnowballC)

function(input, output) {
   
  output$contents <- renderTable({
    inFile <- renderText({input$txtFile}) 
    txt <- input$txtFile
    
    if (is.null(txt))
      return(NULL)
    
    print(txt$datapath)
    txt <- Corpus(DirSource("C:/Users/Dr.Raim/Desktop/txt"), readerControl = list(reader = readPlain,
                                                                 language = "ru",
                                                                 load = T))
    #d <- preprocessing(txt)
    #d
    #KNNprediction <- knn()
   # KNNprediction
    
    #file_data <- read.delim(txt$datapath)
    # head(file_data)
  
})
  
  observe({
    req(input$pdfFile)
    file.copy(" ","www", overwrite = T)
    
    pdf <- Corpus(DirSource(" "), readerControl=list(reader=readPDF))
    d <- preprocessing(pdf)
    d
    
    output$pdfview <- renderUI({
      tags$iframe(style="height:600px; width:100%", src="0.pdf")
    })
    
  })
}
