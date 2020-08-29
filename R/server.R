library(shiny)
library(shinyFiles)
library(PTXQC)

server <- function(input, output, session){
 
  output$ui <- renderUI({
    switch(input$dtype,
           "MaxQuant output folder" =  shinyDirButton("dir", "Browse...", "Choose a directory"),
           "Mztab file" = fileInput("file", "Choose file"))

  })

  shinyDirChoose(input,"dir", session=session,roots=c(wd=getwd()))
  
  inFile <-""
  observeEvent(input$dir, {
    inFile <- parseDirPath(roots=c(wd=getwd()), input$dir)
    output$dir <- renderText(inFile)
  })
}

