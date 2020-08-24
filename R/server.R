library(shiny)
library(shinyFiles)
library(PTXQC)

server <- function(input, output, session){
  setwd("C://Users//Krissi")
  shinyDirChoose(input,'dir', session=session,roots=c(wd=getwd()))
  
  inFile <-""
  observeEvent(input$dir, {
    inFile <- parseDirPath(roots=c(wd=getwd()), input$dir)
    
    output$txt = renderPrint(inFile)
  })
  
  
 
}
