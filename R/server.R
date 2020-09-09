library(shiny)
library(PTXQC)
library(dplyr)
library(shinycssloaders)
library(shinyFiles)
library(shinythemes)
library(shinyjs)

#setwd("C:/Users/Krissi/")

             
server <- function(input, output, session){
  
  
  shinyDirChoose(input,"dir", session=session,roots=c(wd=getwd()))
   
  output$choose.dir <- renderText({"<b>Choose directory</b>"})
  output$dir.txt <- renderText({
    parseDirPath(roots=c(wd=getwd()), input$dir)
  })
  

  
  observeEvent(input$creport, {
    observe({
      if(input$dtype=="MaxQuant output folder"){
        path.old <- parseDirPath(roots=c(wd=getwd()), input$dir)
        path.new <- paste0(tempfile("report", tempdir(), "\\"))
        dir.create(path.new)
        files <- dir(path.old)
        file.copy(paste0(path.old, "\\", files), paste0(path.new, "\\", files))
        
        createReport(txt_folder = path.new)
        
        getPage<-function() {
          return(includeHTML(paste0(path.new, "\\", list.files(path = path.new, pattern = "report.*html"))))
        }
        output$htmlpage<-renderUI({getPage()})
        
        output$pdfdownload <- downloadHandler(
          filename = "PTXQC_Report.pdf", 
          content = function(file){
            file.copy(paste0(path.new, "\\", list.files(path = path.new, pattern = "report.*pdf")), file)
          }
        )
      } else {
        cat(input$file$datapath)
        createReport(txt_folder = NULL, mztab_file = input$file$datapath)
        output$pdfdownload <- downloadHandler(
          filename = "Report.pdf",
          content = function(file){
            file.copy(paste0(input$file$datapath, list.files(path = input$file$datapath, pattern = c("report.*pdf"))), file)
          }
        )
      }
    })
  })
  
}

#createReport(txt_folder = NULL, mztab_file = "C:/Users/Krissi/Desktop/bachelorarbeit/test ptxcq/example.mzTab")


##instead of getwd( maybe C:?

##output$dir.txt directory path MaxQuant output 
##input$file file path mztab


