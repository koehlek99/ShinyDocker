list.of.packages <- c("shiny", "PTXQC", "dplyr", "shinycssloaders", "shinyFiles", "shinythemes", "shinyjs")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)


library(shiny)
library(PTXQC)
library(dplyr)
library(shinycssloaders)
library(shinyFiles)
library(shinythemes)
library(shinyjs)

setwd("C:/")

             
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
        
        mztab_file <- input$file$datapath
        createReport(txt_folder = NULL, mztab_file = mztab_file)
        
        output$pdfdownload <- downloadHandler(
          filename = "Report.pdf",
          content = function(file){
            file.copy(paste0(dirname(input$file$datapath), "\\", list.files(path = dirname(input$file$datapath), pattern = c("report.*pdf"))), file)
          }
        )
        getPage<-function() {
          return(includeHTML(paste0(dirname(input$file$datapath), "\\", list.files(path = dirname(input$file$datapath), pattern = "report.*html"))))
        }
        output$htmlpage<-renderUI({getPage()})
      }
    })
  })
  
}

shinyApp(ui,server)

# mztab_file <- "C:/Users/Krissi/Desktop/bachelorarbeit/test ptxcq/example.mzTab"
# createReport(txt_folder = NULL, mztab_file = mztab_file)


##instead of getwd( maybe C:?

##output$dir.txt directory path MaxQuant output 
##input$file file path mztab


