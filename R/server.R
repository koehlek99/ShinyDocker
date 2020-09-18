
server <- function(input, output, session){
  
  if(.Platform$OS.type == "windows") sep <- "\\"
  else sep <- "/"
  
  setwd(sep)
  
  shinyDirChoose(input,"dir", session=session,roots=c(wd=getwd()))
   
  output$choose.dir <- renderText({"<b>Choose directory</b>"})
  output$dir.txt <- renderText({
    parseDirPath(roots=c(wd=getwd()), input$dir)
  })
  
  
  
  output$yaml.load <- renderUI({
    if(input$yaml){
      fileInput("yamlfile", "Choose yaml file")
    }
  })
  
  
  observeEvent(input$creport, {
    observe({
      if(input$dtype=="MaxQuant output folder"){
        path.old <- parseDirPath(roots=c(wd=getwd()), input$dir)
        path.new <- paste0(tempfile("report", tempdir(), sep))
        dir.create(path.new)
        files <- dir(path.old)
        file.copy(paste0(path.old, sep, files), paste0(path.new, sep, files))
        
        if(input$yaml) yaml.obj <- yaml.load_file(input$yaml$datapath)
        else  yaml.obj <- list()
       
        createReport(txt_folder = path.new, mztab_file = NULL, yaml_obj = yaml.obj)
        
        getPage<-function() {
          return(includeHTML(paste0(path.new, list.files(path = path.new, pattern = "report.*html"))))
        }
        output$htmlpage<-renderUI({getPage()})
        
        output$pdfdownload <- downloadHandler(
          filename = "PTXQC_Report.pdf", 
          content = function(file){
            file.copy(paste0(path.new, list.files(path = path.new, pattern = "report.*pdf")), file)
          }
        )
      } else {
        
        mztab_file <- input$file$datapath
        
        if(input$yaml) yaml.obj <- yaml.load_file(input$yaml$datapath)
        else yaml.obj <- list()
        
        createReport(txt_folder = NULL, mztab_file = mztab_file, yaml_obj = yaml.obj)
        
        output$pdfdownload <- downloadHandler(
          filename = "Report.pdf",
          content = function(file){
            file.copy(paste0(dirname(input$file$datapath), sep, list.files(path = dirname(input$file$datapath), pattern = c("report.*pdf"))), file)
          }
        )
        getPage<-function() {
          return(includeHTML(paste0(dirname(input$file$datapath), sep, list.files(path = dirname(input$file$datapath), pattern = "report.*html"))))
        }
        output$htmlpage<-renderUI({getPage()})
      }
    })
  })
  
}

shinyApp(ui,server)


##output$dir.txt directory path MaxQuant output 
##input$file file path mztab


