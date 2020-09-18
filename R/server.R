server <- function(input, output, session){
  
  if(.Platform$OS.type == "windows") sep <- "\\"
  else sep <- "/"
  
  setwd(sep)
  
  output$dirbutton <- renderUI({
    shinyDirButton("dir", "Browse...", "Choose a directory") #, style = 'padding:3px; font-size:80%')
  })
  ##choosing MaxQuant directory
  shinyDirChoose(input,"dir", session=session,roots=c(wd=getwd()))
   
  output$choose.dir <- renderText({"<b>Choose directory</b>"})
  output$dir.txt <- renderText({
    parseDirPath(roots=c(wd=getwd()), input$dir)
  })
  
  
  ##loading .yaml file
  output$yaml.load <- renderUI({
    if(input$yaml){
      fileInput("yamlfile", "Choose yaml file")
    }
  })
  
  output$pdfd <- renderUI({
    downloadButton("pdfdownload", "Download as PDF", style='padding:3px; font-size:80%')
  })
  output$yamld <- renderUI({
    downloadButton("yamldownload", "Download as PDF", style='padding:3px; font-size:80%') 
  })
  
  ##creating report
  observeEvent(input$creport, {
    
    observe({
      if(input$dtype=="MaxQuant output folder"){
        
        ##copying MaxQuant files into temporary dir
        path.old <- parseDirPath(roots=c(wd=getwd()), input$dir)
        path.new <- paste0(tempfile("report", tempdir(), sep))
        dir.create(path.new)
        files <- dir(path.old)
        file.copy(paste0(path.old, sep, files), paste0(path.new, sep, files))
        
        ##check if .yaml file was load
        if(input$yaml) yaml.obj <- yaml.load_file(input$yaml$datapath)
        else  yaml.obj <- list()
        
        #create report for MaxQuant files 
        createReport(txt_folder = path.new, mztab_file = NULL, yaml_obj = yaml.obj)
        
        ##function for html report output
        getPage<-function() {
          
          output$created <- reactive(return(1))
          outputOptions(output, "created", suspendWhenHidden = FALSE)
          addResourcePath("lib", path.new)
          print((tags$iframe(src = paste0("lib", sep, list.files(path = path.new, pattern = "report.*html")), height = 550, width = "100%", seamless = "seamless")))
        }
        
        output$htmlpage<-renderUI({getPage()})

        
        ##download pdf 
        output$pdfdownload <- downloadHandler(
          filename = "PTXQC_Report.pdf", 
          content = function(file){
            file.copy(paste0(path.new, list.files(path = path.new, pattern = "report.*pdf")), file)
          }
        )
        
        ##download .yaml 
        output$yamldownload <- downloadHandler(
          filename = "PTXQC.yaml", 
          content = function(file){
            file.copy(paste0(path.new, list.files(path = path.new, pattern = "report.*yaml")), file)
          }
        )
          
        
      } else {
        
        mztab_file <- input$file$datapath
        
        ##check if .yaml file was load
        if(input$yaml) yaml.obj <- yaml.load_file(input$yaml$datapath)
        else yaml.obj <- list()
        
        ##creating report for mztab file
        createReport(txt_folder = NULL, mztab_file = mztab_file, yaml_obj = yaml.obj)
        
        ##funtions for html report output
        getPage<-function() {
          output$created <- reactive(return(1))
          outputOptions(output, "created", suspendWhenHidden = FALSE)
          
          addResourcePath("lib", dirname(input$file$datapath))
          print((tags$iframe(src = paste0("lib", sep, list.files(path = dirname(input$file$datapath), pattern = "report.*html")), height = 550, width = "100%", seamless = "seamless")))
        }
        
        output$htmlpage<-renderUI({getPage()})
        
        ##download pdf 
        output$pdfdownload <- downloadHandler(
          filename = "Report.pdf",
          content = function(file){
            file.copy(paste0(dirname(input$file$datapath), sep, list.files(path = dirname(input$file$datapath), pattern = c("report.*pdf"))), file)
          }
        )
        
        ##download .yaml 
        output$yamldownload <- downloadHandler(
          filename = "PTXQC.yaml",
          content = function(file){
            file.copy(paste0(dirname(input$file$datapath), sep, list.files(path = dirname(input$file$datapath), pattern = c("report.*yaml"))), file)
          }
        )
      }
    })
  })
  
}

shinyApp(ui,server)


##output$dir.txt directory path MaxQuant output 
##input$file file path mztab


