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

  
  ##advanced settings
  output$adv.set <- renderUI({
    if(input$settings){
      tagList(
        sliderInput("Thresh_ID_rate", "ID rate treshhold", 0, 50, c(20,35), width = "50%"),
        numericInput("PG_LabelIncTresh_num", "Protein Groups: ratio plot- label inc treshhold", 4, width = "20%"),
        numericInput("PG_IntensityThreshLog2_num", "Protein groups: Intensity log2-threshhold", 25, width = "20%"),
        numericInput("EVD_ProteinCountThresh_num", "Evidence: Protein counts threshhold", 3500, width = "20%"),
        numericInput("EVD_IntensityThreshLog2_num", "Evidence: Intensity log2-threshhold", 23, width = "20%"),
        numericInput("EVD_PeptideCountThresh_num", "Evidence: Peptide counts treshhold", 15000, width = "20%"),
        numericInput("EVD_MQpar_MatchingTimeWindow_num", "MQ parameter- Matching time window", 0.7, width = "20%"),
        selectInput("EVD_MatchBetweenRuns_wA", "Match between runs", c("yes", "no", "auto"), "auto", width = "20%"),
        numericInput("EVD_MQpar_firstSearchTol_num", "MQ parameter- First search tol", 20, width = "50%"),
        numericInput("EVD_firstSearch_outOfCalWarnSD_num", "first search outofcal warnsd", 2, width = "20%"),
        numericInput("EVD_mainSearchTol_num", "main search tol", 4.5, width = "20%"),
        numericInput("MsMsScans_IonInjectionTresh_num", "MsMs Scans: Ion injection time treshhold", 10, width = "20%")
      )
      
    }
  })
  

  
  output$pdfd <- renderUI({
    downloadButton("pdfdownload", "Download as PDF", style='padding:3px; font-size:80%')
  })
  output$yamld <- renderUI({
    downloadButton("yamldownload", "Download as PDF", style='padding:3px; font-size:80%') 
  })
  
  w <- Waiter$new()
  
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
        
        yaml.obj <- list()
        
        #create report for MaxQuant files 
        w$show()
        createReport(txt_folder = path.new, mztab_file = NULL, yaml_obj = yaml.obj)
        w$hide()
        
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
        yaml.obj <- list()
        
        ##creating report for mztab file
        w$show()
        createReport(txt_folder = NULL, mztab_file = mztab_file, yaml_obj = yaml.obj)
        w$hide()
        
        
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


#getMetricsObject und ordnen