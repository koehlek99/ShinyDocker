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
      fileInput("yamlfile", "Choose yaml file")
  })
  
  ##advanced settings
  output$adv.set1 <- renderUI({
      tagList(
        renderText({"Summary:"}),
        sliderInput("Thresh_ID_rate", "ID rate", 0, 100, c(20,35), width = "100%"),
        renderUI({HTML("<br/>")})
      )
  })
  
  output$adv.set2 <- renderUI({
    tagList(
      renderText({"Protein groups: "}), 
      numericInput("PG_LabelIncTresh_num", "label inc", 4, width = "100%"),
      renderUI({HTML("<br/>")}),
      renderText({"Evidence: "}),
      numericInput("EVD_ProteinCountThresh_num", "Protein counts", 3500, width = "100%"),
      numericInput("EVD_PeptideCountThresh_num", "Peptide counts", 15000, width = "100%"),
      numericInput("EVD_mainSearchTol_num", "main search tol", 4.5, width = "100%"),
      numericInput("EVD_firstSearch_outOfCalWarnSD_num", "FS outofcal warnsd", 2, width = "100%"),
      textInput("special_contaminants", "Contaminant and threshhold", "Mycoplasma: 1%"),
      renderUI({HTML("<br/>")}),
      renderText({"MsMs Scans: "}), 
      numericInput("MsMsScans_IonInjectionTresh_num", "Ion injection time", 10, width = "100%")
      
    )
  })

  output$adv.set3 <- renderUI({
    tagList(
      renderUI({HTML("<br/>")}),
      numericInput("PG_IntensityThreshLog2_num", "log2-Intensity", 25, width = "100%"),
      renderUI({HTML("<br/>")}),
      renderUI({HTML("<br/>")}),
      numericInput("EVD_IntensityThreshLog2_num", "log2-Intensity", 23, width = "100%"),
      numericInput("EVD_MQpar_MatchingTimeWindow_num", "Matching time window", 0.7, width = "100%"),
      numericInput("EVD_MQpar_firstSearchTol_num", "First search tol", 20, width = "100%"),
      selectInput("EVD_MatchBetweenRuns_wA", "Match between runs", c("yes", "no", "auto"), "auto", width = "100%")
    )
  })
  
  output$adv.set4 <- renderUI({
    
  })

  
  output$pdfd <- renderUI({
    downloadButton("pdfdownload", "Download as PDF", style='padding:3px; font-size:80%')
  })
  output$yamld <- renderUI({
    downloadButton("yamldownload", "Download yaml", style='padding:3px; font-size:80%') 
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
        
        ##check if .yaml file was load
        if(!is.null(input$yamlfile)) yaml.obj <- yaml.load_file(input$yamlfile$datapath)
        else  yaml.obj <- list()
        
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

        ##check if .yaml file was load
        if(!is.null(input$yamlfile)) yaml.obj <- yaml.load_file(input$yamlfile$datapath)
        else  yaml.obj <- list()
        
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