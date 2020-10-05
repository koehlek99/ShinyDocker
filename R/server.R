
server <- function(input, output, session){
  
  if(.Platform$OS.type == "windows") sep <- "\\"
  else sep <- "/"
  
  setwd(sep)
  
  output$title <- renderText({
    if(input$creport) "Download files"
    else "Upload your data"
  })
  
  output$dirbutton <- renderUI({
    shinyDirButton("dir", "Browse...", "Choose a directory") #, style = 'padding:3px; font-size:80%')
  })
  
  ##choosing MaxQuant directory
  shinyDirChoose(input,"dir", session=session,roots=c(wd=getwd()))
   
  output$choose.dir <- renderText({"<b>Choose directory</b>"})
  output$dir.txt <- renderText({
    paste0(basename(parseDirPath(roots=c(wd=getwd()), input$dir)), "/")
  })

  ##loading .yaml file
  output$yaml.load <- renderUI({
      fileInput("yamlfile", "Choose yaml file")
  })
  
  ##advanced settings
  output$adv.set1 <- renderUI({
      tagList(
        renderText({"Summary:"}),
        tipify(sliderInput("Thresh_ID_rate", "ID rate", 0, 100, c(20,35), width = "100%"), title = "tooltip", placement = "right"),
        renderUI({HTML("<br/>")})
        
      )
  })
  
  
  
  
  
  output$adv.set2 <- renderUI({
    tagList(
      renderText({"Protein groups: "}), 
      tipify(numericInput("PG_LabelIncTresh_num", "label inc", 4, width = "100%"), title = "tooltip", placement = "right"),
      renderUI({HTML("<br/>")}),
      renderText({"Evidence: "}),
      tipify(numericInput("EVD_ProteinCountThresh_num", "Protein counts", 3500, width = "100%"), title = "tooltip", placement = "right"),
      tipify(numericInput("EVD_PeptideCountThresh_num", "Peptide counts", 15000, width = "100%"), title = "tooltip", placement = "right"),
      tipify(numericInput("EVD_mainSearchTol_num", "main search tol", 4.5, width = "100%"), title = "tooltip", placement = "right"),
      tipify(numericInput("EVD_firstSearch_outOfCalWarnSD_num", "FS outofcal warnsd", 2, width = "100%"), title = "tooltip", placement = "right"),
      tipify(textInput("special_contaminants", "Contaminant and threshhold (in %)", "MYCOPLASMA: 1"), title = "Special contaminants to search for with two parameters: - a string = name within the protein identifier
                                                                                                                   - an integer number = a threshold in % which will be plotted to visually ease interpretation", placement = "right"),
      renderUI({HTML("<br/>")}),
      renderText({"MsMs Scans: "}), 
      tipify(numericInput("MsMsScans_IonInjectionTresh_num", "Ion injection time", 10, width = "100%"), title = "tooltip", placement = "right")
      
    )
  })

  output$adv.set3 <- renderUI({
    tagList(
      renderUI({HTML("<br/>")}),
      tipify(numericInput("PG_IntensityThreshLog2_num", "log2-Intensity", 25, width = "100%"), title = "tooltip", placement = "right"),
      renderUI({HTML("<br/>")}),
      renderUI({HTML("<br/>")}),
      tipify(numericInput("EVD_IntensityThreshLog2_num", "log2-Intensity", 23, width = "100%"), title = "tooltip", placement = "right"),
      tipify(numericInput("EVD_MQpar_MatchingTimeWindow_num", "Matching time window", 0.7, width = "100%"), title = "Matching time tolerance (in min) for Match-Between-Runs (should be matched to parameter set in MaxQuant", placement = "right"),
      tipify(numericInput("EVD_MQpar_firstSearchTol_num", "First search tol", 20, width = "100%"), title = "(should be matched to parameter set in MaxQuant)", placement = "right"),
      tipify(selectInput("EVD_MatchBetweenRuns_wA", "Match between runs", c("yes", "no", "auto"), "auto", width = "100%"), title = "tooltip", placement = "right")
    )
  })
  

  
#######################################################################################################################
##############writing parameters into yaml file 
#######################################################################################################################
 
  build.yaml <- function(path.new){
    
    mets <- paste0("qcMetric_", input$metrics)
    yc = YAMLClass$new(list())
    
    if(input$special_contaminants == "no") {              
      contaminants <- FALSE
    } else {
      contaminant_list <- strsplit(input$special_contaminants, ";")
      contaminant_list <- unlist(contaminant_list)
      contaminants <- list()
      for(c in (1:length(contaminant_list))){
        cont <- unlist(strsplit(contaminant_list[c], ":"))
        cont_name <- paste0("cont_", cont[1])
        contaminants[[cont_name]] <- c(name = cont[1], threshold = as.integer(cont[2]))
      }
    }
    
    param_list <- list(c("param_useMQPAR", "PTXQC$UseLocalMQPar", TRUE),
                     c("add_fs_col", "PTXQC$NameLengthMax_num", 14),
                     c("id_rate_bad", "File$Summary$IDRate$Thresh_bad_num", input$Thresh_ID_rate[1], 0, 100),
                     c("id_rate_great", "File$Summary$IDRate$Thresh_great_num", input$Thresh_ID_rate[2], 0, 100),
                     c("pg_ratioLabIncThresh", "File$ProteinGroups$RatioPlot$LabelIncThresh_num", input$PG_LabelIncTresh_num),
                     c("param_PG_intThresh", "File$ProteinGroups$IntensityThreshLog2_num", input$PG_IntensityThreshLog2_num, 1, 100),
                     c("param_EV_protThresh", "File$Evidence$ProteinCountThresh_num", input$EVD_ProteinCountThresh_num, 1, 1e5),
                     c("param_EV_intThresh", "File$Evidence$IntensityThreshLog2_num", input$EVD_IntensityThreshLog2_num, 1, 100),
                     c("param_EV_pepThresh", "File$Evidence$PeptideCountThresh_num", input$EVD_PeptideCountThresh_num, 1, 1e6),
                     c("yaml_contaminants", "File$Evidence$SpecialContaminants", contaminants),
                     c("param_EV_MatchingTolerance", "File$Evidence$MQpar_MatchingTimeWindow_num", input$EVD_MQpar_MatchingTimeWindow_num),
                     c("param_evd_mbr", "File$Evidence$MatchBetweenRuns_wA", input$EVD_MatchBetweenRuns_wA),
                     c("param_EV_PrecursorTolPPM", "File$Evidence$MQpar_firstSearchTol_num", input$EVD_MQpar_firstSearchTol_num),
                     c("param_EV_PrecursorOutOfCalSD", "File$Evidence$firstSearch_outOfCalWarnSD_num", input$EVD_firstSearch_outOfCalWarnSD_num),
                     c("param_EV_PrecursorTolPPMmainSearch", "File$Evidence$MQpar_mainSearchTol_num", input$EVD_mainSearchTol_num),
                     c("param_MSMSScans_ionInjThresh", "File$MsMsScans$IonInjectionThresh_num", input$MsMsScans_IonInjectionTresh_num, 0, 200),
                     c("param_OutputFormats", "PTXQC$OutputFormats", c("html", "plainPDF")),
                     c("param_PageNumbers", "PTXQC$PlainPDF$AddPageNumbers", "on")
    )
    
    createYAML(yc = yc, path = path.new, DEBUG_PTXQC = FALSE, get_parameters = FALSE, 
               metrics = mets, par_list = param_list)
  }
  
###############################################################################
#######################################################################################################################
  
  output$pdfd <- renderUI({
    downloadButton("pdfdownload", "PDF")
  })
  output$yamld <- renderUI({
    downloadButton("yamldownload", "yaml") 
  })
  output$htmld <- renderUI({
    downloadButton("htmldownload", "html")
  })
  
  w <- Waiter$new(html = tagList(spin_6(),
                                HTML("<br/>"),
                                div("Creating the report can take a few minutes...")))
  
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
        ##check if settings were set manually
        if(input$settings == "Change settings manually") {
          build.yaml(paste0(path.new, sep, "yaml_input"))
          yaml.obj <- yaml.load_file(paste0(path.new, sep, "yaml_input"))
        }
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
          
          tags <- tags$html(tags$iframe(src = paste0("lib", sep, list.files(path = path.new, pattern = "report.*html")),
                                   width = "100%",
                                   height = "500",
                                   seamless = "seamless",
                                   scrolling = 'yes',
                                   id = 'htmlframe',
                                   frameBorder = 0)
                            )
    
          
          print(tags)
        }
        
        ##close settings due to layout 
        updateCheckboxInput(session, "showsets", value = 0)
        
        ##generate html output
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
        
        ##download html 
        output$htmldownload <- downloadHandler(
          filename = "PTXQC_Report.html",
          content = function(file){
            file.copy(paste0(path.new, list.files(path = path.new, pattern = "report.*html")), file)
          }
        )
        
        
        
      } else {
        
        mztab_file <- input$file$datapath

        ##check if .yaml file was load
        if(!is.null(input$yamlfile)) yaml.obj <- yaml.load_file(input$yamlfile$datapath)
        ##check if settings were set manually
        if(input$settings == "Change settings manually") {
          build.yaml(paste0(dirname(input$file$datapath), sep, "yaml_input"))
          yaml.obj <- yaml.load_file(paste0(dirname(input$file$datapath), sep, "yaml_input"))
        }
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
          print(tags$iframe(src = paste0("lib", sep, list.files(path = dirname(input$file$datapath), pattern = "report.*html")), 
                            height = 500, 
                            width = "100%", 
                            seamless = "seamless",
                            scrolling = "yes",
                            id = "htmlframe",
                            frameBorder = 0)
                )
        }
        
        updateCheckboxInput(session, "showsets", value = 0)
        
        output$htmlpage<-renderUI({getPage()})
        
        ##download pdf 
        output$pdfdownload <- downloadHandler(
          filename = "PTXQC_Report.pdf",
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
        
        ##download html 
        output$htmldownload <- downloadHandler(
          filename = "PTXQC_Report.html",
          content = function(file){
            file.copy(paste0(dirname(input$file$datapath), sep, list.files(path = dirname(input$file$datapath), pattern = c("report.*html"))), file)
          }
        )
      }
      
      hideElement("dtype")
      hideElement("dirbutton")
      hideElement("dir.txt")
      hideElement("file")
      hideElement("showsets")
      hideElement("choose.dir")
      hideElement("creport")

    })
  })
  
  output$newreport <- renderUI({
    actionButton("newreportb", "Create new report")
  })
  
  observeEvent(input$newreportb, {
    browseURL(url)
  })
  
  
  output$yamldd <- downloadHandler(
    filename = "Default_settings_PTXQC.yaml",
    content = function(file){
      file.copy(yamlpath, file)
    }
  )
  
  output$infoptxqc <- renderUI({
    cran <- a("CRAN. ", href = "https://cran.r-project.org/web/packages/PTXQC/", target = "_blank")
    ptxqcgithub <- a(" here.", href = "https://github.com/cbielow/PTXQC", target = "_blank")
    shinygithub <- a(" here.", href = "https://github.com/koehlek99/Webserver-for-Quality-Control-Reports", target = "_blank")
    build.yaml(yamlpath)
    yamldefault <- downloadLink("yamldd", "here. ")
    
    tagList(HTML("This website allows users of MaxQuant (from .txt files) and OpenMS (from .mzTab files) to generate quality control reports in Html/PDF format using the R package PTXQC. <br/>
                 Advanced settings for creating the report can be either set manually or as parameters in a configuration yaml file. 
                 A yaml file with default parameters can be downloaded "), yamldefault,
            HTML("<br/><br/>More information about the PTXQC package, a manual and vignettes can be found on "), cran,
            HTML("<br/><br/>The code of the PTXQC package is available on Github "), ptxqcgithub,
            HTML("<br/>The code of this Shiny application is available on Github "), shinygithub)
  })
}


shinyApp(ui,server)

