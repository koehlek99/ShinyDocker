shinyServer(function(input, output, session){
  
  options(shiny.maxRequestSize=50*1024^2)
  
  if(.Platform$OS.type == "windows") sep <- "\\"
  else sep <- "/"
  
  setwd(sep)
  
  output$title <- renderText({
    if(input$creport) "Download files"
    else "Upload your data"
  })
  
  
  ##loading .yaml file
  output$yaml.load <- renderUI({
    fileInput("yamlfile", "Choose yaml file")
  })
  
  ##advanced settings
  output$adv.set1 <- renderUI({
    tagList(
      renderText({"Summary:"}),
      tipify(sliderInput("Thresh_ID_rate", "ID rate", 0, 100, c(20,35), width = "100%"), title = "treshold for MS/MS identification bins", placement = "right"),
      renderUI({HTML("<br/>")})
      
    )
  })
  
  
  
  
  
  output$adv.set2 <- renderUI({
    tagList(
      renderText({"Protein groups: "}), 
      tipify(numericInput("PG_LabelIncTresh_num", "label inc", 4, width = "100%"), title = "threshold for label incorporation ratio", placement = "right"),
      renderUI({HTML("<br/>")}),
      renderText({"Evidence: "}),
      tipify(numericInput("EVD_ProteinCountThresh_num", "Protein counts", 3500, width = "100%"), title = "target threshold for protein counts", placement = "right"),
      tipify(numericInput("EVD_PeptideCountThresh_num", "Peptide counts", 15000, width = "100%"), title = "target threshold for peptide counts", placement = "right"),
      tipify(numericInput("EVD_mainSearchTol_num", "main search tol", 4.5, width = "100%"), title = " calibrated mass error tolerance (MQ parameter)", placement = "right"),
      tipify(numericInput("EVD_firstSearch_outOfCalWarnSD_num", "FS outofcal warnsd", 2, width = "100%"), title = "maximum standard deviation for uncalibrated mass error distribution [ppm]", placement = "right"),
      tipify(textInput("special_contaminants", "Contaminant and threshhold (in %)", "MYCOPLASMA: 1"), title = "additional contaminant (name within protein identifier) proportion and threshold [%] to plot", placement = "right"),
      renderUI({HTML("<br/>")}),
      renderText({"MsMs Scans: "}), 
      tipify(numericInput("MsMsScans_IonInjectionTresh_num", "Ion injection time", 10, width = "100%"), title = "treshold for ion injection time [min]", placement = "right")
      
    )
  })
  
  output$adv.set3 <- renderUI({
    tagList(
      renderUI({HTML("<br/>")}),
      tipify(numericInput("PG_IntensityThreshLog2_num", "log2-Intensity", 25, width = "100%"), title = "target treshold for protein intensities", placement = "right"),
      renderUI({HTML("<br/>")}),
      renderUI({HTML("<br/>")}),
      tipify(numericInput("EVD_IntensityThreshLog2_num", "log2-Intensity", 23, width = "100%"), title = "target threshold for peptide intensities", placement = "right"),
      tipify(numericInput("EVD_MQpar_MatchingTimeWindow_num", "Matching time window", 0.7, width = "100%"), title = "Matching tolerance [min] for Match-between-Runs (MQ parameter)", placement = "right"),
      tipify(numericInput("EVD_MQpar_firstSearchTol_num", "First search tol", 20, width = "100%"), title = " uncalibrated mass error tolerance (MQ parameter)", placement = "right"),
      tipify(selectInput("EVD_MatchBetweenRuns_wA", "Match between runs", c("yes", "no", "auto"), "auto", width = "100%"), title = "logical value indicating if MBR should be used for identification (auto offers a heuristic)", placement = "right")
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
    
    param <- list()
    param$id_rate_bad <- input$Thresh_ID_rate[1]
    param$id_rate_great <- input$Thresh_ID_rate[2]
    param$pg_ratioLabIncThresh <- input$PG_LabelIncTresh_num
    param$param_PG_intThresh <- input$PG_IntensityThreshLog2_num
    param$param_EV_protThresh <- input$EVD_ProteinCountThresh_num
    param$param_EV_intThresh <- input$EVD_ProteinCountThresh_num
    param$param_EV_pepThresh <- input$EVD_PeptideCountThresh_num
    param$yaml_contaminants <- contaminants
    param$param_EV_MatchingTolerance <- input$EVD_MQpar_MatchingTimeWindow_num
    param$param_evd_mbr <- input$EVD_MatchBetweenRuns_wA
    param$param_EV_PrecursorTolPPM <- input$EVD_MQpar_firstSearchTol_num
    param$param_EV_PrecursorOutOfCalSD <- input$EVD_firstSearch_outOfCalWarnSD_num
    param$param_EV_PrecursorTolPPMmainSearch <- input$EVD_mainSearchTol_num
    param$param_MSMSScans_ionInjThresh <- input$MsMsScans_IonInjectionTresh_num
    
    
    PTXQC:::createYaml(yc = yc, DEBUG_PTXQC = FALSE, 
               metrics = mets, param = param)
    yc$writeYAML(path.new)
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
      if(input$dtype=="MaxQuant files"){
        
        ##copying MaxQuant files into temporary dir
        path.old <- dirname(input$mqfiles$datapath[1])
        path.new <- paste0(tempfile("report", tempdir(), sep))
        dir.create(path.new)
        file.copy(input$mqfiles$datapath, paste0(path.new, sep, input$mqfiles$name))
        
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
                                        height = "750",
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
                            height = 750, 
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
      #hideElement("dirbutton")
      #hideElement("dir.txt")
      hideElement("file")
      hideElement("mqfiles")
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
    
    tagList(HTML("This website allows users of MaxQuant (from .txt files) and OpenMS (from .mzTab files) to generate quality control reports in Html/PDF format using the R package PTXQC.
                  In case of processing MaxQuant output, please provide the following .txt files: <br/>
                  Evidence,  MsMs,  MsMsScans,  Parameters,  ProteinGroups,  Summary and Mqpar <br/> <br/>
                 Advanced settings for creating the report can be either set manually or as parameters in a configuration yaml file. 
                 A yaml file with default parameters can be downloaded "), yamldefault,
            HTML("<br/><br/>More information about the PTXQC package, a manual and vignettes can be found on "), cran,
            HTML("<br/><br/>The code of the PTXQC package is available on Github "), ptxqcgithub,
            HTML("<br/>The code of this Shiny application is available on Github "), shinygithub)
  })
  
  output$impressum <- renderUI({
    tagList(img(src="BSC.png", 
                alt = "BSC logo",
                height = "371",
                width = "445",
                hspace = "10",
                align = "left"),
            HTML("<br/>    This web application was developed as a part of my bachelor thesis. <br/>
                     I got supervised by Dr. Chris Bielow and Dr. Sandro Andreotti (Bioinformatics Solution Center, Freie Universitaet Berlin). <br/> <br/>"),
            tags$h4("Impressum"),
            HTML(
              "Kristin Koehler <br/>
              Freie Universitaet Berlin <br/>
              koehlek99@zedat.fu-berlin.de"))
    
  })
})


shinyApp(shinyUI,shinyServer)
