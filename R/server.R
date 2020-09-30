
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
##############writing parameters into yaml file (from PTXQC createReport)
#######################################################################################################################
  
  build.yaml <- function(path.new){
    
              #browser()
    
              yc = YAMLClass$new(list())
  
              param_useMQPAR = yc$getYAML("PTXQC$UseLocalMQPar", TRUE)
              
              add_fs_col = yc$getYAML("PTXQC$NameLengthMax_num", 14)
              
              cat(input$Tresh_ID_rate[1])
              id_rate_bad = yc$getYAML("File$Summary$IDRate$Thresh_bad_num", input$Thresh_ID_rate[1], 0, 100)
              id_rate_great = yc$getYAML("File$Summary$IDRate$Thresh_great_num",input$Thresh_ID_rate[2], 0, 100)
              
              GL_name_min_length = 8
              
              pg_ratioLabIncThresh = yc$getYAML("File$ProteinGroups$RatioPlot$LabelIncThresh_num", input$PG_LabelIncTresh_num)
              ## default median intensity in log2 scale
              param_PG_intThresh = yc$getYAML("File$ProteinGroups$IntensityThreshLog2_num", input$PG_IntensityThreshLog2_num, 1, 100)
              
              ## get scoring threshold (upper limit)
              param_EV_protThresh = yc$getYAML("File$Evidence$ProteinCountThresh_num", input$EVD_ProteinCountThresh_num, 1, 1e5)
              
              ## default median intensity in log2 scale
              param_EV_intThresh = yc$getYAML("File$Evidence$IntensityThreshLog2_num", input$EVD_IntensityThreshLog2_num, 1, 100)
              
              ## get scoring threshold (upper limit)
              param_EV_pepThresh = yc$getYAML("File$Evidence$PeptideCountThresh_num", input$EVD_PeptideCountThresh_num, 1, 1e6)
              
              ### warn of special contaminants!
              ## these need to be in FASTA headers (description is not enough)!
              ## syntax:  list( contaminant1 = c(name, threshold), contaminant2 = c(...), ...)
              ##
              ##  if within the YAML file
              ##    SpecialContaminants: no
              ##  is set, then 'yaml_contaminants' will be 'FALSE'
              ##
              contaminant_default = list("cont_MYCO" = c(name="MYCOPLASMA", threshold=1))# name (FASTA), threshold for % of unique peptides
              
              contaminant_list <- strsplit(input$special_contaminants, ";")
              contaminant_list <- unlist(contaminant_list)
              contaminants <- list()
              for(c in (1:length(contaminant_list))){
                cont <- unlist(strsplit(contaminant_list[c], ":"))
                cont_name <- paste0("cont_", cont[1])
                contaminants[[cont_name]] <- c(name = cont[1], threshold = as.integer(cont[2]))
              }

              
              ##contaminant_default = FALSE ## to switch it off by default
              yaml_contaminants = yc$getYAML("File$Evidence$SpecialContaminants", contaminants)
              
              param_EV_MatchingTolerance = yc$getYAML("File$Evidence$MQpar_MatchingTimeWindow_num", input$EVD_MQpar_MatchingTimeWindow_num)

              param_evd_mbr = yc$getYAML("File$Evidence$MatchBetweenRuns_wA", input$EVD_MatchBetweenRuns_wA)
              
              param_EV_PrecursorTolPPM = yc$getYAML("File$Evidence$MQpar_firstSearchTol_num", input$EVD_MQpar_firstSearchTol_num)
              
              param_EV_PrecursorOutOfCalSD = yc$getYAML("File$Evidence$firstSearch_outOfCalWarnSD_num", input$EVD_firstSearch_outOfCalWarnSD_num)
              
              ## we do not dare to have a default, since it ranges from 6 - 4.5 ppm across MQ versions
              param_EV_PrecursorTolPPMmainSearch = yc$getYAML("File$Evidence$MQpar_mainSearchTol_num", input$EVD_mainSearchTol_num)
              if (is.na(param_EV_PrecursorTolPPMmainSearch))
              {
                warning("PTXQC: Cannot draw borders for calibrated mass error, since neither 'File$Evidence$MQpar_mainSearchTol_num' is set nor a mqpar.xml file is present!", immediate. = TRUE)
              }
              
              param_MSMSScans_ionInjThresh = yc$getYAML("File$MsMsScans$IonInjectionThresh_num", input$MsMsScans_IonInjectionTresh_num, 0, 200)
              
              df.meta <- df.meta[,c("order", ".id")]
              mets <- paste0("qcMetric_", input$metrics)
              for(i in 1:nrow(df.meta)){
                pname = paste0("order$", df.meta$.id[i])
                pval = df.meta$order[i]
                if(df.meta$.id[i] %in% mets) yc$getYAML(pname, pval)
                else yc$getYAML(pname, (-1))
              }
              
              yc$writeYAML(paste0(path.new, sep, "yaml_input"))
         
  }
#######################################################################################################################
#######################################################################################################################
  
  output$pdfd <- renderUI({
    downloadButton("pdfdownload", "Download as PDF")
  })
  output$yamld <- renderUI({
    downloadButton("yamldownload", "Download yaml file") 
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
          build.yaml(path.new)
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
        
        
        
      } else {
        
        mztab_file <- input$file$datapath

        ##check if .yaml file was load
        if(!is.null(input$yamlfile)) yaml.obj <- yaml.load_file(input$yamlfile$datapath)
        ##check if settings were set manually
        if(input$settings == "Change settings manually") {
          build.yaml(dirname(input$file$datapath))
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
      
      hideElement("dtype")
      hideElement("dirbutton")
      hideElement("dir.txt")
      hideElement("file")
      hideElement("showsets")
      hideElement("choose.dir")
      hideElement("creport")
      #shinyjs::reset("reset")
      
    })
  })
  
  output$newreport <- renderUI({
    actionButton("newreport", "Create new report")
  })
  
}


shinyApp(ui,server)


##output$dir.txt directory path MaxQuant output 
##input$file file path mztab


#getMetricsObject und ordnen