list.of.packages <- c("shiny", "PTXQC", "dplyr", "shinycssloaders", "shinyFiles", "shinythemes", "shinyjs", "magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)


library(shiny)
library(PTXQC)
library(dplyr)
library(magrittr)
library(shinycssloaders)
library(shinyFiles)
library(shinythemes)
library(shinyjs)


ui <- fluidPage(
  
  
  navbarPage("Proteomix Quality Control Report", theme = shinytheme("flatly"),
    
    tabPanel("Report", fluid = TRUE, #icon =icon()
      sidebarLayout(
      
        sidebarPanel(
          titlePanel("Load your dataset"),
          
          selectInput("dtype", "Data type",
                      c("MaxQuant output folder", "Mztab file")),
          
          conditionalPanel(condition = "input.dtype == 'MaxQuant output folder'",
                           htmlOutput("choose.dir"),
                           fluidRow( 
                             column(3, 
                                    shinyDirButton("dir", "Browse...", "Choose a directory")
                             ), 
                             column(9, 
                                    verbatimTextOutput("dir.txt", placeholder = T)
                             )
                           )
                         ),
          conditionalPanel(condition = "input.dtype == 'Mztab file'",
                           fileInput("file", "Choose file", accept = ".mzTab")
          ),
          
          checkboxInput("yaml", "Add a yaml file"),
          
          uiOutput("yaml.load"),
          
          br(),
          fluidRow(align = "center", 
                   actionButton("creport", "Create report") 
          )
            
        ),
        mainPanel(
          conditionalPanel(condition = "input.creport == 1",
                           fluidRow(align = "center",
                                    downloadButton("pdfdownload", "Download as PDF") %>% withSpinner(type = 5, color = "#0dc5c1")
                           ),
                           br(),
                           htmlOutput("htmlpage")
                           )
          
            
        )
      )
    ),
    
      
    tabPanel("Help", fluid = TRUE, icon("question-circle")),
    
    tabPanel("About", fluid = TRUE, icon("question-circle"))
  
  )
)

