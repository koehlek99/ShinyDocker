list.of.packages <- c("shiny", "devtools", "dplyr", "shinycssloaders", "shinyFiles", "shinythemes", "shinyjs", "magrittr", "yaml", "waiter", "shinyBS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

library(devtools)
install_github("cbielow/PTXQC", dependencies = TRUE, build_vignettes = TRUE)

install_local(path = "C:/Users/Krissi/Desktop/bachelorarbeit/PTXQC code/")

library(shiny)
library(PTXQC)
library(dplyr)
library(magrittr)
library(shinycssloaders)
library(shinyFiles)
library(shinythemes)
library(shinyjs)
library(yaml)
library(shinyjs)
library(waiter)
library(shinyBS)


ui <- fluidPage(
  
  use_waiter(), 
  useShinyjs(),
  
  
  navbarPage("Proteomics Quality Control Report", theme = shinytheme("flatly"),
    
    tabPanel("Report", fluid = TRUE, icon =icon("file-alt"),
      sidebarLayout(
      
        sidebarPanel(
          
          titlePanel(textOutput("title")),
          
          selectInput("dtype", "Data type",
                      c("MaxQuant output folder", "Mztab file")),
          
          conditionalPanel(condition = "input.dtype == 'MaxQuant output folder'",
                           htmlOutput("choose.dir"),
                           fluidRow( 
                             column(3, 
                                    uiOutput("dirbutton")
                             ), 
                             column(9, 
                                    verbatimTextOutput("dir.txt", placeholder = T)
                             )
                           )
                         ),
          conditionalPanel(condition = "input.dtype == 'Mztab file'",
                           fileInput("file", "Choose file", accept = ".mzTab")
          ),
          
          checkboxInput("showsets", "Adjust advanced settings"),
          
          conditionalPanel(condition = "input.showsets",
                           selectInput("settings", label = "How?", choices = c( "Change settings manually", "Upload yaml file")), 
                           conditionalPanel(condition = "input.showsets && input.settings == 'Upload yaml file'", 
                                            uiOutput("yaml.load")),
                           conditionalPanel(condition = "input.settings == 'Change settings manually'",
                                            
                                            tags$style(".popover{
                                                          max-width: 100%;
                                                       }"),
                                            uiOutput("adv.set1"),
                                            #bsTooltip(id="adv.set1",title="Hello! This is a hover pop-up. You'll have to click to see the next one.", placement = right, trigger = "hover"),
                                            
                                            fluidRow(
                                              column(6, 
                                                     uiOutput("adv.set2")
                                                     ),
                                              column(6, 
                                                     uiOutput("adv.set3")
                                                     )
                                              
                                            ),
                                              
                                            checkboxGroupInput("metrics", "Compute metrics", choices = lst_qcMetrics_ord, selected = lst_qcMetrics_ord)
                           )
          ),
          
          

          
          br(),


          div(id = "reset", actionButton("creport", "Create report")),

          conditionalPanel("output.created", 


                br(),
                fluidRow(
                         column(4, 
                                uiOutput("yamld")
                                ),
                         column(4, 
                                uiOutput("pdfd")
                                ), 
                         column(4, 
                                uiOutput("htmld")
                                )
                ),
                br(),
                br(),
                br(),
                fluidRow(align = "center",
                         uiOutput("newreport")
                )
             
          ),
          
          
          br()
          
        ),
        mainPanel(
          
          conditionalPanel(condition = "input.creport == 1",
                           htmlOutput("htmlpage") 
                           )
          
            
        )
      )
    ),
    
      
    tabPanel("Help", fluid = TRUE, icon = icon("question-circle"),
             uiOutput("infoptxqc")),
    
    tabPanel("About", fluid = TRUE, icon = icon("info-circle"))
  
  )
)

