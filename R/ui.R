list.of.packages <- c("shiny", "devtools", "dplyr", "shinycssloaders", "shinyFiles", "shinythemes", "shinyjs", "magrittr", "yaml", "waiter")
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

ui <- fluidPage(
  
  use_waiter(), 
  
  navbarPage("Proteomics Quality Control Report", theme = shinytheme("flatly"),
    
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
          
          checkboxInput("settings", "Show advanced settings"),
          uiOutput("adv.set"),
          
          br(),
          fluidRow(
            column(3, 
                   actionButton("creport", "Create report", style='padding:5px; font-size:80%')
                   ),

            conditionalPanel("output.created", 
              column(4,
                     uiOutput("pdfd")
                    ), 
              column(4, 
                     uiOutput("yamld")
                    )
            )
          ),
          
          br()
          
        ),
        mainPanel(
          conditionalPanel(condition = "input.creport == 1",
                           htmlOutput("htmlpage") #%>% withSpinner(type = 5, color = "#0dc5c1")
                           )
          
            
        )
      )
    ),
    
      
    tabPanel("Help", fluid = TRUE, icon("question-circle")),
    
    tabPanel("About", fluid = TRUE, icon("question-circle"))
  
  )
)

