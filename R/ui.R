library(shiny)
library(shinyFiles)
library(shinythemes)


ui <- fluidPage(
  
  
  #titlePanel("ProTeomiX (PTX) Quality Control (QC) Report"),
  
  navbarPage("Proteomix Quality Control Report", theme = shinytheme("flatly"),
    
    tabPanel("Report", fluid = TRUE, #icon =icon()
      sidebarLayout(
      
        sidebarPanel(
          titlePanel("Load your dataset"),
          
          selectInput("dtype", "Data type",
                      c("MaxQuant output folder", "Mztab file")),
          uiOutput("ui"),
          verbatimtextOutput("dir")
        ),
        mainPanel(
          #heatmap
        )
      )
    ),
    
      
    tabPanel("Help", fluid = TRUE, icon("question-circle")),
    
    tabPanel("About", fluid = TRUE, icon("question-circle"))
  
  )
)

