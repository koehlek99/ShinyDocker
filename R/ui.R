library(shiny)
library(shinyFiles)

ui <- fluidPage(
  
  titlePanel("ProTeomiX (PTX) Quality Control (QC) Report"),
  
  sidebarLayout(
    
    sidebarPanel(
      shinyDirButton('dir', 'Load Dataset', FALSE)
    ),
    mainPanel(
      textOutput("txt")
    )
    
  )
)

