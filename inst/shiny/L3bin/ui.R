library(shiny)
library(raadtools)
oc <- ocfiles(product = "MODISA")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("NASA ocean colour L3 chlorophyll (MODISA)"),

  # Sidebar with date input
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "Date", value = max(oc$date), 
                min = min(oc$date), max = max(oc$date)),
      numericInput("xmin", "Xmin", value = -180, min = -180, max = 180),
      numericInput("xmax", "Xmax", value = 180, min = -180, max = 180), 
    
    numericInput("ymin", "Ymin", value = -90, min = -90, max = 90),
    numericInput("ymax", "Ymax", value = 90, min = -90, max = 90), 
    selectInput("algorithm", "Algorithm", choices = c("oceancolor", "johnson")), 
    textInput("projection", "Projection",value = "longlat")
    ),
    
    # Show a plot of the data
    mainPanel(
      plotOutput("Plot")
    )
  )
))
