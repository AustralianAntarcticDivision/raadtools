library(shiny)
library(raadtools)
## housekeeping
fs <- icefiles()
mindate <- as.Date(min(fs$date))
maxdate <- as.Date(max(fs$date))

shinyUI(pageWithSidebar(
  # Application title
  headerPanel("NSIDC daily sea ice concentration"),
  # date input
  sidebarPanel(
    dateRangeInput("dateRange",
                "Dates to query:",
                start = mindate,
                end = maxdate,
                   min = mindate,
                max = maxdate,
                   separator = "and"), 
    shiny::selectInput("palette", label = "palette", choices = c("viridis", "inferno", "nsidc", "grey"), selected = "inferno")

  ),
  

  # Show the plot
    mainPanel(
    plotOutput("rasterPlot")
      )

    )


)
