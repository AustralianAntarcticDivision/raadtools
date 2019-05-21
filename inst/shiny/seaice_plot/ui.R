library(shiny)

## housekeeping
fs <- icefiles()

maxdate <- as.Date(max(fs$date))
mindate <- as.Date(as.POSIXct(maxdate) - 366 * 24 * 3600)
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("NSIDC daily sea ice concentration"),
  # date input
  sidebarPanel(
    dateRangeInput("dateRange",
                   "Dates range:",
                   start = mindate,
                   end = maxdate,
                   min = mindate,
                   max = maxdate,
                   separator = "to")
      ##,
      ##actionButton('extract values', 'Extract time series values')
  ),
  # Show the plot
    mainPanel(
        plotOutput("rasterPlot", clickId = "locator", hoverId = "hover"),
        plotOutput("timePlot")
      )
    )
)
