library(shiny)
library(raadtools)
library(dplyr)
icf <- icefiles(time.resolution = "monthly")

timeline <- icf %>% 
  transmute(id = as.character(row_number()), content = as.character(row_number()), 
            start = date, end = start + 30 * 24 * 3600)
library(timevis)
ui <- fluidPage(
  timevisOutput("mytime"),
  mainPanel(
    plotOutput("Plot")
  )
)

server <- function(input, output, session) {
  output$mytime <- renderTimevis(timevis(timeline, options = list(height = 180)))
  observeEvent(input$mytime_selected, {
    output$Plot <- renderPlot({
      dated <- timeline %>% filter(id == input$mytime_selected) %>% pull(start)
      plot(readice(dated, inputfiles = icf))
    })
  })
}

shinyApp(ui = ui, server = server)