library(shiny)
library(raadtools)
library(viridis)
library(palr)
shinyServer(function(input, output) {
    ## plot two dates, chosen by the user
  output$rasterPlot <- renderPlot({
      dates <- input$dateRange
      x <- readice(dates)
      names(x) <- format(getZ(x), "NSIDC_%d_%b_%Y")
      pal <- switch(input$palette, 
                    viridis = viridis::viridis(99), 
                    inferno = viridis::inferno(99), 
                    nsidc = palr::icePal(99), 
                    grey = grey(seq(0, 1, length = 100)[-1]))
      plot(x, col = pal, zlim = c(0, 100), axes = FALSE)
  })

})
