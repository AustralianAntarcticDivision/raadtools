library(shiny)

library(raadtools)
library(rgdal)

template <- readice()
proj <- projection(template)
shinyServer(function(input, output) {
    ## plot two dates, chosen by the user
  output$rasterPlot <- renderPlot({
      dates <- input$dateRange[1L]
      x <- readice(dates[1])
      names(x) <- format(getZ(x), "NSIDC_%d_%b_%Y")
      plot(x)

  })

  output$timePlot <- renderPlot({
            if (!is.null(input$locator))  {
                dts <- as.POSIXct(seq(input$dateRange[1L], input$dateRange[2L],length=80))
                d <- data.frame(project(cbind(rep(input$locator$x, length(dts)), input$locator$y), proj, inv = TRUE), dts)
                ##stop(paste(range(dts), collapse = ","))
                ##stop(paste(dts, collapse = ","))
                ##stop(dput(d))
                vals <- extract(readice,d)

                plot(dts, vals, type = "b")
            }
        })
})
