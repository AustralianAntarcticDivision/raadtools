
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(raadtools)
library(roc)
library(rgdal)
scl <- function(x) (x - min(x, na.rm = TRUE))/(diff(range(x, na.rm = TRUE)))
shinyServer(function(input, output) {

  oc <- ocfiles(product = "MODISA")
  output$Plot <- renderPlot({
    #
    xl <- c(input$xmin, input$xmax)
    yl <- c(input$ymin, input$ymax)
    f <- oc$fullname[findInterval(timedateFrom(input$date), oc$date)]
    x <- readL3(f)
    xy <- do.call("cbind", bin2lonlat(x$bin_num, x$NUMROWS))
    chl <- chla(x, sensor = "MODISA", algo = input$algorithm)
    
    if (input$projection != "longlat") {
      prj <- sprintf("+proj=%s +lon_0=%f +lat_0=%f", input$projection, mean(xl), mean(yl))
      xy <- project(xy, prj)  
      xl <- range(xy[,1])
      yl <- range(xy[,2])
    }
    asp <- if(input$projection == "longlat") 1/cos(mean(yl) * pi/180) else 1
     plot(xy, pch = ".", col = chl.pal(chl), 
          xlim = xl, ylim = yl, asp = asp)
    title(basename(f))
##  plot(1, 1)
##text(1, 1, f)
})

})
