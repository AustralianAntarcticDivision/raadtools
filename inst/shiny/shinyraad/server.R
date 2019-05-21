library(shiny)

library(raadtools)
library(rgdal)

options(default.datadir = "/mnt/raadtools")



shinyServer(function(input, output) {
    ## read file provided, or return default voyage track
    returnDataset <- reactive({
        inFile <- input$file1

        if (is.null(inFile)) {
            data(aurora)
            aurora$DATE_TIME_UTC <- aurora$DATE_TIME_UTC - 7.4 * 365.25 * 24 * 3600  ##seq(timedateFrom(mindate), timedateFrom(maxdate), length = nrow(aurora))
            dat <- aurora
        } else {
            ## here we could have control of the input data/time format
            dat <- try(read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote))

            ## ok, here we really need some extra validation
            dt  <- try(timedateFrom(dat[,3]))
            if (inherits(dt, "try-error")) {
                stop("problem with file, or file read options")##, sample lines:<br>adadfd<br>stuff<b>aaa</b>")##, paste(readLines(inFile$datapath, 4), collapse = "\n"))
            } else {
                dat[,3] <- dt
            }
        }
        dat
    })

    selectInterpXY <- reactive({
        input$interpXY
    })
    selectInterpTime <- reactive({
        switch(input$interpTime,
               simple = FALSE,
               bilinear = TRUE)
    })

    selectResizeFactor <- reactive({
        input$resizeFactor
    })
    selectDisplayTable <- reactive({
        input$DisplayTable
    })
    ## choose extract function from dataset name input
    selectExtractFunction <- reactive({
        switch(input$dataset,
               chla_johnson = readchla,
               chla_oceancolor = function(x, ...) readchla(x, product = "oceancolor", ...),
               currentdirection = function(x, ...) readcurr(x, dironly = TRUE, ...),
               currentmagnitude = function(x, ...) readcurr(x, magonly = TRUE, ...),
               fronts = readfronts,
               ice = readice,
               mld = readmld,
               ## turn off until fix is deployed for 1-layer datasets
               arrigo_production = readprod,
               ssh = readssh,
               ssha = function(x, ...) readssh(x, ssha = TRUE, ...),
               sst = readsst,
               topo = function(x, ...) readtopo("etopo2"),
               winddirection = function(x, ...) readwind(x, dironly = TRUE, ...),
               windmagnitude = function(x, ...) readwind(x, magonly = TRUE, ...))

    })

    selectPaletteFunction <- reactive({
        switch(input$dataset,
               chla_johnson= chl.pal,
               chla_oceancolor = chl.pal,
               currentdirection = function(x, ...) rainbow(x, ...),
               currentmagnitude = sst.pal,
               fronts = sst.pal,
              ice = sst.pal,
               arrigo_production = chl.pal,
               mld = sst.pal,
               ssh = sst.pal,
               ssha = sst.pal,
              sst = sst.pal,
               topo = sst.pal,
               winddirection = function(x, ...) rainbow(x, ...),
               windmagnitude = sst.pal)
    })
    ## return the input data set with added attribute from extraction
    returnAttributedDataset <- reactive({
        dataset <- returnDataset()

        varname <- input$dataset
        extractfun <- selectExtractFunction()
        resize <- as.numeric(selectResizeFactor())
        if (!resize > 1) resize <- NULL
        interpXY <- selectInterpXY()
        interpTime <- selectInterpTime()

        xy <- dataset[,1:2]
        xy[xy[,1] > 180 ,1] <- xy[xy[,1] > 180 ,1] - 360
        if (varname == "mld") stop("MLD is a climatology and the lookup-by-month is not supported, yet")
        if (varname == "topo") stop("lookup-without-time is not supported, yet")
        ##return(data.frame(varname,  nrow(dataset), interpTime, interpXY))
        dataset[[varname]] <- extract(extractfun, dataset, contintime = interpTime, method = interpXY, fact = resize)
        dataset
    })


    ## table for the web page
  output$contents <- renderTable({

      dataset <- returnAttributedDataset()
      dataset[[3]] <- format(dataset[[3]])
      maxrows <- input$maxrow
      dataset[seq_len(min(c(maxrows, nrow(dataset)))), ]
  })



  output$rasterPlot <- renderPlot({

      dataset <- returnDataset()
      extractfunction <- selectExtractFunction()
      daterange <- range(dataset[,3])
      ## normalize daterange to the best we can get from the data

      if (input$dataset == "topo") {
          x <- extractfunction()
          files <- list(date = Sys.time())
      } else {
          x <- extractfunction(daterange)
          files <- extractfunction(returnfiles = TRUE)
      }

       daterange[1] <- max(daterange[1L], min(files$date))
      daterange[2] <- min(daterange[2L], max(files$date))
##      xylimit <- c(range(dataset[,1]), range(dataset[,2]))

        pt <- as.matrix(dataset[,1:2])
      pt[pt[,1] > 180 ,1] <- pt[pt[,1] > 180 ,1] - 360

      if (!isLonLat(projection(x))) pt <- project(pt, projection(x))

      ## TOFIX
      ## see here how arrigo production doesn't return a later date, we get 1 layer - got to find the last valid one or something

      names(x) <- paste(input$dataset, format(daterange[seq_len(nlayers(x))], "%Y_%b_%d"), sep = "__")
      cols <- colorRampPalette(c("firebrick", "dodgerblue"))(nrow(pt))

      ##if (input$plotboundtype == "auto") {
          ## expand the range around the query points
          buf <- 0.3
          xl <- range(c(pt[,1]))
          xl <- xl + c(-1, 1) * diff(xl) * buf
          xl[1L] <- max(c(xmin(x), xl[1L]))
          xl[2L] <- min(c(xmax(x), xl[2L]))
          yl <- range(c(pt[,2]))
          yl <- yl + c(-1, 1) * diff(yl) * buf
          yl[1L] <- max(c(ymin(x), yl[1L]))
          yl[2L] <- min(c(ymax(x), yl[2L]))
      ##} else {
      ##    xl <- c(input$lonmin, input$lonmax)
      ##    yl <- c(input$latmin, input$latmax)
      ##    if (!isLonLat(x)) {
      ##        px <- project(as.matrix(expand.grid(xl, yl)), projection(x))
      ##        xl <- bbox(px)[1L,]
      ##        yl <- bbox(px)[2L,]
      ##    }
      ##}
      x <- crop(x, extent(c(xl, yl)))
      resize <- as.numeric(selectResizeFactor())
      if (resize > 1) x <- aggregate(x, fact = resize, fun = "mean")
      breaks <- pretty(cellStats(x, range), 8)
      palfun <- selectPaletteFunction()
##      cols <- palfun(length(breaks) - 1)
      palcols <- palfun(65, alpha = (input$opacity / 100))[-65]
      plot(x, col = palcols,  axes = FALSE, box = FALSE, addfun = function() {points(pt, col = cols, pch = 16, cex = 1.3);lines(pt)})

  })


      output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(returnAttributedDataset(), file)
    }
  )

})
