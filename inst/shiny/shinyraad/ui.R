library(shiny)



# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Query data values with input longitude, latitude, and date-time"),
  # date input
  sidebarPanel(
      tabsetPanel(
          tabPanel("Data",

                   selectInput("dataset", "1. Choose a variable, by default a sample query will be displayed.",
                               choices = c(
                               "ice",
                               "arrigo_production",
                               "chla_johnson",
                               "chla_oceancolor",
                               "currentdirection",
                               "currentmagnitude",
                               "fronts",
                               "mld",
                               "ssh",
                               "ssha",
                               "sst",
                               "topo",
                               "winddirection",
                               "windmagnitude")),
                             helpText("2. Optionally, specify an input file under 'File' tab", br(),br(),
			     	      "3. Specify other options for the query.",
                                      br(),br(),
                                      "4. Download the results, see sample rows on right:"),
				      numericInput("maxrow", "Max number of rows to display", value = 20),
                   downloadButton('downloadData', 'Download results (CSV only)')



                   ),

          tabPanel("File",
                   helpText("NOTE: large files will take a long time to process, please test first with smallish numbers of rows (<200)."),
                   fileInput('file1',
                    'Choose input File (inbuilt track used by default)',  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                   helpText("Set input options:"),
                   checkboxInput('header', 'Header', TRUE),
                   radioButtons('sep', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                'Comma'),
                   radioButtons('quote', 'Quote',
                                c(None='', 'Double Quote'='"', 'Single Quote'="'"), 'Double Quote'),
                   tags$hr(),

                   helpText("File must contain only three columns",
                            "in order of 'longitude', 'latitude', 'date-time'.",
                            "(Column names do not matter and are optional).",
                            br(),br(),
                            "Longitude and latitude must be in decimal degrees",
                            br(),br(),
                            "Date-time can be date only or full date-time, e.g.",
                            br(),
                            Sys.Date(),
                            br(),
                            Sys.time())
                  ),



          tabPanel("Resampling",
                   selectInput("interpXY", "Sampling XY?", choices = c("simple", "bilinear")),
                   selectInput("interpTime", "Sampling Time?", choices = c("simple", "bilinear")),
                   selectInput("resizeFactor", "Spatial resize (improve coverage)", choices = 2^(0:10))
                   ),

          tabPanel("Plot options",
                   numericInput("opacity", "Plot opacity (100% == opaque, min 10%):", 50, min = 10, max = 100, step = 5)
                   ##,
                   ##radioButtons("plotboundtype", "Plot bounds", c(auto = "auto", manual = "manual"), "auto"),
                   ##conditionalPanel(condition = "input.plotboundtype == 'manual'",
                   ##                 numericInput("lonmin", "Longitude minimum", -180, min = -180, max = 170),
                   ##                 numericInput("lonmax", "Longitude maximum", 180, min = -170, max = 180),
                   ##                 numericInput("latmin", "Latitude minimum", -90, min = -90, max = 80),
                   ##                 numericInput("latmax", "Latitude maximum", 0, min = -90, max = 90)


                   )


          )),

  # Show a plot of the first and last available data
  mainPanel(
    plotOutput("rasterPlot"),
      br(),br(),br(),

          tableOutput('contents')

  )))
