
##' SST colours
##'
##' @title SST colours
##' @param x a vector of data values or a single number
##' @param palette logical, if \code{TRUE} return a list with matching colours and values
##' @param alpha value in 0,1 to specify opacity
##' @references Derived from \url{"http://oceancolor.gsfc.nasa.gov/DOCS/palette_sst.txt}.
##' @return colours, palette, or function, see Details
##' @export
sst.pal <- function(x, palette = FALSE, alpha = 1) {
  ##pal <- read.table("http://oceancolor.gsfc.nasa.gov/DOCS/palette_sst.txt", header = TRUE, colClasses = "integer", comment.char = "")
  ##cols <- rgb(pal[,2], pal[,3], pal[,4], maxColorValue = 255)
  ##dput(cols)
  breaks <- seq(-2, 46, length = 256)
  cols <- c("#5B0A76", "#63098B", "#7007AB", "#7C07CA", "#8207DF", "#8007EA",
            "#7807EE", "#6E07EE", "#6407EF", "#5807EF", "#4907EF", "#3607EF",
            "#2208EE", "#1208EC", "#0B08E9", "#0808E4", "#0808E0", "#0808DD",
            "#0808D9", "#0808D4", "#0808CF", "#0808C9", "#0808C4", "#0808BF",
            "#0808B9", "#0808B3", "#0808AD", "#0808A6", "#08089F", "#080899",
            "#080B93", "#08108B", "#081782", "#081E79", "#082672", "#082E6D",
            "#08366A", "#083E68", "#084668", "#084E68", "#08566A", "#085D6B",
            "#08626D", "#086470", "#086372", "#086275", "#08617A", "#086282",
            "#08658B", "#086893", "#086C98", "#08719B", "#08769D", "#087B9E",
            "#08809F", "#08859F", "#0888A0", "#088CA2", "#0891A3", "#0896A5",
            "#089CA6", "#08A1A7", "#08A6A9", "#08ACAC", "#08B1B1", "#08B6B6",
            "#08BBBB", "#08C0C0", "#08C5C5", "#08C8C8", "#08CCCC", "#08D1D1",
            "#08D5D5", "#08D8D8", "#08DBDB", "#08DEDE", "#08E1E1", "#08E3E3",
            "#08E6E6", "#08E9E9", "#08EBEB", "#08EDEC", "#08EEEA", "#08EEE5",
            "#08EEDF", "#08EDD8", "#08EBD1", "#08EACA", "#08E8C0", "#08E7B2",
            "#08E6A2", "#08E593", "#08E385", "#08E27B", "#08E074", "#08DD72",
            "#08DB72", "#08D873", "#08D575", "#08D176", "#08CC76", "#08C775",
            "#08C173", "#08BC70", "#08B86B", "#08B563", "#08B25B", "#08AF54",
            "#08AC51", "#08A950", "#08A450", "#089F50", "#089950", "#08944F",
            "#088F4F", "#088A4F", "#08864E", "#08844B", "#088646", "#08893F",
            "#088C38", "#088C30", "#088B25", "#098A18", "#0E8B0E", "#188D09",
            "#259208", "#2F9708", "#349C08", "#37A108", "#39A608", "#3CAC08",
            "#41B108", "#46B608", "#4BBB08", "#50C008", "#55C408", "#59C708",
            "#5FC908", "#67CC08", "#6ECE08", "#76D108", "#7ED308", "#86D608",
            "#8ED908", "#97DB08", "#A0DE08", "#A9E108", "#B3E308", "#BBE508",
            "#C3E608", "#CDE608", "#D7E608", "#DFE508", "#E2E308", "#E1E008",
            "#E0DD08", "#E0D908", "#E0D308", "#E0CD08", "#E0C608", "#E0BF08",
            "#E0B908", "#E0B408", "#E0AE08", "#E0A608", "#E09F08", "#E09708",
            "#E08F08", "#E08708", "#E07F08", "#E07708", "#E06F08", "#E06708",
            "#E05F08", "#E05908", "#E05408", "#E04E08", "#DF4608", "#DF3F08",
            "#DF3708", "#DE2E08", "#DD2508", "#DB1C08", "#D81308", "#D50C08",
            "#D10908", "#CC0808", "#C80808", "#C40808", "#BE0808", "#B60808",
            "#AF0808", "#A70808", "#9F0808", "#970808", "#8F0808", "#870808",
            "#7F0808", "#780808", "#730808", "#6E0808", "#680808", "#6C0D0D",
            "#701313", "#721616", "#731A1A", "#761E1E", "#772221", "#792626",
            "#7B2929", "#7D2D2D", "#7F3131", "#803435", "#823838", "#843C3C",
            "#86403F", "#884343", "#8A4746", "#8C4B4A", "#8E4F4E", "#8F5252",
            "#915555", "#93595A", "#955D5D", "#966161", "#986464", "#9A6868",
            "#9C6B6C", "#9E706F", "#A07373", "#A27777", "#A47B7B", "#A57E7E",
            "#A88282", "#A98686", "#AA8A89", "#AD8D8D", "#AF9191", "#B09595",
            "#B29999", "#B59C9C", "#B69F9F", "#B7A3A3", "#B9A7A7", "#BCAAAB",
            "#BEAEAE", "#BFB2B2", "#C1B6B6", "#C3B9B9", "#C5BDBD", "#C7C0C1",
            "#C8C5C5", "#CAC8C9", "#CCCCCC", "#000000")
  
  hexalpha <- as.hexmode(round(255 * alpha))
  if (nchar(hexalpha) == 1L) hexalpha <- paste(rep(hexalpha, 2L), collapse = "")
  cols <- paste0(cols, hexalpha)
  
  if (palette) return(list(breaks = breaks, cols = cols))
  
  if (missing(x)) return(colorRampPalette(cols))
  
  if (length(x) == 1L) {
    return(paste0(colorRampPalette(cols)(x), hexalpha))
  } else {
    return(cols[findInterval(x, breaks)])
  }
  
}



##' Ocean colour palette for chlorophyll-a.
##'
##' Flexible control of the chlorophyll-a palette. If \code{x} is a
##' single number, the function returns that many colours evenly
##' spaced from the palette. If \code{x} is a vector of multiple
##' values the palette is queried for colours matching those values,
##' and these are returned. If \code{x} is missing and \code{palette}
##' is \code{FALSE} then a function is returned that will generate n
##' evenly spaced colours from the palette, as per
##' \code{\link{colorRampPalette}}.
##' @title Ocean colour colours for chlorophyll-a.
##' @param x a vector of data values or a single number
##' @param palette logical, if \code{TRUE} return a list with matching colours and values
##' @param alpha value in 0,1 to specify opacity
##' @references Derived from \url{http://oceancolor.gsfc.nasa.gov/DOCS/palette_chl_etc.txt}.
##' @return colours, palette, or function, see Details
##' @export
##' @examples
##' \dontrun{
##' chl <- readchla(xylim = c(100, 110, -50, -40))
##' ## just get a small number of evenly space colours
##' plot(chl, col = chl.pal(10))
##' ## store the full palette and work with values and colours
##' pal <- chl.pal()
##' ## the standard full palette
##' plot(chl, breaks = pal$breaks, col = pal$cols)
##' ## a custom set of values with matching colours
##' plot(chl, col = chl.pal(pal$breaks[seq(1, length(pal$breaks), length = 10)]))
##' ## any number of colours stored as a function
##' myfun <- chl.pal()
##' plot(chl, col = myfun(18))
##' ## just n colours
##' plot(chl, col = chl.pal(18))
##' }
chl.pal <- function(x, palette = FALSE, alpha = 1) {
  
  ##pal <- read.table("http://oceancolor.gsfc.nasa.gov/DOCS/palette_chl_etc.txt", header = TRUE, colClasses = "integer", comment.char = "")
  ##cols <- rgb(pal[,2], pal[,3], pal[,4], maxColorValue = 255)
  ##dput(cols)
##  breaks <-  c(0, exp(round(seq(-4.6, 4.1, length = 255), digits = 2)))
  breaks <- c(0, 10^seq(-2, log10(20), length  = 254), 1000)

  cols <- c("#000000", "#90006F", "#8D0072", "#8A0075", "#870078", "#84007B",
            "#81007E", "#7E0081", "#7B0084", "#780087", "#75008A", "#72008D",
            "#6F0090", "#6C0093", "#690096", "#660099", "#63009C", "#60009F",
            "#5D00A2", "#5A00A5", "#5700A8", "#5400AB", "#5100AE", "#4E00B1",
            "#4B00B4", "#4800B7", "#4500BA", "#4200BD", "#3F00C0", "#3C00C3",
            "#3900C6", "#3600C9", "#3300CC", "#3000CF", "#2D00D2", "#2A00D5",
            "#2700D8", "#2400DB", "#2100DE", "#1E00E1", "#1B00E4", "#1800E7",
            "#1500EA", "#1200ED", "#0F00F0", "#0C00F3", "#0900F6", "#0600F9",
            "#0000FC", "#0000FF", "#0005FF", "#000AFF", "#0010FF", "#0015FF",
            "#001AFF", "#0020FF", "#0025FF", "#002AFF", "#0030FF", "#0035FF",
            "#003AFF", "#0040FF", "#0045FF", "#004AFF", "#0050FF", "#0055FF",
            "#005AFF", "#0060FF", "#0065FF", "#006AFF", "#0070FF", "#0075FF",
            "#007AFF", "#0080FF", "#0085FF", "#008AFF", "#0090FF", "#0095FF",
            "#009AFF", "#00A0FF", "#00A5FF", "#00AAFF", "#00B0FF", "#00B5FF",
            "#00BAFF", "#00C0FF", "#00C5FF", "#00CAFF", "#00D0FF", "#00D5FF",
            "#00DAFF", "#00E0FF", "#00E5FF", "#00EAFF", "#00F0FF", "#00F5FF",
            "#00FAFF", "#00FFFF", "#00FFF7", "#00FFEF", "#00FFE7", "#00FFDF",
            "#00FFD7", "#00FFCF", "#00FFC7", "#00FFBF", "#00FFB7", "#00FFAF",
            "#00FFA7", "#00FF9F", "#00FF97", "#00FF8F", "#00FF87", "#00FF7F",
            "#00FF77", "#00FF6F", "#00FF67", "#00FF5F", "#00FF57", "#00FF4F",
            "#00FF47", "#00FF3F", "#00FF37", "#00FF2F", "#00FF27", "#00FF1F",
            "#00FF17", "#00FF0F", "#00FF00", "#08FF00", "#10FF00", "#18FF00",
            "#20FF00", "#28FF00", "#30FF00", "#38FF00", "#40FF00", "#48FF00",
            "#50FF00", "#58FF00", "#60FF00", "#68FF00", "#70FF00", "#78FF00",
            "#80FF00", "#88FF00", "#90FF00", "#98FF00", "#A0FF00", "#A8FF00",
            "#B0FF00", "#B8FF00", "#C0FF00", "#C8FF00", "#D0FF00", "#D8FF00",
            "#E0FF00", "#E8FF00", "#F0FF00", "#F8FF00", "#FFFF00", "#FFFB00",
            "#FFF700", "#FFF300", "#FFEF00", "#FFEB00", "#FFE700", "#FFE300",
            "#FFDF00", "#FFDB00", "#FFD700", "#FFD300", "#FFCF00", "#FFCB00",
            "#FFC700", "#FFC300", "#FFBF00", "#FFBB00", "#FFB700", "#FFB300",
            "#FFAF00", "#FFAB00", "#FFA700", "#FFA300", "#FF9F00", "#FF9B00",
            "#FF9700", "#FF9300", "#FF8F00", "#FF8B00", "#FF8700", "#FF8300",
            "#FF7F00", "#FF7B00", "#FF7700", "#FF7300", "#FF6F00", "#FF6B00",
            "#FF6700", "#FF6300", "#FF5F00", "#FF5B00", "#FF5700", "#FF5300",
            "#FF4F00", "#FF4B00", "#FF4700", "#FF4300", "#FF3F00", "#FF3B00",
            "#FF3700", "#FF3300", "#FF2F00", "#FF2B00", "#FF2700", "#FF2300",
            "#FF1F00", "#FF1B00", "#FF1700", "#FF1300", "#FF0F00", "#FF0B00",
            "#FF0700", "#FF0300", "#FF0000", "#FA0000", "#F50000", "#F00000",
            "#EB0000", "#E60000", "#E10000", "#DC0000", "#D70000", "#D20000",
            "#CD0000", "#C80000", "#C30000", "#BE0000", "#B90000", "#B40000",
            "#AF0000", "#AA0000", "#A50000", "#A00000", "#9B0000", "#960000",
            "#910000", "#8C0000", "#870000", "#820000", "#7D0000", "#780000",
            "#730000", "#6E0000", "#690000", "#000000")
  
  hexalpha <- as.hexmode(round(255 * alpha))
  if (nchar(hexalpha) == 1L) hexalpha <- paste(rep(hexalpha, 2L), collapse = "")
  cols <- paste0(cols, hexalpha)
  
  if (palette) return(list(breaks = breaks, cols = cols))
  if (missing(x)) return(colorRampPalette(cols))
  
  if (length(x) == 1L) {
    return(paste0(colorRampPalette(cols)(x), hexalpha))
  } else {
    return(cols[findInterval(x, breaks)])
  }
  
}

