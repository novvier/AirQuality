# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::: Graphics functions ::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

GraphAir <- function(data, pollutant = "pm10",
	parameter = "parameter",  clasf = "station", value = "value",
	group = NULL, label = NULL, ECA = "current", prd = "Max", 
  lab.null = FALSE, Print = NULL, ...) {
  # Graph concentrations compared to standard
  #
  # Args:
  #   data: (data.frame) 
	#		pollutant: pollutant (character)
  #   parameter: name column with the pollutant name (character)
	#   clasf: name colum for clasification (character)
	#   value: name colum whit values (character)
	#   group: value for group (character)
	#   label: name column whit labels or NULL for doesn't graph
  #   ECA: regulation number or ECA value (character or numeric)
  #   prd: monitoring period:
	#			"24 horas", "8, horas", "anual", "Max" or "Min
  #   lab.null = doesn't graph labels:
	#			TRUE (not print labels) or FALSE(print labels)
  #   Print: print values in jpg (height & whith) in cm
  #     or NULL for doesn't print graph
	  #
  # Return:
  #   Graphics in window or print in jpg
  #
  if(class(data) != "data.frame"){
    cat("Data is not Data Frame \n")
    return()
  }
  #
  # Select variables
  x <- data[which(data[, parameter] == pollutant), clasf]
  y <- data[which(data[, parameter] == pollutant), value]
  if (!is.null(label))
    l <- data[which(data[, parameter] == pollutant), label]
  if (!is.null(group)) 
    g = data[which(data[, parameter] == pollutant), group]
  if (length(x) == 0) {
    cat(paste("The parameter", pollutant, "does't exist \n"))
    return()
  }
  #
  # Load libraries
  library(dplyr)
  library(lubridate)
  # 
  # Select ECA
  if (is.numeric(ECA)) {
    period.t = ""
    eca.v = ECA
  } else {
  	if(!file.exists("./ECA.RData")){
  		print("Download ECA data \n")
  		download.file(paste0("https://raw.githubusercontent.com/",
  												 "novvier/AirQuality/master/ECA.RData"),
  									"./ECA.RData", method = "auto")
  	}
  	load("./ECA.RData")
  	s.pollutant <- ECA.air %>% filter(parameter == pollutant)
  	#
    if (ECA == "current")
  	  ECA = "003-2017"
  	#
  	s.pollutant[, "legal.f"] <- grepl(ECA, s.pollutant[, "legal"])
  	s.legal <- s.pollutant %>% filter(legal.f == TRUE)
  	#
  	if (prd == "Min") {
  	  s.period = s.legal %>% filter(value == min(value))
  	} else if (prd == "Max") {
    	s.period = s.legal %>% filter(value == max(value))
  	} else {
  		s.period = s.legal %>% filter(period == prd)
  	}
  	#
  	period.t = s.period[, "period"]
  	eca.v = s.period[, "value"]
  }
  
  #
  if (length(eca.v) == 0) {
    print(paste(pollutant, "no presenta ECA para", prd))
    eca.v = 0
  }
  # 
  # Graphic parameters 
  maxy <- max(y, na.rm = TRUE)
  if (eca.v < maxy) {
    limy = maxy * 1.1
  } else limy = eca.v * 1.1
  limc = limy / 100
  #
  # Margin Setup
  library(lattice)
  #
  if (lab.null) {
    lat.op <- list(
      layout.heights = list(bottom.padding = list(x = -1),
                           top.padding = list(x = -1.5)),
      layout.widths = list(left.padding=list(x = -0.9),
                           right.padding=list(x = -1.3))
    )
    lattice.options(lat.op)
    ylab.text = NULL
    main.text = NULL
  } else {
    ylab.text <- expression(paste(mu, g/m^3, sep = ""))
    main.text <- param.expres[which(param.text == pollutant)]
    if (length(main.text) == 0) 
    	main.text = pollutant
  }
  #
  if (is.null(group)) {
    graph <- barchart(y ~ x, ylim = seq(0, limy, limc),
      ylab = ylab.text,
      main = main.text,
      panel = function(x, y, ...) {
        panel.barchart(x, y, col = "gray90", ...)
        panel.grid(h = -1, v = 0, lty = 3)
        if(!is.null(label))
        	panel.text(x, y, l, pos=3, offset = 0.2, cex = 0.75)
        if (eca.v > 0) {
          panel.abline(h = eca.v, lty=2, col="red")
          panel.text(length(x), eca.v, paste("ECA",period.t),
            col="red", pos=3, offset=0.2, cex=0.75)
        }
      }, ...)
  } else {
    graph <- barchart(y ~ x | g, ylim = seq(0, limy, limc),
      ylab = ylab.text,
      main = main.text,
      as.table = TRUE,
      key=list(space = "bottom",
         lines=list(col = "red", lty = 2, lwd = 1),
         text=list(paste("ECA", period.t), col = "red", cex = 0.75)),
      panel = function(x, y, ...){
        panel.barchart(x, y, col="gray90", ...)
        panel.grid(h = -1,v = 0, lty = 3)
        if (eca.v > 0) {
          panel.abline(h = eca.v, lty = 2, col = "red")
        }
      }, ...)
  }
  #
  # JPG print
  if (!is.null(Print)) {
    res = 150
    width.px = round(Print[1] * res / 2.54)
    height.px = round(Print[2] * res / 2.54)
    #
    if(Encoding(pollutant) == "UTF-8")
      pollutant = iconv(pollutant, "UTF-8", "windows-1252")
    if(!dir.exists("./graphair"))
      dir.create("./graphair")
    #
    nameg <- paste0("graphair/",pollutant,".jpg")
    jpeg(nameg, width = width.px, height = height.px, res = res) 
    print(graph)
    dev.off()
  }
  #
  lat.op.def <- list(
    layout.heights=list(bottom.padding=list(x=0.5),
                        top.padding=list(x=0.5)),
    layout.widths=list(left.padding=list(x=0.5),
                       right.padding=list(x=0.5)))
  lattice.options(lat.op.def)
  #
  return(graph)
}

MultiGraph <- function(x, pollutants = "parameter", ECA = "current",
											 prd = "Max") {
  # Create multi graphics
  y <- x[, pollutants]
  y <- as.factor(y)
  y <- levels(y)
  yl <- length(y)
	for (i in 1:yl)
    GraphAir(data = x, pollutant = y[i], ECA = ECA, prd = prd)
}