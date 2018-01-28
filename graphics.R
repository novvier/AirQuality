# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::: Graphics functions ::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

GraphAir <- function(data, pollutant = "pm10",
	parameter = "parameter",  clasf = "station", value = "value",
	group = NULL, label = NULL, ECA = "current", prd = "Max", INCA = TRUE,
  lab.null = FALSE, Print = NULL, font = TRUE, ...) {
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
  # test packages
  pkgTest <- function(x) {
    if (!require(x,character.only = TRUE)) {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE))
          stop("Package not found")
    }
  }
  #
  if(class(data) != "data.frame"){
    cat("Data is not Data Frame \n")
    return()
  }
  #
  pkgTest("tidyverse")
  #
  # Select variables
  datair <- data[, c(parameter, value, clasf)]
  names(datair) <- c("parameter", "value", "clasf")
  datair <- datair %>% filter(parameter == pollutant)
  if (nrow(datair) == 0) {
    cat(paste0("The parameter '", pollutant, "' does't exist \n"))
    return()
  }
  if (!is.null(label))
  	datair$label <- data[which(data[, parameter] == pollutant), label]
  if (!is.null(group))
  	datair$group <- data[which(data[, parameter] == pollutant), group]
  #
  # Load libraries
  # 
  # Select ECA
  ECAs <- c("benceno", "co", "h2s", "Hg", "HT", "no2", "o3", "Pb", "pm10",
             "pm25", "so2", "nox")
  if (is.numeric(ECA)) {
    period.t = ""
    eca.v = ECA
    ECA.s = "Manual"
  } else if (pollutant %in% ECAs) {
  	if(!file.exists("./ECA.RData")){
  		print("Download ECA data")
  		download.file(paste0("https://raw.githubusercontent.com/",
  												 "novvier/AirQuality/master/ECA.RData"),
  									"./ECA.RData", method = "auto")
  	}
  	load("./ECA.RData")
  	if (pollutant == "nox") {
  		print("nox será comparado con el ECA para no2")
  		s.pollutant <- ECA.air %>% filter(parameter == "no2")
  	} else {
  		s.pollutant <- ECA.air %>% filter(parameter == pollutant)
  	}
  	#
    if (ECA == "current")
  	  ECA = "003-2017"
  	# legal f
  	s.pollutant[, "legal.f"] <- grepl(ECA, s.pollutant[, "legal"])
  	s.legal <- s.pollutant %>% filter(legal.f == TRUE)
  	if (nrow(s.legal) == 0) {
  		print(paste0("'", pollutant, "' no presenta ECA para el DS ",
  								 ECA))
  		period.t = ""
  		eca.v = 0
  		ECA.s = ""
  	} else {
  		if (prd == "Min") {
  	  	s.period = s.legal %>% filter(value == min(value))
  		} else if (prd == "Max") {
    		s.period = s.legal %>% filter(value == max(value))
  		} else {
  			s.period = s.legal %>% filter(period == prd)
  			if (nrow(s.period) == 0) {
  				print(paste0("'", pollutant, "' no presenta ECA para ", prd))
  				period.t = ""
  				eca.v = 0
  				ECA.s = ""
  			}
  		}
  		period.t = s.period[, "period"]
    	eca.v = s.period[, "value"]
    	ECA.s = s.period[, "legal"]
  	}
  } else {
  	print(paste0(" '",pollutant,"' no presenta ECA "))
  	period.t = ""
  	eca.v = 0
  	ECA.s = ""
  }
  
  # load INCA
  if (INCA){
   	if (eca.v != 0) {
  		INCAs <- c("co", "h2s", "no2", "o3", "pm10", "pm25", "so2", "nox")
  		# select INCA
  		if (pollutant %in% INCAs) {
  			if (pollutant == "nox") {
  				VUEC %>% filter(parameter == "no2" & period == period.t) -> VUECt
  			} else {
  				VUEC %>%
  					filter(parameter == pollutant & period == period.t) -> VUECt
  			}
      	if (nrow(VUECt) != 0) {
		     	VUECe <- VUECt$value
  				VUECn <- 100 * VUECe / eca.v
  				datair$aqi <- 100 * datair$value / eca.v
  				datair$inca <- cut(datair$aqi,
  						breaks = c(-Inf, 50, 100, VUECn, Inf),
							labels = c("Buena", "Moderada", "Mala", "VUEC"))
  	    } else {
    	  	print(paste0(" '", pollutant,"' no presenta INCA PARA ", period.t," "))
  				VUECe = 0
      	}
  		} else {
  			print(paste0(" '",pollutant,"' no presenta INCA "))
  			VUECe = 0
  		}
   	} else {
   		VUECe = 0
   	}
  } else {
  	VUECe = 0
  }
  #
  # Graphic parameters 
  maxy <- max(datair$value, na.rm = TRUE)
  if (eca.v < maxy) {
    limy = maxy * 1.1
  } else limy = eca.v * 1.1
  #
  # Graphic elements
  ylab.text <- expression(paste(mu, g/m^3, sep = ""))
  if (pollutant %in% ECAs) {
  	if (pollutant == "nox") {
   		main.text = expression("NO"[x])
  	} else {
  		main.text <- param.expres[which(param.text == pollutant)]
  	}
  } else {
  	main.text = pollutant
  }
	#  	
  if (eca.v != 0) {
  	if (pollutant == "nox") {
  		lg.eca = expression("ECA NO"[2])
  	} else {
  		lg.eca = "ECA"
  	}
  	if (VUECe != 0) {
  		if (pollutant == "nox") {
  			lg.inca = expression("INCA NO"[2])
  		} else {
  			lg.inca = "INCA"
  		}
  	}
  }
  #
  # Add graphic
  graph <- ggplot(data = datair, aes(x = clasf, y = value)) +
  	coord_cartesian(ylim = c(0, limy)) +
  	labs(title = main.text) +
  	xlab(NULL) +
    ylab(ylab.text) +
  	theme_bw()
  if (!is.null(group)) {
  	graph <- graph + facet_wrap(~group, ...)
  }
  
  if (eca.v != 0) {
  	graph <- graph + geom_hline(aes(linetype = period.t, yintercept = eca.v),
  															col = "red") +
  		scale_linetype_manual(values = c(2)) +
  		labs(linetype = lg.eca)
  	if (VUECe != 0) {
  		graph <- graph + geom_bar(aes(fill = inca), stat = "identity", 
  														width = 0.5) +
  			scale_fill_manual(values = c("Buena" = "#6CCB6C",
  		  	"Moderada" = "#FFFF4F", "Mala" = "#FFA154", "VUEC" = "#FF6866")) +
  			labs(fill = lg.inca) + 
  			guides(linetype = guide_legend(order=1),
               fil = guide_legend(order=2))
  	} else {
  		graph <- graph + geom_bar(stat = "identity", width = 0.5)
  	}
  } else {
  	graph <- graph + geom_bar(stat = "identity", width = 0.5)
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
    if (font) {
      pkgTest("extrafont")
      lf <- fonts()
      if (!"Arial Narrow" %in% lf){
        font_import(pattern = "ARIALN", prompt = FALSE)
      }
      loadfonts(device = "win", quiet = TRUE)
      jpeg(nameg, width = width.px, height = height.px, res = res,
           family = "Arial Narrow") 
    } else {
      jpeg(nameg, width = width.px, height = height.px, res = res) 
    }
    print(graph)
    dev.off()
  }
  #
  return(graph)
}

MultiGraph <- function(x, pollutants = "parameter", ...) {
  # Create multi graphics
	#
	# Args:
	#   x: data in data.frame
	#   pollutants: name column that contain the names pollutants
	#
	# Return:
	#   list with all graphics
	#
	y <- levels(as.factor(x[, pollutants]))
  yl <- length(y)
  graphs <- list()
	for (i in 1:yl)
    graphs[[i]] <- GraphAir(data = x, pollutant = y[i], ...)
  #
  return(graphs)
}