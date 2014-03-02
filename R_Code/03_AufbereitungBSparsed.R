## Aufbereiten der Datensätze


# Einlesen der geparsten Brickset-Informationen
load(file = "Data/addInfo.Rdata")
makeDF <- function(x, allCol){
  allCol <- make.names(allCol)
  if(is.null(x)) return(NULL)
  x2 <- x[ ,2]
  names(x2) <- x[ ,1]
  safOld <- options()$stringsAsFactors
  options(stringsAsFactors= FALSE)
  x <- do.call(data.frame, as.list(x2))
  x[allCol[!allCol %in% names(x)]] <- rep(NA, times = sum(!allCol %in% names(x)))
  options(stringsAsFactors= safOld)
  return(x)
}
allCol <- unique(unlist(lapply(addInfo, function(x) x[ ,1])))
dat <- lapply(addInfo[-which(sapply(addInfo, is.null))], makeDF, allCol=allCol)
dat <- do.call(rbind, dat)

str(dat)
# Transformatinon der Spalten
x <- dat[1:10, ]

safOld <- options()$stringsAsFactors
options(stringsAsFactors = FALSE)
xNeu <- do.call(rbind, lapply(strsplit(x$Set.number, split="-"), 
                              function(x) data.frame(number = x[[1]], numberVar = x[[2]])))
xNeu$setName <- x$Name
xNeu$setType <- x$Set.type
xNeu$themeGroup <- x$Theme.group
xNeu$theme <- x$Theme
xNeu$year <- x$Year.released
xNeu$pieces <- x$Pieces
xNeu$packaging <- x$Packaging
xNeu$availability <- x$Availability
xNeu$notes <- x$Notes
getRating <- function(y){
  
  getRatingIntern <- function(y){
    
    if(y=="Not yet reviewed") {
      y <- c(NA, 0)
    } else {
      y <- unlist(strsplit(substring(text=y, first=6), split = "from"))
      y <- unlist(lapply(y, gsub, pattern = "[^0-9.]", replacement = ""))
    }
    names(y) <-  c("rating", "reviews")
    return(y)
  }
  do.call(rbind, lapply(y, getRatingIntern))
}

xNeu <- data.frame(xNeu, getRating(x$Rating))
xNeu$subtheme <- x$Subtheme
xNeu$minifigs <- x$Minifigs

allCurrency <- function(x){
  x <- gsub(" ", "", x)
  x <- lapply(x, function(x) strsplit(x, split = "/"))
  x <- unique(unlist(lapply(x, function(x) lapply(x, function(x) gsub("[!^0-9.]", "", x)))))
  x[x != "" & !is.na(x)]
}
allCurrencies <- allCurrency(dat$RRP)


extractPrice <- function(x, allCurrencies){
  getRRP <- function(x){
    y <- gsub("[^0-9.]", "", x)
    names(y) <- gsub("[!^0-9.]", "", x)
    y
  }
  
  x <- lapply(x, function(x){
    if(is.na(x)) {
      y <- rep(NA, NROW(allCurrencies))
      names(y) <- allCurrencies
    }else {
      y <- unlist(lapply(strsplit(gsub(" ", "", x), split = "/"), getRRP))
      if(any(NAs <- !allCurrencies %in% names(y))){
        y <- c(y, rep(NA, sum(NAs)))
        names(y) <- c(names(y)[names(y)!=""], allCurrencies[NAs])
      }
    }
    return(y)
  }
  )
  x <- do.call(rbind, x)
  colnames(x) <- gsub("£", "BP", colnames(x), fixed= TRUE)
  colnames(x) <- gsub("\u20AC", "EUR", colnames(x), fixed= FALSE)
  colnames(x) <- gsub("US$", "US", colnames(x), fixed= TRUE)
  colnames(x) <- paste("Price", colnames(x), sep ="")
  x
}

xNeu <- data.frame(xNeu, extractPrice(x$RRP, allCurrencies=allCurrencies))
dat$Weight
dat$Dimensions
names(x)
