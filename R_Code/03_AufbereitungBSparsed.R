## Aufbereiten der Datensätze
rm(list=ls(all=TRUE))
require(Hmisc)

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

# Transformatinon der Spalten
transformFeatureBox <- function(x){
  safOld <- options()$stringsAsFactors
  options(stringsAsFactors = FALSE)
  xNeu <- do.call(rbind, lapply(strsplit(x$Set.number, split="-"), 
                                function(x) data.frame(number = x[[1]], numberVar = x[[2]])))
  xNeu$setName <- x$Name
  xNeu$setType <- x$Set.type
  xNeu$themeGroup <- x$Theme.group
  xNeu$theme <- x$Theme
  xNeu$year <- as.integer(x$Year.released)
  xNeu$pieces <- as.numeric(as.character(x$Pieces))
  xNeu$packaging <- x$Packaging
  xNeu$availability <- x$Availability
  xNeu$notes <- x$Notes
  getRating <- function(y){
    
    getRatingIntern <- function(y){
      
      if(is.na(y) || y=="Not yet reviewed") {
        y <- c(NA, 0)
      } else {
        y <- unlist(strsplit(substring(text=y, first=6), split = "from"))
        y <- as.numeric(unlist(lapply(y, gsub, pattern = "[^0-9.]", replacement = "")))
      }
      names(y) <-  c("rating", "reviews")
      return(y)
    }
    do.call(rbind, lapply(y, getRatingIntern))
  }
  
  xNeu <- data.frame(xNeu, getRating(x$Rating))
  xNeu$subtheme <- x$Subtheme
  xNeu$minifigs <- as.numeric(as.character(x$Minifigs))
  
  allCurrency <- function(x){
    x <- gsub(" ", "", x)
    x <- lapply(x, function(x) strsplit(x, split = "/"))
    x <- unique(unlist(lapply(x, function(x) lapply(x, function(x) gsub("[!^0-9.]", "", x)))))
    x[x != "" & !is.na(x)]
  }
  allCurrencies <- allCurrency(x$RRP)
  
  
  extractPrice <- function(x, allCurrencies){
    
    getRRP <- function(x){
      y <- gsub("[^0-9.]", "", x)
      names(y) <- gsub("[!^0-9.]", "", gsub(" ", "", x))
      y
    }
    
    x <- lapply(x, function(x){
      
      if(is.na(x)) {
        y <- rep(NA, NROW(allCurrencies))
        names(y) <- allCurrencies
      }else {
        y <- unlist(lapply(strsplit(gsub(" ", "", x), split = "/"), getRRP))
        y <- y[y!=""]
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
    apply(x, 2, as.numeric)
  }
  
  xNeu <- data.frame(xNeu, extractPrice(x$RRP, allCurrencies=allCurrencies))
  
  getDim <- function(x){
    
    do.call(rbind, lapply(x, function(x){
      if(is.na(x) | x == "") return(c(vol_cm3 = as.numeric(NA), vol_in3 = as.numeric(NA)))
      
      x <- strsplit(strsplit(gsub(" ", "", x) ,split="(", fixed = TRUE)[[1]], split = "x")
      y <- lapply(x, function(x) gsub("[^0-9.]", "", x))
      unit <- lapply(x, function(x) gsub("[!^0-9).]", "", x))
      unit <- lapply(unit, function(x) x[nchar(x)>0])
      vol <- unlist(lapply(y, function(x) prod(as.numeric(x))))
      names(vol) <- paste("vol_", unlist(unit), "3", sep ="")
      as.numeric(vol)
    }))
  }
  xNeu <- data.frame(xNeu, getDim(x$Dimensions))
  
  getWeight <- function(x){
    
    do.call(rbind, lapply(x, function(x){
      if(is.na(x) | x == "") return(c(Weight_Kg = as.numeric(NA), Weight_lb = as.numeric(NA)))
      x <- strsplit(gsub(" ", "", x), split = "(", fixed = TRUE)
      y <- unlist(lapply(x, function(x) gsub("[^0-9.]" , "", x)))
      units <- unlist(lapply(x, function(x) gsub("[!^0-9).]" , "", x)))
      names(y) <- paste("Weight_", units, sep ="")
      as.numeric(y)}))
  }
  
  xNeu <- data.frame(xNeu, getWeight(x$Weight))
  options(stringsAsFactors=safOld)
  return(xNeu)
}


datNeu <- transformFeatureBox(dat[!is.na(dat$Set.number) & dat$Set.number!="",])


## Analyse
summary(datNeu)
names(datNeu)
themesOfInterest <- c("City", "Star Wars", "Friends", "Creator", "Teenage Mutant Ninja Turtles",
                      "Legends Of Chima", "Ninjago", "Harry Potter")
lmResPr <- lm(vol_cm3 ~ ns(PriceUS, 6) + factor(theme), data = datNeu, subset = year >= 2013 & theme %in% themesOfInterest)
summary(lmResPr)
anova(lmResPr)
termplot(lmResPr, rug = TRUE, partial.resid=TRUE, se = TRUE)

lmResPc <- lm(pieces ~ ns(PriceUS, 6) + factor(theme), data = datNeu, subset = year >= 2013 & theme %in% themesOfInterest)
summary(lmResPc)
anova(lmResPc)
termplot(lmResPc, rug = TRUE, partial.resid=TRUE, se = TRUE)

lmResW <- lm(I(Weight_Kg/pieces) ~ ns(PriceUS, 6) + factor(theme), data = datNeu, subset = year >= 2013 & theme %in% themesOfInterest)
summary(lmResW)
anova(lmResW)
termplot(lmResW, rug = TRUE, partial.resid=TRUE, se = TRUE)
