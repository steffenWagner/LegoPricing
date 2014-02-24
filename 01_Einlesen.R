rm(list=ls(all = TRUE))
require(httr)
require(Hmisc)
require(rms)

query <- function(apiKey="", userHash="", query="",
                  theme="", subtheme="", setNumber="",
                  year="", Owned="", Wanted=""){
  
  cmd <- "search?"
  
  paste(cmd, 
        paste("apiKey=", apiKey, sep =""),
        paste("userHash=", userHash, sep = ""),
        paste("query=", query, sep = ""),
        paste("theme=", theme, sep = ""),
        paste("subtheme=", subtheme, sep = ""),
        paste("setNumber=", setNumber, ifelse(setNumber == "", "", "-1"), sep = ""),
        paste("year=", year, sep = ""),
        paste("Owned=", Owned, sep = ""),
        paste("Wanted=", Wanted, sep = ""), sep = "&")
  
}
GETquery <- function(...){
  url <- "http://brickset.com/webServices/brickset.asmx/"
  apiKey <- "yiSz-PevZ-udiD"
  userHash <- "_MF5xuZjrS"
  GET(paste(url, query(userHash=userHash, apiKey=apiKey, ...), sep =""))
}
TRANSFORM <- function(bricksetXML){
  colOfInterest <- c("setID", "number", "numberVariant",
                    "setName", "year", "minifigs",
                    "theme", "subtheme",
                    "UKRetailPrice", "USRetailPrice", "CARetailPrice")
  xmlDat <- xmlTreeParse(content(bricksetXML, as = "text"))
  extractData <- function(x){
    data <- lapply(xmlChildren(x), xmlValue)[colOfInterest]
    data[sapply(data, length)==0] <- NA
    do.call(data.frame, data)
  }
  datList <- lapply(xmlChildren(xmlDat$doc$children$ArrayOfSetData), extractData)
  dat <- do.call(rbind, datList)
  
  # Setzen der Klassen
  names(dat)
  dat$setID <- factor(dat$setID)
  dat$number <- as.numeric(dat$number)
  dat$numberVariant <- as.numeric(dat$numberVariant)
  dat$year <- as.numeric(dat$year)
  dat$minifigs <- as.numeric(dat$minifigs)
  dat$minifigs[is.na(dat$minifigs)] <- 0
  dat$USRetailPrice <- as.numeric(dat$USRetailPrice)
  dat$UKRetailPrice <- as.numeric(dat$UKRetailPrice)
  dat$CARetailPrice <- as.numeric(dat$CARetailPrice)

  return(dat)
}
# Read Years
years <- 2000:2014
XMLdata <- lapply(years, function(x) GETquery(year = x))
save(XMLdata, file = "XMLdata.Rdata", compress = TRUE)
dat <- do.call(rbind, lapply(XMLdata, TRANSFORM))
summary(dat)

sort(table(dat$theme), decreasing = TRUE)
themesOfInterest <- c("City", "Star Wars", "Friends", "Creator", "Teenage Mutant Ninja Turtles",
                      "Legends Of Chima", "Ninjago", "Harry Potter")
lm1 <- lm(USRetailPrice ~ number*theme + rcs(minifigs, 4), 
          data = dat, subset = theme %in% themesOfInterest)
summary(lm1)
anova(lm1)
termplot(lm1, partial.resid = FALSE, rug = TRUE, 
         ylim = "free", smooth = panel.smooth, las = 2)
