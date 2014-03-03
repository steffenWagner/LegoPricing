require(XML)
require(RCurl)
require(httr)


getAddSetInfo <- function(baseURL = "http://brickset.com/sets/", number, numberVar, setName){
  
  url <- paste(baseURL, paste(number, numberVar, sep = "-"), sep = "")
  
  doc.html = htmlTreeParse(url, useInternal = TRUE)
  
  # Extract all the paragraphs (HTML tag is p, starting at
  # the root of the document). Unlist flattens the list to
  # create a character vector.
  # //section[@class='featurebox']//dd
  doc.text <- cbind(unlist(xpathApply(doc.html, '//dt', xmlValue)),
                    unlist(xpathApply(doc.html, '//dd', xmlValue)))
  doc.text
}

addInfo <- mapply(function(number, numberVar) getAddSetInfo(number = number, numberVar=numberVar),
                  dat$number, dat$numberVariant)

save(addInfo, file = "Data/addInfo.Rdata", compress = TRUE)


###
allCol <- unique(unlist(lapply(addInfo, function(x) x[ ,1])))

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
dat2 <- lapply(addInfo[-which(sapply(addInfo, is.null))], makeDF, allCol=allCol)
dat2 <- do.call(rbind, dat2)
vapply(dat3, function(x) mean(!is.na(x)), numeric(1))
table(dat3$Set.type)



## Einlesen der rebrickable Daten
readLines("Data/set_pieces.csv", n = 10)
dat3 <- read.table("Data/set_pieces.csv", header = TRUE, sep = ",", 
                   colClasses=c("factor", "factor", "integer", "factor", "factor"))
summary(dat3)


dat4 <- read.csv("Data/pieces.csv")
summary(dat4)





