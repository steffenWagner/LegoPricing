# Beispiel für API ZUgriff via R
require(httr)
require(XML)

# Funktion zum Einlesen der Daten von Brickset
searchBS <- function( url = "http://brickset.com/webServices/brickset.asmx/",
                      apiKey = "", userHash = "", query = "",
                      theme = "", subtheme = "", setNumber = "",
                      year = "", Owned = "", Wanted = ""){
  
  cmd <- "search?"
  
  nDash <- NROW(strsplit(setNumber, split = "-", fixed = TRUE)[[1]])
  if(nDash == 1){
    setNumber <- paste(setNumber, 1, sep = "-")
  }else if(nDash >1) warning("invalid setNumber format\n")
  
  searchQuery <- paste(cmd, 
                       paste("apiKey=", apiKey, sep =""),
                       paste("userHash=", userHash, sep = ""),
                       paste("query=", query, sep = ""),
                       paste("theme=", theme, sep = ""),
                       paste("subtheme=", subtheme, sep = ""),
                       paste("setNumber=", setNumber, sep = ""),
                       paste("year=", year, sep = ""),
                       paste("Owned=", Owned, sep = ""),
                       paste("Wanted=", Wanted, sep = ""), sep = "&")
  
  
  xmlRes <- GET(paste(url, searchQuery, sep =""))
  class(xmlRes) <- c("bsSearch", class(xmlRes))
  xmlRes
}

# Beispiel: Cragger's Command Ship
bsRes <- searchBS(setNumber="70006")
content(bsRes)


