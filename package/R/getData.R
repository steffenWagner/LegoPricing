#' Brickset search GET request
#' @description Perform brickset search request via http GET
#' @param url brickset webservices url
#' @param apiKey brickset apiKey (s. details)
#' @param userHash brickset userHash (s. details)
#' @param query A search string.
#' @param theme Valid name of a theme.
#' @param substheme Valid name of a subtheme.
#' @param setNumber string or integer (s. details)
#' @param year string or integer
#' @param Owned Pass a '1' to get a list of set(s) the specified user owns.
#' @param Wanted Pass a '1' to get a list of set(s) the specified user wants.
#' @details If \code{setNumber} does not include a '\code{-#}' after the number (e.g. 70006-1), the suffix \code{-1} is added automatically
#' 
#' Without providing a valid \code{apiKey} a maximum of 20 results will be returned.
#' 
#' If \code{userHash} is provided, the returned data will contain flags indicating whether the specified user owns and/or wants the set.
#' @return brickset XML response set

searchBS <- function( url = "brickset.com//api/v2.asmx/", cmd,
                       apiKey = "", userHash = "", query = "",
                       theme = "", subtheme = "", setNumber = "",
                       year = "", Owned = "", Wanted = "",
                      orderBy = "", pageSize = "", pageNumber = "", userName = ""){
  
    nDash <- NROW(strsplit(setNumber, split = "-", fixed = TRUE)[[1]])
    if(nDash == 1){
      setNumber <- paste(setNumber, 1, sep = "-")
    }else if(nDash >1) warning("invalid setNumber format\n")
    
    searchQuery <- paste(paste0(cmd, "?"),
                         paste("query=", query, sep = ""),
                         paste("apiKey=", apiKey, sep =""),
                         paste("userHash=", userHash, sep = ""),
                         paste("theme=", theme, sep = ""),
                         paste("subtheme=", subtheme, sep = ""),
                         paste("setNumber=", setNumber, sep = ""),
                         paste("year=", year, sep = ""),
                         paste("Owned=", Owned, sep = ""),
                         paste("Wanted=", Wanted, sep = ""),
                         paste("orderBy=", orderBy, sep = ""), 
                         paste("pageSize=", pageSize, sep = ""), 
                         paste("pageNumber=", pageNumber, sep = ""), 
                         paste("userName=", userName, sep = ""), 
                         sep = "&")
    

  xmlRes <- GET(paste(url, searchQuery, sep =""))
  class(xmlRes) <- c("bsSearch", class(xmlRes))
  xmlRes
}


