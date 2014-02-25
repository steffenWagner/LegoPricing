valueClasses <- data.frame(value = c("setID", "number", "numberVariant", "setName", 
                                     "year", "theme", "subtheme", 
                                     "pieces", "minifigs", "image", 
                                     "imageFilename", "thumbnailURL", 
                                     "imageURL", "bricksetURL", 
                                     "own", "want", "qtyOwned", 
                                     "userNotes", "UKRetailPrice", 
                                     "USRetailPrice", "CARetailPrice", 
                                     "instructionsAvailable", "EAN", "UPC", 
                                     "lastUpdated"),
                           class = c("factor", "factor", "factor", "factor", 
                                     "integer", "factor", "factor", 
                                     "integer", "integer", "character", 
                                     "character", "character", "character", 
                                     "character", "logical", "logical", 
                                     "integer", "character", "numeric", 
                                     "numeric", "numeric", "logical", 
                                     "character", "character", "character"))

logicalValues <- data.frame(R = c(FALSE, TRUE),
                            BS = c("false", "true"))

#' Generic function to transform brickset-XML respsonses into data.frames
transformBS <- function(x, ...) UseMethod("transformBS")

#' transform brickset Search XML response into data.frame
transformBS.bsSearch <- function(bsSearchObj){
  
  # Create XMLDocument
  xmlDat <- xmlTreeParse(content(bsSearchObj, as = "text"), getDTD = FALSE)
  # Extract result Sets
  listOfSetData <- xmlChildren(xmlDat$children$ArrayOfSetData)
  # Extract values
  resultList <- lapply(listOfSetData,
                       function(y) do.call(data.frame, lapply(xmlChildren(y), 
                             function(x) ifelse(length(value <- xmlValue(x)), value, NA))))
  # Control for different amount of Values
  namesUnique <- unique(unlist(lapply(resultList, names)))
  resultList <- lapply(resultList, function(x) {if(any(missingName <- !namesUnique %in% names(x))){
    x[namesUnique[missingName]] <- NA
    return(x)
  } else { return(x) }
  }
  )
  
  # Combine to data.frame
  result <- do.call(rbind, resultList)
  # Order Colums
  result <- result[ , valueClasses$value[valueClasses$value %in% names(result)]]
    
  return(setClasses(result))
}

#' Set Classes According to valueClasses
setClasses <- function(x){
  
  colNames <- names(x)
  # check if all required information is available
  if(any(noClass <- !colNames %in% valueClasses$value)) warning("No class information available for: ", colNames[which(noClass)], "\n")
  
  for(col in colNames[!noClass]){
    classToSet <- valueClasses$class[match(col, valueClasses$value)]
    if(classToSet == "logical"){
      x[ , col] <- as.character(x[ , col])
      x[ , col] <- ifelse(x[ , col] == logicalValues$BS[which(logicalValues$R)], TRUE,
                          ifelse(x[ , col] == logicalValues$BS[which(!logicalValues$R)], FALSE, x[ , col]))
      class(x[ , col]) <- as.character(classToSet)
    } else if(classToSet == "factor"){
      x[ , col] <- factor(x[ , col]) 
    } else {
     class(x[ , col]) <- as.character(valueClasses$class[match(col, valueClasses$value)])
    }
  }
  return(x)
}

  
  


