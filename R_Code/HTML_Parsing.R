require(XML)
require(RCurl)
require(httr)


getAddSetInfo <- function(baseURL = "http://brickset.com/sets/", number, numberVar, setName){
  
  url <- paste(baseURL, paste(number, numberVar, sep = "-"), sep = "")
  
  doc.html = htmlTreeParse(url, useInternal = TRUE)
  
  # Extract all the paragraphs (HTML tag is p, starting at
  # the root of the document). Unlist flattens the list to
  # create a character vector.
  doc.text <- cbind(unlist(xpathApply(doc.html, '//dt', xmlValue)),
                    unlist(xpathApply(doc.html, '//dd', xmlValue)))
  doc.text
}

addInfo <- mapply(function(number, numberVar) getAddSetInfo(number = number, numberVar=numberVar),
                  dat$number, dat$numberVariant)



dat[dat$number == 60000, c("number", "numberVariant", "setName")]
names(dat)
urlAddSetInfoBS(number = 6000, numberVar=1, setName = "Fire Motorcycle")

# Read and parse HTML file
doc.html = htmlTreeParse(,
                         useInternal = TRUE)

# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.
doc.text <- cbind(unlist(xpathApply(doc.html, '//dt', xmlValue)),
                  unlist(xpathApply(doc.html, '//dd', xmlValue)))
doc.text


# Replace all \n by spaces
doc.text = gsub('\\n', ' ', doc.text)

# Join all the elements of the character vector into a single
# character string, separated by spaces
doc.text = paste(doc.text, collapse = ' ')