require(XML)
require(RCurl)
require(httr)

html <- content(GET("http://brickset.com/sets/60000-1/Fire-Motorcycle"))
dom <- html$children$html
names(dom)
lapply(xmlChildren(asXMLNode(dom[[3]])), xmlChildren)

xmlChildren(asXMLNode(xmlChildren(asXMLNode(dom[[3]]))$div))$div


# sink("testHTML.txt", append = FALSE)
as.character(dom[[3]])[[3]]
sink()


  regexpr("<section class='featurebox '>(.*)", as.character(dom[[3]])[[3]])


library(XML)

# Read and parse HTML file
doc.html = htmlTreeParse('http://brickset.com/sets/60000-1/Fire-Motorcycle',
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