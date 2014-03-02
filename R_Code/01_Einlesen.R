rm(list=ls(all = TRUE))
require(rLego)
require(scrapeR)
require(RHTMLForms)

# brickset API setting
apiKey <- "yiSz-PevZ-udiD"

## Einlesen der Daten:
years <- 2000:2014
XMLdata <- lapply(years, function(x) searchBS(year = x, apiKey=apiKey))

## Aufbereiten der Daten
dat <- do.call(rbind, lapply(XMLdata, transformBS))
save(dat, file = paste("Data/SetsYears_", paste(range(years), collapse = "-"), ".Rdata", sep = ""), compress = TRUE)


## Einlesen der Feature Boxes
x <- GET("http://brickset.com/sets/7261-2/Clone-Turbo-Tank-%28light-up-2005-edition%29")
xy <- htmlTreeParse(content(x, as = "text"),
                   asText = TRUE )
xy <- as.character(xy$children$html)
regmatches(xy, regexpr("featurebox ", ))


