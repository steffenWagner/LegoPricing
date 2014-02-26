rm(list=ls(all = TRUE))
require(rLego)

# brickset API setting
apiKey <- "yiSz-PevZ-udiD"

## Einlesen der Daten:
years <- 2000:2014
XMLdata <- lapply(years, function(x) searchBS(year = x, apiKey=apiKey))

## Aufbereiten der Daten
datYear <- lapply(XMLdata[1:2], transformBS)
lapply(datYear, str)
dat <- do.call(rbind, datYear)
save(dat, file = paste("Data/SetsYears_", paste(range(years), collapse = "-"), ".Rdata", sep = ""), compress = TRUE)

