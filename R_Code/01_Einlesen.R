rm(list=ls(all = TRUE))
require(rLego)

# brickset API setting
apiKey <- "yiSz-PevZ-udiD"

## Einlesen der Daten:
years <- 2000:2014
XMLdata <- lapply(years, function(x) searchBS(year = x, apiKey=apiKey))

## Aufbereiten der Daten
datYear <- lapply(XMLdata, transformBS)
dat <- setClasses(do.call(rbind, datYear))
dat$minifigs[is.na(dat$minifigs)] <- 0
save(dat, file = paste("Data/SetsYears_", paste(range(years), collapse = "-"), ".Rdata", sep = ""), compress = TRUE)

