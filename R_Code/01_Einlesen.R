rm(list=ls(all = TRUE))
require(rLego)

# brickset API setting
apiKey <- "yiSz-PevZ-udiD"

## Einlesen der Daten:
years <- 2000:2014
XMLdata <- lapply(years, function(x) searchBS(year = x, apiKey=apiKey))

## Aufbereiten der Daten
datYear <- lapply(XMLdata, transformBS)

sapply(datYear, function(x) sapply(x, class))
apply(sapply(datYear, function(x) sapply(x, class)), 1, unique)

dat <- do.call(rbind, datYear)

debugonce(setClasses)
transformBS(XMLdata[[4]])


save(dat, file = paste("Data/SetsYears_", paste(range(years), collapse = "-"), ".Rdata", sep = ""), compress = TRUE)

rLego::valueClasses
rLego::logicalValues


x <-1:10
class(x)
x
class(x) <- "logical"
x


sort(table(dat$theme), decreasing = TRUE)
themesOfInterest <- c("City", "Star Wars", "Friends", "Creator", "Teenage Mutant Ninja Turtles",
                      "Legends Of Chima", "Ninjago", "Harry Potter")
lm1 <- lm(USRetailPrice ~ number*theme + rcs(minifigs, 4), 
          data = dat, subset = theme %in% themesOfInterest)
summary(lm1)
anova(lm1)
termplot(lm1, partial.resid = FALSE, rug = TRUE, 
         ylim = "free", smooth = panel.smooth, las = 2)
