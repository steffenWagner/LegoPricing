rm(list=ls(all = TRUE))
require(rLego)

# brickset API setting
apiKey <- "yiSz-PevZ-udiD"

## Einlesen der Daten:
years <- 2000:2014
XMLdata <- lapply(years, function(x) searchBS(year = x, apiKey=apiKey))

## Aufbereiten der Daten
dat <- do.call(rbind, lapply(XMLdata, transformBS))
dput(dat, file = paste("Data/SetsYears_", paste(range(years), collapse = "-"), ".R", sep = ""))

## Filtern auf relevante Themenwelten und Jahre
dat <- dat[as.numeric(dat$year) >= 2010, ]

# Entferne bestimmte sub-Themes (Black-List)
dat$subthemeFirst <- unlist(lapply(strsplit(dat$subtheme, split = "/"), function(x) x[[1]]))
names(sort(table(dat$subthemeFirst), decreasing= TRUE)[1:20])
skipSubTHeme <- c("Key Chains", "Video Games", "Product Collection", "Magnets", "Minifig Pack",
                  "Books", "Watches", "LEGO Store Grand Opening Set","Monthly Mini Model Build",
                  "Product collection", 
                  "Promotional", "Virtual Product Collection", "Storage",
                  "Housewares", "Spinners", "Christmas", "Miscellaneous",
                  "Seasonal")
dat <- dat[!dat$subthemeFirst %in% skipSubTHeme, ]
dat <- dat[!grepl("^Animals series", dat$subthemeFirst), ]
sort(names(table(dat$subthemeFirst)))

# Beschränkung auf bestimmte Themes (WhiteList)
useTheme <- c("Atlantis", "Castle", "City", "Creator", "DC Comics Super Heroes", "Dino", 
              "Friends", "Harry Potter", "Legends Of Chima", 
              "Lord of the Rings", "Marvel Super Heroes", "Ninjago", "Pirates of the Caribbean",
              "Star Wars", "Teenage Mutant Ninja Turtles", "The LEGO Movie", "Toy Story")
dat <- dat[tolower(dat$theme) %in% tolower(useTheme), ]

# Entferne Sets, für die nicht in allen drei Währungen Infos vorliegen
filterNaPrice <- apply(sapply(grep("RetailPrice$", names(dat)), function(x) is.na(dat[x])), 1, any)
dat <- dat[!filterNaPrice, ]


# Betrachtung der relevanten Sets im jpeg format
dat <- dat[order(dat$theme, dat$number), ]

library(jpeg)
library(RCurl)

plotSet <- function (number, dat = dat) {
  dat <- dat[dat$number == number, ]
  plot(1, xlim=c(0,100), ylim = c(0,100), 
       type = "n", frame = FALSE, axes = FALSE, xlab = "", ylab ="",
       sub = paste(dat$theme, dat$subtheme, "\npieces:", dat$pieces),
       main = paste(dat$number, dat$setName))
  if(!is.na(dat$imageURL)){
  img <- readJPEG(getBinaryURL(dat$imageURL), native=TRUE)
  rasterImage(img, 0 , 0, 100, 100)
  } else {
    text(50, 50, "no image available")
  }
}
pdf("Data/SetsUsed.pdf", height = 5, width = 7)
invisible(lapply(dat$number, plotSet, dat = dat))
dev.off()


# Entferne weitere Sets
skipSubTHeme <- unique(c(skipSubTHeme, "Planet Set", "Speedorz", 
                         "MicroFighters", "Trains", "Booster pack", "Mini Building Set"))
dat <- dat[!dat$subthemeFirst %in% skipSubTHeme, ]
dat <- dat[-grep("name sign", dat$setName, ignore.case=TRUE), ]
dat <- dat[-grep("Accessory Set", dat$setName, ignore.case=TRUE), ]
dat <- dat[-grep("Battle Pack", dat$setName, ignore.case=TRUE), ]
dat <- dat[-grep("troopers", dat$setName, ignore.case=TRUE), ]



## Einlesen der Feature Boxes
featureBoxXML <- function (x) {
  x <- htmlParse(x, asText = TRUE )
  xpathSApply(x, "//section[@class='featurebox ']", xmlValue)[[1]]
}

featureBoxRaw <- lapply(dat$bricksetURL, function(x) readLines(x))
featureBoxList <- lapply(featureBoxRaw, featureBoxXML)
featureBoxList <- lapply(featureBoxList, function(set) lapply(strsplit(set, split = "\n"), 
                                                              function(x) {x <- x[-1]
                                                                           mat <- matrix(x, ncol = 2, byrow = TRUE)
                                                                           x <- mat[ ,2]
                                                                           names(x) <- mat[ ,1]
                                                                           x})[[1]])
uniqueNames <- unique(unlist(lapply(featureBoxList, names)))
featureBoxList <- lapply(featureBoxList, function(x) {missingNames <- uniqueNames[!uniqueNames %in% names(x)]
                                                      if(length(missingNames)){
                                                        xAdd <- rep(NA, length(missingNames))
                                                        names(xAdd) <- missingNames
                                                        x <- c(x, xAdd)
                                                      }
                                                      return(x)
                                                      })
dat2 <- do.call(rbind, featureBoxList)



