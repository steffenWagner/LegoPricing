rm(list=ls(all = TRUE))
require(rLego)

# brickset API setting
apiKey <- "yiSz-PevZ-udiD"

## Einlesen der Daten:
years <- 2010:2014
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
## Einlesen der Feature Boxes
extractFeatureBox <- function (x) {
  names <- xpathApply(x, '//dt', xmlValue)
  values <- xpathApply(x, '//dd', xmlValue)
  
  if(length(names) != length(values)) stop("Unterschiedliche Anzahl Bezeichner/Werte")
  
  names(values) <- names
  return(unlist(values))
}

featureBoxRaw <- lapply(dat$bricksetURL, function(x) htmlTreeParse(x, useInternal = TRUE))
featureBoxList <- lapply(featureBoxRaw, extractFeatureBox)
uniqueNames <- unique(unlist(lapply(featureBoxList, names)))
featureBoxList <- lapply(featureBoxList, function(x) {missingNames <- uniqueNames[!uniqueNames %in% names(x)]
                                                      if(length(missingNames)){
                                                        xAdd <- rep(NA, length(missingNames))
                                                        names(xAdd) <- missingNames
                                                        x <- c(x, xAdd)
                                                      }
                                                      x <- x[sort(names(x))]
                                                      return(x)
                                                      })
dat2 <- as.data.frame(do.call(rbind, featureBoxList), stringsAsFactors = FALSE)

# Aufbereitung der zusätzlichen Variablen

# Entferne exklusive Sets
dat2 <- dat2[!grepl("exclusive", dat2$Notes, ignore.case=TRUE), ]
dat2$Weight2 <- sapply(strsplit(dat2$Weight, split = "Kg"), function(x) as.numeric(x[[1]]))
dat2$Volume <- sapply(strsplit(sapply(strsplit(dat2$Dimensions, split = "cm"), 
                                      function(x) x[[1]]), split = " x"),
                      function(x) prod(as.numeric(x)))
dat2$VolLitres <- dat2$Volume/1000
priceList <- lapply(strsplit(dat2$RRP, split = "/"), 
                    function(x){ 
                      currency <- gsub("[0-9]|\\.| ", "", x)
                      value <- as.numeric(mapply(substring, gsub(" ", "", x), nchar(currency) + 1))
                      names(value) <- currency
                      value          
                    })
allCurrencies <- unique(unlist(lapply(priceList, names)))
priceDat <- as.data.frame(do.call(rbind, lapply(priceList, 
                                                function(x) { 
                                                  missingCurr <- allCurrencies[!allCurrencies %in% names(x)]
                                                  if(length(missingCurr)){
                                                    xAdd <- rep(NA, length(missingCurr))
                                                    names(xAdd) <- missingCurr
                                                    x <- c(x, xAdd)
                                                  }
                                                  x <- x[order(names(x))]
                                                  return(x)
                                                })))
dat2 <- cbind(dat2, priceDat)



# Wo fehlt die EUR Preis Information
pdf("Data/SetsNoEurPr.pdf", height = 5, width = 7)
filter <- dat$number %in% sapply(strsplit(dat2$"Set number"[is.na(dat2$"???")], split = "-"), function(x) x[[1]])
invisible(lapply(dat$number[filter], plotSet, dat = dat[filter, ]))
dev.off()

dat2  <- dat2[!is.na(dat2$"???"), ]
table(is.na(dat2$Volume), is.na(dat2$Weight2), dnn = c("missVol", "missWght"))
dat2 <- dat2[!is.na(dat2$Weight2) & !is.na(dat2$Volume), ]

names(dat2)[names(dat2) == "???"] <- "EUR"
names(dat2)[names(dat2) == "Year released"] <- "Year"
dat2$Pieces <- as.integer(dat2$Pieces)
dat2$Theme <- factor(dat2$Theme)
dat2$Theme <- relevel(dat2$Theme, ref = "City")


dput(dat2, file = "Data/dat2Final.R")

# Deskriptive Grafiken
dat2 <- dget("Data/dat2Final.R")
dat2$meanWeight <- dat2$Weight2/dat2$Pieces*1000
dat2 <- dat2[dat2$Theme %in% c("City", "Star Wars", "Legends Of Chima"), ]
dat2$Theme <- factor(dat2$Theme)
table(dat2$EUR <= 100, dat2$Theme)
dat2 <- dat2[dat2$EUR <= 100, ]
dat2$CPP <- dat2$EUR/dat2$Pieces
dat2$WPP <- dat2$Weight2/dat2$Pieces*1000

library(xtable)
library(relaimpo)
library(splines)
library(RColorBrewer)
colorTable <- data.frame(Theme = c("City", "Star Wars", "Legends Of Chima"),
#                          Color = brewer.pal(3, "RdYlBu"),
                         Color = c("darkred", "cornflowerblue", "orange"),
                         stringsAsFactors = FALSE)
colorTable <- colorTable[order(colorTable$Theme), ]

# Tabelle: Sets pro Theme
print(xtable(table(as.character(dat2$Theme))), type = "html")

# Bivariater Plot
colOfInterest <- c("EUR", "VolLitres", "Pieces", "Weight2")
dat2plot <- dat2[, colOfInterest]
names(dat2plot) <- c("Preis [???]", "Packungsgröße [l]", "Teile", "Gewicht [kg]")
png(file = "Figures/PreisVolTeileGewicht.png",
    width = 1024, height = 1024, type = "cairo-png")
parOld <- par(no.readonly = TRUE)
pairs(dat2plot, col = colorTable$Color[match(dat2$Theme, colorTable$Theme)], 
      pch = 16, las = 1, oma = c(3, 3, 8, 3), cex = 2.5, cex.lab = 1.5, cex.axis = 1.2)
par(xpd = TRUE)
legend("top", legend = colorTable$Theme, col = colorTable$Color, pch = 16, ncol = nrow(colorTable),
       bty = "n", cex = 1.2)
par(parOld)
dev.off()

## 3er Panel
png(file = "Figures/LEGO_PreisVsVolTeileGewicht.png",
    width = 1024, height = 480, type = "cairo-png")
parOld <- par(no.readonly = TRUE)
layout(matrix(c(4, 4, 4, 1, 2, 3), nrow = 2, byrow = TRUE), 
       height = c(1, 5), width = c(2.55, 2, 2))
par(mai = c(1.1, 1.1, 0.05, 0.5))
plot(EUR ~ VolLitres, data = dat2,
     col = colorTable$Color[match(dat2$Theme, colorTable$Theme)],
     xlab = "Packunsgröße [l]", ylab = "Preis [EUR]", pch = 16, 
     cex = 2.5, cex.lab = 1.5, cex.axis = 1.2, type = "n", las = 1)
grid()
abline(h = 69.99)
points(EUR ~ VolLitres, data = dat2,
     col = colorTable$Color[match(dat2$Theme, colorTable$Theme)], pch = 16, 
     cex = 2.5)
par(mai = c(1.1, 0.05, 0.05, 0.5))
plot(EUR ~ Pieces, data = dat2,
     col = colorTable$Color[match(dat2$Theme, colorTable$Theme)],
     xlab = "# Teile", ylab = "", pch = 16, frame = TRUE, axes = FALSE,
     cex = 2.5, cex.lab = 1.5, cex.axis = 1.2, type = "n", las = 1)
grid()
abline(h = 69.99)
axis(1)
points(EUR ~ Pieces, data = dat2,
       col = colorTable$Color[match(dat2$Theme, colorTable$Theme)], pch = 16, 
       cex = 2.5)
par(mai = c(1.1, 0.05, 0.05, 0.5))
plot(EUR ~ Weight2, data = dat2,
     col = colorTable$Color[match(dat2$Theme, colorTable$Theme)],
     xlab = "Gewicht [kg]", ylab = "", pch = 16, frame = TRUE, axes = FALSE,
     cex = 2.5, cex.lab = 1.5, cex.axis = 1.2, type = "n", las = 1)
grid()
abline(h = 69.99)
axis(1)
points(EUR ~ Weight2, data = dat2,
       col = colorTable$Color[match(dat2$Theme, colorTable$Theme)], pch = 16, 
       cex = 2.5)
par(mai = c(0.05, 1.1, 0.05, 0.5))
plot(1:10, type = "n", frame = FALSE, axes = FALSE, xlab ="", ylab ="")
legend("bottom", legend = colorTable$Theme, col = colorTable$Color, pch = 16, ncol = nrow(colorTable),
       bty = "n", cex = 1.7, title = "LEGO Themenwelten")

par(parOld)
dev.off()



# Grafik: mittlere Preise und Gewichte
lwd <- 1.5
parOld <- par(no.readonly = TRUE)
layout(matrix(c(3,4,1,2), ncol = 2, byrow = TRUE), widths=c(4, 1), heights=c(1, 4))
par(mai = c(1.1, 1.1, 0.05, 0.05))
plot(dat2$WPP, dat2$CPP, bg = ifelse(dat2$Theme == "City", colors[1], colors[2]), pch = 21,
     ylab = "Preis pro Teil [EUR]", xlab = "Gewicht pro Teil [g]",
     xlim = range(dat2$WPP), ylim = range(dat2$CPP), cex = sqrt(dat2$Pieces)/15, type = "n")
grid()
points(dat2$WPP, dat2$CPP, bg = colorTable$Color[match(dat2$Theme, colorTable$Theme)], pch = 21,
       cex = sqrt(dat2$Pieces)/15)
# CPP densities
par(mai = c(1.1, 0.05, 0.05, 0.6))
dListCPP <- lapply(split(dat2, f= dat2$Theme), function(x) density(x$CPP))
plot(dListCPP[[1]]$y, dListCPP[[1]]$x, ylim = range(dat2$CPP), xlab = "", ylab = "",
     xlim=range(dListCPP[[1]]$y), type = "n", frame = TRUE, axes = FALSE) 
abline(h = axTicks(2), col = "grey", lty = 3)
invisible(
  lapply(names(dListCPP), function(x) {
    lines(dListCPP[[x]]$y, dListCPP[[x]]$x, col = colorTable$Color[match(x, colorTable$Theme)], type = "l", lwd = lwd)
  }))
# WPP densities
par(mai = c(0.05, 1.1, 0.6, 0.05))     
dListWPP <- lapply(split(dat2, f= dat2$Theme), function(x) density(x$WPP))
plot(dListCPP[[1]]$x, dListCPP[[1]]$y, xlim = range(dat2$WPP), 
     axes = FALSE, frame = TRUE, ylim=range(dListWPP[[1]]$y), type = "n",
     xlab = "", ylab = "")
abline(v = axTicks(1), col = "grey", lty = 3)
invisible(
  lapply(names(dListWPP), function(x) {
    lines(dListWPP[[x]]$x, dListWPP[[x]]$y, col = colorTable$Color[match(x, colorTable$Theme)], type = "l", lwd = lwd)
  }))
#Legend
par(mai = c(0.05, 0.05, 0.6, 0.06)) 
plot(1:10, type = "n", frame = FALSE, axes = FALSE)
legend("left", lty = 1, pch = 21, legend = colorTable$Theme, pt.bg = colorTable$Color, col = colorTable$Color,
       bty = "n", pt.cex = 1.5, title = "LEGO")
par(parOld)


## Plot mit Boxplot
# Grafik: mittlere Preise und Gewichte
png(file = "Figures/LEGO_CPPvsWPP.png",
    width = 1024, height = 1024, type = "cairo-png")
lwd <- 1.5
parOld <- par(no.readonly = TRUE)
layout(matrix(c(3,4,1,2), ncol = 2, byrow = TRUE), widths=c(4, 1), heights=c(1, 4))
par(mai = c(1.1, 1.1, 0.05, 0.05))
plot(dat2$WPP, dat2$CPP, bg = ifelse(dat2$Theme == "City", colors[1], colors[2]), pch = 21,
     ylab = "Preis pro Teil [EUR]", xlab = "Gewicht pro Teil [g]",
     xlim = range(dat2$WPP), ylim = range(dat2$CPP), type = "n", cex.lab = 1.75, cex.axis = 1.8)
grid()
points(dat2$WPP, dat2$CPP, bg = colorTable$Color[match(dat2$Theme, colorTable$Theme)], pch = 21,
       cex = sqrt(dat2$Pieces)/7.5)
# CPP densities
par(mai = c(1.1, 0.05, 0.05, 0.6))
plot(1, ylim = range(dat2$CPP), xlab = "", ylab = "",
     xlim= c(0.5, nrow(colorTable) + 0.5), type = "n", frame = TRUE, axes = FALSE) 
abline(h = axTicks(2), col = "grey", lty = 3)
boxplot(CPP ~ Theme, data = dat2, col = colorTable$Color, add = TRUE, axes = FALSE)
# WPP densities
par(mai = c(0.05, 1.1, 0.6, 0.05))     
plot(1, xlim = range(dat2$WPP), 
     axes = FALSE, frame = TRUE, ylim=c(0.5, nrow(colorTable) + 0.5), type = "n",
     xlab = "", ylab = "")
abline(v = axTicks(1), col = "grey", lty = 3)
boxplot(WPP ~ Theme, data = dat2, col = colorTable$Color, 
        add = TRUE, axes = FALSE, horizontal = TRUE)
#Legend
par(mai = c(0.05, 0.05, 0.6, 0.06)) 
plot(1:10, type = "n", frame = FALSE, axes = FALSE)
legend("left", lty = 1, pch = 21, legend = colorTable$Theme, pt.bg = colorTable$Color, col = colorTable$Color,
       bty = "n", pt.cex = 2.5, title = "LEGO", seg.len = 2.5, cex = 1.75)
par(parOld)
dev.off()

# Analse CPP ~ WPP + Theme
library(quantreg)
rqRes <- rq(CPP ~ poly(WPP, 2) + Theme, tau = 0.5, data = dat2, subset = CPP < 0.21 & WPP <= 3)
rqResSpl <- rq(CPP ~ ns(WPP, 4) + Theme, tau = 0.5, data = dat2, subset = CPP < 0.21 & WPP <= 3)
rqResLin <- step(rq(CPP ~ WPP + Theme - 1, tau = 0.5, data = dat2, subset = CPP < 0.21 & WPP <= 3))
summary(rqRes)
summary(rqResSpl)
summary(rqResLin)
x <- seq(min(dat2$WPP), 3.5, length.out=100)
plot(x, predict(rqResLin, newdata = data.frame(Theme = "City", WPP = x)), type = "l")
lines(x, predict(rqResSpl, newdata = data.frame(Theme = "City", WPP = x)),
     type = "l", col = "red")
lines(x, predict(rqRes, newdata = data.frame(Theme = "City", WPP = x)),
      type = "l", col = "blue")


lmRes <- step(lm(CPP ~ WPP + Theme , data = dat2, subset = CPP < 0.21 & WPP <= 3))
summary(lmRes)
calc.relimp(lmRes, rela = TRUE)

x <- 2
X <- 100
X/x*(x* coef(lmRes)["WPP"] + coef(lmRes)["ThemeStar Wars"])


dat2Sub <- subset(dat2, subset = CPP < 0.21 & WPP <= 3)

dat2write <- dat2[ ,c("Set number", "Name", "Theme", "Pieces", "Dimensions", "Weight", "RRP", "Year")]
write.csv2(dat2write, file = "Data/LEGO_Data.csv", row.names = FALSE)

# CLusteranalyse
library(mclust)
train <- MclustDA(dat2[ ,c("WPP", "CPP")], class = dat2$Theme, G = 3, modelType = "EDDA", modelNames = "EEE")
summary(train, newdata = dat2[ ,c("WPP", "CPP")], newclass = dat2$Theme)
