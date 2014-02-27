rm(list=ls(all = TRUE))
require(rLego)
require(Hmisc)
require(rms)
require(gdata)
files <- list.files(path="Data", pattern="^SetsYears", full.names=TRUE)
files
load(file = files[1])

# Deskriptive Analyse
describe(dat)

# Welche Sets haben keine Teile-Information 'pices'?
head(dat[is.na(dat$pieces), c("number", "setName", "theme")])
table(dat$theme, is.na(dat$pieces))

# Wieviele Teile hat der kleine Feuerwehr-Bausatz (60000)
dat[dat$number == 60000, "pieces"]

filter <- dat$pieces >= 40 & !is.na(dat$USRetailPrice) & 
    !is.na(dat$UKRetailPrice) & !is.na(dat$pieces)
dat <- dat[filter, ]
sort(table(dat$theme), decreasing = TRUE)
summary(dat)

# Deskriptive Analyse
s2 <- spearman2(USRetailPrice ~ pieces + theme + minifigs + subtheme + factor(year), data = dat)
plot(s2)
cor(dat[ ,c("minifigs", "pieces", "USRetailPrice")], use= "p")

sort(table(dat$theme), decreasing = TRUE)[1:20]
sort(table(dat$subtheme), decreasing = TRUE)[1:20]
themesOfInterest <- c("City", "Star Wars", "Friends", "Creator", "Teenage Mutant Ninja Turtles",
                      "Legends Of Chima", "Ninjago", "Harry Potter")
dat <- dat[dat$theme %in% themesOfInterest,]
plsmo(dat$pieces, dat$USRetailPrice, group = as.character(dat$theme))
dat$theme <- relevel(factor(dat$theme), ref = "City")
dat$subtheme <- factor(dat$subtheme)
dat$avgPiecePr <- dat$USRetailPrice/dat$pieces
lm1 <- lm(avgPiecePr ~ pieces + theme + minifigs + factor(year), 
          data = dat, subset = theme %in% themesOfInterest & year > 2011)

summary(lm1)
anova(lm1)
termplot(lm1, partial.resid = TRUE, rug = TRUE, 
         ylim = "free", smooth = panel.smooth, las = 2)

makeURL <- function(number){
  url <- "http://brickset.com/sets/"
  numberVariant <- dat4206-2/Recycling-Truck
  
  
}

paste()


boxplot(avgPiecePr ~ year, data = dat)
dat[order(dat$avgPiecePr, decreasing = TRUE), c("number", "setName", "theme", "subtheme")][1:20, ]

sort(table(dat$subtheme)[table(dat$theme)>0], decreasing = TRUE)


dat[which(dat$subtheme == "Virtual Product Collection"), ]
dat[dat$subtheme == "Virtual Product Collection", "number"]
table(grepl("collection", dat$theme, ignore.case=TRUE))
str(dat)
