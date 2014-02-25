rm(list=ls(all = TRUE))
require(rLego)
require(Hmisc)
require(rms)
files <- list.files(path="Data", pattern="^SetsYears", full.names=TRUE)
files
load(file = files[1])

# Wieviele Teile hat der kleine Feuerwehr-Bausatz (60000)
dat[dat$number == "60000", "pieces"]
filter <- dat$pieces >= 40 & !is.na(dat$USRetailPrice) & 
    !is.na(dat$UKRetailPrice) & !is.na(dat$pieces)
dat <- dat[filter, ]
sort(table(as.character(dat$theme)), decreasing = TRUE)
summary(dat)

# Deskriptive Analyse
s2 <- spearman2(USRetailPrice ~ pieces + theme + minifigs + subtheme, data = dat)
plot(s2)
cor(dat[ ,c("minifigs", "pieces", "USRetailPrice")], use= "p")

sort(table(dat$theme), decreasing = TRUE)[1:20]
sort(table(dat$subtheme), decreasing = TRUE)[1:20]
themesOfInterest <- c("City", "Star Wars", "Friends", "Creator", "Teenage Mutant Ninja Turtles",
                      "Legends Of Chima", "Ninjago", "Harry Potter")
dat <- dat[dat$theme %in% themesOfInterest,]
plsmo(dat$pieces, dat$USRetailPrice, group = as.character(dat$theme))
plsmo(dat$pieces, dat$USRetailPrice, group = as.character(dat$year))

lm1 <- lm(USRetailPrice ~ rcs(pieces, 4) : theme + factor(year), 
          data = dat, subset = theme %in% themesOfInterest)
summary(lm1)
anova(lm1)
termplot(lm1, partial.resid = FALSE, rug = TRUE, 
         ylim = "free", smooth = panel.smooth, las = 2)



str(dat)
