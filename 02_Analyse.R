rm(list=ls(all=TRUE))
require(httr)

# Analyse
load("setInfos.Rdata")
content(setInfos[[1]])

# Erstellen der Preis und Bauteile-Info
dat <- do.call(rbind, 
               lapply(setInfos, 
                      function(x) data.frame(boxNo = content(x)[[1]]$boxNo,
                                             price = ifelse(is.null(content(x)[[1]]$price), NA, content(x)[[1]]$price),
                                             pieces = ifelse(is.null(content(x)[[1]]$pieces), NA, content(x)[[1]]$pieces),
                                             released = content(x)[[1]]$released,
                                             name = content(x)[[1]]$name,
                                             legoModelName = content(x)[[1]]$legoModelName,
                                             stringsAsFactors = FALSE)))


dat <- na.omit(dat)
plot(price ~ pieces, data = na.omit(dat), pch = 16,
     xlim = c(0, 1000))
with(dat[dat$pieces >= 10, ], smoothScatter(pieces, price))
plot(ecdf(dat$pieces))
lm1 <- step(lm(price ~ pieces*released, data = dat, subset = pieces >=25))
plot(lm1)
table(dat$pieces)



set <- GET("http://brickset.com/webservices/brickset.asmx/searchBySetID?SetID=70006")
content(set, as = "parsed")

set <- GET("http://brickset.com/webservices/brickset.asmx/search?apiKey=yiSz-PevZ-udiD&userHash=_MF5xuZjrS&query=string&theme=string&subtheme=string&setNumber=string&year=string&Owned=string&Wanted=7936")

set <- GET("http://brickset.com/webservices/brickset.asmx/search?apiKey=yiSz-PevZ-udiD&userHash=_MF5xuZjrS&query=7936&theme=city&&setNumber=7936")
content(set)

POST("http://brickset.com/webservices/brickset.asmx/searchBySetID", body = "SetID=\"7936\"")
