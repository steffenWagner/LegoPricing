require(httr)

url <- "http://www.cubiculus.com/api-rest"
apiKey <- "464d1an3ut8croj3628t4btrmtd90ooe9tgo7tq78sor89hiv257s802fltnhs97"
cmd <- "lego-set"
id <- 70006

getSetInfo <- function(id){
  cmd <- "lego-set"
  urlString <- paste(url, cmd, apiKey, id, sep ="/")
  GET(urlString)
}
res <- getSetInfo(8214)
content(res, as="parsed")

# Building Instructions
buildInst <- GET(paste(url, "building-instructions", apiKey, sep ="/"))
content(buildInst, as="parsed")

# POST requests
cmd <- "range-price"
POST(paste(url, cmd, apiKey, sep ="/"), body = "[]")

cmd <- "range-year"
POST(paste(url, cmd, apiKey, sep ="/"), body = "[]")

pages <- 1:100
getSets <- function(page){
  cmd <- "lego-sets"
  body <- paste("{ \"currentPageNo\" : ", page, 
                ", \"propertiesParams\" : { \"idPropertyValues\" : [], \"priceRange\" : { \"from\" : \"0\", \"to\" : \"1000\" }, \"yearRange\" : { \"from\" : \"2010\", \"to\" : \"2012\" } }, \"rowPerPage\" : 100 } ",
                sep = " ")
  POST(paste(url, cmd, apiKey, sep ="/"), body = body)
}
sets <- lapply(pages, getSets)
save(sets, file="sets.Rdata", compress=TRUE)
load(file="sets.Rdata", compress=TRUE)

dat <- sapply(sets, function(x)
  do.call(rbind, lapply(content(x, as = "parsed")$currentPage, 
                        function(x) data.frame(boxNo = x$boxNo,
                                               name = x$name,
                                               pieces = ifelse(is.null(x$pieces), NA, x$pieces),
                                               price = ifelse(is.null(x$price), NA, x$price),
                                               stringsAsFactors = FALSE))))
dat <- dat[!sapply(dat, is.null)]
dat <- do.call(rbind, dat)

# Auslesen der Set-Infos
setInfos <- lapply(dat$boxNo, getSetInfo)
save(setInfos, file="setInfos.Rdata", compress=TRUE)





