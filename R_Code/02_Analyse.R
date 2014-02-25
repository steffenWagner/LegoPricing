rm(list=ls(all = TRUE))
require(rLego)
require(Hmisc)
years <- 2000:2014
load(file = paste("Data/SetsYears_", paste(range(years), collapse = "-"), ".Rdata", sep = ""))

str(dat)
describe(dat)

