################################################################################
################################################################################
## Title: Maths Fair Demos
## Author: Steve Lane
## Date: Thu 19/06/2014
## Synopsis: Whip up some quick demo graphs for the maths fair talk.
################################################################################
################################################################################
rm(list = ls())
require(reshape2)
require(rCharts)
require(lubridate)
require(plyr)
hpi <- read.csv(file = "HPI.csv", header = TRUE, stringsAsFactors = FALSE)
hpi <- within(hpi, {
    Quarter <- dmy(Quarter)
    Qtr <- quarter(Quarter, with_year = TRUE)
    QtrDate <- as.Date(Quarter)
})
hpi <- melt(hpi, id.vars = c("Quarter", "Qtr", "QtrDate"))
hpi$Index <- "Index"
hpi$Index[388:774] <- "Change"
hpi <- within(hpi, {
    variable <- gsub("Index", "", variable)
    variable <- gsub("Change", "", variable)
})
hpi2 <- subset(hpi, Index == "Index")
hpi2 <- hpi2[, !(names(hpi2) %in% "Index")]
hpi2 <- rename(hpi2, c("value" = "Index"))
hpi2 <- within(hpi2, {
    Change <- with(hpi, value[Index == "Change"])
})

## Options (from http://ramnathv.github.io/posts/rcharts-nvd3/index.html)
## options(
##   rcharts.mode = 'iframesrc',
##   rcharts.cdn = TRUE
## )

plHPI <- nPlot(Index ~ QtrDate, group = c("variable"), data = hpi2, type =
               "lineWithFocusChart")
plHPI$xAxis(tickFormat =
            "#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#")
plHPI$x2Axis(tickFormat =
             "#!function(d) {return d3.time.format('%Y')(new Date( d * 86400000 ));}!#")
plHPI$addControls("y", value = "Index", values = c("Index", "Change"))
plHPI$LIB$url <- "../libraries/widgets/nvd3"
plHPI$save("HPI.html")
