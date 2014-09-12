################################################################################
################################################################################
## Title: BMD Chart
## Author: Steve Lane
## Date: Fri 12/09/2014
## Synopsis: Create the nvd3 chart for presentation.
################################################################################
################################################################################
rm(list = ls())
require(reshape2)
require(rCharts)
require(plyr)
gos <- read.csv(file = "../../Softer_Orford/Data/GOS_Long.csv", header = TRUE)
set.seed(42)
gos <- subset(gos, ID %in% sample(ID, 250))
gosf0 <- gos[-which(gos$f0 > 5 | is.na(gos$f0) | is.na(gos$age) | gos$s8 > 5 |
                    gos$b9 > 5 | gos$a2 > 5),
             which(names(gos) %in% c("ID", "age", "f0", "s8", "b9", "a2"))]
gosf0 <- within(gosf0, {
    age <- round(age, 3)
    f0 <- round(f0, 3)
    s8 <- round(s8, 3)
    b9 <- round(b9, 3)
    a2 <- round(a2, 3)
})
## Change IDs so only a few show up. Easiest way would be to just append some
## rows!
id <- sample(unique(gosf0$ID), 5)
gosf0$IDn <- "All"
gosf0$cex <- 1
sub <- gosf0[which(gosf0$ID %in% id),]
sub$IDn <- sub$ID
sub$cex <- 2
gosf0 <- rbind(gosf0, sub)
gosf0 <- gosf0[, -which(names(gosf0) %in% "ID")]
remove <- as.logical(gosf0$IDn != "All")
plf0 <- nPlot(f0 ~ age, group = "IDn", data = gosf0, type = "scatterChart")
plf0$set(disabled = remove)
plf0$chart(size = '#! function(d){return d.cex} !#')
plf0$addControls("y", value = "f0", values = c("f0", "s8", "b9", "a2"))
plf0$setTemplate(script = system.file('libraries', 'nvd3', 'controls',
                 'datgui.html', package = 'rCharts'))
plf0$set(width = 600, height = 400)
plf0$LIB$url <- "../libraries/widgets/nvd3"
plf0$save("f0.html")
