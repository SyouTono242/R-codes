library(ggplot2)

is.POSIXct <- function(x) {
  inherits(x, "POSIXct")
}

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

# Import dataset into R
# dailyDataset <- read.csv(file.choose(),stringsAsFactors=FALSE)
# dailyDataset <- na.omit(OriginalDataset)

dailyDataset <- Alpha

dailyDataset$DateTime <- as.POSIXct(dailyDataset$DateTime, format="%Y-%m-%d %H:%M")
DateTime <- dailyDataset$DateTime

if (ncol(dailyDataset)==2 && sapply(dailyDataset, is.POSIXct)){
  tempdf <- dailyDataset
  tempdf$hour <- format(dailyDataset[[1]], format="%H")
  tempdf$minute <- format(dailyDataset[[1]], format="%M")
  tempdf$time <- paste(tempdf$hour,tempdf$minute,sep=":")
  tempdf$hour <- as.numeric(as.character(tempdf$hour))
  tempdf$minute <- as.numeric(as.character(tempdf$minute))
  order <- tempdf[3]*60+tempdf[4]
  tempdf$sort_order <- order[,1]
  
  
  ggplot(tempdf) +
    geom_boxplot(aes(x=reorder(time, sort_order), y=tempdf[,2], colour="red")) +
    scale_x_discrete(breaks = every_nth(n = 8)) +
    labs(x="Time", y=colnames(tempdf)[2]) + 
    # ylim(18,20) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), text = element_text(size=21), legend.position = "none")
}
