library(dbscan)

# Import dataset into R
OriginalDataset <- read.csv(file.choose(),stringsAsFactors=FALSE)
OriginalDataset <- na.omit(OriginalDataset)


# Custom function `normalize` to normalize time data
normalize <- function(datetime, value) {
  valueMax <- ifelse(!all(is.na(value)), max(value, na.rm=T), NA)
  valueMin <- ifelse(!all(is.na(value)), min(value, na.rm=T), NA)
  return (((datetime - min(datetime)) / (max(datetime)-min(datetime)))*(valueMax-valueMin)+valueMin)
}

# Custom function `plotDBSCAN` to plot DBSCAN graphs
plotDBSCAN <- function(datetime, value){
  datetime_norm <- normalize(datetime, value)
  tempdf <- data.frame(datetime_norm,value)
  kNNdistplot(tempdf, k=5)
  epsInput <- as.numeric(readline(prompt = "Hi human, please enter the eps: "))
  if (is.na(epsInput)){
    print("Eps should be a number, please try again.")
  } else {
    db <- dbscan(tempdf, eps=epsInput, minPts=5)
    plot(tempdf, col=db$cluster+1L, xaxt='n', xlab="Time", ylab=names(tempdf[2]), main=paste("Original Dataset of", toString(names(tempdf[2]))))
    axis(1, at=tempdf$datetime_norm[xaxisPosition], labels=xaxis[1:7])
    legend('bottomright',legend=c('Cluster','Outlier'),col=c('red','black'),pch=20)
    return (db$cluster)
  }
}

  


# Change the class of Date_Time from character to POSIXct (a date&time format used in R)
OriginalDataset$DateTime <- as.POSIXct(OriginalDataset$DateTime, format="%Y-%m-%d %H:%M")

# Add a column of time difference from 2018-09-01 00:00:00 to the dataset
DiffMins <- as.numeric(difftime(OriginalDataset$DateTime,'2016-01-01 00:00:00', units='min'))
OriginalDataset$DiffMins <- DiffMins
OriginalDataset <- OriginalDataset[,c("DateTime","DiffMins","Temp","DO","Airflow","OxygenFraction")]

# Choose 7 timepoints with even intervals to replace x-axis values later
xaxisPosition <- as.integer(seq(1,length(OriginalDataset$DateTime),length.out=7))
xaxis <- OriginalDataset$DateTime[c(xaxisPosition)]

# Plot it!
for (i in (4:ncol(OriginalDataset))){
  results <- plotDBSCAN(datetime=OriginalDataset$DiffMins,value=OriginalDataset[i])
  returndf <- data.frame(OriginalDataset[1], OriginalDataset[i], results)
  returndf[returndf==0] <- NA
  returndf <- na.omit(returndf)
  returndf <- subset(returndf,select = -c(3))
  assign(colnames(OriginalDataset)[i],returndf)
}

newDataset <- merge(Temp, DO, by="DateTime")
newDataset <- merge(newDataset, Airflow, by="DateTime")
newDataset <- merge(newDataset, OxygenFraction, by="DateTime")

write.csv(newDataset, "newDataset.csv")

print("Thank you for trying out our DBSCAN code. Have fun plotting lol")