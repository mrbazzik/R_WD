pollutantmean <- function(directory, pollutant, id = 1:332) {
    sumVect <- vector()
    for(i in id){
        filename <- formatC(i, width=3, flag="0")
        portion <- read.csv(paste(directory,"/",filename,".csv",sep=""))
        sumVect <- rbind(sumVect, portion)
    }
    mean(sumVect[[pollutant]], na.rm=TRUE)
}
