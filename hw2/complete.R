complete <- function(directory, id1 = 1:332) {
    
    sumVect <- data.frame(id=vector(), nobs=vector())
    for(i in id1){
        filename <- formatC(i, width=3, flag="0")
        portion <- read.csv(paste(directory,"/",filename,".csv",sep=""))
        compport <- portion[!is.na(portion$sulfate) & !is.na(portion$nitrate),]
        sumVect <- rbind(sumVect, data.frame(id=i,nobs=nrow(compport)))
    }
    sumVect
}