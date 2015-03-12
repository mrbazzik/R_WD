corr <- function(directory, threshold = 0) {
    obs <- complete(directory)
    throbs <- obs[obs$nobs>threshold,]
    
    sumVect <- vector()
    for(i in throbs$id){
        filename <- formatC(i, width=3, flag="0")
        portion <- read.csv(paste(directory,"/",filename,".csv",sep=""))
        compport <- portion[!is.na(portion$sulfate) & !is.na(portion$nitrate),]
        sumVect <- c(sumVect,cor(compport$sulfate, compport$nitrate))
                
    }
    sumVect
}