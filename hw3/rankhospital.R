rankhospital <- function(state, outcome, num="best"){
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    words <- strsplit(outcome," ", fixed=TRUE)[[1]]
    param <- paste(toupper(substring(words,1,1)),substring(words,2), sep="", collapse=".")
    colName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",param, sep="")
    h1 <- df[df$State == state,]
    if(nrow(h1)==0) stop("invalid state")
    if(!(colName %in% colnames(h1))) stop("invalid outcome")
    h2 <- h1[,c("Hospital.Name",colName)]
    h2 <- h2[order(as.numeric(h2[[colName]]),h2$Hospital.Name),]
    ##h2 <- split(h2,as.numeric(h2[[colName]]))
    h2<-h2[!is.na(as.numeric(h2[[colName]])),]
    if(num=="best") n<-1
    else if(num=="worst")n<-nrow(h2)
    else if (num <= nrow(h2)) n<-num
    else return(NA)
    h2[n,"Hospital.Name"]
    ##ha <- h1[[colName]]
    
    ##ha<-sort(as.numeric(ha))
    ##split(ha,ha[[colName]])
    
    
    
}