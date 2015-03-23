rankall <- function(outcome, num="best"){
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    words <- strsplit(outcome," ", fixed=TRUE)[[1]]
    param <- paste(toupper(substring(words,1,1)),substring(words,2), sep="", collapse=".")
    colName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",param, sep="")
    
    if(!(colName %in% colnames(df))) stop("invalid outcome")
    
    
    h2 <- split(df[,c("Hospital.Name","State",colName)],df$State)
    result <- data.frame(hospital=vector(), state=vector())
    getelem <- function(x){
        st <- x[1,"State"]
        y <- x[!is.na(as.numeric(x[[colName]])),]
        y<- y[order(as.numeric(y[[colName]]), y$Hospital.Name),]
        if(num=="best") hosp <- y[1,"Hospital.Name"]
        else if(num=="worst") hosp <- y[nrow(y),"Hospital.Name"]
        else if (num <= nrow(y)) hosp <- y[num,"Hospital.Name"]
        else hosp <- NA
        result <<- rbind(result, data.frame(hospital=hosp,state=st))
    }
    f1 <- lapply(h2,getelem)
    result
    
    
    
}