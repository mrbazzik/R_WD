best <- function(state, outcome){
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    words <- strsplit(outcome," ", fixed=TRUE)[[1]]
    param <- paste(toupper(substring(words,1,1)),substring(words,2), sep="", collapse=".")
    h1 <- df[df$State == state,]
    if(nrow(h1)==0) stop("invalid state")
    ha <- h1[[paste("Hospital.30.Day.Death..Mortality..Rates.from.",param, sep="")]]
    if(is.null(ha)) stop("invalid outcome")
    ha<-as.numeric(ha)
    rate <- min(ha, na.rm=TRUE)
    mask <- !is.na(ha) & (ha == rate)
    hosps <- sort(h1[mask,"Hospital.Name"])
    hosps[1]
}