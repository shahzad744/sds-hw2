require(tseries, quietly = TRUE)
stock <- read.csv("stocks.csv")
symbols <- stock[["Symbol"]]
data <- NULL
for (row in symbols) {
  if (is.null(data)) {
    data <- suppressWarnings(get.hist.quote(instrument = row, start = "2003-01-01", end = "2008-01-01", quote = "Close", provider = "yahoo", drop = TRUE))
    
  }
  else {
    temp <- suppressWarnings(get.hist.quote(instrument = row, start = "2003-01-01", end = "2008-01-01", quote = "Close", provider = "yahoo", drop = TRUE))
    data <- cbind(data, temp)
    
  }
  
}
cMatrix <- as.matrix(data)
colnames(cMatrix) <- symbols
cMatrix[is.na(cMatrix)] <- 0
logMatrix <- diff(log(cMatrix))
logMatrix[is.na(logMatrix)] <- 0
plot(logMatrix)
