require(tseries, quietly = TRUE)
library("ggpubr")
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

#saving the data into RDS object 

saveRDS(logMatrix,file="x_matrix.rds")

# getting the matrix from rds object 
logMatrix<- readRDS(file="x_matrix.rds")
plot(logMatrix)

#install.packages("ggpubr")

#df_logmatrix <- as.data.frame(logMatrix)

# ggscatter(head(df_logmatrix,100), x = "AAP", y = "AAPL", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "Closing prices for AAP ", ylab = "Closing prices for AAPL")
# 
# # AAP
# ggqqplot(df_logmatrix$AAL, ylab = "AAP")
# # AAPL
# ggqqplot(df_logmatrix$AAPL, ylab = "AAPL")
# 
# res <- cor.test(df_logmatrix$AAL, df_logmatrix$AAPL, 
#                 method = "pearson")
# res

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

# calculated the pearson correlation matrix
pearson_correlation_matrix <- cor(logMatrix, method = "pearson", use = "complete.obs")
head(pearson_correlation_matrix) # outputing the rows
save.image("main.RDaata")
load("main.RDaata")


