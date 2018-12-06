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
pearson_correlation_matrix <- cor(logMatrix, method = "pearson", use = "everything")
head(pearson_correlation_matrix) # outputing the rows
save.image("main.RDaata")
load("main.RDaata")
R <- pearson_correlation_matrix
R[is.na(R)] <- 0

#Calculating bootstarp confidence interval

B = 10000
brep = rep(NA, B)
set.seed(1213)  # for reproducibility
for (b in 1:B){
  idx = sample(1:(nrow(R)*ncol(R)), replace = T)
  bsamp   = as.vector(R)[idx]        # bootstrap sample
  btheta  = sqrt(nrow(R)*ncol(R))*max(bsamp-as.vector(R))   # bootstrap replicate
  brep[b] = btheta            # save
}

Gstar<- ecdf(brep)
plot(Gstar)
# so inverse of ECDF of 0.95 would b 110 and Confidence intervals will be 110/ sqrt(n) = 1
confidence <- 0.1
matrix_apply <- function(m,ee,con) {
  m2 <- m
  for (r in seq(nrow(m2))){
    for (c in seq(ncol(m2))){
      l <- m[[r,c]]-con
      h <- m[[r,c]]+con
      eel <- ee*-1
      eeh <- ee
      #print(c(h,l,eeh,eel))
      if(h < eel || l > eeh){
        m2[[r,c]]<-1
      }
      else{
        m2[[r,c]]<-0
      }
    }
  }
  
  return(m2)
}
ee<-0.2
m<- matrix_apply(R,ee,confidence)
plot(simplify(graph_from_incidence_matrix(m)))
for( i in 1:20){
  

} 

