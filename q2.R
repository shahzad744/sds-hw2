require(tseries, quietly = TRUE)
stock <- read.csv("stocks_short.csv")
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
cor_mat <- matrix(1:ncol(logMatrix)**2,nrow=ncol(logMatrix),dimnames=list(colnames(logMatrix),colnames(logMatrix)))
save.image("Q2.RData")
require(energy)
load("Q2.RData")
logMatrix[is.na(logMatrix)] <- 0
logMatrix[is.infinite(logMatrix)] <- 0
alpha <- 0.05
for(i in seq(nrow(cor_mat))){
  for(j in seq(ncol(cor_mat))){
    X <- logMatrix[,i]
    Y <- logMatrix[,j]
    dd <- dcov.test(X,Y,index=0.01,R=1000)
    cor_mat[i,j] <- dd$p.value
  }
}
save.image("Q2.RData")
matrix_apply <- function(m,alpha) {
  m2 <- m
  for (r in seq(nrow(m2))){
    for (c in seq(ncol(m2))){
      val <- m[[r,c]]
      if(val>=alpha){
        m2[[r,c]]<-1
      }
      else{
        m2[[r,c]]<-0
      }
    }
  }
  
  return(m2)
}
color_graph <- function(graph,stocks){
  sectors <- unique(stocks$GICSSector)
  colors <- rainbow(length(sectors))
  
  for(i in 1:nrow(stocks)) {
    row <- stocks[i,]
    
    #assigning the colors
    for(j in 1:length(sectors)) {
      if(sectors[j] == row$GICSSector){
        V(graph)[i]$color <- colors[j]
        break
      }
    }
  }
  return (graph)
}
m <- matrix_apply(cor_mat,alpha)
g<-simplify(graph_from_adjacency_matrix(m,diag = FALSE,mode = "undirected"))
plot(color_graph(g,stock))

