data <- read.csv2("pwt91.csv")
x<-data$year
y<-data$pl_k
h<-10

uniformPartitioning<-function(h){
  len<-length(x)
  s1 <- seq(0, len-h, by = h/2)
  s2<-seq(h/2, len-h/2, by= h/2)
  s3<-seq(h, len, by = h/2)
  A<-Map(c, s1,s2,s3)
  plotPartitions(A)
}


func <- function(Ai, x) {
  ci<-Ai[1]
  ciii<-Ai[length(Ai)]
  cii<-median(Ai)
  ifelse(x>=ci & x<cii, (x-ci)/(cii-ci), ifelse(x>=cii & x<=ciii, (ciii-x)/(ciii-cii), 0))
}
plotPartitions <- function(A) {
  plot(NULL, xlim=c(0,length(x)), ylim=c(0,1.5), ylab="y label", xlab="x lablel")
  len <- length(A)
  for (i in 1:len){
    xd <- A[[i]]
    x <- seq(xd[1], xd[length(xd)], 0.1)
    curve(func(xd, x), from=xd[1], to=xd[length(xd)], type="l", add=TRUE)
  }
}

