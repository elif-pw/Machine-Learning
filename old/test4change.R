data <- read.csv2("pwt91.csv")
x<-data$year
y<-data$pl_k
h<-8
A<-uniformPartitioning(h)

B0<-vector()
B1<-vector()
tmpF<-1


#beta 0
for (i in 1:length(A)){
  Ai <- A[[i]]
  avg <- 0
  x2<-seq(Ai[1], Ai[length(Ai)], 1)
  for (j in x2[1]:x2[length(x2)]){
    #print(j)
    if (j==0){
      next;
    }
    avg <- avg + y[j]*func(Ai, j)
  }
  sum1 <- sum(func(Ai,x2))
  avg <- avg / sum1
  B0[i] <- avg
}

#beta 1
for (i in 1:length(A)){
  Ai <- A[[i]]
  avg <- 0
  x2<-seq(Ai[1], Ai[length(Ai)], 0.001)
  for (j in x2[1]:x2[length(x2)]){
    #print(j)
    if (j==0){
      next;
    }
    avg <- avg + y[j]*func(Ai, j)*(j-Ai[2])
    sum1 <- sum1 + func(Ai,j)*(j-Ai[2])^2
  }
  avg <- avg / sum1
  B1[i] <- avg
}

#F vector
F<-vector()
fi<-vector()
for (i in 1:length(A)){
  for (j in seq(A[[i]][1], A[[i]][3], 0.001)){
    fi[j]<-B0[i]+B1[i]*(j-A[[i]][2])
  }
  F<-fi
}


#Partitions
uniformPartitioning<-function(h){
  len<-length(x)
  s1 <- seq(0, len-h, by = h/2)
  s2<-seq(h/2, len-h/2, by= h/2)
  s3<-seq(h, len, by = h/2)
  A<-Map(c, s1,s2,s3)
  plotPartitions(A)
  return(A)
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
