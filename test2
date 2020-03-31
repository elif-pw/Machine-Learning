func <- function(Ai, x) {
  ci<-Ai[1]
  ciii<-Ai[length(Ai)]
  cii<-median(Ai)
  ifelse(x>=ci & x<cii, (x-ci)/(cii-ci), ifelse(x>=cii & x<=ciii, (ciii-x)/(ciii-cii), 0))
}

funcHelper <- function(A) {
  len <- length(A)
  tmpX <- seq(A[[1]][1], A[[length(A)]][length(A[[length(A)]])], 0.1)
  for (i in 1:len){
    xd <- A[[i]]
    x <- seq(xd[1], xd[length(xd)], 0.1)
    if (i=0){
      curve(func(xd, x), from=xd[1], to=xd[length(xd)], type="l")
    } 
    else {
      curve(func(xd, x), from=xd[1], to=xd[length(xd)], type="l", add=TRUE)
    }
  }
}

data <- read.csv2("https://raw.githubusercontent.com/elif-pw/Machine-Learning/master/pwt91.csv?token=AKUJXQBVWFK4PU2UQH2S2GC6QM5RO")
x<-data$year
y<-data$pl_k

sampleSD <- sd(y)
Ai <- c(1)
tmp <- 1
A <- list()

tmp<-2
tmpA<-1
for (i in 2:length(x)) {
  #print(x[i])
  if (length(Ai)>=10 | abs(y[i]-y[i-1])>sampleSD) {
    A[[tmpA]] <- Ai
    tmpA = tmpA + 1
    tmp <- 1
    Ai <- vector()
  }
  Ai[tmp]<-i
  tmp <- tmp + 1
}
A[[tmpA]]<-Ai


