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

