data <- read.csv2("pwt91.csv")
x<-data$year
y<-data$pl_k
h<-4

A<-uniformPartitioning(h, x)

B0 <- get_beta0(A, y)
B1 <- get_beta1(A, y)
#plot(B1, type="l")
Fxd <- get_F(A, B0, B1)
#plot(Fxd, type="l")
#plot(x,y,type="l")
# nodes 
the_best_range_ever <- 1.75*tmp2 <= B1 & B1 <= 1.75*tmp
plotPartitions(A, the_best_range_ever)
lines(Fxd, col="green")
lines(y, col="red")
