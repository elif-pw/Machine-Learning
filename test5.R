data <- read.csv2("pwt91.csv")
x<-data$year
y<-data$pl_k
h<-4

A<-uniformPartitioning(h, x)

#B0 <- get_beta0(A, y)
B1 <- get_beta1(A, y)
plot(B1, type="l")
#Fxd <- get_F(A, B0, B1)

# nodes testing
