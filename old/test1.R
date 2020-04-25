ap <- AirPassengers
h <- (ap[length(ap)]-ap[1])/length(ap)
nodes <- seq(from = ap[1], to = ap[length(ap)], by = h)

A0 <- c(nodes[1], nodes[1+1])
A <- list(A0)
for (i in 2:length(nodes)-1) {
  A[[i]] <- c(nodes[i-1], nodes[i], nodes[i+1])
}

A[[length(nodes)]] <- c(nodes[length(nodes)-1], nodes[length(nodes)])

