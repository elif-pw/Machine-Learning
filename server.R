data<-read.csv2("data_all_4.csv")
x<-data$year

shinyServer(
  function(input, output, session){
    
    
    output$Plot<-renderPlot({
      country<-input$countryInput
      chosenh<-input$horizonInput
      #print(country)
      y<-data[match(country, colnames(data))][[1]]
      y<-y[!is.na(y)]
      xdStart<-length(x)-length(y)
      xdEnd<-length(x)
      x<-x[xdStart:xdEnd]
      break_years_B1values<-plot_breaks(x,y, chosenh)
      
      output$RightContextStr<-renderText({
        paste("Context right boundary absolute value: ")
      })  
      output$RightContextVal<-renderText({
        break_years_B1values$right_context
      })
      output$YearStr<-renderText({
        paste("Structural Breaks happened in:")
      })
      output$Years<-renderText(sep = ", ",{
        break_years_B1values$years
      })
      output$B1Str<-renderText({
        paste("Break points B1 values")
      })
      output$B1Values<-renderText(sep = ", ",{
        break_years_B1values$B1_values
      })
    })  
  }
)

#Partitions
uniformPartitioning<-function(h,xd){
  len<-length(xd)
  s1 <- seq(0, len-h, by = h/2)
  s2<-seq(h/2, len-h/2, by= h/2)
  s3<-seq(h, len, by = h/2)
  A<-Map(c, s1,s2,s3)
  if(len%%h!=0) {
    last_element_of_A <- A[[length(A)]]
    A[[length(A)+1]]<-c(last_element_of_A[2], last_element_of_A[3], last_element_of_A[3]+h/2)
  }
  #plotPartitions(A)
  return(A)
}

func <- function(Ai, x) {
  ci<-Ai[1]
  ciii<-Ai[length(Ai)]
  cii<-median(Ai)
  ifelse(x>=ci & x<cii, (x-ci)/(cii-ci), ifelse(x>=cii & x<=ciii, (ciii-x)/(ciii-cii), 0))
}

plotPartitions <- function(A, D, y, xdd, maxVal, minVal) {
  plot(NULL, xlim=c(1,length(xdd)), ylim=c(minVal*100-1,maxVal*100+1), ylab="GDP annual growth (%)", xlab="Time (years)", xaxt="n")
  len <- length(A)
  scale<-maxVal/7.5*100
  structural_break_years<-vector()
  counter_sb<-1
  for (i in 1:len){
    xd <- A[[i]]
    x <- seq(xd[1], xd[length(xd)], 0.1)
    if(!D[i]) {
      #curve(func(xd, x)*scale, from=xd[1], to=xd[length(xd)], type="l", add=TRUE, lty=3)
      polygon(c(xd[1], xd[2], xd[3]), c(func(xd, x)[1], func(xd, x)[func(xd, x)==1]*scale, func(xd, x)[length(func(xd, x))]), col="yellow", lty=3)
      structural_break_years[counter_sb]<-xdd[xd[2]]
      counter_sb=counter_sb+1
    } else {
      polygon(c(xd[1], xd[2], xd[3]), c(func(xd, x)[1], func(xd, x)[func(xd, x)==1]*scale, func(xd, x)[length(func(xd, x))]), lty=3)
      #curve(func(xd, x)*scale, from=xd[1], to=xd[length(xd)], type="l", add=TRUE, lty=3)
    }
  }
  axis(1, at=seq(1,length(xdd)), labels=xdd, las=2)
  return(structural_break_years)
}

#beta 0
get_beta0 <- function(A, y) {
  B0<-vector()
  sum1 <- 0
  for (i in 1:length(A)){
    Ai <- A[[i]]
    avg <- 0
    x2<-seq(Ai[1], Ai[length(Ai)], 1)
    for (j in x2[1]:x2[length(x2)]){
      if(j>length(y)) {
        break;
      }
      #print(j)
      #print(y[j])
      if (j==0){
        next;
      }
      avg <- avg + y[j]*func(Ai, j)
    }
    sum1 <- sum(func(Ai,x2))
    avg <- avg / sum1
    B0[i] <- avg
  }
  return(B0)
}

#beta 1
get_beta1 <- function(A, y) {
  B1<-vector()
  for (i in 1:length(A)){
    sum1<-0
    Ai <- A[[i]]
    avg <- 0
    x2<-seq(Ai[1], Ai[length(Ai)], 0.000001)
    for (j in x2[1]:x2[length(x2)]){
      #print(j)
      if(j>length(y)) {
        break;
      }
      if (j==0){
        next;
      }
      avg <- avg + y[j]*func(Ai, j)*(j-Ai[2])
      sum1 <- sum1 + func(Ai,j)*(j-Ai[2])^2
    }
    avg <- avg / sum1
    B1[i] <- avg
  }
  return(B1)
}

#F
get_F <- function(A, B0, B1, y) {
  F <- vector()
  tmpF <- 1
  fi<-vector()
  for (i in 1:length(A)){
    for (j in seq(A[[i]][1], A[[i]][3], 0.001)){
      if(j>length(y)) {
        break;
      }
      fi[j]<-B0[i]+B1[i]*(j-A[[i]][2])
    }
    #tmpF = tmpF + 1
  }
  F<-fi
  return(F)
}


#plot breaks
plot_breaks<-function(x,y,h){
  A<-uniformPartitioning(h, x)
  B0 <- get_beta0(A, y)
  B1 <- get_beta1(A, y)
  Fxd <- get_F(A, B0, B1, y)
  right_context <- abs(sd(y)/2/h)
  satisfactoryB1 <- B1>right_context
  maxVal<-max(Fxd,y,na.rm=TRUE)
  minVal<-min(Fxd,y,na.rm=TRUE)
  B1_breakvalues<-B1[satisfactoryB1==FALSE]
  # print(length(Fxd))
  # print(length(y))
  break_years<-plotPartitions(A, satisfactoryB1, y, x, maxVal, minVal)
  lines(Fxd*100, col="green")
  lines(y*100, col="red")
  legend("bottomright", legend=c("Approximation", "Data"), col=c("green", "red"), lty=1, cex=0.8)
  right_context<-abs(sd(y)/2/h)
  right_context_break_years_B1values<-list("right_context"=right_context,"years"=break_years, "B1_values"=B1_breakvalues)
  return(right_context_break_years_B1values)
}


