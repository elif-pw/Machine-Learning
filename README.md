# Project for Machine Learning Course

## Description

We followed the implmentation of the algorithm for Detection of Structural Breaks in Time Series Using Fuzzy Techniques by Vilem Novak.

The application is built using Shiny package. The Shiny package for R allows us to build a simple HTML based app without putting much thought into actual web development, hence the code is split in 2 main parts regarding the app itself.  
  
```1.``` ***ui.R***  
Code for UI is the much simpler part, here we simply define what will be visible in the application using functions (controlos) from the shiny package, for exmaple
```{r, results='hide'}
names<-colnames(read.csv2("data_all_4.csv"))
selectInput("countryInput","Select country", choices = names[names!="year"])
```
Creates input control responsible for picking country for which we want to display the data. We also define output controls, such as:
```{r, results='hide'}
plotOutput("Plot")
textOutput("Years")
```
so that we can show our calculated results. The actual calculations take place in the 2nd part of the app.  
  
```2.``` ***server.R***  
This is the part of the code where actual calculations take place and plots are created, we can actually split this .R into part that calculates necessary data and then part that uses this data in order to plot obtained results. The *shinyServer()* function is responsible for actual server, in this function we render the plot using other functions written by us.  
Initially we have to create our partitions, we've decided to make them uniform so as to ease up our work and we create them using *uniformPartitioning* function, whose main part is:
```{.R}
s1 <- seq(0, len-2*h, by = h)
s2<-seq(h, len-h, by= h)
s3<-seq(2*h, len, by = h)
A<-Map(c, s1,s2,s3)
```
Where len is the length of vector of *x* values and *h* is fuzzy partition horizon picked by user.  
The three functions responsible for calculating function approximation, *get_F*,  *get_beta0* and *get_beta1*. Each of them works with accordance to aforementioned algorithms (report section 2).  
Hence in order to get $\beta^0$, we calculate weighted averages of the functional values (provided as the data) where weights are the membership degrees. Before diving into *get_beta0* itself, let's take a look at how we calculate the weights. 

```{r}
func <- function(Ai, x) {
  ci<-Ai[1]
  ciii<-Ai[length(Ai)]
  cii<-median(Ai)
  ifelse(x>=ci & x<cii, (x-ci)/(cii-ci), ifelse(x>=cii & x<=ciii, (ciii-x)/(ciii-cii), 0))
}
```

2. Plot our results.  
```{.r}
plotPartitions(A, satisfactoryB1, y, x, maxVal, minVal)
lines(F*100, col="green")
lines(y*100, col="red")
```
takes care of plotting, where *plotPartitions()* plots the partitions we obtained earlier, colouring ones with structural breaks.  
We first create an empty plot with preset limitations based on x amount of data points to plot, minimal and maximal values to be plotted as well as the the partitions. *polygon()* function creates polygons indicating partitions, which are then scaled based on maximal value so that they don't appear completely ridiculous in some extreme cases.
```{.r}
plot(NULL, xlim=c(1,length(x_years)), ylim=c(minVal*100-1, maxVal*100+1), 
  ylab="GDP annual growth (%)", xlab="Time (years)", xaxt="n")
scale<-maxVal/7.5*100 # maxVal & minVal are the maximum and minimum value to be plotted
for (i in 1:len){
  Ai <- A[[i]]
  x <- seq(Ai[1], Ai[length(Ai)], 0.1)
  if(!D[i]) { # condition used to colour partition if it contains a break
    polygon(c(Ai[1], Ai[2], Ai[3]), c(func(Ai, x)[1], func(Ai, x)[func(Ai, x)==1]*scale, 
      func(Ai, x)[length(func(Ai, x))]), col="yellow", lty=3)
  } else {
    polygon(c(Ai[1], Ai[2], Ai[3]), c(func(Ai, x)[1], func(Ai, x)[func(Ai, x)==1]*scale, 
      func(Ai, x)[length(func(Ai, x))]), lty=3)
  }
}
# setting xticks as years instead of raw x values
axis(1, at=seq(1,length(x_years)), labels=x_years, las=2) 
```
