shinyServer(
function(input, output, session){
  
  output$Plot<-renderPlot({
    country<-input$Country
    chosenh<-input$`Fuzzy Partition Horizon`
    
    data<-read.csv2("gdpgrowth.csv")
    x<-data$year
    switch(country,
           "Australia"={y<-data$australia },
           "France"={y<-data$france },
           "Côte d'Ivoire"={y<-data$cotedivorine},
           "Germany"={y<-data$germany},
           "Iceland"={y<-data$iceland}, 
           "Iran"={y<-data$iran},
           "Spain"={y<-data$iran}
           )
    
    plot_breaks(x,y, chosenh)
    
  })
  
  
}

)
