shinyUI(
  pageWithSidebar(
    headerPanel("Economic Growth Structural Break Detection App"),
    
    sidebarPanel(
      selectInput("Country","Select",
                  choices = c("Australia", "Côte d'Ivoire","France","Germany","Iceland", "Iran","Spain")),

      sliderInput("Fuzzy Partition Horizon","Select",
                  min=2, max=10,value=6, step=1)

    ),
    mainPanel(
      
      plotOutput("Plot")
      
    )

    
  )
  
  
)
