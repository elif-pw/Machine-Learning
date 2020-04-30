names<-colnames(read.csv2("data_all_4.csv"))

shinyUI(
  pageWithSidebar(
    headerPanel("Economic Growth Structural Break Detection App"),
    sidebarPanel(
      selectInput("countryInput","Select country",
                  choices = names[names!="year"]),
      sliderInput("horizonInput","Fuzzy Partition Horizon",
                  min=2, max=10,value=6, step=1)
    ),
    mainPanel(
      plotOutput("Plot"),
      textOutput("RightContextStr"),
      textOutput("RightContextVal"),
      textOutput("YearStr"),
      textOutput("Years"),
      textOutput("B1Str"),
      textOutput("B1Values"),
    )
  )
)
