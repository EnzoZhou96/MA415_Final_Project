#ShinyApp 1


library(shiny)
library(ggplot2)

ui <- fluidPage(
  headerPanel('Plots for Residential Fire Fatalities of 2006 ~ 2015'),
  sidebarPanel(
    selectInput('year', 'Which year?',c('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015'))
  ),
  mainPanel(
    plotOutput('plot1'),
    
    textOutput('text1')
  )

  
)


server <- function(input, output){
  
  selectedData <- reactive({
    
    if(input$year == "2006")
    {
      selectedData <- clean2006location
    }
    else if(input$year == "2007")
    {
      selectedData <- clean2007location
    }
    else if(input$year == "2008")
    {
      selectedData <- clean2008location
    }
    else if(input$year == "2009")
    {
      selectedData <- clean2009location
    }
    else if(input$year == "2010")
    {
      selectedData <- clean2010location
    }
    else if(input$year == "2011")
    {
      selectedData <- clean2011location
    }
    else if(input$year == "2012")
    {
      selectedData <- clean2012location
    }
    else if(input$year == "2013")
    {
      selectedData <- clean2013location
    }
    else if(input$year == "2014")
    {
      selectedData <- clean2014location
    }
    else if(input$year == "2015")
    {
      selectedData <- clean2015location
    }
   
  })
  
  selectedData_filter <- reactive({
    selectedData %in% input$year
  })
  
  
  output$plot1 <- renderPlot({
    ggplot()  + annotation_raster(usa, -Inf, Inf, -Inf, Inf, interpolate = TRUE)  + geom_point(data = selectedData_filter(), mapping=aes(x=longitude, y=latitude, color=factor(CAUSE_IGN) )) + coord_equal(ratio = 1) + ggtitle("Residential Fire Fatalities")
  })
  
  

}

  

shinyApp(ui = ui, server = server)

