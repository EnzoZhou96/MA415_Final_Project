#Shinyapp 2

library(shiny)
library(foreign)

results <- read.dbf("results.dbf")

ui <- fluidPage(
  titlePanel("Number of Fatalities or Incidents"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("year", "Which year?",
                   choices = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015),
                   selected = 2006),
      selectInput("column", "Which column? 2:for Fatalities 3:for Incidents",
                  choices = c("Fatalities" <- 2, "Incidents" <- 3))
    ),
    mainPanel(
      textOutput("number"),
      br(), br(),
      tableOutput("table"),
      plotOutput("plot")
    )
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlot(ggplot()  + annotation_raster(usa, -Inf, Inf, -Inf, Inf, interpolate = TRUE)  + geom_point(data=clean2015location, mapping=aes(x=longitude, y=latitude, color=factor(CAUSE_IGN) )) + coord_equal(ratio = 1) + ggtitle("2015 Residential Fire Fatalities")) 
  output$number <- renderText(results[as.numeric(input$year)-2005,as.numeric(input$column)])
  output$table <- renderTable(results)
  
}

shinyApp(ui = ui, server = server)