
##This app is using one year of sleep data from August 2019 to August 2020
##Building off of the Kmeans app
## install.packages('rsconnect') and load its library(rsconnect)
## configure your R instance and rsconnect with your shinyapps.io account 
##rsconnect::setAccountInfo(name='wesleycoates',
##token='<BLARG>',
##secret='<SECRET>')
## install readxl if you must: install.packages("readxl")
library("readxl")

## create a dataframe from the Excel file
sleep_scores <- read_xlsx("sleep_scores_Aug2019_2020.xlsx", 
                          sheet = "SleepScores")

## need to omit first 2 fields from this dataframe, as they won't work in Kmeans
sleep_kmeans <- sleep_scores[c(3,4,5,6,7,8,9)]

## now it's time to create the web app!
# if needed install.packages('shiny')
library(shiny)

# User Interface side of the code
ui <- fluidPage(
  headerPanel("Matt's sleep k-means clustering"),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(sleep_kmeans)),
    selectInput('ycol', 'Y Variable', names(sleep_kmeans),
                selected = names(sleep_kmeans)[[2]]),
    numericInput('clusters', 'Cluster count', 1, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

# And this is the server-side of the code
server <- function(input, output) {
  
  selectedData <- reactive({
    sleep_kmeans[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })  
}

shinyApp(ui = ui, server = server)
