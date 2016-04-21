library(readr)
library(leaflet)
library(shiny)
library(plotly)
library(tidyr)


if (!exists("abs2011")) source("ReadABS.R")
nat_map <- read_csv("AECdata/National-map.csv")

nat_map$ELECT_DIV <- tolower(nat_map$ELECT_DIV)
abs2011$ELECT_DIV <- tolower(abs2011$Name)

longAbs <- gather(abs2011, variable, value, -ID, -Name, -State)
longAbs$value <- as.numeric(longAbs$value)
longAbs$Name <- tolower(longAbs$Name)
longAbs <- longAbs[!is.na(longAbs$value),]

idx <- grepl("^Age", longAbs$variable)
ages <- longAbs[idx, ]
other <- longAbs[!idx, ]
ages$variable <- factor(
  ages$variable, 
  levels = unique(ages$variable)
)


electorates <- unique(abs2011$ELECT_DIV)

ui <- fluidPage(
  selectizeInput(
    "electorates", "Select electorate(s):", choices = electorates, 
    selected = c("brisbane", "melbourne"), multiple = TRUE
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("ages", height = 800)
    ),
    column(
      width = 6,
      plotOutput("densities", height = 1000)
    )
  )
)

server <- function(input, output) {
  
  output$ages <- renderPlot({
    p <- ggplot(ages, aes(value)) + geom_density()
    if (!is.null(input$electorates)) {
      d <- ages[ages$Name %in% input$electorates, ]
      p <- p + geom_vline(data = d, aes(xintercept = value, color = Name))
    }
    p + 
      facet_wrap(~variable, ncol = 1) + 
      labs(x = NULL, y = NULL) + theme(legend.position = "none")
  })
  
  
  output$densities <- renderPlot({
    p <- ggplot(other, aes(value)) + geom_density()
    if (!is.null(input$electorates)) {
      d <- other[other$Name %in% input$electorates, ]
      p <- p + geom_vline(data = d, aes(xintercept = value, color = Name))
    }
    p + 
      facet_wrap(~variable, scales = "free", ncol = 1) + 
      labs(x = NULL, y = NULL)
  })
  
}

shinyApp(ui, server)