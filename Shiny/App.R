library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

load("Data/proportions.rda")
load("Data/parties.rda")
load("Data/datamap.rda")
load("Data/states.rda")

ui<-fluidPage(
  
  tags$style(HTML("
                  @import url('https://fonts.googleapis.com/css?family=Poppins');
                  
                  body {
                  
                  font-family: 'Poppins', 'Lucida Grande', Verdana, Lucida, Helvetica, Arial, Calibri, sans-serif;
                  color: rgb(0,0,0);
                  background-color: #d2d2d2;
                  }
                  ")),
  
  titlePanel("Australia Election 2016"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("party", "Select a party to add to the map",
                  choices = c("", parties$party), selected = "", 
                  size = , selectize = FALSE),
      actionButton("clear1", "Clear all parties"),
      p(),
      plotOutput("leg", height = "100px"),
      p(),
      sliderInput("sc", "Scale size of circles (also redraws map to show only the last added party)",
                  min = 0.5, max = 5, value = 1, step = 0.1),
      p(),
      selectInput("state", "Select a state to add to the map",
                  choices = c("", states$state), selected = "", 
                  size = , selectize = FALSE),
      actionButton("clear1", "Clear all states"),
      p(),
      p("This plot is used to visualize area size of each state"),
      plotOutput("MyPlot1", height= "100px"),
      h2("About"),
      HTML("<p>Created by Fang Zhou with R and Shiny.  R users can download the 
           cleaned and tidy data from <a href = 'https://github.com/ellisp/nzelect'>https://github.com/ellisp/nzelect</a>.  
           Original source is <a href = 'http://www.electionresults.govt.nz/'>
           http://www.electionresults.govt.nz/</a>")
      ),
    
    
    mainPanel(
      leafletOutput("MyMap", height = 800)
      
    )
      )
    )

server<-function(input, output, session) {
  # input <- data.frame(party = "ALP")
  the_data_polling <- reactive({
    tmp <- proportions %>%
      filter(PartyAb == input$party)
    
    if(input$party != ""){
      thecol <- data.frame(parties)[parties$party == input$party, "colour"]
    } else {
      tmp <- proportions[1,]
      thecol <- NULL
      
    }
    
    return(list(df = tmp, thecol = thecol))
  })
  
  the_data_state <- reactive({
    tmp <- datamap %>%
      filter(STATE== input$state)
    
    if(input$state != ""){
      thecol <- data.frame(states)[states$state == input$state, "colour"]
    } else {
      tmp <- datamap[1,]
      thecol <- NULL
      
    }
    
    return(list(df = tmp, thecol = thecol))
  })
  
  output$MyMap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Watercolor") %>%
      addProviderTiles("Stamen.TonerLabels") %>%
      fitBounds(112, -11, 154, -44)
  })
  
  observe({
    leafletProxy("MyMap", data = the_data_polling()$df) %>%
      addCircleMarkers(~Longitude, 
                       ~Latitude,
                       color = the_data_polling()$thecol,
                       radius = ~prop * 30 * input$sc,
                       popup = ~lab) 
  })
  
  observe({
    x <- input$clear1
    updateSelectInput(session, "party", selected = "")
    leafletProxy("MyMap") %>% clearMarkers()
  })
  
  observe({
    x <- input$sc
    leafletProxy("MyMap") %>% clearMarkers()
  })
  
  #observe({
  # leafletProxy("MyMap", data = the_data_state()$df) %>%
  #    addPolylines(~long, 
  #                 ~lat,
  #                color = the_data_state()$thecol
  #                    ) 
  #})
  
  observe({
    leafletProxy("MyMap", data = the_data_state()$df) %>%
      addCircleMarkers(~longcen, 
                       ~latcen,
                       color = the_data_state()$thecol,
                       popup = ~lab
                       
      ) 
  })
  
  observe({
    x <- input$clear1
    updateSelectInput(session, "state", selected = "")
    leafletProxy("MyMap") %>% clearMarkers()
  })
  
  observe({
    x <- input$sc
    leafletProxy("MyMap") %>% clearMarkers()
  })
  
  output$MyPlot1 <- renderPlot({
    ggplot(datamap,aes(x=STATE,y=AREA_SQKM,fill=STATE))+
      geom_bar(stat="identity") 
  })
  
  
  output$leg <- renderPlot({
    par(mar = c(0,0,0,0))
    plot(1:10, 1:10, type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "")
    legend(1, 10, parties$party, col = parties$colour, pch = 19, bty = "n", 
           cex = 1.3, xjust = 0, yjust = 1, pt.cex = 2, ncol = 2)
  })
  
  
  
}

shinyApp(ui,server)
