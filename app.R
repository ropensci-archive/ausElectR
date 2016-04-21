#' Shiny app for exploring census and electorate data
#' 
#' @import shiny
#' @import ggplot2
#' @importFrom tidyr gather
#' @export
#' @examples \dontrun{
#' launchApp()
#' }

launchApp <- function() {
  data("abs2011", package = "echidnaR")
  longAbs <- tidyr::gather(abs2011, variable, value, -ID, -Name, -State)
  longAbs$value <- as.numeric(longAbs$value)
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
      selected = c("Brisbane", "Melbourne"), multiple = TRUE
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
}





