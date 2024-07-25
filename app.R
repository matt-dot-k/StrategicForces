require(shiny)
require(bslib)
require(tidyverse)
require(RColorBrewer)

source("theme.R")

# Load data --------
strategicForces <- read.csv("./data/strategicForces.csv")

ui <- fluidPage(
    fluidRow(
        column(3,
            fluidRow(
                column(9, wellPanel(
                    style = "height: 250px",
                    p(tags$em("Click on a point on the plot for more details")),
                    htmlOutput("info_click")))),
            fluidRow(
                column(9, wellPanel(
                    selectInput(
                        inputId = "triadLeg",
                        label = "Select a Triad Leg",
                        c("Air", "Land", "Sea")),
                    selectInput(
                        inputId = "deployment",
                        label = "Select a Deployment",
                        c("Delivery Vehicle", "Warhead")),
                    sliderInput(
                        inputId = "year",
                        label = "Select a Date Range",
                        min = 1945, max = 2023,
                        value = c(1945, 2023), sep = ""))))),
        column(9, wellPanel(
            plotOutput("forcesChart", height = 720, click = "infoClick"))
        )
    )
)

server <- function(input, output) {

    # Get data from UI settings --------
    getData <- reactive({
        req(input$triadLeg)
        req(input$deployment)
        req(input$year)

        data <- strategicForces %>%
            filter(
                Leg == input$triadLeg & Type == input$deployment) %>%
            filter(
                Year >= input$year[1] & Year <= input$year[2])
    })

    # Draw chart --------
    output$forcesChart <- renderPlot({
        legend_title <- switch(
            input$deployment,
            "Delivery Vehicle" = "Platform",
            "Warhead" = "Warhead")
            
        ggplot(
            data = getData(),
            aes(x = Year, y = Number, color = System)
        ) +
            geom_point(
                size = 2.0
        ) +
            geom_line(
                linewidth = 2.0
        ) +
            labs(
                x = "Year", y = "Number", color = legend_title
        )
    })

    # Get chart info --------
    output$info_click <- renderPrint({

        info_str <- function(click) {
                        
            info <- if(nrow(click) == 0) {
                if (input$deployment == "Delivery Vehicle") {
                    p(tags$b("System: "), br(),
                      tags$b("Range: "), br(),
                      tags$b("CEP: "), br(),
                      tags$b("Guidance: "), br(),
                      tags$b("Payload: "), br())
                } else if(input$deployment == "Warhead") {
                    p(tags$b("System: "), br(),
                      tags$b("Designer: "), br(),
                      tags$b("Mass: "), br(),
                      tags$b("Detonation Mechanism: "), br(),
                      tags$b("Blast Yield: "), br())
                }
            } else if(nrow(click >= 0)) {
                if (input$deployment == "Delivery Vehicle") {
                    p(tags$b("System: ",), tags$em(click$System), br(),
                      tags$b("Range: "), tags$em(click$Info_1), br(),
                      tags$b("CEP: "), tags$em(click$Info_2), br(),
                      tags$b("Guidance: "), tags$em(click$Info_3), br(),
                      tags$b("Payload: "), tags$em(click$Info_4), br())
                } else if(input$deployment == "Warhead") {
                    p(tags$b("System: "), tags$em(click$System), br(),
                      tags$b("Designer: "), tags$em(click$Info_1), br(),
                      tags$b("Mass: "), tags$em(click$Info_2), br(),
                      tags$b("Detonation Mechanism: "), tags$em(click$Info_3), br(),
                      tags$b("Blast Yield: "), tags$em(click$Info_4), br())
                }
            } else {
                NA
            }               
            return(info)
        }

        info_str(nearPoints(
            getData() %>%
                filter(Leg == input$triadLeg & Type == input$deployment) %>%
                filter(Year >= input$year[1] & Year <= input$year[2]),
            coordinfo = input$infoClick) %>%
            select(System, Info_1, Info_2, Info_3, Info_4))
    })
}

shinyApp(ui = ui, server = server)