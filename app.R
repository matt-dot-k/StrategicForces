require(shiny)
require(bslib)
require(shinythemes)
require(DT)
require(tidyverse)
require(ggthemes)
require(RColorBrewer)

source("theme.R")

# Load data --------
strategicForces <- read.csv("./data/strategicForces.csv")
nuclearStockpiles <- read.csv("./data/nuclearStockpiles.csv")

ui <- navbarPage(
    title = "Strategic Nuclear Forces",
    id = "strategicForces",

    tabPanel("Force Structure",
        fluidRow(
            column(9, card(
                card_header(p(tags$h4("Evolution of Deployed U.S. Strategic Forces"))),
                plotOutput("forcesChart", height = 720, click = "plot_click"))),
            column(3,
                fluidRow(
                    column(9, card(
                        card_header(p(tags$h4("System Info"))),
                        style = "height: 300px",
                        p(tags$em("Click on a point on the plot to view some details about the system")),
                        htmlOutput("info_click")))),
                fluidRow(
                    column(9, card(
                        card_header(p(tags$h4("Data Options"), br())),
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
                            value = c(1945, 2023), sep = ""))))
            )
        )
    ),
    tabPanel("Nuclear Stockpiles",
        fluidRow(
            column(9, 
                navset_card_underline(
                    title = p(tags$h4("Visualisations")),
                    nav_panel(
                        p(tags$b("Plot")), plotOutput("stockpileChart", height = 720)),
                    nav_panel(
                        p(tags$b("Tables")), DTOutput("stockpileTable")))),
            column(3,
                fluidRow(
                    column(9, card(
                        card_header(p(tags$h4("Data Options"), br())),
                        selectInput(
                            inputId = "stockpileCategory",
                            label = "Select a Data Series",
                            c("Warhead Stockpiles", "Nuclear Tests")),
                        selectInput(
                            inputId = "country",
                            label = "Select countries to highlight",
                            choices = unique(nuclearStockpiles$Country), 
                            selected = unique(nuclearStockpiles$Country),
                            multiple = TRUE),
                        sliderInput(
                            inputId = "year",
                            label = "Select a Date Range",
                            min = 1945, max = 2023,
                            value = c(1945, 2023), sep = ""),
                        checkboxInput(
                            inputId = "smoother",
                            label = p(tags$b("Show trend lines?"),
                            value = FALSE))))
                )
            )
        )
    )
)

server <- function(input, output, session) {

    # Get data from UI settings --------
    forceData <- reactive({
        req(input$triadLeg)
        req(input$deployment)
        req(input$year)

        data <- strategicForces %>%
            as_tibble() %>%
            filter(
                Leg == input$triadLeg & Type == input$deployment) %>%
            filter(
                Year >= input$year[1] & Year <= input$year[2])
    })

    stockpileData <- reactive({
        req(input$stockpileCategory)
        req(input$year)
        req(input$country %in% nuclearStockpiles$Country)

        data <- nuclearStockpiles %>%
            filter(
                Type == input$stockpileCategory) %>%
            filter(
                Year >= input$year[1] & Year <= input$year[2]) %>%
            filter(
                Country %in% input$country) %>%
            select(-Type, -X)
    })

    stockpileData <- reactive({
        req(input$stockpileCategory)
        req(input$year)
        req(input$country)

        data <- nuclearStockpiles %>%
            as_tibble() %>%
            filter(
                Type == input$stockpileCategory) %>%
            filter(
                Year >= input$year[1] & Year <= input$year[2]) %>%
            filter(
                Country %in% input$country) %>%
            select(-Type, -X)
    })

    # Force structure tab --------
    # Draw chart
    output$forcesChart <- renderPlot({

        legend_title <- switch(
            input$deployment,
            "Delivery Vehicle" = "Platform",
            "Warhead" = "Warhead"
        )
            
        ggplot(
            data = forceData(),
            aes(x = Year, y = Number, color = System)
        ) +
            geom_line(
                linewidth = 2.5
        ) +
            geom_point(
                size = 3.0,
                shape = 24
        ) +
            labs(
                x = "Year", y = "Number", color = legend_title
        ) +
            scale_color_brewer(
                palette = "Paired"
        ) +
            scale_x_continuous(
                    breaks = seq(input$year[1], input$year[2], 12)
        ) +
            theme_fivethirtyeight(
        ) +
            theme
    })

    # Get chart info --------)
    output$info_click <- renderPrint({

        info_str <- function(click) {
                        
            info <- if(nrow(click) == 0) {
                if (input$deployment == "Delivery Vehicle") {
                    p(tags$h4(""),
                      tags$b("Range: "), br(), br(),
                      tags$b("CEP: "), br(), br(),
                      tags$b("Guidance: "), br(), br(),
                      tags$b("Payload: "), br())
                } else if(input$deployment == "Warhead") {
                    p(tags$h4(""),
                      tags$b("Designer: "), br(), br(),
                      tags$b("Mass: "), br(), br(),
                      tags$b("Detonation Mechanism: "), br(), br(),
                      tags$b("Blast Yield: "), br())
                }
            } else if(nrow(click >= 0)) {
                if (input$deployment == "Delivery Vehicle") {
                    p(tags$h4(click$System),
                      tags$b("Range: "), tags$em(click$Info_1), br(), br(),
                      tags$b("CEP: "), tags$em(click$Info_2), br(), br(),
                      tags$b("Guidance: "), tags$em(click$Info_3), br(), br(),
                      tags$b("Payload: "), tags$em(click$Info_4), br())
                } else if(input$deployment == "Warhead") {
                    p(tags$h4(click$System),
                      tags$b("Designer: "), tags$em(click$Info_1), br(), br(),
                      tags$b("Mass: "), tags$em(click$Info_2), br(), br(),
                      tags$b("Detonation Mechanism: "), tags$em(click$Info_3), br(), br(),
                      tags$b("Blast Yield: "), tags$em(click$Info_4), br())
                }
            } else {
                NA
            }               
            return(info)
        }

        info_str(nearPoints(
            forceData() %>%
                filter(Leg == input$triadLeg & Type == input$deployment) %>%
                filter(Year >= input$year[1] & Year <= input$year[2]),
            coordinfo = input$plot_click) %>%
            select(System, Info_1, Info_2, Info_3, Info_4))
    })

    # Stockpiles tab --------
    # Render chart and data table
    output$stockpileChart <- renderPlot({

        y_label <- switch(
            input$stockpileCategory,
            "Warhead Stockpiles" = "Warheads",
            "Nuclear Tests" = "Tests")

        chart <- ggplot(
            data = stockpileData(),
            aes(x = Year, y = Number, color = Country)
        ) +
            geom_point(
                size = 2.0,
                shape = 24
        ) +
            geom_line(
                linewidth = 1.5
        ) +
            labs(
                x = "Year", y = y_label, color = "Country"
        ) +
            scale_color_brewer(
                palette = "Paired"
        ) +
            theme_fivethirtyeight(
        ) +
            theme
        
        if(input$smoother == TRUE) {
            chart <- chart +
                geom_smooth(
                    aes(group = Country),
                    method = "gam",
                    se = FALSE,
                    linewidth = 1.2,
                    color = "#000000"
            )
        } else {
            chart <- chart
        }

        chart

    })

    output$stockpileTable <- renderDT({
        datatable(stockpileData())
    })
}

shinyApp(ui = ui, server = server)