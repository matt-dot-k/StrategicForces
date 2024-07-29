library(shiny)
library(bslib)
library(tidyverse)
library(ggthemr)
library(DT)
library(maps)

ggthemr("fresh")
source("theme.R")

# Load data --------
strategicForces <- read.csv("./data/strategicForces.csv")
nuclearStockpiles <- read.csv("./data/nuclearStockpiles.csv")
nuclearFacilities <- read.csv("./data/nuclearFacilities.csv")

# User interface layout
ui <- page_navbar(
    title = "Strategic Nuclear Forces",
    id = "strategicForces",
    inverse = TRUE,
    theme = bs_theme(version = 5, bootswatch = "shiny"),
    nav_panel("Force Structure",
        fluidRow(
            column(9,
                navset_card_underline(
                    title = p(tags$h5("Evolution of Deployed U.S. Strategic Forces")),
                    nav_panel(
                        p(tags$b("Chart")), plotOutput("forcesChart", height = 760, click = "sys_click")),
                    nav_panel(
                        p(tags$b("Tables")), DTOutput("forcesTable")))),
            column(3,
                fluidRow(
                    column(12, card(
                        card_header(p(tags$h5("Data Options"))),
                        selectInput(
                            inputId = "triadLeg",
                            label = "Select a triad leg",
                            c("Air", "Land", "Sea")),
                        selectInput(
                            inputId = "deployment",
                            label = "Select a deployment",
                            c("Delivery Vehicle", "Warhead")),
                        sliderInput(
                            inputId = "year_1",
                            label = "Select a date range",
                            min = 1945, max = 2023,
                            value = c(1945, 2023), sep = "")))),
                fluidRow(
                    column(12, card(
                        p(tags$b("Click on a point on the plot to view some details about the system"))),
                        card(
                            card_header(p(tags$img(src="stratcom.png", width = 64, height = 64, style="float:left; margin-right: 15px"), tags$h5("System Info"))),
                            style = "height: 350px",
                            htmlOutput("systemInfo"))))
            )
        )
    ),
    nav_panel("Nuclear Stockpiles",
        fluidRow(
            column(9, 
                navset_card_underline(
                    title = p(tags$h5("Global Warhead Stockpiles and Nuclear Tests")),
                    nav_panel(
                        p(tags$b("Chart")), plotOutput("stockpileChart", height = 760, click = "treaty_click")),
                    nav_panel(
                        p(tags$b("Tables")), 
                        fluidRow(
                            column(5, card(
                                card_header(p(tags$h6("Summary Statistics"))),
                                DTOutput("stats"))),
                            column(7, card(
                                card_header(p(tags$h6("Data Table"))),
                                DTOutput("stockpileTable"))))))),
            column(3,
                fluidRow(
                    column(12, card(
                        card_header(p(tags$h5("Data Options"))),
                        selectInput(
                            inputId = "stockpileCategory",
                            label = "Select a data series",
                            c("Warhead Stockpiles", "Nuclear Tests")),
                        selectInput(
                            inputId = "country",
                            label = "Select countries to highlight",
                            choices = unique(nuclearStockpiles$Country),
                            multiple = TRUE),
                        sliderInput(
                            inputId = "year_2",
                            label = "Select a date range",
                            min = 1945, max = 2023,
                            value = c(1945, 2023), sep = ""),
                        checkboxInput(
                            inputId = "smoother",
                            label = "Show trend lines?",
                            value = FALSE),
                        checkboxInput(
                            inputId = "armsControl",
                            label = "Show major arms control treaties?",
                            value = FALSE)))),
                fluidRow(
                    column(12, card(
                        p(tags$b("Click on an arms control treaty to view some info about the treaty"))),
                        card(
                            style = "height: 260px",
                            htmlOutput("treatyInfo"))))
            )
        )
    ),
    nav_panel("Nuclear Weapons Complex",
        fluidRow(
            column(9, card(
                    card_header(p(tags$h5("U.S. Nuclear Weapons Facilities"))),
                    plotOutput("facilityMap", height = 720, click = "facility_click"))),
            column(3,
                fluidRow(
                    column(12, card(
                        p(tags$b("Click on a facility to view some more information about the facility"))),
                        card(align = "center",
                            imageOutput("facilityImage"),
                            style = "height: 250px"),
                        card(
                            htmlOutput("facilityInfo"),
                            style = "height: 300px")))
            )
        )
    ),
    nav_panel("About",
        fluidRow(
            column(12, card(
                includeHTML("about.html")))
        )
    )
)

# Server logic
server <- function(input, output, session) {

    # Get data from UI settings --------
    forceData <- reactive({
        req(input$triadLeg)
        req(input$deployment)
        req(input$year_1)

        data <- strategicForces %>%
            as_tibble() %>%
            filter(
                Leg == input$triadLeg & Type == input$deployment) %>%
            filter(
                Year >= input$year_1[1] & Year <= input$year_1[2])
    })

    stockpileData <- reactive({
        req(input$stockpileCategory)
        req(input$year_2)
        req(input$country %in% nuclearStockpiles$Country)

        data <- nuclearStockpiles %>%
            filter(
                Type == input$stockpileCategory) %>%
            filter(
                Year >= input$year_2[1] & Year <= input$year_2[2]) %>%
            filter(
                Country %in% input$country) %>%
            select(-Type, -X)
    })

    facilityData <- reactive({
        data <- nuclearFacilities
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
                linewidth = 2.0
        ) +
            geom_point(
                size = 2.5
        ) +
            labs(
                x = "Year", y = "Number", color = legend_title
        ) +
            scale_x_continuous(
                breaks = seq(input$year_1[1], input$year_1[2], 1)
        ) + theme

    }, res = 96)

    output$forcesTable <- renderDT({
        datatable(forceData() %>% 
            select(-X, -Info_1, -Info_2, -Info_3, -Info_4) %>%
            replace(is.na(.), 0)
        )
    }, height = 720)

    # Get chart info
    output$systemInfo <- renderPrint({
        req(input$sys_click, cancelOutput = TRUE)

        system_info_str <- function(click) {
                        
            info <- if(nrow(click) == 0) {
                if (input$deployment == "Delivery Vehicle") {
                    p(tags$h5(""),
                      tags$b("Range: "),
                      tags$b("CEP: "),
                      tags$b("Guidance: "),
                      tags$b("Payload: "))
                } else if(input$deployment == "Warhead") {
                    p(tags$h5(""),
                      tags$b("Designer: "),
                      tags$b("Mass: "),
                      tags$b("Detonation Mechanism: "),
                      tags$b("Blast Yield: "))
                }
            } else if(nrow(click >= 0)) {
                if (input$deployment == "Delivery Vehicle") {
                    p(tags$h5(click$System),
                      tags$b(paste0("Range: ", click$Info_1)),
                      tags$b(paste0("CEP: ", click$Info_2)),
                      tags$b(paste0("Guidance: ", click$Info_3)),
                      tags$b(paste0("Payload: ", click$Info_4)))
                } else if(input$deployment == "Warhead") {
                    p(tags$h5(click$System),
                      tags$b(paste0("Designer: "), click$Info_1),
                      tags$b(paste0("Mass: ", click$Info_2)),
                      tags$b(paste0("Detonation Mechanism: ", click$Info_3)),
                      tags$b(paste0("Blast Yield: ", click$Info_4)))
                }
            } else {
                NA
            }               
            return(info)
        }

        system_info_str(nearPoints(
            forceData() %>%
                filter(Leg == input$triadLeg & Type == input$deployment) %>%
                filter(Year >= input$year_1[1] & Year <= input$year_1[2]),
            coordinfo = input$sys_click) %>%
            select(System, Info_1, Info_2, Info_3, Info_4) %>%
            slice_head(n = 1)
        )
    })

    # Stockpiles tab --------
    # Update data with arms control agreements
    arms_control <- reactive({
        req(input$armsControl)

        data <- tibble(
            Year = c(1963, 1968, 1972, 1979, 1987, 1996, 2010),
            Number = 1.05 * max(stockpileData()$Number),
            Agreement = c(
                "PTBT", "NPT", "SALT I", "SALT II", "INF", "CTBT", "New START"),
            Info = c(
                "The Partial Test Ban Treaty (PTBT) was the first major treaty concerning nuclear tests, and prohibited signatories from conducting nuclear weapons tests in the atmosphere, outer-space, or underwater.",
                "Aimed at preventing the proliferation, the Non Proliferation Treaty (NPT) requires non-nuclear states to commit to not acquiring nuclear weapons, and states with nuclear weapons to commit to disarmament.",
                "Strategic Arms Limitation Talks (SALT I) was the first round of arms control talks between the US and USSR. Negotiations between both sides led to the signing of the ABM Treaty, which put limits on deployments of ballistic missile defence systems.",
                "A continuation of the SALT I agreement, SALT II resulted in the US and USSR placing reciprocal limits on deployments of ICBMs, SLBMs, bombers, MIRV systems, and also prohibited construction of new ICBM launchers.",
                "Signed shortly after NATO's deployment of Pershing II MRBMs and BGM-109 cruise missiles to Western Europe, the Intermediate Nuclear Forces (INF) treaty resulted in the elimination of missiles with ranges between 500 and 5,500km.",
                "A follow-up to the PTBT, the Comprehensive Test Ban Treaty (CTBT) prohibits all tests involving nuclear detonations. However, eight signatories have not yet ratified the treaty, and so it has not entered into force.",
                "New Strategic Arms Reduction Treaty (START) marked another round of arms reductions between the US and Russia, and placed new aggregate limits on deployed strategic warheads, limiting each country to 1,550 deployed warheads each.")
        )
    })

    # Render chart and data table
    output$stockpileChart <- renderPlot({

        label <- switch(
            input$stockpileCategory,
            "Warhead Stockpiles" = "Warheads",
            "Nuclear Tests" = "Tests")

        chart <- ggplot(
        ) +
            geom_line(
                data = stockpileData(),
                aes(x = Year, y = Number, color = Country),
                linewidth = 2.0
        ) +
            labs(
                x = "Year", y = label, color = "Country"
        ) +
            scale_y_continuous(
                limits = c(0, 1.05 * max(stockpileData()$Number))
        ) + 
            scale_x_continuous(
                breaks = seq(input$year_2[1], input$year_2[2], 1)
        ) +
            theme
        
        if(input$smoother == TRUE) {
            chart <- chart +
                geom_smooth(
                    data = stockpileData(),
                    aes(x = Year, y = Number, group = Country),
                    method = "gam", se = FALSE,
                    linewidth = 0.7,
                    alpha = 0.5,
                    color = "#000000"
            )
        } else {
            chart <- chart
        }

        if(input$armsControl == TRUE) {
            chart <- chart +
                geom_vline(
                    data = arms_control(),
                    aes(xintercept = Year),
                    linewidth = 0.8,
                    linetype = "dashed",
                    alpha = 0.7,
                    color = "gray30"
            ) +
                geom_label(
                    data = arms_control(),
                    aes(x = Year, y = 1.05 * max(stockpileData()$Number), label = Agreement)
            )
        } else {
            chart <- chart
        }

        chart

    }, res = 96)

    output$stockpileTable <- renderDT({
        datatable(stockpileData())
    })

    output$stats <- renderDT({
        datatable(stockpileData() %>%
            group_by(Country) %>%
            reframe(
                Avg = round(mean(Number), 2), Min = min(Number), Max = max(Number))
        )
    })

    # Get arms treaty info
    output$treatyInfo <- renderPrint({
        req(input$treaty_click)
        
        treaty_info_str <- function(click) {

            info <- if(nrow(click) == 0) {
                p(tags$h6(""), tags$em(""))
            } else if(nrow(click) >= 1) {
                p(tags$h6(click$Agreement), tags$em(click$Info))
            } else {
                NA
            }
            return(info)
        }

        treaty_info_str(nearPoints(
            arms_control(),
            coordinfo = input$treaty_click,
            threshold = 10) %>%
            select(Agreement, Info)
        )
    })

    # Facilities tab --------
    # Render map
    output$facilityMap <- renderPlot({

        ggplot() +
            geom_polygon(
                data = map_data("state"),
                aes(x = long, y = lat, group = group),
                fill = "white",
                color = "black",
                linewidth = 1.0
        ) +
            geom_point(
                data = facilityData(),
                aes(x = long, y = lat, color = Category),
                size = 4.0
        ) +
            labs(
                color = "Facility Type"
        ) +
            theme +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "bottom"
        )
    }, res = 96)

    # Get facility info
    output$facilityInfo <- renderPrint({
        req(input$facility_click)

        facility_info_str <- function(click) {

            info <- if(nrow(click) == 0) {
                p(tags$h5(""), tags$em(""))
            } else if(nrow(click) >= 1) {
                p(tags$h5(click$Facility), tags$em(click$Description))
            } else {
                NA
            }
            return(info)
        }

        facility_info_str(nearPoints(
            facilityData(),
            coordinfo = input$facility_click,
            threshold = 10)
        )
    })

    output$facilityImage <- renderImage({
        req(input$facility_click)

        outfile <- paste0("./www/", nearPoints(facilityData(), coordinfo = input$facility_click)$Facility, ".jpg")

        list(
            src = outfile, 
            height = "225px")

    }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)

