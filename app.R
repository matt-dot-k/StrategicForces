require(tidyverse)
require(shiny)
require(ggthemes)
require(bslib)

# Read data for triad legs ---

air_forces <- list(
    systems = read.csv("data/air_systems.csv"),
    warheads = read.csv("data/air_warheads.csv")
)

ggplot(
    data = air_forces$systems %>% as_tibble(),
    aes(x = Year, y = Number, color = System)
) +
    geom_line()

land_forces <- list(
    systems = read.csv("data/land_systems.csv"),
    warheads = read.csv("data/land_warheads.csv")
)

sea_forces <- list(
    systems = read.csv("data/sea_systems.csv"),
    warheads = read.csv("data/sea_warheads.csv")
)

# Define UI ----

ui <- page_sidebar(
    title = "U.S. Strategic Nuclear Forces",
    sidebar = sidebar(
        selectInput(
            inputId = "triadLeg",
            label = "Select a Triad Leg",
            c("Air", "Land", "Sea")
        ),
        selectInput(
            inputId = "deployment",
            label = "Select a Deployment",
            c("Delivery Vehicles", "Warheads")
        ),
        sliderInput(
            inputId = "Year",
            label = "Select a Date Range",
            min = 1945,
            max = 2023,
            value = c(1945, 2023)
        )
    ),
    mainPanel(
        plotOutput("plot")
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        data <- switch(
            input$triadLeg,
            "Air" = air_forces,
            "Land" = land_forces,
            "Sea" = sea_forces
        )
        data <- switch(
            input$deployment,
            "Delivery Vehicles" = data$systems,
            "Warheads" = data$warheads
        )
        legend_title <- switch(
            input$deployment,
            "Delivery Vehicles" = "Platform",
            "Warheads" = "Warhead"
        )
        ggplot(
            data = data,
            aes(x = Year, y = Number, color = System)
        ) +
        geom_point(
            size = 1.2
        ) +
        geom_line(
            linewidth = 1.2
        ) +
        labs(
            x = "Year",
            y = "Number",
            color = legend_title
        )
    })
}

shinyApp(ui = ui, server = server)
