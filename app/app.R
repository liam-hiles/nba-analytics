# RShiny ----

## Library ----

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

## Data ----

lakers_colours <- c("AD Era" = "#FDB927", "Luka Era" = "#552583")

shot_zones <- readRDS("shot_zones.rds")

player_choices <- sort(unique(shot_zones$player_name))

## UI ----

ui <- fluidPage(
  titlePanel("Lakers Evolution Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId  = "player",
        label    = "Choose a player",
        choices  = player_choices,
        selected = "Rui Hachimura"
      ),
      p("Each bar shows the share of that player's attempts from a given zone.
         Compare the AD Era (before 1 Feb 2025) to the Luka Era (from 1 Feb 2025).")
    ),
    mainPanel(
      width = 9,
      plotOutput("zone_bars", height = "500px")
    )
  )
)


server <- function(input, output, session) {
  output$zone_bars <- renderPlot({
    shot_zones |>
      filter(player_name == input$player) |>
      ggplot(aes(x = shot_zone_basic, y = share, fill = era)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = lakers_colours) +
      labs(title = "Rui Hachimura: shot distribution by zone",
           x = NULL, y = "Share of attempts", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
