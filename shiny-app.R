rm(list = ls())
library(leaflet)
library(RColorBrewer)
library(shiny)
library(spData)
library(tidyverse)
library(xts)

strike_data <- read_csv("data/processed/strike-data-geocode.csv") %>%
  mutate(president = case_when(
    date < as.Date("2009-01-20") ~ "Bush",
    date < as.Date("2017-01-20") ~ "Obama",
    TRUE ~ "Trump"
  )) %>%
  mutate_at("date", as.Date)

# Color palette for strike timeline
## Use an exponential scale because fewer strikes have larger death tolls
paletteBins <- c(0, exp(0:6)) %>% ceiling()
## Upper bound should be the maximum number of people killed in a single strike
paletteBins[length(paletteBins)] <- max(strike_data$max_ppl_killed)
colorPalette <- colorBin(palette = "YlOrBr",
                         domain = strike_data$max_ppl_killed,
                         bins = paletteBins)


# User Interface ----
ui <- fluidPage(
  dateRangeInput(inputId = "date_range",
                 strong("Range of Strike Dates"),
                 start = "2002-11-03", end = "2020-02-19",
                 min = "2002-11-03", max = "2020-02-19"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "Countries",
        label = "Countries of Interest",
        choices = c("Pakistan", "Somalia", "Yemen"),
        selected = c("Pakistan", "Somalia", "Yemen")
      ),
      checkboxGroupInput(
        "Presidents",
        label = "President",
        choices = c("Bush", "Obama", "Trump"),
        selected = c("Bush", "Obama", "Trump")
      ),
      uiOutput("dateUI")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative Deaths",
                 plotOutput("cum_death", width = "100%", height = "750px")),
        tabPanel("Cumulative Deaths by Country",
                 plotOutput("country_death", width = "100%", height = "750px")),
        tabPanel("Cumulative Deaths by President",
                 plotOutput("pres_death", width = "100%", height = "750px")),
        tabPanel("Strike Map",
                 plotOutput("strike_map", width = "100%", height = "750px")),
        tabPanel("Strike Timeline",
                 leafletOutput("timeline_map", width = "100%", height = "750px"))
      )
    )
  )
)

# Server ----
server <- function(input, output) {
  col_vals <- rgb(c(0.5, 0, 0), c(0, 0.5, 0), c(0, 0, 0.5))
  fill_vals <- rgb(c(0.5, 0, 0), c(0, 0.5, 0), c(0, 0, 0.5),
                   alpha = 0.5)
  names(col_vals) <- c("Pakistan", "Somalia", "Yemen")
  names(fill_vals) <- c("Pakistan", "Somalia", "Yemen")
  color_scheme <- reactive({
    col_vals[input$Countries]
  })
  fill_scheme <- reactive({
    fill_vals[input$Countries]
  })
  strikes <- reactive({
    strike_data %>%
      filter(between(as.Date(date), input$date_range[1], input$date_range[2]),
             country %in% input$Countries,
             president %in% input$Presidents)
  })

  # Range of casualties over time ----
  output$cum_death <- renderPlot({
    strikes() %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = cumsum(min_ppl_killed)), col = "goldenrod") +
      geom_line(aes(y = cumsum(max_ppl_killed)), col = "goldenrod") +
      geom_ribbon(aes(ymin = cumsum(min_ppl_killed), ymax = cumsum(max_ppl_killed)),
                  fill = "goldenrod", alpha = 0.5) +
      labs(title = "Cumulative Deaths from Drone Strikes",
           x = "Year",
           y = "Total Deaths from Drone Strikes")
  })

  ## Split range by country ----
  output$country_death <- renderPlot({
    strikes() %>%
      group_by(country) %>%
      mutate(country_min_killed = cumsum(min_ppl_killed),
             country_max_killed = cumsum(max_ppl_killed)) %>%
      ggplot(aes(x = date, color = country, fill = country)) +
      geom_line(aes(y = country_min_killed)) +
      geom_line(aes(y = country_max_killed)) +
      scale_color_manual(values = color_scheme()) +
      scale_fill_manual(values = fill_scheme()) +
      geom_ribbon(aes(ymin = country_min_killed, ymax = country_max_killed)) +
      labs(title = "Cumulative Deaths from Drone Strikes",
           x = "Year",
           y = "Total Deaths from Drone Strikes",
           fill = "Country",
           color = "Country") +
      facet_wrap(vars(country), ncol = 1)
  })

  ## Split range by President ---
  output$pres_death <- renderPlot({
    strikes() %>%
      group_by(president) %>%
      mutate(country_min_killed = cumsum(min_ppl_killed),
             country_max_killed = cumsum(max_ppl_killed)) %>%
      ggplot(aes(x = date, color = president, fill = president)) +
      geom_line(aes(y = country_min_killed)) +
      geom_line(aes(y = country_max_killed)) +
      geom_ribbon(aes(ymin = country_min_killed, ymax = country_max_killed)) +
      labs(title = "Cumulative Deaths from Drone Strikes",
           x = "Year",
           y = "Total Deaths from Drone Strikes",
           fill = "President",
           color = "President") +
      facet_wrap(vars(president), ncol = 1)
  })

  # Plot drone strike locations ----
  country_maps <- map_data("world") %>%
    # Bound the countries to plot based on latitude and longitude
    # Boundaries quasi-arbitrary: Used the "eye test" for a manual cutoff
    filter(long > 40, long < 80,
           lat > -5, lat < 40,
           !(region %in% c("Turkey", "China")))
  output$strike_map <- renderPlot({
    ggplot() +
      geom_rect(aes(xmin = 40, xmax = 80, ymin = -5, ymax = 40),
                fill = "lightblue", colour = "black") +
      # Turkey's region should be grey
      geom_rect(aes(xmin = 40, xmax = 45, ymin = 35, ymax = 40),
                fill = "lightgrey", colour = "black") +
      # China's region should be grey
      geom_rect(aes(xmin = 70, xmax = 80, ymin = 30, ymax = 40),
                fill = "lightgrey", colour = "black") +
      # Add the countries within the region
      geom_polygon(aes(x = long, y = lat, group = group), country_maps,
                   fill = "lightgrey", colour = "black") +
      # Plot drone strike locations
      geom_point(aes(x = long, y = lat, colour = country), data = strikes()) +
      scale_color_manual(values = color_scheme()) +
      labs(title = "Location of Drone Strikes",
           x = "Longitude",
           y = "Latitude",
           colour = "Country of Strike") +
      coord_equal() +
      theme_minimal() +
      theme(title = element_text(size = 16),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            panel.grid = element_blank())
  })

  # Timeline Map of Strike Data ----
  observe({
    # Strike dates with coordinates
    allDates <- strikes() %>%
      filter(!is.na(lat), !is.na(long)) %>%
      pull(date) %>%
      unique()
    # Range of dates by individual day
    eligibleDates <- allDates[endpoints(allDates, on = "days")]
    output$dateUI <- renderUI({
      sliderInput("dateSel",
                  label = "Date for Strike Timeline",
                  min = min(eligibleDates),
                  max = max(eligibleDates),
                  value = min(eligibleDates),
                  # Day increments
                  step = 1,
                  animate = animationOptions(interval = 100, loop = FALSE)
      )
    })
  })

  date_strikes <- reactive({
    req(input$dateSel)
    # Only look at strikes on a specific date with nonmissing coordinates
    strikes() %>%
      filter(date == input$dateSel,
             !is.na(lat), !is.na(long))
  })

  output$timeline_map <- renderLeaflet({
    leaflet(world) %>%
      # Add country boundaries
      addTiles() %>%
      # Area of interest: Middle East
      fitBounds(lng1 = 40, lat1 = -5, lng2 = 80, lat2 = 40) %>%
      leaflet::addLegend(pal = colorPalette,
                         values = strikes()$max_ppl_killed,
                         opacity = 0.9,
                         title = "Maximum Total Deaths",
                         position = "bottomleft")
  })

  observe({
    # Update for each individual date
    leafletProxy("timeline_map", data = date_strikes()) %>%
      # Remove previous strike marks
      clearMarkers() %>%
      addCircleMarkers(lng = ~long,
                       lat = ~lat,
                       radius = ~max_ppl_killed,
                       weight = 1,
                       opacity = 1,
                       color = "black",
                       fillColor = ~colorPalette(max_ppl_killed),
                       fillOpacity = 0.8)

  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
