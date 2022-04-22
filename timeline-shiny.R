# https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
#load R packages
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)
library(spData)
library(tidyverse)
library(shiny)


strike_data <- read_csv("data/processed/strike-data-geocode.csv") %>%
  mutate(president = case_when(
    date < as.Date("2009-01-20") ~ "Bush",
    date < as.Date("2017-01-20") ~ "Obama",
    TRUE ~ "Trump"
  )) %>%
  mutate_at("date", as.Date)


#define colorpalette for chart legend
paletteBins <- c(0, 50000, 100000, 250000, 500000, 1000000, 2500000, 5000000, 10000000) / 50000
colorPalette <- colorBin(palette = "YlOrBr", domain = strike_data$max_ppl_killed, na.color = "transparent", bins = paletteBins)

#shiny UI
ui <- fluidPage(
  titlePanel("Drone Strikes in Pakistan, Somalia, and Yemen"),

  sidebarPanel(uiOutput("dateUI")),

  mainPanel(width = 10,

            leafletOutput("map", width = "70%", height = "750px")

  )
)


#shiny server
server <- function(input, output, session) {

  #create slider input depending on data frequency
  observe({

    allDates <- strike_data %>%
      filter(!is.na(lat), !is.na(long)) %>%
      pull(date) %>%
      unique()
    eligibleDates <- allDates[xts::endpoints(allDates, on = "days")]

    stepSize <- 1

    output$dateUI <- renderUI({
      sliderInput("dateSel", "Date",
                  min = min(eligibleDates),
                  max = max(eligibleDates),
                  value = min(eligibleDates),
                  step = stepSize,
                  animate = animationOptions(interval = 100, loop = FALSE)
      )
    })
  })

  #filter data depending on selected date
  filteredData <- reactive({
    req(input$dateSel)
    strike_data %>%
      filter(date == input$dateSel,
             !is.na(lat), !is.na(long))
  })

  #create the base leaflet map
  output$map <- renderLeaflet({

    leaflet(world) %>%
      addTiles() %>%
      fitBounds(lng1 = 40, lat1 = -5, lng2 = 80, lat2 = 40) %>%

      #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function
      leaflet::addLegend(pal = colorPalette, values = strike_data$max_ppl_killed, opacity = 0.9, title = "Cases", position = "bottomleft")

  })


  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({
    leafletProxy("map", data = filteredData()) %>%
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


shinyApp(ui, server)
