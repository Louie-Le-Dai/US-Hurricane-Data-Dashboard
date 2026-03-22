# app.R -  Shiny App Operation

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(sf)
library(tigris)
library(plotly)

options(tigris_use_cache = TRUE)


# Loading Dataset
zhvi <- read_csv("data/zhvi_clean.csv", show_col_types = FALSE) %>%
  mutate(date = ymd(date))

hurricanes <- read_csv("data/hurricanes_clean.csv", show_col_types = FALSE) %>%
  mutate(across(c(declaration_date, begin_date, end_date), as.Date))

county_info <- read_csv("data/county_info.csv", show_col_types = FALSE)


# Fetch County-level shapefile

# Download shapefile
us_counties_sf <- counties(cb = TRUE, year = 2021, resolution = "20m") %>%
  mutate(fips = GEOID) %>%
  semi_join(county_info, by = "fips") %>%
  left_join(county_info, by = "fips") %>%
  st_transform(4326)

# UI

ui <- fluidPage(
  
  # CSS
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f8f9fa;
      }
      .title-panel {
        background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
        color: white;
        padding: 20px 30px;
        margin-bottom: 20px;
        border-radius: 0 0 8px 8px;
      }
      .title-panel h1 {
        margin: 0;
        font-size: 28px;
        font-weight: 600;
      }
      .title-panel p {
        margin: 5px 0 0 0;
        opacity: 0.8;
        font-size: 14px;
      }
      .map-container {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .chart-container {
        background: white;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .info-box {
        background: white;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px 20px;
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .info-box h4 {
        margin-top: 0;
        color: #1a1a2e;
      }
      .hurricane-badge {
        display: inline-block;
        background-color: #e74c3c;
        color: white;
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 12px;
        margin: 2px;
      }
    "))
  ),
  

  div(
    class = "title-panel",
    h1("Hurricane Strikes and Home Values"),
    p("Select a county on the map to explore how home values evolved after hurricane strikes. ",
      "Highlighted counties have both Zillow ZHVI data and FEMA hurricane disaster declarations since 2000.")
  ),
  
  fluidRow(
    column(
      7,
      div(
        class = "map-container",
        leafletOutput("map", height = "550px")
      )
    ),
    
    column(
      5,
      div(
        class = "info-box",
        h4("County Information"),
        uiOutput("county_detail")
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      12,
      div(
        class = "chart-container",
        h4(textOutput("chart_title")),
        plotlyOutput("timeseries", height = "420px")
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      12,
      tags$footer(
        style = "text-align: center; color: #888; font-size: 12px; padding: 10px;",
        "Data sources: Zillow ZHVI (home values) and FEMA Disaster Declarations (hurricane records). ",
        "ECON 573 Data Dashboard Assignment."
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  selected_fips <- reactiveVal(NULL)
  
  # Map
  output$map <- renderLeaflet({
    
    # Color by NO. of Hurricanes
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = us_counties_sf$n_hurricanes
    )
    
    leaflet(us_counties_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId = ~fips,
        fillColor = ~pal(n_hurricanes),
        fillOpacity = 0.6,
        color = "#444",
        weight = 0.8,
        opacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 2.5,
          color = "#0f3460",
          fillOpacity = 0.8,
          bringToFront = TRUE
        ),
        label = ~paste0(county, ", ", state, " (", n_hurricanes, " hurricane(s))"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "font-size" = "13px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~n_hurricanes,
        title = "Hurricane<br>Declarations",
        opacity = 0.7
      ) %>%
      setView(lng = -90, lat = 33, zoom = 5)
  })
  
  # Click County to Select
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) {
      selected_fips(click$id)
    }
  })
  
  # Fetch Selected County
  selected_data <- reactive({
    req(selected_fips())
    zhvi %>% filter(fips == selected_fips())
  })
  
  selected_hurricanes <- reactive({
    req(selected_fips())
    hurricanes %>% filter(fips == selected_fips())
  })
  
  selected_info <- reactive({
    req(selected_fips())
    county_info %>% filter(fips == selected_fips())
  })
  
  # County Information Panel
  output$county_detail <- renderUI({
    if (is.null(selected_fips())) {
      return(
        p(style = "color: #888; font-style: italic;",
          "Click on a county on the map to see details and the home value time series.")
      )
    }
    
    info <- selected_info()
    hurrs <- selected_hurricanes()
    
    hurricane_badges <- lapply(unique(hurrs$disaster_name), function(h) {
      span(class = "hurricane-badge", h)
    })
    
    tagList(
      h3(style = "margin-top: 0;", paste0(info$county, ", ", info$state)),
      p(strong("ZHVI coverage: "),
        format(as.Date(info$date_min), "%b %Y"), " — ",
        format(as.Date(info$date_max), "%b %Y"),
        paste0(" (", info$n_months, " months)")),
      p(strong("Hurricane declarations: "), info$n_hurricanes),
      div(style = "margin-top: 5px;", hurricane_badges),
      hr(),
      p(style = "font-size: 13px; color: #555;",
        "The chart below shows the monthly Zillow Home Value Index (ZHVI) for this county. ",
        "Red shaded regions mark the one-year period following each hurricane strike.")
    )
  })
  
  output$chart_title <- renderText({
    if (is.null(selected_fips())) {
      "Home Value Time Series — Select a county to begin"
    } else {
      info <- selected_info()
      paste0("Home Value Time Series: ", info$county, ", ", info$state)
    }
  })
  
  # Time Series
  output$timeseries <- renderPlotly({
    
    if (is.null(selected_fips())) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Click a county on the map to display its home value time series.",
                 size = 5, color = "grey60") +
        theme_void()
      return(ggplotly(p, tooltip = "none") %>%
               config(displayModeBar = FALSE))
    }
    
    df <- selected_data()
    hurrs <- selected_hurricanes()
    
    # Shapes + Annotations
    shapes_list <- list()
    annotations_list <- list()
    
    if (nrow(hurrs) > 0) {
      for (i in seq_len(nrow(hurrs))) {
        h_start <- hurrs$begin_date[i]
        h_end   <- h_start %m+% years(1)
        h_name  <- hurrs$disaster_name[i]
        
        shapes_list[[i]] <- list(
          type = "rect",
          xref = "x", yref = "paper",
          x0 = as.character(h_start),
          x1 = as.character(h_end),
          y0 = 0, y1 = 1,
          fillcolor = "#e74c3c",
          opacity = 0.12,
          layer = "below",
          line = list(width = 0)
        )
        
        y_positions <- c(0.8, 0.9, 0.7, 0.865, 0.85, 0.7, 0.75, 0.6)  
        
        annotations_list[[i]] <- list(
          x = as.character(h_start),
          y = y_positions[((i - 1) %% length(y_positions)) + 1],  
          xref = "x", yref = "paper",
          text = h_name,
          showarrow = FALSE,
          font = list(color = "#c0392b", size = 10),
          xanchor = "left"
        )
      }
    }
    
    plot_ly(df, x = ~date, y = ~zhvi,
            type = "scatter", mode = "lines",
            line = list(color = "#2c3e50", width = 1.5),
            hovertemplate = "date: %{x}<br>zhvi: %{y:$,.0f}<extra></extra>") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Zillow Home Value Index (ZHVI)",
                     tickformat = "$,.0f"),
        hovermode = "x unified",
        margin = list(t = 40),
        shapes = shapes_list,
        annotations = annotations_list
      ) %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"))
  })
}

# Run
shinyApp(ui = ui, server = server)
