library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(ggradar)
library(gt)
library(gtExtras)
library(scales)
library(stringr)
library(lubridate)
library(bslib)
library(DT)

# load data
data_raw <- read_csv("Apartment Building Evaluation.csv")

# UI
ui <- page_sidebar(
  title = "Toronto Apartment Building Evaluation",
  theme = bs_theme(bootswatch = "flatly"),
  
  # custom CSS in order for bold text in tables
  tags$head(
    tags$style(HTML("
      .dt-body-bold {
        font-weight: bold !important;
      }
    "))
  ),
  
  sidebar = sidebar(
    h4("Filters and Controls"),
    
    # navigation part
    radioButtons("view", "Select Visualization:",
                choices = c("Overview" = "overview",
                            "Hexbin Plot" = "hexbin",
                           "Geographical Map" = "geo",
                           "Radar Chart" = "radar",
                           "Ward Table" = "table"),
                selected = "overview"),
    
    # filter for geometry visuals
    conditionalPanel(
      condition = "input.view == 'geo'",
      selectInput("geo_color", "Color Ward By:", 
                 choices = c("Average Score" = "avg_score",
                            "Number of Buildings" = "n_buildings",
                            "Areas Evaluated" = "eval_areas"),
                 selected = "avg_score"),
      checkboxInput("show_buildings", "Show Building Locations", value = TRUE),
      radioButtons("cluster_buildings", "Cluster Buildings:", 
                  choices = c("Yes" = TRUE, "No" = FALSE), 
                  selected = TRUE, inline = TRUE),
      selectInput("building_color", "Building Markers Color By:",
                 choices = c("None" = "none",
                            "Score" = "SCORE",
                            "Year Built" = "YEAR_BUILT",
                            "Building Age" = "BuildingAge"),
                 selected = "SCORE"),
      sliderInput("opacity", "Layer Opacity:", 
                 min = 0, max = 1, value = 0.7, step = 0.1)
    ),
    
    # filters for radar
    conditionalPanel(
      condition = "input.view == 'radar'",
      h5("Score Groups"),
      selectInput("radar_score_breaks", "Score Group Breakpoints:", 
                 choices = c("0-50-70-100" = "default",
                            "0-60-80-100" = "higher",
                            "0-40-60-100" = "lower",
                            "Custom" = "custom"),
                 selected = "default"),
      
      # show custom breakpoints
      conditionalPanel(
        condition = "input.radar_score_breaks == 'custom'",
        sliderInput("custom_breaks", "Custom Breakpoints:", 
                   min = 0, max = 100, value = c(45, 75), step = 5)
      ),
      
      h5("Building Filters"),
      sliderInput("radar_age_range", "Building Age Range:",
                 min = 0, max = 100, value = c(0, 100), step = 5),
      
      numericInput("radar_min_units", "Minimum Units:",
                  value = 0, min = 0, step = 10),
      
      selectInput("radar_ward_filter", "Filter by Ward:",
                 choices = c("All Wards" = "all"),
                 selected = "all"),
      
      h5("Facility Selection"),
      checkboxGroupInput("radar_facilities", "Select Facilities:", 
                        choices = c( # easy read name tranformation
                          "Entrance Lobby" = "ENTRANCE_LOBBY",
                          "Security" = "SECURITY",
                          "Elevators" = "ELEVATORS",
                          "Stairwells" = "STAIRWELLS",
                          "Laundry Rooms" = "LAUNDRY_ROOMS",
                          "Internal Guards/Rails" = "INTERNAL_GUARDS_HANDRAILS",
                          "Garbage Chutes" = "GARBAGE_CHUTE_ROOMS",
                          "Garbage Bins" = "GARBAGE_BIN_STORAGE_AREA",
                          "Storage/Lockers" = "STORAGE_AREAS_LOCKERS",
                          "Interior Walls/Ceiling" = "INTERIOR_WALL_CEILING_FLOOR",
                          "Interior Lighting" = "INTERIOR_LIGHTING_LEVELS",
                          "Graffiti" = "GRAFFITI",
                          "Exterior Cladding" = "EXTERIOR_CLADDING",
                          "Exterior Grounds" = "EXTERIOR_GROUNDS",
                          "Exterior Walkways" = "EXTERIOR_WALKWAYS",
                          "Balcony Guards" = "BALCONY_GUARDS",
                          "Water Penetration" = "WATER_PEN_EXT_BLDG_ELEMENTS",
                          "Parking Area" = "PARKING_AREA",
                          "Other Facilities" = "OTHER_FACILITIES"
                        ),
                        selected = c("ENTRANCE_LOBBY", "SECURITY", "ELEVATORS", "EXTERIOR_GROUNDS")),
      
      h5("Display Options"),
      sliderInput("radar_axis_label_size", "Axis Label Size:", 
                 min = 2, max = 6, value = 3, step = 0.5),
      checkboxInput("radar_show_legend", "Show Legend", value = TRUE),
      selectInput("radar_color_scheme", "Color Scheme:",
                 choices = c("Default" = "default",
                            "Viridis" = "viridis",
                            "Blues" = "blues",
                            "Reds" = "reds"),
                 selected = "default")
    ),
    
    # filters for Ward Table
    conditionalPanel(
      condition = "input.view == 'table'",
      h5("Table Filters"),
      sliderInput("table_score_range", "Average Score Range:",
                 min = 0, max = 100, value = c(0, 100), step = 5),
      numericInput("table_min_risk", "Minimum High Risk Buildings:",
                  value = 0, min = 0, step = 1),
      numericInput("table_min_units", "Minimum Average Units:",
                  value = 0, min = 0, step = 10),
      selectInput("table_sort_by", "Sort By:",
                 choices = c( # easy read name tranformation
                   "Ward Number" = "WARD",
                   "Average Score" = "avg_score",
                   "High Risk Buildings" = "high_risk",
                   "Average Units" = "avg_units",
                   "Evaluation Density" = "eval_density"
                 ),
                 selected = "WARD"),
      radioButtons("table_sort_dir", "Sort Direction:", 
                  choices = c("Ascending" = "asc", "Descending" = "desc"), 
                  selected = "asc", inline = TRUE) #
    )
  ),
  
  # Code for Main area
  mainPanel(
    conditionalPanel(
      condition = "input.view == 'overview'",
      div(
        style = "padding: 15px;",
        h2("Introduction", style = "border-bottom: 1px solid #eee; padding-bottom: 10px;"),
        p("This dashboard integrates multiple interactive views to explore Toronto's apartment building evaluations. By examining building age, spatial distribution, facility sub‑scores, and building summaries, we would like to find out the maintenance patterns, identify areas of concern, and highlight best‑practice wards."),

        h2("Methods", style = "border-bottom: 1px solid #eee; padding-bottom: 10px; margin-top: 20px;"),
        tags$ul(
          tags$li(tags$b("Hexbin Plot:"), " We calculated and ploted each building's age against its overall evaluation score, using hexbins to aggregate counts and reveal density trends."),
          tags$li(tags$b("Geographical Map:"), " We overlaid ward boundaries with choropleth coloring by average score, evaluation areas, or building count, and added optional clustered markers for individual building details."),
          tags$li(tags$b("Radar Chart:"), " We grouped buildings into low, medium, high score group and computed average scores for key facilities, then visualized these normalized values on an overlaid radar chart."),
          tags$li(tags$b("Ward Table:"), " We aggregated by ward, then calculating average score, high‑risk building count, average units, and evaluation density, and sorted and color‑coded to facilitate comparative ranking.")
        ),

        h2("Results", style = "border-bottom: 1px solid #eee; padding-bottom: 10px; margin-top: 20px;"),
        tags$ul(
          tags$li(tags$b("Age vs. Score:"), " Newer buildings cluster in high‑scoring hexbins, while older buildings are more spread out and show some low scores."),
          tags$li(tags$b("Spatial Patterns:"), " The York area records the highest average scores but with relatively few buildings evaluated, downtown wards also score well with dense coverage, while most other wards show more lower results."),
          tags$li(tags$b("Facility Breakdown:"), " High‑score buildings still dip on surfaces and bin areas, while medium‑score buildings show larger gaps in lighting and chute maintenance."),
          tags$li(tags$b("Ward Rankings:"), " The table identifies wards with the most high‑risk buildings and those that consistently exceed quality benchmarks.")
        ),
        
        h2("Conclusion", style = "border-bottom: 1px solid #eee; padding-bottom: 10px; margin-top: 20px;"),
        p("These views provide actionable insights: prioritize preventative maintenance for aging stock, deploy targeted facility upgrades where sub‑scores lag, and benchmark high‑performing wards to inform citywide quality standards."),
        
        h2("Citation", style = "border-bottom: 1px solid #eee; padding-bottom: 10px; margin-top: 20px;"),
        p("City of Toronto. (2018). Ward Profiles: Ward 25 – 44‑Ward Model (2014–2018) [Dataset]. City of Toronto. Retrieved April 22, 2025, from", 
          a(href="https://www.toronto.ca/city-government/data-research-maps/neighbourhoods-communities/ward-profiles/44-ward-model/ward-profiles-ward-25/", 
            "https://www.toronto.ca/city-government/data-research-maps/neighbourhoods-communities/ward-profiles/44-ward-model/ward-profiles-ward-25/", 
            target="_blank", style="word-break: break-all;")),
        

        p("Gelfand, S., & City of Toronto. (2025). opendatatoronto: Access the City of Toronto Open Data Portal (R package version 0.1.6) [Software]. CRAN. ", 
          a(href="https://CRAN.R-project.org/package=opendatatoronto", 
            "https://CRAN.R-project.org/package=opendatatoronto",
            target="_blank", style="word-break: break-all;"))
      )
    ),
    conditionalPanel(
      condition = "input.view == 'hexbin'",
      plotOutput("hexbinPlot", height = "600px")
    ),
    conditionalPanel(
      condition = "input.view == 'geo'",
      leafletOutput("geoPlot", height = "600px"),
      tags$div(style = "margin-top: 10px;",
               actionButton("reset_view", "Reset Map View", 
                            icon = icon("refresh"),
                            style = "margin-bottom: 10px;"),
               HTML("<p><small>Click on wards or buildings for details, 
                    use the layer control to toggle features, zoom and pan to explore.</small></p>"))
    ),
    conditionalPanel(
      condition = "input.view == 'radar'",
      plotOutput("radarPlot", height = "600px")
    ),
    conditionalPanel(
      condition = "input.view == 'table'",
      DTOutput("wardTable"),
      uiOutput("wardDetails")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # data cleaning
  processed_data <- reactive({

    data_raw %>%
      mutate(EvaluationYear = year(ymd(EVALUATION_COMPLETED_ON)),
             BuildingAge = EvaluationYear - YEAR_BUILT,
             # Create simplified popup text using only columns we know exist
             popup_text = paste0(
               "<strong>Score:</strong> ", SCORE, "<br>",
               "<strong>Year Built:</strong> ", YEAR_BUILT, "<br>",
               "<strong>Building Age:</strong> ", BuildingAge, " years<br>",
               paste0("<strong>Units:</strong> ", CONFIRMED_UNITS, "<br>") ,
               paste0("<strong>Ward:</strong> ", WARD, " - ", WARDNAME)
             ))
  })
  
  # Option 1: Hexbin plot
  output$hexbinPlot <- renderPlot({
    ggplot(processed_data(), aes(x = BuildingAge, y = SCORE)) +
      stat_binhex(bins = 60, aes(fill = after_stat(count)), color = NA, alpha = 0.8) +
      scale_fill_viridis_c(name = "Building Count") +
      labs(title = "Building Age VS Evaluation Score",
           subtitle = "Color represents count of buildings in each hexbin",
           x = "Building Age (Years)",
           y = "Evaluation Score") +
      theme_minimal(base_size = 14)
  })
  
  # Option 2: Map
  output$geoPlot <- renderLeaflet({
    #load Toronto ward boundaries
    wards <- st_read("25-ward/WARD_WGS84.shp", quiet = TRUE) %>%
      mutate(AREA_S_CD = as.character(as.integer(AREA_S_CD)))
    
    # Aggregate apartment evaluation data by ward
    ward_scores <- processed_data() %>%
      filter(!is.na(SCORE)) %>%
      group_by(WARD) %>%
      summarise(
        avg_score = mean(SCORE, na.rm = TRUE),
        eval_areas = sum(NO_OF_AREAS_EVALUATED, na.rm = TRUE),
        n_buildings = n(),
        .groups = "drop"
      )
    
    # Merge ward boundaries with aggregated scores
    ward_map <- merge(
      x = wards,
      y = ward_scores,
      by.x = "AREA_S_CD",
      by.y = "WARD",
      all.x = TRUE
    ) %>%
      # popup content
      mutate(popup = paste0(
        "<strong>Ward: ", AREA_S_CD, 
        ifelse("AREA_NAME" %in% names(.), paste0(" - ", AREA_NAME), ""), 
        "</strong><br>",
        "Average Score: <b>", round(avg_score, 1), "</b><br>",
        "Number of Buildings: <b>", n_buildings, "</b><br>",
        "Total Areas Evaluated: <b>", eval_areas, "</b>"
      ))
    
    # this is sf object for points
    apt_sf <- processed_data() %>%
      filter(!is.na(LONGITUDE) & !is.na(LATITUDE)) %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    
    # color palettes for color blind
    pal_wards <- colorNumeric(
      palette = "YlGnBu",
      domain = ward_map[[input$geo_color]],
      na.color = "#DDDDDD"
    )
    
    # building color palette
    if (input$building_color != "none") {
      pal_buildings <- colorNumeric(
        palette = "RdYlGn",
        domain = apt_sf[[input$building_color]],
        na.color = "#888888"
      )
    }
    
    # base map
    map <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -79.3832, lat = 43.7001, zoom = 11)  # Toronto coordinates
    
    # ddd ward polygons
    map <- map %>%
      addPolygons(
        data = ward_map,
        fillColor = ~pal_wards(get(input$geo_color)),
        weight = 1,
        opacity = 1,
        color = "#666666",
        dashArray = "3",
        fillOpacity = input$opacity,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~AREA_NAME,
        popup = ~popup,
        group = "Wards"
      )
    
    # Add building marker
    if (input$show_buildings) {
      if (as.logical(input$cluster_buildings)) { # Add clustered markers
        if (input$building_color != "none") {
          map <- map %>%
            addCircleMarkers(
              data = apt_sf,
              radius = 5,
              fillColor = ~pal_buildings(get(input$building_color)),
              color = "#000000",
              weight = 1,
              opacity = input$opacity,
              fillOpacity = input$opacity,
              popup = ~popup_text,
              clusterOptions = markerClusterOptions(),
              group = "Buildings"
            )
        } else {
          map <- map %>%
            addMarkers(
              data = apt_sf,
              popup = ~popup_text,
              clusterOptions = markerClusterOptions(),
              group = "Buildings"
            )
        }
      } else {
        # Add non-clustered markers
        if (input$building_color != "none") {
          map <- map %>%
            addCircleMarkers(
              data = apt_sf,
              radius = 5,
              fillColor = ~pal_buildings(get(input$building_color)),
              color = "#000000",
              weight = 1,
              opacity = input$opacity,
              fillOpacity = input$opacity,
              popup = ~popup_text,
              group = "Buildings"
            )
        } else {
          map <- map %>%
            addCircleMarkers(
              data = apt_sf,
              radius = 5,
              fillColor = "#E69F00",
              color = "#000000",
              weight = 1,
              opacity = input$opacity,
              fillOpacity = input$opacity,
              popup = ~popup_text,
              group = "Buildings"
            )
        }
      }
    }
    
    # legends
    map <- map %>%
      addLegend(
        position = "bottomright",
        pal = pal_wards,
        values = ward_map[[input$geo_color]],
        title = switch(input$geo_color,
                     "avg_score" = "Average Score",
                     "n_buildings" = "Building Count",
                     "eval_areas" = "Areas Evaluated"),
        opacity = 0.7,
        group = "Wards"
      )
    
    # building legend
    if (input$show_buildings && input$building_color != "none") {
      map <- map %>%
        addLegend(
          position = "bottomleft",
          pal = pal_buildings,
          values = apt_sf[[input$building_color]],
          title = switch(input$building_color,
                       "SCORE" = "Building Score",
                       "YEAR_BUILT" = "Year Built",
                       "BuildingAge" = "Building Age"),
          opacity = 0.7,
          group = "Buildings"
        )
    }
    
    # search control
    map <- map %>%
      addSearchOSM() %>%
      addResetMapButton()
    
    # layer controls
    map %>%
      addLayersControl(
        baseGroups = c("CartoDB Light", "OpenStreetMap", "Satellite"),
        overlayGroups = c("Wards", "Buildings"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        toggleDisplay = TRUE
      ) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  # Reset map view
  observeEvent(input$reset_view, {
    leafletProxy("geoPlot") %>%
      setView(lng = -79.3832, lat = 43.7001, zoom = 11)
  })
  
  # Option 3: Radar chart with facility categories
  output$radarPlot <- renderPlot({
    if (input$radar_ward_filter == "all" && 
        length(isolate(input$radar_ward_filter)) == 1 && 
        "WARD" %in% names(data_raw)) {
      
      ward_choices <- c("All Wards" = "all", sort(unique(data_raw$WARD)))
      updateSelectInput(session, "radar_ward_filter", 
                       choices = ward_choices, 
                       selected = "all")
    }
    
    # selected facilities
    facilities <- if(length(input$radar_facilities) > 0) 
                   input$radar_facilities 
                 else 
                   c("ENTRANCE_LOBBY", "SECURITY", "ELEVATORS", "EXTERIOR_GROUNDS")
    
    # Define score breakpoints based on selection
    breaks <- if(input$radar_score_breaks == "custom") {      # use the slider values when custom breakpoints, 
      c(0, input$custom_breaks[1], input$custom_breaks[2], 100)
    } else {
      switch(input$radar_score_breaks,
            "default" = c(0, 50, 70, 100),
            "higher" = c(0, 60, 80, 100),
            "lower" = c(0, 40, 60, 100))
    }
    
    labels <- c(
      paste0("Low Score(0-", breaks[2], ")"),
      paste0("Medium Score(", breaks[2], "-", breaks[3], ")"),
      paste0("High Score(", breaks[3], "-100)")
    )
    
    # Filter data based on selected criteria
    filtered_data <- processed_data() %>%
      filter(
        BuildingAge >= input$radar_age_range[1] & 
        BuildingAge <= input$radar_age_range[2]
      )
    
    if ("CONFIRMED_UNITS" %in% names(filtered_data)) {
      filtered_data <- filtered_data %>%
        filter(CONFIRMED_UNITS >= input$radar_min_units)
    }
    
    if (input$radar_ward_filter != "all" && "WARD" %in% names(filtered_data)) {
      filtered_data <- filtered_data %>%
        filter(WARD == input$radar_ward_filter)
    }
    
    # data cleaning for ward
    facility_scores <- filtered_data %>%       # overall SCORE into groups based on selected things
      mutate(score_group = cut(
        SCORE, 
        breaks = breaks, 
        labels = labels
      )) %>%
      select(score_group, all_of(facilities)) %>%
      pivot_longer(
        cols = -score_group, 
        names_to = "facility", 
        values_to = "score"
      ) %>%
      group_by(score_group, facility) %>%
      summarise(
        avg_score = mean(score, na.rm = TRUE) / 5,  # normalize to 0-1
        .groups = "drop"
      ) %>%
      mutate(facility = recode(facility, #Rename for easy read
        "ENTRANCE_LOBBY" = "Lobby",
        "SECURITY" = "Security",
        "ELEVATORS" = "Lifts",
        "STAIRWELLS" = "Stairs",
        "LAUNDRY_ROOMS" = "Laundry",
        "INTERNAL_GUARDS_HANDRAILS" = "Guards/Rails",
        "GARBAGE_CHUTE_ROOMS" = "Chutes",
        "GARBAGE_BIN_STORAGE_AREA" = "Bins",
        "STORAGE_AREAS_LOCKERS" = "Lockers",
        "INTERIOR_WALL_CEILING_FLOOR" = "Surfaces",
        "INTERIOR_LIGHTING_LEVELS" = "Lighting",
        "GRAFFITI" = "Graffiti",
        "EXTERIOR_CLADDING" = "Cladding",
        "EXTERIOR_GROUNDS" = "Grounds",
        "EXTERIOR_WALKWAYS" = "Walkways",
        "BALCONY_GUARDS" = "Balcony",
        "WATER_PEN_EXT_BLDG_ELEMENTS" = "Ext. Elements",
        "PARKING_AREA" = "Parking",
        "OTHER_FACILITIES" = "Other"
      ))
    
    # Wrap long labels, avoid overlap
    facility_scores <- facility_scores %>%
      mutate(facility = str_wrap(facility, width = 12))
    
    radar_data <- facility_scores %>%
      pivot_wider(
        names_from = facility, 
        values_from = avg_score
      )
    
    #  colors
    group_colors <- switch(input$radar_color_scheme,
                          "default" = setNames(c("#1B9E77", "#D95F02", "#7570B3"), labels),
                          "viridis" = setNames(viridis::viridis(3), labels),
                          "blues" = setNames(RColorBrewer::brewer.pal(3, "Blues"), labels),
                          "reds" = setNames(RColorBrewer::brewer.pal(3, "Reds"), labels))
    
    plot <- ggradar(
      radar_data,
      grid.min = 0,
      grid.mid = 0.5,
      grid.max = 1,
      values.radar = c("0%", "50%", "100%"),
      grid.label.size = 3,
      axis.label.size = input$radar_axis_label_size,
      axis.label.offset = 1.2,
      legend.text.size = 8,
      group.point.size = 2,
      group.line.width = 0.7,
      background.circle.colour = "white",
      gridline.mid.colour = "grey80",
      group.colours = group_colors,
      plot.legend = input$radar_show_legend,
      legend.position = "bottom"
    )
    
    plot <- plot +
      labs(
        title = "Facility Sub‑Score Comparison by Score Group",
        subtitle = paste0(
          "Building Age: ", input$radar_age_range[1], "-", input$radar_age_range[2], " years",
          if(input$radar_min_units > 0) paste0(" | Min Units: ", input$radar_min_units) else "",
          if(input$radar_ward_filter != "all") paste0(" | Ward: ", input$radar_ward_filter) else ""
        )
      ) +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5,
          vjust = 2
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5,
          vjust = 2
        )
      )
    
    plot
  })
  
  # table data
  filtered_ward_table <- reactive({
    processed_data() %>%
      group_by(WARD, WARDNAME) %>%
      summarise(
        avg_score = mean(SCORE, na.rm = TRUE),
        high_risk = sum(SCORE < 50, na.rm = TRUE),
        avg_units = mean(CONFIRMED_UNITS, na.rm = TRUE),
        eval_density = sum(NO_OF_AREAS_EVALUATED) / n(),
        n_buildings = n(),
        .groups = "drop"
      ) %>%
      filter(# Apply filters
        avg_score >= input$table_score_range[1] & 
        avg_score <= input$table_score_range[2],
        high_risk >= input$table_min_risk,
        avg_units >= input$table_min_units
      ) %>%
      arrange(# sorting
        if(input$table_sort_dir == "asc") 
          get(input$table_sort_by) 
        else 
          desc(get(input$table_sort_by))
      )
  })
  
  # Store selected row for details view
  selected_ward <- reactiveVal(NULL)
  
  output$wardTable <- renderDT({
    table_data <- filtered_ward_table()
  
    datatable(
      table_data,
      colnames = c(
        "Ward #" = "WARD",
        "Ward Name" = "WARDNAME",
        "Avg Score" = "avg_score",
        "High Risk Buildings" = "high_risk",
        "Avg Units" = "avg_units",
        "Eval Density" = "eval_density",
        "# Buildings" = "n_buildings"
      ),
      selection = 'single',
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 'ltipr',  
        columnDefs = list(
          list(targets = c(2), render = JS("function(data, type) { 
            if (type === 'display') {
              var color = 'green';
              if (data < 50) { color = '#FF0000'; }  // Brighter red
              else if (data < 70) { color = 'orange'; }
              return '<span style=\"color:' + color + '; font-weight: bold;\">' + data.toFixed(1) + '</span>';
            }
            return data;
          }")),
          # all text bold for better view
          list(targets = '_all', className = 'dt-body-bold')
        ),
        searching = TRUE,  # Enable search box
        ordering = TRUE,   # Enable column ordering
        autoWidth = TRUE,
        scroller = TRUE
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; font-size: 1.2em; font-weight: bold; margin-bottom: 10px;',
        'Toronto Ward Apartment Building Evaluation'
      ),
      extensions = c('Buttons', 'Responsive')
    ) %>%
      formatRound(columns = c(3, 5, 6), digits = 1) %>%
      formatStyle(
        3,  # Column position for 'Avg Score'
        target = 'row',
        backgroundColor = styleInterval(
          c(40, 50, 60, 70, 80, 90),
          c('#FF0000', '#FF4500', '#FFDD66', '#DDFF66', '#AAFFAA', '#66FF66', '#00FF00')
        )
      ) %>%
      # highlighting for high risk buildings
      formatStyle(
        4,
        backgroundColor = styleInterval(
          c(5, 10),
          c('transparent', '#FFE0E0', '#FF9999')
        )
      )
  })
  
  # Detail view for selected ward
  observeEvent(input$wardTable_rows_selected, {
    idx <- input$wardTable_rows_selected
    if(length(idx) > 0) {
      selected_ward_data <- filtered_ward_table()[idx, ]
      selected_ward(selected_ward_data$WARD)
    }
  })
  
  # Render detailed info for selected ward
  output$wardDetails <- renderUI({
    ward_id <- selected_ward()
    if(is.null(ward_id)) {
      return(div(
        style = "margin-top: 20px; text-align: center;",
        h4("Select a ward from the table to view detailed information")
      ))
    }
    
    # Get buildings in the selected ward
    ward_buildings <- processed_data() %>%
      filter(WARD == ward_id) %>%
      mutate(BuildingAge = as.integer(BuildingAge)) %>%
      arrange(desc(SCORE))
    
    # Calculate ward statistics
    ward_stats <- ward_buildings %>%
      summarise(
        avg_score = mean(SCORE, na.rm = TRUE),
        median_score = median(SCORE, na.rm = TRUE),
        min_score = min(SCORE, na.rm = TRUE),
        max_score = max(SCORE, na.rm = TRUE),
        avg_age = mean(BuildingAge, na.rm = TRUE),
        newest_building = min(BuildingAge, na.rm = TRUE),
        oldest_building = max(BuildingAge, na.rm = TRUE),
        total_buildings = n(),
        high_risk = sum(SCORE < 50, na.rm = TRUE),
        medium_risk = sum(SCORE >= 50 & SCORE < 70, na.rm = TRUE),
        low_risk = sum(SCORE >= 70, na.rm = TRUE)
      )
    
    # Create summary table for the ward
    div(
      style = "margin-top: 20px; border-top: 1px solid #ddd; padding-top: 15px;",
      
      h3(paste("Ward", ward_id, "Details"), style = "margin-bottom: 15px;"),
      
      div(
        style = "display: flex; flex-wrap: wrap; gap: 20px;",
        
        # Left column - stats
        div(
          style = "flex: 1; min-width: 300px;",
          h4("Ward Statistics", style = "border-bottom: 1px solid #eee; padding-bottom: 5px;"),
          
          div(style = "display: flex; flex-wrap: wrap; gap: 10px;",
              div(style = "flex: 1; min-width: 130px; background: #f8f9fa; padding: 10px; border-radius: 5px;",
                  h5("Scores", style = "margin: 0 0 5px 0;"),
                  p(style = "margin: 0;", HTML(paste0(
                    "Average: <b>", round(ward_stats$avg_score, 1), "</b><br>",
                    "Median: <b>", round(ward_stats$median_score, 1), "</b><br>",
                    "Range: <b>", round(ward_stats$min_score, 1), " - ", round(ward_stats$max_score, 1), "</b>"
                  )))
              ),
              div(style = "flex: 1; min-width: 130px; background: #f8f9fa; padding: 10px; border-radius: 5px;",
                  h5("Building Age", style = "margin: 0 0 5px 0;"),
                  p(style = "margin: 0;", HTML(paste0(
                    "Average: <b>", round(ward_stats$avg_age, 1), " years</b><br>",
                    "Newest: <b>", ward_stats$newest_building, " years</b><br>",
                    "Oldest: <b>", ward_stats$oldest_building, " years</b>"
                  )))
              ),
              div(style = "flex: 1; min-width: 130px; background: #f8f9fa; padding: 10px; border-radius: 5px;",
                  h5("Risk Profile", style = "margin: 0 0 5px 0;"),
                  p(style = "margin: 0;", HTML(paste0(
                    "High Risk: <b style='color: red;'>", ward_stats$high_risk, "</b><br>",
                    "Medium Risk: <b style='color: orange;'>", ward_stats$medium_risk, "</b><br>",
                    "Low Risk: <b style='color: green;'>", ward_stats$low_risk, "</b>"
                  )))
              )
          )
        ),
        
        # Right column: building list
        div(
          style = "flex: 1; min-width: 300px;",
          h4(paste0("Buildings (", nrow(ward_buildings), ")"), 
             style = "border-bottom: 1px solid #eee; padding-bottom: 5px;"),
          
          div(style = "max-height: 200px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 5px;",
              renderDT({
                building_table <- ward_buildings %>%
                  select(SCORE, BuildingAge, CONFIRMED_UNITS, PROPERTY_TYPE, EVALUATION_COMPLETED_ON)
                
                datatable(
                  building_table,
                  colnames = c(
                    "Score" = "SCORE",
                    "Age" = "BuildingAge", 
                    "Units" = "CONFIRMED_UNITS",
                    "Type" = "PROPERTY_TYPE",
                    "Evaluated" = "EVALUATION_COMPLETED_ON"
                  ),
                  options = list(
                    pageLength = 5,
                    dom = 'tp',
                    scrollY = "150px",
                    scrollCollapse = TRUE,
                    ordering = TRUE,
                    searching = FALSE,
                    columnDefs = list(
                      list(targets = c(0), render = JS("function(data, type) { 
                        if (type === 'display') {
                          var color = 'green';
                          if (data < 50) { color = '#FF0000'; }  // Brighter red
                          else if (data < 70) { color = 'orange'; }
                          return '<span style=\"color:' + color + '; font-weight: bold;\">' + data.toFixed(1) + '</span>';
                        }
                        return data;
                      }")),
                      list(targets = '_all', className = 'dt-body-bold')
                    )
                  ),
                  rownames = FALSE,
                  selection = "none",
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; font-weight: bold; margin-bottom: 5px;',
                    'Building Details'
                  )
                ) %>%
                  formatRound(columns = 1, digits = 1) %>%
                  formatStyle(
                    1,
                    target = 'row',
                    backgroundColor = styleInterval(
                      c(40, 50, 60, 70, 80, 90),
                      c('#FF0000', '#FF4500', '#FFDD66', '#DDFF66', '#AAFFAA', '#66FF66', '#00FF00')
                    )
                  ) %>%
                  # Enhanced building age styling
                  formatStyle(
                    2,
                    color = styleInterval(
                      c(25, 50),
                      c('#2D6CA2', '#5E35B1', '#D81B60')
                    )
                  )
              })
          )
        )
      ),
      
      # Button to clear selection
      div(
        style = "margin-top: 15px; text-align: right;",
        actionButton("clear_selection", "Clear Selection", 
                    icon = icon("times"),
                    style = "background-color: #f8f9fa; color: #333;")
      )
    )
  })
  
  # Clear ward selection
  observeEvent(input$clear_selection, {
    selected_ward(NULL)
    # Clear table selection
    proxy <- dataTableProxy("wardTable")
    proxy %>% selectRows(NULL)
  })
}

# Run
shinyApp(ui, server) 