# ============================================================
#  Fast Food, Obesity & Poverty in America - Shiny App v3
#  Data: USDA Food Environment Atlas, BLS LAUS, Census ACS
# ============================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(scales)
library(DT)
library(plotly)

options(tigris_use_cache = TRUE)

# -- 1. LOAD DATA ---------------------------------------------

# County-level atlas data
# FIX: use formatC to zero-pad FIPS; sprintf("%05s") only space-pads strings,
# which causes CA (06xxx), AZ (04xxx), CO (08xxx), AR (05xxx) to fail map joins.
atlas <- read.csv("atlas_wide.csv", stringsAsFactors = FALSE) %>%
  mutate(
    FIPS = formatC(as.integer(FIPS), width = 5, flag = "0"),
    across(c(CHILDPOVRATE21, DEEPPOVRATE21, DSPTH20, FFR20, FFRPTH20,
             FOODINSEC_21_23, GROCPTH20, MEDHHINC21, METRO23,
             PCT_DIABETES_ADULTS19, PCT_HISP20,
             PCT_LACCESS_LOWI19, PCT_LACCESS_POP19,
             PCT_NHASIAN20, PCT_NHBLACK20, PCT_NHWHITE20,
             PCT_OBESE_ADULTS22, PCT_SNAP22, POVRATE21, RECFACPTH20),
           ~ suppressWarnings(as.numeric(na_if(as.character(.), ""))))
  )

# State-level data: BLS LAUS unemployment + Census ACS poverty + median income
state_data <- read.csv("state_data.csv", stringsAsFactors = FALSE) %>%
  mutate(MedianHHIncome2022 = as.numeric(MedianHHIncome2022))

# CPI data (USDA ERS 1974-2024)
cpi <- read.csv("historical_cpi.csv", stringsAsFactors = FALSE)

# Datafiniti fast food locations (lat/lon point data)
ff_locs <- read.csv("Datafiniti_Fast_Food_Restaurants.csv", stringsAsFactors = FALSE) %>%
  filter(!is.na(latitude), !is.na(longitude),
         latitude  >  24,  latitude  <  50,
         longitude > -125, longitude < -66) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(name = trimws(name))

# -- 2. SHAPEFILES ---------------------------------------------
message("Loading shapefiles...")
county_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(4326) %>%
  mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

county_map <- county_sf %>% left_join(atlas, by = "FIPS")

state_sf <- states(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(4326) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS")) %>%
  left_join(state_data, by = c("STUSPS" = "StateAbbr"))

# -- 3. VARIABLE METADATA -------------------------------------
map_vars <- list(
  "Obesity Rate (%)"                 = "PCT_OBESE_ADULTS22",
  "Diabetes Rate (%)"                = "PCT_DIABETES_ADULTS19",
  "Poverty Rate (%)"                 = "POVRATE21",
  "Child Poverty Rate (%)"           = "CHILDPOVRATE21",
  "Median Household Income ($)"      = "MEDHHINC21",
  "Fast Food Restaurants per 1k pop" = "FFRPTH20",
  "Food Insecurity Rate (%)"         = "FOODINSEC_21_23",
  "SNAP Participation (%)"           = "PCT_SNAP22",
  "Low Food Access (%)"              = "PCT_LACCESS_POP19",
  "Dollar Stores per 1k pop"         = "DSPTH20",
  "Grocery Stores per 1k pop"        = "GROCPTH20",
  "Recreation Facilities per 1k pop" = "RECFACPTH20",
  "% Black or African American"      = "PCT_NHBLACK20",
  "% Hispanic or Latino"             = "PCT_HISP20",
  "% White (Non-Hispanic)"           = "PCT_NHWHITE20"
)

scatter_vars <- c(
  "Obesity Rate (%)"            = "PCT_OBESE_ADULTS22",
  "Diabetes Rate (%)"           = "PCT_DIABETES_ADULTS19",
  "Poverty Rate (%)"            = "POVRATE21",
  "Child Poverty Rate (%)"      = "CHILDPOVRATE21",
  "Median HH Income ($)"        = "MEDHHINC21",
  "Fast Food per 1k pop"        = "FFRPTH20",
  "Food Insecurity (%)"         = "FOODINSEC_21_23",
  "SNAP Participation (%)"      = "PCT_SNAP22",
  "Low Food Access (%)"         = "PCT_LACCESS_POP19",
  "Dollar Stores per 1k pop"    = "DSPTH20",
  "Grocery Stores per 1k pop"   = "GROCPTH20",
  "% Black or African American" = "PCT_NHBLACK20",
  "% Hispanic or Latino"        = "PCT_HISP20"
)

# Clean named lookup for bar chart labels
bar_var_labels <- c(
  PCT_OBESE_ADULTS22    = "Avg Obesity Rate (%)",
  PCT_DIABETES_ADULTS19 = "Avg Diabetes Rate (%)",
  FOODINSEC_21_23       = "Avg Food Insecurity (%)",
  POVRATE21             = "Avg Poverty Rate (%)",
  CHILDPOVRATE21        = "Avg Child Poverty Rate (%)",
  MEDHHINC21            = "Median HH Income ($)",
  PCT_SNAP22            = "SNAP Participation (%)",
  FFRPTH20              = "Fast Food per 1k pop",
  PCT_LACCESS_POP19     = "Low Food Access (%)",
  DSPTH20               = "Dollar Stores per 1k",
  GROCPTH20             = "Grocery Stores per 1k"
)

palette_choices <- c("YlOrRd", "RdPu", "Blues", "Greens", "PuBu", "Oranges", "Purples")

# -- 4. UI -----------------------------------------------------
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Food, Health & Poverty"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("County Map",        tabName = "map",    icon = icon("map")),
      menuItem("State Overview",    tabName = "state",  icon = icon("flag-usa")),
      menuItem("Scatter Explorer",  tabName = "scatter",icon = icon("circle-dot")),
      menuItem("State Rankings",    tabName = "bars",   icon = icon("chart-bar")),
      menuItem("Food Price Trends", tabName = "cpi",    icon = icon("chart-line")),
      menuItem("Demographics",      tabName = "demo",   icon = icon("users")),
      menuItem("Location Map",      tabName = "locmap", icon = icon("location-dot")),
      menuItem("Data Table",        tabName = "data",   icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-red .main-header .logo  { background-color:#b5230a; font-size:14px; }
      .skin-red .main-header .navbar{ background-color:#c0392b; }
      .skin-red .main-sidebar        { background-color:#2c3e50; }
      .skin-red .sidebar a           { color:#ecf0f1 !important; }
      .info-box      { min-height:80px; }
      .info-box-icon { height:80px; line-height:80px; }
      #map_leaf   { height:600px !important; }
      #state_leaf { height:480px !important; }
      #loc_map    { height:650px !important; }
      .src { font-size:0.78em; color:#888; margin-top:6px; }
    "))),
    tabItems(
      
      # TAB 1: COUNTY MAP ----------------------------------------
      tabItem("map",
              fluidRow(
                box(width = 3, title = "Map Controls", status = "danger", solidHeader = TRUE,
                    selectInput("map_var", "Variable:", choices = map_vars,
                                selected = "PCT_OBESE_ADULTS22"),
                    selectInput("map_palette", "Palette:", choices = palette_choices,
                                selected = "YlOrRd"),
                    checkboxInput("map_reverse", "Reverse palette", FALSE),
                    hr(),
                    strong("Summary Statistics"),
                    verbatimTextOutput("map_summary"),
                    hr(),
                    p(class = "src", "Source: USDA Food Environment Atlas 2020-2022"),
                    p(class = "src", "Click any county for details.")
                ),
                box(width = 9, title = "U.S. County Map (Continental)", status = "danger",
                    solidHeader = TRUE, leafletOutput("map_leaf", height = 600))
              ),
              fluidRow(
                valueBoxOutput("vbox_obese",  width = 3),
                valueBoxOutput("vbox_ff",     width = 3),
                valueBoxOutput("vbox_pov",    width = 3),
                valueBoxOutput("vbox_access", width = 3)
              )
      ),
      
      # TAB 2: STATE OVERVIEW ------------------------------------
      tabItem("state",
              fluidRow(
                box(width = 3, title = "State Map Controls", status = "warning",
                    solidHeader = TRUE,
                    selectInput("state_var", "Variable:",
                                choices = c(
                                  "Family Poverty Rate (%)"    = "PovertyPct_FamilyACS",
                                  "Unemployment Rate 2023 (%)" = "Unemp2023",
                                  "Unemployment Rate 2022 (%)" = "Unemp2022",
                                  "Unemployment Rate 2021 (%)" = "Unemp2021",
                                  "Unemployment Rate 2020 (%)" = "Unemp2020",
                                  "Unemployment Rate 2019 (%)" = "Unemp2019",
                                  "Median HH Income 2022 ($)"  = "MedianHHIncome2022"
                                ), selected = "PovertyPct_FamilyACS"),
                    hr(),
                    p(class = "src",
                      strong("Poverty:"), " Census ACS 2019-2023 5-yr, family poverty rate.", br(), br(),
                      strong("Unemployment:"), " BLS LAUS annual averages.", br(), br(),
                      strong("Income:"), " Census ACS 2022, median household income.")
                ),
                box(width = 9, title = "State-Level Map", status = "warning",
                    solidHeader = TRUE, leafletOutput("state_leaf", height = 480))
              ),
              fluidRow(
                box(width = 6,
                    title = "State Poverty Rankings - Family Poverty (ACS 2019-2023)",
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("state_pov_bar", height = 370)),
                box(width = 6,
                    title = "Unemployment Rate 2019-2023 by State (BLS LAUS)",
                    status = "warning", solidHeader = TRUE,
                    p(class = "src", "Select states to highlight (leave blank = show all)"),
                    selectInput("unemp_filter", "Filter states:",
                                choices = c("", sort(state_data$State)),
                                multiple = TRUE),
                    plotlyOutput("unemp_trend", height = 310))
              )
      ),
      
      # TAB 3: SCATTER -------------------------------------------
      tabItem("scatter",
              fluidRow(
                box(width = 3, title = "Scatter Controls", status = "primary",
                    solidHeader = TRUE,
                    selectInput("sc_x", "X Axis:", choices = scatter_vars,
                                selected = "POVRATE21"),
                    selectInput("sc_y", "Y Axis:", choices = scatter_vars,
                                selected = "PCT_OBESE_ADULTS22"),
                    selectInput("sc_color", "Color by:",
                                choices = c("Metro / Non-Metro" = "metro", "None" = "none"),
                                selected = "metro"),
                    checkboxInput("sc_smooth", "Show trend line", TRUE),
                    sliderInput("sc_alpha", "Point opacity:", 0.1, 1, 0.35, 0.05),
                    hr(),
                    strong("Pearson Correlation"),
                    verbatimTextOutput("sc_cor"),
                    hr(),
                    p(class = "src", "Source: USDA Food Env. Atlas | ~3,100 counties")
                ),
                box(width = 9, title = "County-Level Scatter Plot", status = "primary",
                    solidHeader = TRUE, plotlyOutput("scatter_plot", height = 560))
              )
      ),
      
      # TAB 4: STATE RANKINGS ------------------------------------
      tabItem("bars",
              fluidRow(
                box(width = 3, title = "Chart Controls", status = "success",
                    solidHeader = TRUE,
                    selectInput("bar_var", "Variable:",
                                choices = list(
                                  Health = c(
                                    "Avg Obesity Rate (%)"    = "PCT_OBESE_ADULTS22",
                                    "Avg Diabetes Rate (%)"   = "PCT_DIABETES_ADULTS19",
                                    "Avg Food Insecurity (%)" = "FOODINSEC_21_23"),
                                  Economics = c(
                                    "Avg Poverty Rate (%)"    = "POVRATE21",
                                    "Avg Child Poverty (%)"   = "CHILDPOVRATE21",
                                    "Median HH Income ($)"    = "MEDHHINC21",
                                    "SNAP Participation (%)"  = "PCT_SNAP22"),
                                  "Food Environment" = c(
                                    "Fast Food per 1k pop"    = "FFRPTH20",
                                    "Low Food Access (%)"     = "PCT_LACCESS_POP19",
                                    "Dollar Stores per 1k"    = "DSPTH20",
                                    "Grocery Stores per 1k"   = "GROCPTH20")
                                ), selected = "PCT_OBESE_ADULTS22"),
                    sliderInput("bar_n", "Number of states:", 10, 51, 25),
                    radioButtons("bar_order", "Order:",
                                 c("Highest first" = "desc", "Lowest first" = "asc")),
                    checkboxInput("bar_refline", "Show US avg line", TRUE),
                    hr(),
                    p(class = "src", "State value = mean of all counties within state."),
                    p(class = "src", "Source: USDA Food Env. Atlas 2020-2022.")
                ),
                box(width = 9, title = "State Rankings", status = "success",
                    solidHeader = TRUE, plotlyOutput("bar_plot", height = 560))
              )
      ),
      
      # TAB 5: CPI -----------------------------------------------
      tabItem("cpi",
              fluidRow(
                box(width = 3, title = "CPI Controls", status = "info",
                    solidHeader = TRUE,
                    checkboxGroupInput("cpi_series", "Series:",
                                       choices = c(
                                         "All Food"            = "AllFood",
                                         "Food at Home"        = "FoodAtHome",
                                         "Food Away from Home" = "FoodAwayFromHome"),
                                       selected = c("AllFood", "FoodAtHome", "FoodAwayFromHome")),
                    sliderInput("cpi_years", "Year range:",
                                min = 1974, max = 2024, value = c(2000, 2024), sep = ""),
                    hr(),
                    p(style = "font-size:0.84em; color:#444;",
                      "Since 2020, restaurant/fast-food prices have inflated much faster",
                      " than grocery prices, squeezing households that depend on",
                      " affordable food options."),
                    hr(),
                    p(class = "src", "Source: USDA ERS, CPI 1974-2024.")
                ),
                box(width = 9, title = "Annual Food Price Inflation (CPI % Change)",
                    status = "info", solidHeader = TRUE,
                    plotlyOutput("cpi_plot", height = 390),
                    hr(),
                    plotlyOutput("cpi_cumulative", height = 210))
              )
      ),
      
      # TAB 6: DEMOGRAPHICS --------------------------------------
      tabItem("demo",
              fluidRow(
                box(width = 3, title = "Controls", status = "danger",
                    solidHeader = TRUE,
                    selectInput("demo_yvar", "Outcome (Y-axis):",
                                choices = c(
                                  "Obesity Rate (%)"       = "PCT_OBESE_ADULTS22",
                                  "Diabetes Rate (%)"      = "PCT_DIABETES_ADULTS19",
                                  "Poverty Rate (%)"       = "POVRATE21",
                                  "Food Insecurity (%)"    = "FOODINSEC_21_23",
                                  "Low Food Access (%)"    = "PCT_LACCESS_POP19",
                                  "SNAP Participation (%)" = "PCT_SNAP22"),
                                selected = "PCT_OBESE_ADULTS22"),
                    hr(),
                    p(style = "font-size:0.84em; color:#444;",
                      "Each panel shows counties colored by racial/ethnic share vs.",
                      " the selected outcome. Lines show the linear trend.",
                      " Each dot = one county."),
                    hr(),
                    p(class = "src", "Race/ethnicity: Census 2020 via Food Env. Atlas."),
                    p(class = "src", "Outcomes: USDA Food Env. Atlas 2019-2022.")
                ),
                box(width = 9,
                    title = "Outcome vs. Racial/Ethnic Composition - County Level",
                    status = "danger", solidHeader = TRUE,
                    plotlyOutput("demo_plot", height = 520))
              )
      ),
      
      # TAB 8: LOCATION MAP --------------------------------------
      tabItem("locmap",
              fluidRow(
                box(width = 3, title = "Controls", status = "danger", solidHeader = TRUE,
                    p(class = "src", paste0("Dataset: ", nrow(ff_locs), " locations")),
                    hr(),
                    strong("Filter by Chain"),
                    br(), br(),
                    actionButton("loc_all",  "Select All", class = "btn-sm btn-default",
                                 style = "margin-bottom:6px;"),
                    actionButton("loc_none", "Clear All",  class = "btn-sm btn-danger",
                                 style = "margin-bottom:10px;"),
                    checkboxGroupInput("loc_chains", NULL,
                                       choices  = sort(unique(ff_locs$name)),
                                       selected = sort(unique(ff_locs$name))
                    ),
                    hr(),
                    p(class = "src", "Source: Datafiniti Business Database"),
                    p(class = "src", "Points cluster at low zoom - click to expand.")
                ),
                box(width = 9,
                    title = "Fast Food Restaurant Locations - Individual Sites",
                    status = "danger", solidHeader = TRUE,
                    leafletOutput("loc_map", height = 650))
              )
      ),
      
      # TAB 7: DATA TABLE ----------------------------------------
      tabItem("data",
              fluidRow(
                box(width = 12, title = "County Data Explorer", status = "danger",
                    solidHeader = TRUE,
                    p(style = "font-size:0.9em; color:#555;",
                      "All values from USDA Food Environment Atlas 2020-2022.",
                      " Empty cells = suppressed or unavailable data."),
                    DTOutput("data_table")
                )
              )
      )
      
    )
  )
)

# -- 5. SERVER -------------------------------------------------
server <- function(input, output, session) {
  
  get_var_label <- function(code) {
    all_vars <- unlist(map_vars)
    lbl <- names(all_vars)[all_vars == code]
    if (length(lbl) == 0) code else lbl[1]
  }
  
  # VALUE BOXES -------------------------------------------------
  output$vbox_obese <- renderValueBox({
    valueBox(paste0(round(mean(atlas$PCT_OBESE_ADULTS22, na.rm = TRUE), 1), "%"),
             "Avg County Obesity Rate",
             icon = icon("weight-scale"), color = "red")
  })
  output$vbox_ff <- renderValueBox({
    valueBox(round(mean(atlas$FFRPTH20, na.rm = TRUE), 2),
             "Avg Fast Food / 1k pop",
             icon = icon("burger"), color = "orange")
  })
  output$vbox_pov <- renderValueBox({
    valueBox(paste0(round(mean(atlas$POVRATE21, na.rm = TRUE), 1), "%"),
             "Avg County Poverty Rate",
             icon = icon("house-chimney-crack"), color = "yellow")
  })
  output$vbox_access <- renderValueBox({
    valueBox(paste0(round(mean(atlas$PCT_LACCESS_POP19, na.rm = TRUE), 1), "%"),
             "Avg Low Food Access",
             icon = icon("store-slash"), color = "blue")
  })
  
  # MAP SUMMARY -------------------------------------------------
  output$map_summary <- renderPrint({
    var  <- input$map_var
    vals <- atlas[[var]]
    vals <- vals[!is.na(vals)]
    cat(sprintf("n:      %d\nMin:    %.1f\nMedian: %.1f\nMean:   %.1f\nMax:    %.1f",
                length(vals), min(vals), median(vals), mean(vals), max(vals)))
  })
  
  # COUNTY MAP --------------------------------------------------
  output$map_leaf <- renderLeaflet({
    var       <- input$map_var
    var_label <- get_var_label(var)
    is_dollar <- grepl("Income", var_label)
    data      <- county_map
    vals      <- data[[var]]
    
    pal <- colorNumeric(
      palette  = input$map_palette,
      domain   = vals,
      na.color = "#cccccc",
      reverse  = input$map_reverse
    )
    
    # FIX: assign labFormat to a variable BEFORE passing to addLegend.
    # Inline if/else as a named function argument causes R parse errors.
    leg_format <- if (is_dollar) {
      labelFormat(prefix = "$", big.mark = ",", digits = 0)
    } else {
      labelFormat(digits = 1)
    }
    
    fmt <- function(v) {
      if (is.na(v)) "N/A"
      else if (is_dollar) paste0("$", formatC(v, format = "f", digits = 0, big.mark = ","))
      else as.character(round(v, 1))
    }
    
    labels <- sprintf("<strong>%s, %s</strong><br/>%s: %s",
                      data$County, data$State, var_label,
                      sapply(vals, fmt)) %>% lapply(htmltools::HTML)
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor    = ~pal(vals),
        fillOpacity  = 0.8,
        color        = "#555",
        weight       = 0.3,
        smoothFactor = 0.5,
        highlight    = highlightOptions(
          weight = 2, color = "#222",
          fillOpacity = 0.95, bringToFront = TRUE),
        label        = labels,
        labelOptions = labelOptions(
          style    = list("font-weight" = "normal", padding = "3px 6px"),
          textsize = "13px", direction = "auto")
      ) %>%
      addLegend(
        pal       = pal,
        values    = vals,
        title     = var_label,
        position  = "bottomright",
        labFormat = leg_format
      ) %>%
      setView(lng = -96, lat = 38, zoom = 4)
  })
  
  # STATE MAP ---------------------------------------------------
  output$state_leaf <- renderLeaflet({
    var <- input$state_var
    lbl_map <- c(
      PovertyPct_FamilyACS = "Family Poverty Rate (%)",
      Unemp2023            = "Unemployment Rate 2023 (%)",
      Unemp2022            = "Unemployment Rate 2022 (%)",
      Unemp2021            = "Unemployment Rate 2021 (%)",
      Unemp2020            = "Unemployment Rate 2020 (%)",
      Unemp2019            = "Unemployment Rate 2019 (%)",
      MedianHHIncome2022   = "Median HH Income 2022 ($)"
    )
    var_label <- lbl_map[var]
    is_dollar <- grepl("Income", var_label)
    data      <- state_sf
    vals      <- data[[var]]
    
    pal <- colorNumeric(
      palette  = "YlOrRd",
      domain   = vals,
      na.color = "#cccccc",
      reverse  = is_dollar
    )
    
    # FIX: same pattern - pre-assign labFormat before addLegend
    leg_format <- if (is_dollar) {
      labelFormat(prefix = "$", big.mark = ",", digits = 0)
    } else {
      labelFormat(suffix = "%", digits = 1)
    }
    
    fmt <- function(v) {
      if (is.na(v)) "N/A"
      else if (is_dollar) paste0("$", formatC(v, format = "f", digits = 0, big.mark = ","))
      else paste0(round(v, 1), "%")
    }
    
    labels <- sprintf("<strong>%s</strong><br/>%s: %s",
                      data$NAME, var_label,
                      sapply(vals, fmt)) %>% lapply(htmltools::HTML)
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor    = ~pal(vals),
        fillOpacity  = 0.8,
        color        = "#555",
        weight       = 0.5,
        smoothFactor = 0.5,
        highlight    = highlightOptions(
          weight = 2.5, color = "#222",
          fillOpacity = 0.95, bringToFront = TRUE),
        label        = labels,
        labelOptions = labelOptions(
          style    = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "14px", direction = "auto")
      ) %>%
      addLegend(
        pal       = pal,
        values    = vals,
        title     = var_label,
        position  = "bottomright",
        labFormat = leg_format
      ) %>%
      setView(lng = -96, lat = 38, zoom = 4)
  })
  
  # STATE POVERTY BAR -------------------------------------------
  output$state_pov_bar <- renderPlotly({
    d <- state_data %>%
      filter(!is.na(PovertyPct_FamilyACS)) %>%
      arrange(PovertyPct_FamilyACS) %>%
      mutate(State = factor(State, levels = State))
    
    p <- ggplot(d, aes(x = State, y = PovertyPct_FamilyACS,
                       fill = PovertyPct_FamilyACS,
                       text = paste0(State, ": ", PovertyPct_FamilyACS, "%"))) +
      geom_col() +
      scale_fill_gradient(low = "#f9e79f", high = "#c0392b") +
      geom_hline(yintercept = 8.7, linetype = "dashed",
                 color = "#2c3e50", linewidth = 0.7) +
      annotate("text", x = 5, y = 9.3, label = "US avg 8.7%",
               color = "#2c3e50", size = 3, hjust = 0) +
      labs(y = "Family Poverty Rate (%)", x = NULL,
           caption = "Source: Census ACS 2019-2023 5-year estimates") +
      theme_minimal(base_size = 10) +
      theme(axis.text.x  = element_text(angle = 60, hjust = 1, size = 7),
            legend.position = "none",
            plot.caption    = element_text(color = "#999", size = 8))
    
    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(tickangle = -60))
  })
  
  # UNEMPLOYMENT TREND ------------------------------------------
  output$unemp_trend <- renderPlotly({
    filter_st   <- input$unemp_filter
    highlighted <- length(filter_st) > 0
    
    d <- state_data %>%
      select(State, StateAbbr,
             "2019" = Unemp2019, "2020" = Unemp2020,
             "2021" = Unemp2021, "2022" = Unemp2022, "2023" = Unemp2023) %>%
      pivot_longer(c("2019", "2020", "2021", "2022", "2023"),
                   names_to = "Year", values_to = "UnempRate") %>%
      mutate(Year = as.integer(Year))
    
    if (highlighted) {
      d <- d %>% filter(State %in% filter_st)
    }
    
    # FIX: pre-compute conditional values BEFORE ggplot call to avoid inline if/else
    line_alpha  <- if (highlighted) 0.9 else 0.4
    legend_pos  <- if (highlighted) "right" else "none"
    show_legend <- highlighted
    
    p <- ggplot(d, aes(x = Year, y = UnempRate,
                       group = StateAbbr, color = StateAbbr,
                       text = paste0(State, " (", Year, "): ", UnempRate, "%"))) +
      geom_line(linewidth = 1, alpha = line_alpha) +
      geom_point(size = 1.8) +
      scale_x_continuous(breaks = 2019:2023) +
      labs(x = NULL, y = "Unemployment Rate (%)",
           caption = "Source: BLS Local Area Unemployment Statistics") +
      theme_minimal(base_size = 11) +
      theme(legend.position = legend_pos,
            plot.caption = element_text(color = "#999", size = 8))
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = show_legend)
  })
  
  # SCATTER PLOT ------------------------------------------------
  output$scatter_plot <- renderPlotly({
    xvar   <- input$sc_x
    yvar   <- input$sc_y
    cvar   <- input$sc_color
    xlabel <- names(scatter_vars)[scatter_vars == xvar]
    ylabel <- names(scatter_vars)[scatter_vars == yvar]
    
    d <- atlas %>%
      select(County, State, METRO23, all_of(c(xvar, yvar))) %>%
      filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]])) %>%
      mutate(AreaType = ifelse(!is.na(METRO23) & METRO23 == 1, "Metro", "Non-Metro"))
    
    if (cvar == "metro") {
      p <- ggplot(d, aes(x = .data[[xvar]], y = .data[[yvar]],
                         color = AreaType,
                         text  = paste0("<b>", County, ", ", State, "</b>"))) +
        scale_color_manual(
          values = c("Metro" = "#e74c3c", "Non-Metro" = "#3498db"),
          name   = "Area Type")
    } else {
      p <- ggplot(d, aes(x = .data[[xvar]], y = .data[[yvar]],
                         text = paste0("<b>", County, ", ", State, "</b>")))
    }
    
    p <- p + geom_point(alpha = input$sc_alpha, size = 1.6)
    
    if (input$sc_smooth) {
      p <- p + geom_smooth(
        data        = d,
        aes(x = .data[[xvar]], y = .data[[yvar]]),
        method      = "lm",
        se          = TRUE,
        color       = "#2c3e50",
        fill        = "#2c3e50",
        linewidth   = 1.1,
        alpha       = 0.12,
        inherit.aes = FALSE
      )
    }
    
    p <- p +
      labs(x = xlabel, y = ylabel,
           caption = "Source: USDA Food Env. Atlas 2020-2022 | Each point = 1 county") +
      theme_minimal(base_size = 13) +
      theme(plot.caption    = element_text(color = "#999", size = 9),
            legend.position = "top")
    
    ggplotly(p, tooltip = c("text", "x", "y")) %>%
      layout(legend = list(orientation = "h", x = 0, y = 1.08))
  })
  
  output$sc_cor <- renderPrint({
    xvar <- input$sc_x
    yvar <- input$sc_y
    d    <- atlas %>% filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]]))
    r    <- cor(d[[xvar]], d[[yvar]], use = "complete.obs")
    cat(sprintf("r  = %+.3f\nr2 = %.3f\nn  = %d counties", r, r^2, nrow(d)))
  })
  
  # STATE RANKINGS BAR ------------------------------------------
  output$bar_plot <- renderPlotly({
    var       <- input$bar_var
    n         <- input$bar_n
    ord       <- input$bar_order
    var_label <- bar_var_labels[var]
    if (is.na(var_label)) var_label <- var
    
    state_agg <- atlas %>%
      filter(!is.na(.data[[var]]), nchar(State) > 0) %>%
      group_by(State) %>%
      summarise(val = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    
    us_avg <- mean(state_agg$val, na.rm = TRUE)
    
    state_agg <- if (ord == "desc") {
      state_agg %>% arrange(desc(val)) %>% slice_head(n = n)
    } else {
      state_agg %>% arrange(val) %>% slice_head(n = n)
    }
    state_agg <- mutate(state_agg, State = factor(State, levels = State))
    
    is_dollar <- grepl("Income|income", var_label)
    fmt_hover <- if (is_dollar) {
      paste0("$", formatC(state_agg$val, format = "f", digits = 0, big.mark = ","))
    } else {
      as.character(round(state_agg$val, 2))
    }
    
    p <- ggplot(state_agg,
                aes(x = State, y = val, fill = val,
                    text = paste0(State, ": ", fmt_hover))) +
      geom_col(alpha = 0.9) +
      scale_fill_gradient(low = "#f9e79f", high = "#c0392b", guide = "none") +
      labs(y = var_label, x = NULL,
           caption = "Source: USDA Food Env. Atlas | Values = county mean within state") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x  = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(color = "#999", size = 9))
    
    if (input$bar_refline && nrow(state_agg) > 0) {
      ref_lbl <- if (is_dollar) {
        paste0("US avg: $", formatC(us_avg, format = "f", digits = 0, big.mark = ","))
      } else {
        paste0("US avg: ", round(us_avg, 1))
      }
      p <- p +
        geom_hline(yintercept = us_avg, linetype = "dashed",
                   color = "#2c3e50", linewidth = 0.8) +
        annotate("text",
                 x     = levels(state_agg$State)[2],
                 y     = us_avg * 1.04,
                 label = ref_lbl,
                 color = "#2c3e50", size = 3.5, hjust = 0)
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  # CPI ANNUAL --------------------------------------------------
  output$cpi_plot <- renderPlotly({
    series <- input$cpi_series
    years  <- input$cpi_years
    if (length(series) == 0) return(plotly_empty())
    
    lbl <- c(AllFood          = "All Food",
             FoodAtHome       = "Food at Home (Groceries)",
             FoodAwayFromHome = "Food Away from Home (Restaurants)")
    clr <- c("All Food"                         = "#e67e22",
             "Food at Home (Groceries)"          = "#27ae60",
             "Food Away from Home (Restaurants)" = "#c0392b")
    
    d <- cpi %>%
      filter(Year >= years[1], Year <= years[2]) %>%
      pivot_longer(c(AllFood, FoodAtHome, FoodAwayFromHome),
                   names_to = "Series", values_to = "Pct") %>%
      filter(Series %in% series) %>%
      mutate(Label = lbl[Series])
    
    p <- ggplot(d, aes(x = Year, y = Pct,
                       color = Label, group = Label,
                       text = paste0(Label, " (", Year, "): ", round(Pct, 1), "%"))) +
      geom_hline(yintercept = 0, color = "#ccc", linetype = "dashed") +
      geom_line(linewidth = 1.4) +
      geom_point(size = 2.2) +
      scale_color_manual(values = clr, name = NULL) +
      labs(y = "Annual % Change", x = NULL,
           caption = "Source: USDA Economic Research Service, CPI 1974-2024") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            plot.caption = element_text(color = "#999", size = 9))
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = 1.18))
  })
  
  # CPI CUMULATIVE ----------------------------------------------
  output$cpi_cumulative <- renderPlotly({
    series <- input$cpi_series
    years  <- input$cpi_years
    if (length(series) == 0) return(plotly_empty())
    
    lbl <- c(AllFood          = "All Food",
             FoodAtHome       = "Food at Home",
             FoodAwayFromHome = "Food Away from Home")
    clr <- c("All Food"            = "#e67e22",
             "Food at Home"        = "#27ae60",
             "Food Away from Home" = "#c0392b")
    
    d <- cpi %>%
      filter(Year >= years[1], Year <= years[2]) %>%
      pivot_longer(c(AllFood, FoodAtHome, FoodAwayFromHome),
                   names_to = "Series", values_to = "Pct") %>%
      filter(Series %in% series) %>%
      group_by(Series) %>%
      arrange(Year) %>%
      mutate(Cumul = cumprod(1 + Pct / 100) * 100 - 100,
             Label = lbl[Series]) %>%
      ungroup()
    
    p <- ggplot(d, aes(x = Year, y = Cumul,
                       color = Label, group = Label,
                       text = paste0(Label, " (", Year, "): +", round(Cumul, 1), "%"))) +
      geom_area(aes(fill = Label), alpha = 0.07, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = clr, name = NULL) +
      scale_fill_manual(values  = clr, name = NULL) +
      labs(y = "Cumulative % Rise", x = NULL,
           title = "Cumulative price increase from start of selected period") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none",
            plot.title = element_text(size = 10, color = "#555"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # DEMOGRAPHICS ------------------------------------------------
  output$demo_plot <- renderPlotly({
    yvar <- input$demo_yvar
    ylbl_map <- c(
      PCT_OBESE_ADULTS22    = "Obesity Rate (%)",
      PCT_DIABETES_ADULTS19 = "Diabetes Rate (%)",
      POVRATE21             = "Poverty Rate (%)",
      FOODINSEC_21_23       = "Food Insecurity (%)",
      PCT_LACCESS_POP19     = "Low Food Access (%)",
      PCT_SNAP22            = "SNAP Participation (%)"
    )
    ylabel <- ylbl_map[yvar]
    
    d <- atlas %>%
      select(County, State, all_of(yvar),
             PCT_NHBLACK20, PCT_HISP20, PCT_NHWHITE20, PCT_NHASIAN20) %>%
      filter(complete.cases(.)) %>%
      pivot_longer(c(PCT_NHBLACK20, PCT_HISP20, PCT_NHWHITE20, PCT_NHASIAN20),
                   names_to  = "Race",
                   values_to = "RacePct") %>%
      mutate(Race = dplyr::recode(Race,
                                  PCT_NHBLACK20 = "% Black",
                                  PCT_HISP20    = "% Hispanic",
                                  PCT_NHWHITE20 = "% White (Non-Hisp)",
                                  PCT_NHASIAN20 = "% Asian"))
    
    clr <- c("% Black"            = "#e74c3c",
             "% Hispanic"         = "#f39c12",
             "% White (Non-Hisp)" = "#3498db",
             "% Asian"            = "#2ecc71")
    
    p <- ggplot(d, aes(x = RacePct, y = .data[[yvar]],
                       color = Race,
                       text  = paste0(County, ", ", State))) +
      geom_point(alpha = 0.12, size = 1.2) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.6) +
      facet_wrap(~Race, scales = "free_x", nrow = 2) +
      scale_color_manual(values = clr) +
      labs(x = "Share of County Population (%)", y = ylabel,
           caption = "Source: USDA Food Env. Atlas + Census 2020 | Each point = 1 county") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
            strip.text   = element_text(face = "bold", size = 11),
            plot.caption = element_text(color = "#999", size = 9))
    
    ggplotly(p, tooltip = c("text", "x", "y")) %>%
      layout(showlegend = FALSE)
  })
  
  # LOCATION MAP ------------------------------------------------
  observeEvent(input$loc_all, {
    updateCheckboxGroupInput(session, "loc_chains",
                             selected = sort(unique(ff_locs$name)))
  })
  observeEvent(input$loc_none, {
    updateCheckboxGroupInput(session, "loc_chains", selected = character(0))
  })
  
  output$loc_map <- renderLeaflet({
    chains <- input$loc_chains
    
    if (length(chains) == 0) {
      return(
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -96, lat = 38, zoom = 4)
      )
    }
    
    d <- ff_locs %>% filter(name %in% chains)
    
    chain_pal <- colorFactor(
      palette = "Set1",
      domain  = ff_locs$name   # full domain keeps colors stable as chains toggle
    )
    
    leaflet(d) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng         = ~longitude,
        lat         = ~latitude,
        radius      = 5,
        color       = ~chain_pal(name),
        fillOpacity = 0.75,
        stroke      = FALSE,
        label       = ~paste0(name, " | ", city, ", ", province, " ", postalCode),
        clusterOptions = markerClusterOptions(
          maxClusterRadius    = 40,
          zoomToBoundsOnClick = TRUE
        )
      ) %>%
      addLegend(
        pal      = chain_pal,
        values   = ~name,
        title    = "Chain",
        position = "bottomright"
      ) %>%
      setView(lng = -96, lat = 38, zoom = 4)
  })
  
  # DATA TABLE --------------------------------------------------
  output$data_table <- renderDT({
    atlas %>%
      select(
        FIPS, State, County,
        `Obesity (%)`         = PCT_OBESE_ADULTS22,
        `Diabetes (%)`        = PCT_DIABETES_ADULTS19,
        `Poverty (%)`         = POVRATE21,
        `Child Poverty (%)`   = CHILDPOVRATE21,
        `Med Income ($)`      = MEDHHINC21,
        `Fast Food/1k`        = FFRPTH20,
        `Food Insecurity (%)` = FOODINSEC_21_23,
        `SNAP (%)`            = PCT_SNAP22,
        `Low Access (%)`      = PCT_LACCESS_POP19,
        `Dollar Stores/1k`    = DSPTH20,
        `Grocery/1k`          = GROCPTH20,
        `% Black`             = PCT_NHBLACK20,
        `% Hispanic`          = PCT_HISP20,
        `% White`             = PCT_NHWHITE20,
        `% Asian`             = PCT_NHASIAN20
      ) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
  },
  extensions = "Buttons",
  options = list(
    pageLength  = 15,
    scrollX     = TRUE,
    dom         = "Bfrtip",
    buttons     = c("csv", "excel"),
    columnDefs  = list(list(className = "dt-center", targets = "_all"))
  ))
  
} # end server

shinyApp(ui, server)
