# ============================================================
#  Fast Food, Obesity & Poverty in America — Shiny App
#  Data: USDA Food Environment Atlas, CDC, Census SAIPE
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

# ── 1. LOAD DATA ─────────────────────────────────────────────

atlas <- read.csv("atlas_wide.csv", stringsAsFactors = FALSE) %>%
  mutate(
    FIPS = sprintf("%05s", FIPS),
    across(c(CHILDPOVRATE21, DEEPPOVRATE21, DSPTH20, FFR20, FFRPTH20,
             FOODINSEC_21_23, GROCPTH20, MEDHHINC21,
             PCT_DIABETES_ADULTS19, PCT_HISP20,
             PCT_LACCESS_LOWI19, PCT_LACCESS_POP19,
             PCT_NHASIAN20, PCT_NHBLACK20, PCT_NHWHITE20,
             PCT_OBESE_ADULTS22, PCT_SNAP22, POVRATE21, RECFACPTH20),
           ~ suppressWarnings(as.numeric(na_if(as.character(.), ""))))
  )

cpi <- read.csv("historical_cpi.csv", stringsAsFactors = FALSE)

state_poverty <- read.csv("poverty_by_state.csv", stringsAsFactors = FALSE) %>%
  filter(State != "United States") %>%
  mutate(State = trimws(State))

# ── 2. COUNTY SHAPEFILE ──────────────────────────────────────
message("Loading county shapefile...")
county_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(4326) %>%
  mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  filter(!STATEFP %in% c("02","15","60","66","69","72","78"))  # continental US

county_map <- county_sf %>%
  left_join(atlas, by = "FIPS")

# ── 3. VARIABLE METADATA ─────────────────────────────────────
map_vars <- list(
  "Obesity Rate (%)"                  = "PCT_OBESE_ADULTS22",
  "Diabetes Rate (%)"                 = "PCT_DIABETES_ADULTS19",
  "Poverty Rate (%)"                  = "POVRATE21",
  "Child Poverty Rate (%)"            = "CHILDPOVRATE21",
  "Median Household Income ($)"       = "MEDHHINC21",
  "Fast Food Restaurants per 1k pop"  = "FFRPTH20",
  "Food Insecurity Rate (%)"          = "FOODINSEC_21_23",
  "SNAP Participation (%)"            = "PCT_SNAP22",
  "Low Food Access (%)"               = "PCT_LACCESS_POP19",
  "Dollar Stores per 1k pop"          = "DSPTH20",
  "Grocery Stores per 1k pop"         = "GROCPTH20",
  "Recreation Facilities per 1k pop"  = "RECFACPTH20",
  "% Black or African American"       = "PCT_NHBLACK20",
  "% Hispanic or Latino"              = "PCT_HISP20",
  "% White (Non-Hispanic)"            = "PCT_NHWHITE20"
)

scatter_vars <- c(
  "Obesity Rate (%)"                 = "PCT_OBESE_ADULTS22",
  "Diabetes Rate (%)"                = "PCT_DIABETES_ADULTS19",
  "Poverty Rate (%)"                 = "POVRATE21",
  "Median Household Income ($)"      = "MEDHHINC21",
  "Fast Food per 1k pop"             = "FFRPTH20",
  "Food Insecurity (%)"              = "FOODINSEC_21_23",
  "SNAP Participation (%)"           = "PCT_SNAP22",
  "Low Food Access (%)"              = "PCT_LACCESS_POP19",
  "Dollar Stores per 1k pop"         = "DSPTH20",
  "Grocery Stores per 1k pop"        = "GROCPTH20",
  "% Black or African American"      = "PCT_NHBLACK20",
  "% Hispanic or Latino"             = "PCT_HISP20"
)

palette_choices <- c("YlOrRd","RdPu","Blues","Greens","PuBu","Oranges","Purples")

# ── 4. UI ─────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "🍔 Food, Health & Poverty"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("County Map",        tabName = "map",   icon = icon("map")),
      menuItem("Scatter Explorer",  tabName = "scatter",icon = icon("circle-dot")),
      menuItem("State Rankings",    tabName = "bars",  icon = icon("chart-bar")),
      menuItem("Food Price Trends", tabName = "cpi",   icon = icon("chart-line")),
      menuItem("Demographics",      tabName = "demo",  icon = icon("users")),
      menuItem("Data Table",        tabName = "data",  icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-red .main-header .logo { background-color: #b5230a; }
      .skin-red .main-header .navbar { background-color: #c0392b; }
      .skin-red .main-sidebar { background-color: #2c3e50; }
      .skin-red .sidebar a { color: #ecf0f1 !important; }
      .info-box { min-height: 80px; }
      .info-box-icon { height: 80px; line-height: 80px; }
      .info-box-content { padding-top: 10px; }
      #map_leaf { height: 620px !important; }
      .section-header { font-size: 1.1em; font-weight: 600;
                        color: #2c3e50; margin: 8px 0 4px 0; }
    "))),
    
    tabItems(
      
      # ── TAB 1: MAP ──────────────────────────────────────────
      tabItem("map",
              fluidRow(
                box(width = 3, title = "Map Controls", status = "danger", solidHeader = TRUE,
                    selectInput("map_var", "Variable to Map:",
                                choices = map_vars, selected = "PCT_OBESE_ADULTS22"),
                    selectInput("map_palette", "Color Palette:",
                                choices = palette_choices, selected = "YlOrRd"),
                    checkboxInput("map_reverse", "Reverse palette", FALSE),
                    hr(),
                    p(class = "section-header", "Summary Statistics"),
                    verbatimTextOutput("map_summary"),
                    hr(),
                    p(style = "font-size:0.8em; color:#777;",
                      "Click a county on the map for details.")
                ),
                box(width = 9, title = "U.S. County Map", status = "danger", solidHeader = TRUE,
                    leafletOutput("map_leaf", height = 620)
                )
              ),
              fluidRow(
                valueBoxOutput("vbox_obese",  width = 3),
                valueBoxOutput("vbox_ff",     width = 3),
                valueBoxOutput("vbox_pov",    width = 3),
                valueBoxOutput("vbox_access", width = 3)
              )
      ),
      
      # ── TAB 2: SCATTER ──────────────────────────────────────
      tabItem("scatter",
              fluidRow(
                box(width = 3, title = "Scatter Controls", status = "warning", solidHeader = TRUE,
                    selectInput("sc_x", "X Axis:",
                                choices = scatter_vars, selected = "POVRATE21"),
                    selectInput("sc_y", "Y Axis:",
                                choices = scatter_vars, selected = "PCT_OBESE_ADULTS22"),
                    selectInput("sc_color", "Color points by:",
                                choices = c("Metro/Non-Metro" = "METRO23",
                                            "State" = "State",
                                            "None" = "none"),
                                selected = "METRO23"),
                    checkboxInput("sc_smooth", "Show trend line", TRUE),
                    sliderInput("sc_alpha", "Point opacity:", 0.1, 1, 0.4, 0.05),
                    hr(),
                    p(class = "section-header", "Correlation"),
                    verbatimTextOutput("sc_cor")
                ),
                box(width = 9, title = "Scatter Plot — County Level", status = "warning", solidHeader = TRUE,
                    plotlyOutput("scatter_plot", height = 560)
                )
              )
      ),
      
      # ── TAB 3: STATE BARS ────────────────────────────────────
      tabItem("bars",
              fluidRow(
                box(width = 3, title = "Chart Controls", status = "success", solidHeader = TRUE,
                    selectInput("bar_var", "Variable:",
                                choices = list(
                                  "Health" = c(
                                    "Avg Obesity Rate (%)"    = "PCT_OBESE_ADULTS22",
                                    "Avg Diabetes Rate (%)"   = "PCT_DIABETES_ADULTS19",
                                    "Avg Food Insecurity (%)" = "FOODINSEC_21_23"
                                  ),
                                  "Economics" = c(
                                    "Avg Poverty Rate (%)"    = "POVRATE21",
                                    "Avg Child Poverty (%)"   = "CHILDPOVRATE21",
                                    "Median HH Income ($)"    = "MEDHHINC21",
                                    "SNAP Participation (%)"  = "PCT_SNAP22"
                                  ),
                                  "Food Environment" = c(
                                    "Fast Food per 1k pop"    = "FFRPTH20",
                                    "Low Food Access (%)"     = "PCT_LACCESS_POP19",
                                    "Dollar Stores per 1k"    = "DSPTH20",
                                    "Grocery Stores per 1k"   = "GROCPTH20"
                                  )
                                ),
                                selected = "PCT_OBESE_ADULTS22"),
                    sliderInput("bar_n", "Show top N states:", 10, 51, 20),
                    radioButtons("bar_order", "Order by:",
                                 c("Highest first" = "desc", "Lowest first" = "asc")),
                    checkboxInput("bar_refline", "Show US average line", TRUE)
                ),
                box(width = 9, title = "State Rankings", status = "success", solidHeader = TRUE,
                    plotlyOutput("bar_plot", height = 580)
                )
              )
      ),
      
      # ── TAB 4: CPI TIME SERIES ───────────────────────────────
      tabItem("cpi",
              fluidRow(
                box(width = 3, title = "CPI Controls", status = "primary", solidHeader = TRUE,
                    checkboxGroupInput("cpi_series", "Series to display:",
                                       choices = c("All Food"            = "AllFood",
                                                   "Food at Home"        = "FoodAtHome",
                                                   "Food Away from Home" = "FoodAwayFromHome"),
                                       selected = c("AllFood","FoodAtHome","FoodAwayFromHome")),
                    sliderInput("cpi_years", "Year range:",
                                min = 1974, max = 2024, value = c(2000, 2024), sep = ""),
                    hr(),
                    p(class = "section-header", "Key Insight"),
                    p(style = "font-size:0.85em;",
                      "\"Food away from home\" (restaurants/fast food) has consistently",
                      " outpaced grocery inflation since 2020, making fast food",
                      " increasingly expensive relative to home cooking.")
                ),
                box(width = 9, title = "Annual Food Price Inflation (CPI % Change)", status = "primary", solidHeader = TRUE,
                    plotlyOutput("cpi_plot", height = 420),
                    hr(),
                    plotlyOutput("cpi_cumulative", height = 200)
                )
              )
      ),
      
      # ── TAB 5: DEMOGRAPHICS ─────────────────────────────────
      tabItem("demo",
              fluidRow(
                box(width = 3, title = "Demo Controls", status = "info", solidHeader = TRUE,
                    selectInput("demo_yvar", "Health/Economic Outcome (Y):",
                                choices = c("Obesity Rate (%)"    = "PCT_OBESE_ADULTS22",
                                            "Diabetes Rate (%)"   = "PCT_DIABETES_ADULTS19",
                                            "Poverty Rate (%)"    = "POVRATE21",
                                            "Food Insecurity (%)" = "FOODINSEC_21_23",
                                            "Low Food Access (%)" = "PCT_LACCESS_POP19"),
                                selected = "PCT_OBESE_ADULTS22"),
                    hr(),
                    p(class = "section-header", "State Poverty Ranking"),
                    p(style = "font-size:0.8em; color:#555;",
                      "Bar chart below shows family poverty rates by state (2019–2023 avg).")
                ),
                box(width = 9, title = "Outcome by Race/Ethnicity Share — County Level", status = "info", solidHeader = TRUE,
                    plotlyOutput("demo_plot", height = 400)
                )
              ),
              fluidRow(
                box(width = 12, title = "State Poverty Rates — Family Poverty (2019–2023 avg)", status = "info", solidHeader = TRUE,
                    plotlyOutput("state_pov_plot", height = 340)
                )
              )
      ),
      
      # ── TAB 6: DATA TABLE ───────────────────────────────────
      tabItem("data",
              fluidRow(
                box(width = 12, title = "County-Level Data Explorer", status = "danger", solidHeader = TRUE,
                    p("Browse, search, and download the underlying county data.
               All values from USDA Food Environment Atlas 2020–2022."),
                    DTOutput("data_table")
                )
              )
      )
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# ── 5. SERVER ─────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Summary stats for map sidebar ──────────────────────────
  output$map_summary <- renderPrint({
    var <- input$map_var
    vals <- atlas[[var]]
    vals <- vals[!is.na(vals)]
    cat(sprintf("Counties with data: %d\n", length(vals)))
    cat(sprintf("Min:    %.1f\n", min(vals)))
    cat(sprintf("Median: %.1f\n", median(vals)))
    cat(sprintf("Mean:   %.1f\n", mean(vals)))
    cat(sprintf("Max:    %.1f\n", max(vals)))
  })
  
  # ── Value boxes ─────────────────────────────────────────────
  output$vbox_obese <- renderValueBox({
    val <- round(mean(atlas$PCT_OBESE_ADULTS22, na.rm = TRUE), 1)
    valueBox(paste0(val, "%"), "Avg County Obesity Rate", icon = icon("weight-scale"), color = "red")
  })
  output$vbox_ff <- renderValueBox({
    val <- round(mean(atlas$FFRPTH20, na.rm = TRUE), 2)
    valueBox(val, "Avg Fast Food / 1k pop", icon = icon("burger"), color = "orange")
  })
  output$vbox_pov <- renderValueBox({
    val <- round(mean(atlas$POVRATE21, na.rm = TRUE), 1)
    valueBox(paste0(val, "%"), "Avg County Poverty Rate", icon = icon("house-chimney-crack"), color = "yellow")
  })
  output$vbox_access <- renderValueBox({
    val <- round(mean(atlas$PCT_LACCESS_POP19, na.rm = TRUE), 1)
    valueBox(paste0(val, "%"), "Avg Low Food Access", icon = icon("store-slash"), color = "blue")
  })
  
  # ── MAP ─────────────────────────────────────────────────────
  output$map_leaf <- renderLeaflet({
    var     <- input$map_var
    pal_name <- input$map_palette
    reverse  <- input$map_reverse
    
    data <- county_map
    vals <- data[[var]]
    
    pal <- colorNumeric(
      palette = pal_name,
      domain  = vals,
      na.color = "#cccccc",
      reverse = reverse
    )
    
    var_label <- names(map_vars)[map_vars == var]
    is_dollar  <- grepl("Income", var_label)
    
    fmt_val <- function(v) {
      if (is.na(v)) return("N/A")
      if (is_dollar) paste0("$", formatC(v, format = "f", digits = 0, big.mark = ","))
      else paste0(round(v, 1))
    }
    
    labels <- sprintf(
      "<strong>%s, %s</strong><br/>%s: %s",
      data$County, data$State,
      var_label,
      sapply(vals, fmt_val)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~pal(vals),
        fillOpacity = 0.8,
        color       = "#555",
        weight      = 0.3,
        smoothFactor = 0.5,
        highlight   = highlightOptions(
          weight = 2, color = "#333", fillOpacity = 0.95, bringToFront = TRUE),
        label       = labels,
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "3px 6px"),
          textsize  = "13px", direction = "auto")
      ) %>%
      addLegend(
        pal      = pal,
        values   = vals,
        title    = var_label,
        position = "bottomright",
        labFormat = if (is_dollar)
          labelFormat(prefix = "$", big.mark = ",", digits = 0)
        else
          labelFormat(digits = 1)
      ) %>%
      setView(lng = -96, lat = 38, zoom = 4)
  })
  
  # ── SCATTER ─────────────────────────────────────────────────
  output$scatter_plot <- renderPlotly({
    xvar  <- input$sc_x
    yvar  <- input$sc_y
    cvar  <- input$sc_color
    
    xlabel <- names(scatter_vars)[scatter_vars == xvar]
    ylabel <- names(scatter_vars)[scatter_vars == yvar]
    
    d <- atlas %>%
      select(County, State, all_of(c(xvar, yvar, if (cvar != "none") cvar))) %>%
      filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]]))
    
    p <- ggplot(d, aes(
      x    = .data[[xvar]],
      y    = .data[[yvar]],
      text = paste0("<b>", County, ", ", State, "</b>")
    ))
    
    if (cvar == "METRO23") {
      d$ColorGroup <- ifelse(d$METRO23 == 1, "Metro", "Non-Metro")
      p <- ggplot(d, aes(x = .data[[xvar]], y = .data[[yvar]],
                         color = ColorGroup,
                         text  = paste0("<b>", County, ", ", State, "</b>"))) +
        scale_color_manual(values = c("Metro" = "#e74c3c", "Non-Metro" = "#3498db"),
                           name = "Area Type")
    } else if (cvar == "State") {
      p <- ggplot(d, aes(x = .data[[xvar]], y = .data[[yvar]],
                         color = State,
                         text  = paste0("<b>", County, ", ", State, "</b>"))) +
        guides(color = "none")
    }
    
    p <- p +
      geom_point(alpha = input$sc_alpha, size = 1.8)
    
    if (input$sc_smooth) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#2c3e50",
                           linewidth = 1, alpha = 0.15, inherit.aes = FALSE,
                           aes(x = .data[[xvar]], y = .data[[yvar]]))
    }
    
    p <- p +
      labs(x = xlabel, y = ylabel,
           caption = "Source: USDA Food Environment Atlas 2020-2022") +
      theme_minimal(base_size = 13) +
      theme(plot.caption = element_text(color = "#999", size = 9))
    
    ggplotly(p, tooltip = c("text", "x", "y")) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.15))
  })
  
  output$sc_cor <- renderPrint({
    xvar <- input$sc_x
    yvar <- input$sc_y
    d    <- atlas %>% filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]]))
    r    <- cor(d[[xvar]], d[[yvar]], use = "complete.obs")
    r2   <- r^2
    cat(sprintf("r  = %.3f\nr² = %.3f\nn  = %d counties", r, r2, nrow(d)))
  })
  
  # ── STATE BAR CHART ─────────────────────────────────────────
  output$bar_plot <- renderPlotly({
    var    <- input$bar_var
    n      <- input$bar_n
    order  <- input$bar_order
    
    var_label <- names(unlist(list(
      "Avg Obesity Rate (%)"    = "PCT_OBESE_ADULTS22",
      "Avg Diabetes Rate (%)"   = "PCT_DIABETES_ADULTS19",
      "Avg Food Insecurity (%)" = "FOODINSEC_21_23",
      "Avg Poverty Rate (%)"    = "POVRATE21",
      "Avg Child Poverty (%)"   = "CHILDPOVRATE21",
      "Median HH Income ($)"    = "MEDHHINC21",
      "SNAP Participation (%)"  = "PCT_SNAP22",
      "Fast Food per 1k pop"    = "FFRPTH20",
      "Low Food Access (%)"     = "PCT_LACCESS_POP19",
      "Dollar Stores per 1k"    = "DSPTH20",
      "Grocery Stores per 1k"   = "GROCPTH20"
    )))[unlist(list(
      "Avg Obesity Rate (%)"    = "PCT_OBESE_ADULTS22",
      "Avg Diabetes Rate (%)"   = "PCT_DIABETES_ADULTS19",
      "Avg Food Insecurity (%)" = "FOODINSEC_21_23",
      "Avg Poverty Rate (%)"    = "POVRATE21",
      "Avg Child Poverty (%)"   = "CHILDPOVRATE21",
      "Median HH Income ($)"    = "MEDHHINC21",
      "SNAP Participation (%)"  = "PCT_SNAP22",
      "Fast Food per 1k pop"    = "FFRPTH20",
      "Low Food Access (%)"     = "PCT_LACCESS_POP19",
      "Dollar Stores per 1k"    = "DSPTH20",
      "Grocery Stores per 1k"   = "GROCPTH20"
    )) == var]
    
    state_agg <- atlas %>%
      filter(!is.na(.data[[var]]), State != "") %>%
      group_by(State) %>%
      summarise(val = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    
    us_avg <- mean(state_agg$val, na.rm = TRUE)
    
    if (order == "desc") {
      state_agg <- state_agg %>% arrange(desc(val)) %>% slice_head(n = n)
    } else {
      state_agg <- state_agg %>% arrange(val) %>% slice_head(n = n)
    }
    
    state_agg$State <- factor(state_agg$State, levels = state_agg$State)
    
    is_dollar <- grepl("Income", var)
    fmt_hover <- if (is_dollar)
      paste0("$", formatC(state_agg$val, format = "f", digits = 0, big.mark = ","))
    else
      paste0(round(state_agg$val, 1))
    
    p <- ggplot(state_agg, aes(x = State, y = val,
                               text = paste0(State, ": ", fmt_hover))) +
      geom_col(fill = "#c0392b", alpha = 0.85) +
      labs(y = var, x = NULL,
           caption = "Source: USDA Food Environment Atlas") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(color = "#999", size = 9))
    
    if (input$bar_refline) {
      p <- p + geom_hline(yintercept = us_avg, linetype = "dashed",
                          color = "#2c3e50", linewidth = 0.8) +
        annotate("text", x = 1.5, y = us_avg * 1.02,
                 label = paste0("US avg: ", round(us_avg, 1)),
                 color = "#2c3e50", size = 3.5, hjust = 0)
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  # ── CPI TIME SERIES ──────────────────────────────────────────
  output$cpi_plot <- renderPlotly({
    series <- input$cpi_series
    years  <- input$cpi_years
    
    cpi_long <- cpi %>%
      filter(Year >= years[1], Year <= years[2]) %>%
      pivot_longer(c(AllFood, FoodAtHome, FoodAwayFromHome),
                   names_to = "Series", values_to = "PctChange") %>%
      filter(Series %in% series)
    
    labels_map <- c(AllFood = "All Food",
                    FoodAtHome = "Food at Home (Groceries)",
                    FoodAwayFromHome = "Food Away from Home (Restaurants)")
    
    cpi_long$SeriesLabel <- labels_map[cpi_long$Series]
    
    colors <- c("All Food" = "#e67e22",
                "Food at Home (Groceries)" = "#27ae60",
                "Food Away from Home (Restaurants)" = "#c0392b")
    
    p <- ggplot(cpi_long, aes(x = Year, y = PctChange,
                              color = SeriesLabel,
                              group = SeriesLabel,
                              text = paste0(SeriesLabel, " (", Year, "): ",
                                            round(PctChange, 1), "%"))) +
      geom_hline(yintercept = 0, color = "#ccc", linetype = "dashed") +
      geom_line(linewidth = 1.4) +
      geom_point(size = 2.5) +
      scale_color_manual(values = colors, name = NULL) +
      labs(y = "Annual % Change", x = NULL,
           caption = "Source: USDA Economic Research Service") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            plot.caption = element_text(color = "#999", size = 9))
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = 1.15))
  })
  
  output$cpi_cumulative <- renderPlotly({
    years <- input$cpi_years
    series <- input$cpi_series
    
    cpi_long <- cpi %>%
      filter(Year >= years[1], Year <= years[2]) %>%
      pivot_longer(c(AllFood, FoodAtHome, FoodAwayFromHome),
                   names_to = "Series", values_to = "PctChange") %>%
      filter(Series %in% series) %>%
      group_by(Series) %>%
      arrange(Year) %>%
      mutate(Cumulative = cumprod(1 + PctChange/100) * 100 - 100) %>%
      ungroup()
    
    labels_map <- c(AllFood = "All Food",
                    FoodAtHome = "Food at Home",
                    FoodAwayFromHome = "Food Away from Home")
    cpi_long$SeriesLabel <- labels_map[cpi_long$Series]
    colors <- c("All Food" = "#e67e22",
                "Food at Home" = "#27ae60",
                "Food Away from Home" = "#c0392b")
    
    p <- ggplot(cpi_long, aes(x = Year, y = Cumulative,
                              color = SeriesLabel, group = SeriesLabel,
                              text = paste0(SeriesLabel, " (", Year, "): +",
                                            round(Cumulative, 1), "%"))) +
      geom_area(aes(fill = SeriesLabel), alpha = 0.08, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values  = colors, name = NULL) +
      labs(y = "Cumulative % Change", x = NULL,
           title = "Cumulative price increase from start of selected range") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none",
            plot.title = element_text(size = 10, color = "#555"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # ── DEMOGRAPHICS ─────────────────────────────────────────────
  output$demo_plot <- renderPlotly({
    yvar   <- input$demo_yvar
    ylabel <- names(c("Obesity Rate (%)"    = "PCT_OBESE_ADULTS22",
                      "Diabetes Rate (%)"   = "PCT_DIABETES_ADULTS19",
                      "Poverty Rate (%)"    = "POVRATE21",
                      "Food Insecurity (%)" = "FOODINSEC_21_23",
                      "Low Food Access (%)" = "PCT_LACCESS_POP19"))[
                        c("PCT_OBESE_ADULTS22","PCT_DIABETES_ADULTS19",
                          "POVRATE21","FOODINSEC_21_23","PCT_LACCESS_POP19") == yvar]
    
    d <- atlas %>%
      select(County, State,
             all_of(yvar),
             PCT_NHBLACK20, PCT_HISP20, PCT_NHWHITE20, PCT_NHASIAN20) %>%
      filter(complete.cases(.)) %>%
      pivot_longer(c(PCT_NHBLACK20, PCT_HISP20, PCT_NHWHITE20, PCT_NHASIAN20),
                   names_to = "Race", values_to = "RacePct") %>%
      mutate(Race = recode(Race,
                           PCT_NHBLACK20 = "% Black",
                           PCT_HISP20    = "% Hispanic",
                           PCT_NHWHITE20 = "% White (Non-Hisp)",
                           PCT_NHASIAN20 = "% Asian"))
    
    p <- ggplot(d, aes(x = RacePct, y = .data[[yvar]], color = Race)) +
      geom_point(alpha = 0.15, size = 1.2) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
      facet_wrap(~Race, scales = "free_x", nrow = 1) +
      scale_color_manual(values = c(
        "% Black"          = "#e74c3c",
        "% Hispanic"       = "#f39c12",
        "% White (Non-Hisp)" = "#3498db",
        "% Asian"          = "#2ecc71")) +
      labs(x = "Share of County Population (%)", y = ylabel,
           caption = "Source: USDA Food Environment Atlas / Census 2020") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold"),
            plot.caption = element_text(color = "#999", size = 9))
    
    ggplotly(p, tooltip = c("x","y")) %>%
      layout(showlegend = FALSE)
  })
  
  output$state_pov_plot <- renderPlotly({
    d <- state_poverty %>%
      arrange(PovertyPct) %>%
      mutate(State = factor(State, levels = State))
    
    p <- ggplot(d, aes(x = State, y = PovertyPct,
                       text = paste0(State, ": ", PovertyPct, "%"))) +
      geom_col(aes(fill = PovertyPct)) +
      scale_fill_gradient(low = "#f9e79f", high = "#c0392b", name = "Poverty %") +
      geom_hline(yintercept = 8.7, linetype = "dashed", color = "#2c3e50") +
      annotate("text", x = 3, y = 9.3, label = "US avg: 8.7%",
               color = "#2c3e50", size = 3) +
      labs(y = "Family Poverty Rate (%)", x = NULL,
           caption = "Source: HDPulse / Census ACS 2019–2023") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 8),
            legend.position = "none",
            plot.caption = element_text(color = "#999", size = 9))
    
    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(tickangle = -55))
  })
  
  # ── DATA TABLE ───────────────────────────────────────────────
  output$data_table <- renderDT({
    atlas %>%
      select(FIPS, State, County,
             `Obesity (%)` = PCT_OBESE_ADULTS22,
             `Diabetes (%)` = PCT_DIABETES_ADULTS19,
             `Poverty (%)` = POVRATE21,
             `Med Income ($)` = MEDHHINC21,
             `Fast Food/1k` = FFRPTH20,
             `Food Insecurity (%)` = FOODINSEC_21_23,
             `SNAP (%)` = PCT_SNAP22,
             `Low Access (%)` = PCT_LACCESS_POP19,
             `Dollar Stores/1k` = DSPTH20,
             `Grocery/1k` = GROCPTH20,
             `% Black` = PCT_NHBLACK20,
             `% Hispanic` = PCT_HISP20,
             `% White` = PCT_NHWHITE20) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
  },
  extensions = "Buttons",
  options = list(
    pageLength = 15,
    scrollX = TRUE,
    dom = "Bfrtip",
    buttons = c("csv", "excel"),
    columnDefs = list(list(className = "dt-center", targets = "_all"))
  ))
  
} # end server

shinyApp(ui, server)
