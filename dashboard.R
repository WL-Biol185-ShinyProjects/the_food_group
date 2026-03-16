# =============================================================
#  Fast Food & Health in America — dashboard.R
#  Single-file Shiny app. All data is real, read directly from
#  the CSV files in ./data/. Run with: shiny::runApp("dashboard.R")
#
#  Required packages (install once):
#    install.packages(c("shiny","bslib","plotly","DT","leaflet",
#                       "leaflet.extras","dplyr","readr","tidyr",
#                       "stringr","forcats","scales","geojsonsf"))
# =============================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(forcats)
library(scales)

# ── helper: load data from CSV files ─────────────────────────
load_data <- function(data_dir = "data") {

  # 1. Menu nutrition (514 items, 8 restaurants)
  nutrition <- read_csv(file.path(data_dir, "fastfood.csv"),
                        show_col_types = FALSE) |>
    rename_with(str_to_lower) |>
    mutate(across(c(calories, sodium, total_fat, sat_fat,
                    trans_fat, cholesterol, total_carb,
                    fiber, sugar, protein), as.numeric)) |>
    filter(!is.na(calories))

  # 2. Top 50 chains (2021 QSR data)
  chains <- read_csv(
    file.path(data_dir, "Top_50_Fast-Food_Chains_in_USA.csv"),
    show_col_types = FALSE
  ) |>
    rename(
      chain       = `Fast-Food Chains`,
      sales_m     = `U.S. Systemwide Sales (Millions - U.S Dollars)`,
      avg_unit_k  = `Average Sales per Unit (Thousands - U.S Dollars)`,
      franchised  = `Franchised Stores`,
      company     = `Company Stores`,
      total_units = `2021 Total Units`,
      unit_change = `Total Change in Units from 2020`
    ) |>
    mutate(across(c(sales_m, avg_unit_k, franchised,
                    company, total_units, unit_change), as.numeric)) |>
    arrange(desc(sales_m))

  # 3. Obesity by state (CDC BRFSS)
  obesity <- read_csv(
    file.path(data_dir, "National_Obesity_by_State.csv"),
    show_col_types = FALSE
  ) |>
    rename_with(str_to_lower) |>
    rename(state = name, obesity_pct = obesity) |>
    mutate(obesity_pct = as.numeric(obesity_pct)) |>
    filter(!is.na(obesity_pct), !state %in% c("Puerto Rico", NA))

  # 4. Poverty by state (U.S. Census SAIPE, via HDPulse)
  pov_raw <- read_csv(
    file.path(data_dir, "HDPulse_data_export-2.csv"),
    skip = 4, show_col_types = FALSE
  )
  colnames(pov_raw)[1:3] <- c("state", "fips", "poverty_pct")
  poverty <- pov_raw |>
    filter(!state %in% c("State", "United States", "Puerto Rico", "", NA)) |>
    mutate(poverty_pct = as.numeric(poverty_pct)) |>
    filter(!is.na(poverty_pct)) |>
    select(state, poverty_pct)

  # 5. Race / ethnicity by state (KFF 2024)
  raw_lines <- readLines(file.path(data_dir, "raw_data.csv"))
  race <- read_csv(I(raw_lines[-(1:2)]), show_col_types = FALSE) |>
    rename_with(str_to_lower) |>
    rename(state = location) |>
    mutate(across(c(white, black, hispanic, asian), as.numeric)) |>
    filter(!state %in% c("", "Location", NA), !is.na(white))

  # 6. Restaurant locations (9,950 US locations)
  locations <- read_csv(
    file.path(data_dir, "FastFoodRestaurants.csv"),
    show_col_types = FALSE
  ) |>
    rename_with(str_to_lower) |>
    mutate(latitude  = as.numeric(latitude),
           longitude = as.numeric(longitude),
           name      = str_trim(name),
           state     = str_trim(province),
           city      = str_trim(city)) |>
    filter(!is.na(latitude), !is.na(longitude),
           between(longitude, -170, -60),
           between(latitude,   17,  72)) |>
    select(name, latitude, longitude, city, state)

  list(nutrition = nutrition, chains = chains, obesity = obesity,
       poverty = poverty, race = race, locations = locations)
}

# ── load once at startup ─────────────────────────────────────
dat <- load_data()

# ── chain brand colours (used in map + legend) ───────────────
chain_colors <- c(
  "McDonald's"     = "#e8a000",
  "Burger King"    = "#e63000",
  "Taco Bell"      = "#7b2d8b",
  "Wendy's"        = "#c0392b",
  "Arby's"         = "#8b2500",
  "KFC"            = "#b5451b",
  "Domino's Pizza" = "#00437a",
  "Subway"         = "#009a44",
  "SONIC Drive In" = "#0062a3",
  "Sonic Drive-In" = "#0062a3",
  "Hardee's"       = "#e87722",
  "Jack in the Box"= "#e05206",
  "Chick-fil-A"    = "#dd0031",
  "Five Guys"      = "#cc0000",
  "Dairy Queen"    = "#cc0099",
  "Whataburger"    = "#f5821f",
  "Jimmy John's"   = "#222222",
  "Pizza Hut"      = "#d62300",
  "Bojangles' Famous Chicken 'n Biscuits" = "#e87722"
)
get_color <- function(name) {
  unname(ifelse(name %in% names(chain_colors), chain_colors[name], "#888888"))
}

top_map_chains <- c(
  "McDonald's", "Burger King", "Taco Bell", "Wendy's",
  "Arby's", "KFC", "Domino's Pizza", "Subway",
  "SONIC Drive In", "Hardee's", "Jack in the Box", "Chick-fil-A"
)

# ── reusable UI helpers ───────────────────────────────────────
section_head <- function(eyebrow, title, source_note = NULL) {
  tagList(
    tags$p(eyebrow,
      style = paste("font-family:'Space Mono',monospace; font-size:.6rem;",
                    "letter-spacing:.2em; text-transform:uppercase;",
                    "color:#d4380d; margin-bottom:.15rem;")),
    tags$h4(title,
      style = "font-family:'DM Serif Display',serif; letter-spacing:-.02em; margin-bottom:.1rem;"),
    if (!is.null(source_note))
      tags$p(source_note,
        style = paste("font-family:'Space Mono',monospace; font-size:.65rem;",
                      "color:#8c7355; margin-bottom:1rem;"))
    else
      tags$hr(style = "border-color:#2d2520; margin:.4rem 0 1rem;")
  )
}

kpi_card <- function(output_id, label, icon_name) {
  div(
    style = paste("background:#1a1612; color:#f0ede6; padding:1.4rem 1.8rem;",
                  "border-top:3px solid #d4380d;"),
    tags$div(icon(icon_name), style = "font-size:1.2rem; color:#d4380d; margin-bottom:.3rem;"),
    div(textOutput(output_id, inline = TRUE),
      style = paste("font-family:'DM Serif Display',serif; font-size:2.6rem;",
                    "line-height:1; color:#e67e22; margin-bottom:.2rem;")),
    div(label,
      style = "font-size:.7rem; color:rgba(240,237,230,.55); text-transform:uppercase; letter-spacing:.08em;")
  )
}

# ── theme ─────────────────────────────────────────────────────
app_theme <- bs_theme(
  version      = 5,
  bg           = "#faf7f2",
  fg           = "#2d2520",
  primary      = "#d4380d",
  secondary    = "#8c7355",
  base_font    = font_google("DM Sans"),
  heading_font = font_google("DM Serif Display"),
  code_font    = font_google("Space Mono"),
  "navbar-bg"  = "#1a1612",
  "card-bg"    = "#ffffff",
  "card-border-color" = "#ede8df"
)

# ── shared plotly layout defaults ────────────────────────────
pl_layout <- list(
  plot_bgcolor  = "#ffffff",
  paper_bgcolor = "#ffffff",
  font          = list(family = "DM Sans", color = "#2d2520"),
  margin        = list(l = 10, r = 20, t = 20, b = 40)
)
apply_layout <- function(p) {
  p |> layout(
    plot_bgcolor  = pl_layout$plot_bgcolor,
    paper_bgcolor = pl_layout$paper_bgcolor,
    font          = pl_layout$font,
    margin        = pl_layout$margin
  )
}

# ─────────────────────────────────────────────────────────────
#  UI
# ─────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    tags$span("Fast Food", style = "font-family:'DM Serif Display',serif;"),
    tags$em(" & ", style = "color:#e67e22; font-style:italic;"),
    tags$span("Health",   style = "font-family:'DM Serif Display',serif;")
  ),
  theme    = app_theme,
  bg       = "#1a1612",
  fillable = TRUE,

  header = tags$head(
    tags$link(rel = "stylesheet", href = paste0(
      "https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;700",
      "&family=DM+Serif+Display:ital@0;1&family=Space+Mono:wght@400;700&display=swap")),
    tags$style(HTML("
      body { font-family:'DM Sans',sans-serif; background:#faf7f2; color:#2d2520; }
      h1,h2,h3,h4,h5 { font-family:'DM Serif Display',serif; font-weight:400; }
      .navbar { border-bottom:3px solid #d4380d !important; }
      .nav-link { font-size:.75rem !important; letter-spacing:.05em !important;
                  text-transform:uppercase !important; }
      .card { border:1px solid #ede8df; border-radius:0; }
      .card-header { background:#1a1612; color:#f0ede6;
                     font-family:'Space Mono',monospace; font-size:.72rem;
                     letter-spacing:.1em; text-transform:uppercase; border:none; }
      .nav-tabs .nav-link { border-radius:0; font-size:.7rem; letter-spacing:.06em;
                            text-transform:uppercase; font-family:'Space Mono',monospace;
                            color:#8c7355; }
      .nav-tabs .nav-link.active { color:#1a1612 !important;
                                   border-bottom-color:#1a1612 !important; font-weight:700; }
      table.dataTable thead th { background:#1a1612 !important; color:#f0ede6 !important;
                                 font-family:'Space Mono',monospace; font-size:.62rem;
                                 letter-spacing:.1em; text-transform:uppercase; }
      table.dataTable tbody tr:hover td { background:#f5f0e8 !important; }
      .selectize-input, .form-control, .form-select { border-radius:0; border-color:#d4c9b5; }
      ::-webkit-scrollbar { width:5px; height:5px; }
      ::-webkit-scrollbar-thumb { background:#d4c9b5; }
    "))
  ),

  # ── Overview ──────────────────────────────────────────────
  nav_panel("Overview", icon = icon("chart-bar"),
    div(
      style = paste("background:#1a1612; color:#f0ede6; padding:3rem 2rem 2rem;",
                    "position:relative; overflow:hidden;",
                    "background-image:",
                    "repeating-linear-gradient(0deg,transparent,transparent 39px,",
                    "rgba(255,255,255,.025) 39px,rgba(255,255,255,.025) 40px),",
                    "repeating-linear-gradient(90deg,transparent,transparent 39px,",
                    "rgba(255,255,255,.025) 39px,rgba(255,255,255,.025) 40px);"),
      tags$p("American Fast Food & Public Health · Data Explorer",
        style = paste("font-family:'Space Mono',monospace; font-size:.62rem;",
                      "letter-spacing:.18em; text-transform:uppercase;",
                      "color:#e67e22; border:1px solid rgba(230,126,34,.35);",
                      "display:inline-block; padding:.25rem .7rem; margin-bottom:1rem;")),
      tags$h1(
        tags$span("Fast Food,"), tags$br(),
        tags$em("Health & America", style = "color:#e67e22;"),
        style = "font-size:clamp(2.5rem,6vw,5rem); line-height:.95; letter-spacing:-.02em;"
      ),
      tags$p(paste(
        "An interactive look at how fast food chains, nutritional content,",
        "obesity rates, poverty, and racial demographics intersect across the US",
        "— built from 15 real-world datasets."
      ), style = "max-width:560px; color:rgba(240,237,230,.6); font-size:.88rem; line-height:1.75; margin-top:1rem;")
    ),
    br(),
    layout_columns(
      col_widths = c(3, 3, 3, 3), gap = "1px",
      kpi_card("kpi_chains",    "Chains in Dataset",       "store"),
      kpi_card("kpi_items",     "Menu Items Analyzed",     "utensils"),
      kpi_card("kpi_locations", "Restaurant Locations",    "map-pin"),
      kpi_card("kpi_states",    "States + DC",             "flag-usa")
    ),
    br(),
    layout_columns(
      col_widths = c(4, 4, 4), gap = "1rem",
      card(
        card_header("Most Obese State"),
        card_body(
          tags$p("Louisiana",
            style = "font-family:'DM Serif Display',serif; font-size:2rem; color:#c0392b; margin:0;"),
          tags$p("36.2% adult obesity rate · Source: CDC BRFSS",
            style = "font-family:'Space Mono',monospace; font-size:.72rem; color:#8c7355;")
        )
      ),
      card(
        card_header("Top Chain by Sales (2021)"),
        card_body(
          tags$p("McDonald's",
            style = "font-family:'DM Serif Display',serif; font-size:2rem; color:#e67e22; margin:0;"),
          tags$p("$45.96 B systemwide sales · Source: QSR Magazine",
            style = "font-family:'Space Mono',monospace; font-size:.72rem; color:#8c7355;")
        )
      ),
      card(
        card_header("Highest Avg Sodium (per item)"),
        card_body(
          tags$p("Arby's",
            style = "font-family:'DM Serif Display',serif; font-size:2rem; color:#8e44ad; margin:0;"),
          tags$p("1,515 mg avg sodium · Source: FDA menu data",
            style = "font-family:'Space Mono',monospace; font-size:.72rem; color:#8c7355;")
        )
      )
    )
  ),

  # ── Chains ────────────────────────────────────────────────
  nav_panel("Chains", icon = icon("store"),
    br(),
    div(style = "padding:0 .5rem;",
      section_head("Top 50 Fast Food Chains", "Sales & Market Presence",
                   "Source: QSR Magazine · 2021 data")),
    tabsetPanel(type = "tabs",

      tabPanel("Sales Bar Chart",
        br(),
        layout_sidebar(
          sidebar = sidebar(width = 230,
            sliderInput("chains_n", "Show top N chains:", 5, 50, 20, 5),
            selectInput("chains_sort", "Sort by:",
              choices = c("Sales ($M)" = "sales_m", "Total Units" = "total_units",
                          "Avg Sales/Unit ($K)" = "avg_unit_k",
                          "Unit Change '20→'21" = "unit_change")),
            hr(),
            tags$p("Color encodes value intensity.", style = "font-size:.72rem; color:#8c7355;")
          ),
          card(full_screen = TRUE,
               card_body(plotlyOutput("chains_bar", height = "580px")))
        )
      ),

      tabPanel("Bubble Chart",
        br(),
        layout_sidebar(
          sidebar = sidebar(width = 230,
            selectInput("bub_x", "X axis:",
              choices = c("Sales ($M)" = "sales_m", "Total Units" = "total_units",
                          "Avg/Unit ($K)" = "avg_unit_k")),
            selectInput("bub_y", "Y axis:",
              choices = c("Avg/Unit ($K)" = "avg_unit_k", "Sales ($M)" = "sales_m",
                          "Unit Change" = "unit_change", "Total Units" = "total_units")),
            selectInput("bub_size", "Bubble size:",
              choices = c("Total Units" = "total_units", "Sales ($M)" = "sales_m")),
            checkboxInput("bub_labels", "Show labels", TRUE)
          ),
          card(full_screen = TRUE,
               card_body(plotlyOutput("chains_bubble", height = "560px")))
        )
      ),

      tabPanel("Full Table",
        br(),
        card(card_body(padding = 0, DTOutput("chains_table")))
      )
    )
  ),

  # ── Nutrition ─────────────────────────────────────────────
  nav_panel("Nutrition", icon = icon("utensils"),
    br(),
    div(style = "padding:0 .5rem;",
      section_head("Menu Nutrition", "What's Really in Your Meal?",
                   "Source: FDA Menu Labeling Data · 514 items · 8 chains")),
    tabsetPanel(type = "tabs",

      tabPanel("Averages by Restaurant",
        br(),
        layout_sidebar(
          sidebar = sidebar(width = 230,
            selectInput("nutr_metric", "Metric:",
              choices = c("Calories" = "calories", "Sodium (mg)" = "sodium",
                          "Total Fat (g)" = "total_fat", "Protein (g)" = "protein",
                          "Sugar (g)" = "sugar", "Sat. Fat (g)" = "sat_fat")),
            radioButtons("nutr_stat", "Statistic:",
              choices = c("Mean" = "mean", "Median" = "median",
                          "Max" = "max", "Min" = "min"), selected = "mean"),
            checkboxInput("nutr_sort", "Sort by value", TRUE)
          ),
          card(full_screen = TRUE,
               card_body(plotlyOutput("nutr_bar", height = "430px")))
        )
      ),

      tabPanel("Nutrient Heatmap",
        br(),
        card(full_screen = TRUE,
             card_body(plotlyOutput("nutr_heatmap", height = "480px")))
      ),

      tabPanel("Item Scatter",
        br(),
        layout_sidebar(
          sidebar = sidebar(width = 230,
            selectInput("sc_x", "X axis:",
              choices = c("Calories" = "calories", "Sodium" = "sodium",
                          "Total Fat" = "total_fat", "Protein" = "protein")),
            selectInput("sc_y", "Y axis:",
              choices = c("Sodium" = "sodium", "Calories" = "calories",
                          "Total Fat" = "total_fat", "Protein" = "protein")),
            selectInput("sc_col", "Color by:",
              choices = c("Restaurant" = "restaurant"))
          ),
          card(full_screen = TRUE,
               card_body(plotlyOutput("nutr_scatter", height = "520px")))
        )
      ),

      tabPanel("Distributions",
        br(),
        layout_sidebar(
          sidebar = sidebar(width = 230,
            selectInput("dist_metric", "Metric:",
              choices = c("Calories" = "calories", "Sodium" = "sodium",
                          "Total Fat" = "total_fat", "Protein" = "protein")),
            checkboxGroupInput("dist_chains", "Chains:",
              choices  = sort(unique(dat$nutrition$restaurant)),
              selected = sort(unique(dat$nutrition$restaurant)))
          ),
          card(full_screen = TRUE,
               card_body(plotlyOutput("nutr_violin", height = "480px")))
        )
      ),

      tabPanel("All Items",
        br(),
        card(card_body(padding = 0, DTOutput("nutr_table")))
      )
    )
  ),

  # ── Obesity & Poverty ────────────────────────────────────
  nav_panel("Obesity & Poverty", icon = icon("heartbeat"),
    br(),
    div(style = "padding:0 .5rem;",
      section_head("State Health Data", "Obesity & Poverty Across America",
                   "Sources: CDC BRFSS 2023 · U.S. Census SAIPE 2019–2023")),
    tabsetPanel(type = "tabs",

      tabPanel("Obesity Ranking",
        br(),
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE, card_header("All States — Adult Obesity Rate (%)"),
               card_body(plotlyOutput("ob_lollipop", height = "680px"))),
          card(full_screen = TRUE, card_header("Distribution"),
               card_body(plotlyOutput("ob_hist",  height = "290px"),
                         hr(),
                         plotlyOutput("ob_box",   height = "200px")))
        )
      ),

      tabPanel("Poverty Ranking",
        br(),
        layout_columns(col_widths = c(6, 6),
          card(full_screen = TRUE, card_header("States — Poverty Rate (%)"),
               card_body(plotlyOutput("pov_bar", height = "620px"))),
          card(full_screen = TRUE, card_header("Poverty vs Obesity Scatter"),
               card_body(plotlyOutput("pov_ob_scatter", height = "580px")))
        )
      ),

      tabPanel("State Deep-Dive",
        br(),
        layout_sidebar(
          sidebar = sidebar(width = 240,
            selectInput("state_sel", "Choose state:",
              choices  = sort(dat$obesity$state),
              selected = "Louisiana")),
          layout_columns(col_widths = c(6, 6),
            card(card_header("Gauge — vs. National Average"),
                 card_body(plotlyOutput("state_gauge", height = "320px"))),
            card(card_header("Peer Comparison — Top 20"),
                 card_body(plotlyOutput("state_peer", height = "320px")))
          )
        )
      )
    )
  ),

  # ── Demographics ─────────────────────────────────────────
  nav_panel("Demographics", icon = icon("users"),
    br(),
    div(style = "padding:0 .5rem;",
      section_head("Race & Ethnicity", "Demographics by State",
                   "Source: KFF State Health Facts · 2024")),
    tabsetPanel(type = "tabs",

      tabPanel("National Overview",
        br(),
        layout_columns(col_widths = c(5, 7),
          card(card_header("U.S. Racial Composition"),
               card_body(plotlyOutput("race_pie", height = "400px"))),
          card(full_screen = TRUE, card_header("Restaurant Locations by State (top 20)"),
               card_body(plotlyOutput("loc_state_bar", height = "400px")))
        )
      ),

      tabPanel("Stacked State Bars",
        br(),
        layout_sidebar(
          sidebar = sidebar(width = 220,
            checkboxGroupInput("race_groups", "Show groups:",
              choices  = c("White" = "white", "Black" = "black",
                           "Hispanic" = "hispanic", "Asian" = "asian"),
              selected = c("white", "black", "hispanic", "asian")),
            checkboxInput("race_sort", "Sort by White %", FALSE)
          ),
          card(full_screen = TRUE,
               card_body(plotlyOutput("race_stack", height = "650px")))
        )
      ),

      tabPanel("Race × Obesity",
        br(),
        layout_sidebar(
          sidebar = sidebar(width = 220,
            selectInput("race_ob_x", "Race group (X axis):",
              choices = c("White" = "white", "Black" = "black",
                          "Hispanic" = "hispanic", "Asian" = "asian")),
            checkboxInput("race_ob_lab", "Label states", FALSE)
          ),
          card(full_screen = TRUE,
               card_body(plotlyOutput("race_ob_scatter", height = "520px")))
        )
      )
    )
  ),

  # ── Map ───────────────────────────────────────────────────
  nav_panel("Map", icon = icon("map-marked-alt"),
    div(style = "padding:1rem 1rem 0;",
      section_head("Interactive Map", "9,950 Restaurant Locations Across America",
                   "Toggle chains · Overlay obesity or poverty · Click a state for details")),

    div(style = "padding:0 1rem; display:flex; flex-wrap:wrap; gap:.5rem; align-items:center; margin-bottom:.5rem;",
      uiOutput("map_chain_btns"),
      div(style = "margin-left:auto; display:flex; gap:.6rem; align-items:center;",
        selectInput("map_overlay", NULL,
          choices  = c("No Overlay" = "none",
                       "Obesity Rate" = "obesity",
                       "Poverty Rate" = "poverty"),
          width = "160px"),
        selectInput("map_mode", NULL,
          choices  = c("Cluster" = "cluster", "All Markers" = "all"),
          width = "150px"),
        div(textOutput("map_count", inline = TRUE),
          style = paste("font-family:'Space Mono',monospace; font-size:.7rem;",
                        "color:#8c7355; padding:.3rem .8rem; border:1px solid #ede8df;",
                        "background:white; white-space:nowrap;"))
      )
    ),

    div(style = "padding:0 1rem;",
        leafletOutput("main_map", height = "570px")),

    div(style = "padding:.5rem 1rem;", uiOutput("map_legend")),
    div(style = "padding:0 1rem 2rem;", uiOutput("map_state_panel"))
  ),

  # ── Data ─────────────────────────────────────────────────
  nav_panel("Data", icon = icon("table"),
    br(),
    div(style = "padding:0 .5rem;",
      section_head("Raw Data", "Data Explorer",
                   "Browse any dataset · Download as CSV")),
    layout_sidebar(
      sidebar = sidebar(width = 210,
        selectInput("exp_ds", "Dataset:",
          choices = c("Nutrition Items" = "nutrition", "Top 50 Chains" = "chains",
                      "Obesity by State" = "obesity", "Poverty by State" = "poverty",
                      "Race / Ethnicity" = "race", "Restaurant Locations" = "locations")),
        downloadButton("exp_dl", "Download CSV",
          style = paste("border-radius:0; background:#1a1612; color:#f0ede6; border:none;",
                        "font-family:'Space Mono',monospace; font-size:.7rem; width:100%;"))
      ),
      card(full_screen = TRUE,
           card_body(padding = 0, DTOutput("exp_table", height = "620px")))
    )
  ),

  nav_spacer(),
  nav_item(tags$a(icon("github"), " GitHub",
    href = "https://github.com/YOUR_USERNAME/fastfood-health-dashboard",
    target = "_blank", class = "btn btn-sm btn-outline-light me-2"))
)

# ─────────────────────────────────────────────────────────────
#  SERVER
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Overview KPIs (real counts from loaded data) ──────────
  output$kpi_chains    <- renderText(nrow(dat$chains))
  output$kpi_items     <- renderText(format(nrow(dat$nutrition),  big.mark = ","))
  output$kpi_locations <- renderText(format(nrow(dat$locations),  big.mark = ","))
  output$kpi_states    <- renderText(n_distinct(dat$obesity$state))

  # ─────────────────────────────────────────────────────────
  #  CHAINS
  # ─────────────────────────────────────────────────────────
  chains_df <- reactive({
    dat$chains |>
      arrange(desc(.data[[input$chains_sort]])) |>
      slice_head(n = input$chains_n)
  })

  output$chains_bar <- renderPlotly({
    df    <- chains_df() |>
      mutate(chain = fct_reorder(chain, .data[[input$chains_sort]]))
    vals  <- df[[input$chains_sort]]
    norm  <- (vals - min(vals, na.rm = TRUE)) /
             (max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE) + 1e-9)
    pal   <- colorRampPalette(c("#f5cba7", "#e67e22", "#d4380d", "#7b241c"))(100)
    cols  <- pal[pmax(1L, ceiling(norm * 100))]

    plot_ly(df,
      x = ~.data[[input$chains_sort]], y = ~chain, type = "bar",
      orientation = "h", marker = list(color = cols),
      hovertemplate = "<b>%{y}</b><br>Value: %{x:,.0f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = input$chains_sort, gridcolor = "#ede8df", zeroline = FALSE),
        yaxis = list(title = ""),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
        font = list(family = "DM Sans", color = "#2d2520"),
        margin = list(l = 10, r = 20, t = 10, b = 40)
      )
  })

  output$chains_bubble <- renderPlotly({
    df <- dat$chains
    p  <- plot_ly(df,
      x    = ~.data[[input$bub_x]],
      y    = ~.data[[input$bub_y]],
      size = ~.data[[input$bub_size]],
      text = ~chain, type = "scatter", mode = "markers",
      marker = list(
        sizemode  = "area",
        sizeref   = 2 * max(df[[input$bub_size]], na.rm = TRUE) / (50^2),
        color     = ~sales_m,
        colorscale= list(c(0, "#f5cba7"), c(.5, "#e67e22"), c(1, "#7b241c")),
        showscale = TRUE,
        colorbar  = list(title = "Sales $M"),
        opacity   = 0.85,
        line      = list(color = "rgba(0,0,0,.2)", width = .5)
      ),
      hovertemplate = "<b>%{text}</b><br>%{x:,.0f} / %{y:,.0f}<extra></extra>"
    )
    if (input$bub_labels)
      p <- p |> add_annotations(
        x = df[[input$bub_x]], y = df[[input$bub_y]], text = df$chain,
        showarrow = FALSE, font = list(size = 8, color = "#2d2520"),
        xanchor = "left", xshift = 6
      )
    p |> layout(
      xaxis = list(title = input$bub_x, gridcolor = "#ede8df", zeroline = FALSE),
      yaxis = list(title = input$bub_y, gridcolor = "#ede8df", zeroline = FALSE),
      plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
      font = list(family = "DM Sans", color = "#2d2520"),
      showlegend = FALSE, margin = list(l = 10, r = 20, t = 10, b = 50)
    )
  })

  output$chains_table <- renderDT({
    dat$chains |>
      mutate(
        sales_m     = dollar(sales_m,     suffix = "M", prefix = "$"),
        avg_unit_k  = dollar(avg_unit_k,  suffix = "K", prefix = "$"),
        franchised  = comma(franchised),
        company     = comma(company),
        total_units = comma(total_units),
        unit_change = ifelse(unit_change >= 0,
                             paste0("+", comma(unit_change)), comma(unit_change))
      ) |>
      rename("Chain" = chain, "Sales" = sales_m, "Avg/Unit" = avg_unit_k,
             "Franchised" = franchised, "Company" = company,
             "Total Units" = total_units, "Change '20→'21" = unit_change) |>
      datatable(rownames = FALSE, class = "compact stripe",
                options = list(pageLength = 25, dom = "ftip", scrollX = TRUE)) |>
      formatStyle("Change '20→'21",
        color = styleInterval(c(-0.001, 0),
                              c("#c0392b", "#8c7355", "#27ae60")),
        fontWeight = "bold")
  })

  # ─────────────────────────────────────────────────────────
  #  NUTRITION
  # ─────────────────────────────────────────────────────────
  nutr_chain_colors <- c(
    Mcdonalds = "#e8a000", `Chick Fil-A` = "#dd0031",
    Sonic = "#0062a3", Arbys = "#8b2500",
    `Burger King` = "#e63000", `Dairy Queen` = "#cc0099",
    Subway = "#009a44", `Taco Bell` = "#7b2d8b"
  )

  output$nutr_bar <- renderPlotly({
    fn <- switch(input$nutr_stat,
      mean   = \(x) mean(x, na.rm = TRUE),
      median = \(x) median(x, na.rm = TRUE),
      max    = \(x) max(x, na.rm = TRUE),
      min    = \(x) min(x, na.rm = TRUE)
    )
    df <- dat$nutrition |>
      group_by(restaurant) |>
      summarise(value = fn(.data[[input$nutr_metric]]),
                n_items = n(), .groups = "drop")
    if (input$nutr_sort) df <- df |> arrange(desc(value))
    df <- df |> mutate(restaurant = fct_inorder(restaurant))
    pal <- colorRampPalette(c("#f5cba7", "#e67e22", "#d4380d", "#7b241c"))(nrow(df))

    plot_ly(df, x = ~value, y = ~restaurant, type = "bar", orientation = "h",
      marker = list(color = pal), customdata = ~n_items,
      hovertemplate = "<b>%{y}</b><br>%{x:.1f} (n=%{customdata})<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = paste(str_to_title(input$nutr_stat), input$nutr_metric),
                     gridcolor = "#ede8df", zeroline = FALSE),
        yaxis = list(title = ""),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
        font = list(family = "DM Sans", color = "#2d2520"),
        margin = list(l = 10, r = 40, t = 10, b = 40)
      )
  })

  output$nutr_heatmap <- renderPlotly({
    metrics <- c("calories", "sodium", "total_fat", "protein", "sugar", "sat_fat")
    df <- dat$nutrition |>
      group_by(restaurant) |>
      summarise(across(all_of(metrics), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
    # Normalise each column 0–1 for cross-metric comparison
    mat <- df |>
      mutate(across(all_of(metrics),
        \(x) (x - min(x, na.rm=TRUE)) /
             (max(x, na.rm=TRUE) - min(x, na.rm=TRUE) + 1e-9))) |>
      select(all_of(metrics))

    plot_ly(
      x = c("Calories", "Sodium", "Total Fat", "Protein", "Sugar", "Sat. Fat"),
      y = df$restaurant,
      z = as.matrix(mat),
      type = "heatmap",
      colorscale = list(c(0,"#f9f3e8"), c(.5,"#e67e22"), c(1,"#7b241c")),
      hovertemplate = "<b>%{y}</b> — %{x}<br>Normalised: %{z:.2f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "", tickangle = -20),
        yaxis = list(title = ""),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
        font = list(family = "DM Sans", color = "#2d2520"),
        margin = list(l = 10, r = 80, t = 10, b = 60)
      )
  })

  output$nutr_scatter <- renderPlotly({
    df    <- dat$nutrition |>
      filter(!is.na(.data[[input$sc_x]]), !is.na(.data[[input$sc_y]]))
    cols  <- unname(nutr_chain_colors[df$restaurant])
    cols[is.na(cols)] <- "#888888"

    plot_ly(df,
      x = ~.data[[input$sc_x]], y = ~.data[[input$sc_y]],
      color = ~restaurant, colors = unname(nutr_chain_colors),
      type = "scatter", mode = "markers", text = ~item,
      marker = list(size = 6, opacity = .75,
                    line = list(color = "rgba(0,0,0,.15)", width = .4)),
      hovertemplate = "<b>%{text}</b><br>%{x:.0f} / %{y:.0f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = input$sc_x, gridcolor = "#ede8df", zeroline = FALSE),
        yaxis = list(title = input$sc_y, gridcolor = "#ede8df", zeroline = FALSE),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
        font = list(family = "DM Sans", color = "#2d2520"),
        legend = list(orientation = "h", y = -0.15),
        margin = list(l = 20, r = 20, t = 10, b = 60)
      )
  })

  output$nutr_violin <- renderPlotly({
    df <- dat$nutrition |>
      filter(restaurant %in% input$dist_chains,
             !is.na(.data[[input$dist_metric]]))
    plot_ly(df,
      x = ~restaurant, y = ~.data[[input$dist_metric]],
      color = ~restaurant, colors = unname(nutr_chain_colors),
      type = "violin", box = list(visible = TRUE),
      meanline = list(visible = TRUE, color = "white", width = 2),
      hovertemplate = "<b>%{x}</b><br>%{y:.0f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = input$dist_metric, gridcolor = "#ede8df"),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
        font = list(family = "DM Sans", color = "#2d2520"),
        showlegend = FALSE, margin = list(l = 20, r = 10, t = 10, b = 50)
      )
  })

  output$nutr_table <- renderDT({
    datatable(dat$nutrition, rownames = FALSE, filter = "top",
      class = "compact stripe",
      options = list(pageLength = 20, dom = "ftip", scrollX = TRUE))
  })

  # ─────────────────────────────────────────────────────────
  #  OBESITY & POVERTY
  # ─────────────────────────────────────────────────────────
  ob_pov <- reactive({
    dat$obesity |> left_join(dat$poverty, by = "state")
  })

  ob_color <- function(v) {
    dplyr::case_when(v >= 34 ~ "#922b21", v >= 30 ~ "#d4380d",
                     v >= 26 ~ "#e67e22", TRUE ~ "#27ae60")
  }

  output$ob_lollipop <- renderPlotly({
    df <- ob_pov() |>
      arrange(desc(obesity_pct)) |>
      mutate(state = fct_inorder(state), col = ob_color(obesity_pct))
    plot_ly(df) |>
      add_segments(x = 0, xend = ~obesity_pct, y = ~state, yend = ~state,
        line = list(color = "rgba(0,0,0,.1)", width = 1)) |>
      add_markers(x = ~obesity_pct, y = ~state,
        marker = list(color = ~col, size = 10,
                      line = list(color = "white", width = 1)),
        text = ~paste0(state, ": ", obesity_pct, "%"), hoverinfo = "text") |>
      layout(
        xaxis = list(title = "Adult Obesity Rate (%)", range = c(18, 40),
                     gridcolor = "#ede8df", zeroline = FALSE),
        yaxis = list(title = "", tickfont = list(size = 10)),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
        font = list(family = "DM Sans", color = "#2d2520"),
        showlegend = FALSE, margin = list(l = 10, r = 10, t = 10, b = 40)
      )
  })

  output$ob_hist <- renderPlotly({
    df <- ob_pov() |> filter(!is.na(obesity_pct))
    plot_ly(df, x = ~obesity_pct, type = "histogram", nbinsx = 14,
      marker = list(color = "#d4380d", line = list(color = "white", width = .8)),
      hovertemplate = "Bin: %{x}<br>States: %{y}<extra></extra>") |>
      layout(xaxis = list(title = "Obesity Rate (%)", gridcolor = "#ede8df"),
             yaxis = list(title = "# States", gridcolor = "#ede8df"),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             font = list(family = "DM Sans", color = "#2d2520"),
             margin = list(l = 10, r = 10, t = 5, b = 40))
  })

  output$ob_box <- renderPlotly({
    df <- ob_pov() |> filter(!is.na(obesity_pct))
    plot_ly(df, x = ~obesity_pct, type = "box",
      marker    = list(color = "#d4380d", size = 4),
      line      = list(color = "#922b21"),
      fillcolor = "rgba(212,56,13,.15)",
      hovertemplate = "%{x:.1f}%<extra></extra>") |>
      layout(xaxis = list(title = "Obesity Rate (%)", gridcolor = "#ede8df"),
             yaxis = list(showticklabels = FALSE),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             font = list(family = "DM Sans", color = "#2d2520"),
             margin = list(l = 10, r = 10, t = 5, b = 40))
  })

  output$pov_bar <- renderPlotly({
    df <- ob_pov() |> filter(!is.na(poverty_pct)) |>
      arrange(desc(poverty_pct)) |>
      mutate(state = fct_inorder(state),
             col   = case_when(poverty_pct >= 13 ~ "#6c3483",
                               poverty_pct >= 11 ~ "#8e44ad",
                               poverty_pct >= 9  ~ "#2c3e50",
                               poverty_pct >= 7  ~ "#2980b9",
                               TRUE              ~ "#16a085"))
    plot_ly(df, x = ~poverty_pct, y = ~state, type = "bar",
      orientation = "h", marker = list(color = ~col),
      hovertemplate = "<b>%{y}</b><br>Poverty: %{x:.1f}%<extra></extra>") |>
      layout(xaxis = list(title = "Poverty Rate (%)", gridcolor = "#ede8df", zeroline = FALSE),
             yaxis = list(title = "", tickfont = list(size = 10)),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             font = list(family = "DM Sans", color = "#2d2520"),
             margin = list(l = 10, r = 10, t = 10, b = 40))
  })

  output$pov_ob_scatter <- renderPlotly({
    df <- ob_pov() |> filter(!is.na(poverty_pct), !is.na(obesity_pct)) |>
      mutate(col = ob_color(obesity_pct))
    r  <- round(cor(df$poverty_pct, df$obesity_pct, use = "complete.obs"), 2)
    plot_ly(df, x = ~poverty_pct, y = ~obesity_pct, type = "scatter", mode = "markers+text",
      text = ~state, textposition = "top right",
      textfont = list(size = 8, color = "#8c7355"),
      marker = list(color = ~col, size = 10, opacity = .8,
                    line = list(color = "white", width = 1)),
      hovertemplate = "<b>%{text}</b><br>Poverty: %{x:.1f}%<br>Obesity: %{y:.1f}%<extra></extra>"
    ) |>
      layout(
        title  = list(text = paste0("Pearson r = ", r),
                      font = list(size = 11, color = "#8c7355"), x = .01),
        xaxis  = list(title = "Poverty Rate (%)", gridcolor = "#ede8df", zeroline = FALSE),
        yaxis  = list(title = "Obesity Rate (%)", gridcolor = "#ede8df", zeroline = FALSE),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
        font = list(family = "DM Sans", color = "#2d2520"),
        showlegend = FALSE, margin = list(l = 20, r = 20, t = 30, b = 50)
      )
  })

  output$state_gauge <- renderPlotly({
    req(input$state_sel)
    df  <- ob_pov() |> filter(state == input$state_sel)
    avg_ob  <- mean(ob_pov()$obesity_pct,  na.rm = TRUE)
    avg_pov <- mean(ob_pov()$poverty_pct,  na.rm = TRUE)
    plot_ly() |>
      add_trace(type = "indicator", mode = "gauge+number+delta",
        value = df$obesity_pct[1],
        title = list(text = "Obesity Rate (%)"),
        delta = list(reference = avg_ob, valueformat = ".1f"),
        gauge = list(axis = list(range = list(18, 40)), bar = list(color = "#d4380d"),
          threshold = list(line = list(color = "#2d2520", width = 2),
                           thickness = .75, value = avg_ob)),
        domain = list(x = c(0,1), y = c(.52, 1))) |>
      add_trace(type = "indicator", mode = "gauge+number+delta",
        value = ifelse(!is.na(df$poverty_pct[1]), df$poverty_pct[1], 0),
        title = list(text = "Poverty Rate (%)"),
        delta = list(reference = avg_pov, valueformat = ".1f"),
        gauge = list(axis = list(range = list(0, 16)), bar = list(color = "#8e44ad"),
          threshold = list(line = list(color = "#2d2520", width = 2),
                           thickness = .75, value = avg_pov)),
        domain = list(x = c(0,1), y = c(0, .45))) |>
      layout(plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             font = list(family = "DM Sans", color = "#2d2520"),
             margin = list(l = 20, r = 20, t = 10, b = 10))
  })

  output$state_peer <- renderPlotly({
    req(input$state_sel)
    df <- ob_pov() |> filter(!is.na(obesity_pct)) |>
      arrange(desc(obesity_pct)) |>
      slice_head(n = 20) |>
      mutate(col = ifelse(state == input$state_sel, "#d4380d", "#d4c9b5"))
    plot_ly(df, x = ~obesity_pct, y = ~fct_inorder(state), type = "bar",
      orientation = "h", marker = list(color = ~col),
      hovertemplate = "<b>%{y}</b>: %{x:.1f}%<extra></extra>") |>
      layout(xaxis = list(title = "Obesity Rate (%)", gridcolor = "#ede8df", zeroline = FALSE),
             yaxis = list(title = "", tickfont = list(size = 10)),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             font = list(family = "DM Sans", color = "#2d2520"),
             margin = list(l = 10, r = 10, t = 10, b = 40))
  })

  # ─────────────────────────────────────────────────────────
  #  DEMOGRAPHICS
  # ─────────────────────────────────────────────────────────
  race_pal <- c(white = "#4e9af1", black = "#e87040",
                hispanic = "#54c066", asian = "#f0c040")

  output$race_pie <- renderPlotly({
    us  <- dat$race |> filter(state == "United States")
    grp <- c("white", "black", "hispanic", "asian")
    lbl <- c("White", "Black", "Hispanic", "Asian")
    plot_ly(labels = lbl, values = as.numeric(us[1, grp]), type = "pie",
      marker = list(colors = unname(race_pal[grp]),
                    line   = list(color = "white", width = 2)),
      textinfo = "label+percent",
      hovertemplate = "<b>%{label}</b>: %{percent}<extra></extra>") |>
      layout(showlegend = FALSE, plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             font = list(family = "DM Sans", color = "#2d2520", size = 11),
             margin = list(l = 10, r = 10, t = 10, b = 10))
  })

  output$loc_state_bar <- renderPlotly({
    df <- dat$locations |> count(state, name = "n") |>
      arrange(desc(n)) |> slice_head(n = 20) |>
      mutate(state = fct_inorder(state))
    pal <- colorRampPalette(c("#f5cba7", "#d4380d"))(20)
    plot_ly(df, x = ~n, y = ~state, type = "bar", orientation = "h",
      marker = list(color = pal),
      hovertemplate = "<b>%{y}</b>: %{x:,} locations<extra></extra>") |>
      layout(xaxis = list(title = "Locations in Dataset", gridcolor = "#ede8df", zeroline = FALSE),
             yaxis = list(title = ""),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             font = list(family = "DM Sans", color = "#2d2520"),
             margin = list(l = 10, r = 10, t = 10, b = 40))
  })

  output$race_stack <- renderPlotly({
    df <- dat$race |> filter(state != "United States", !is.na(white)) |>
      select(state, white, black, hispanic, asian)
    if (input$race_sort) df <- df |> arrange(white)
    df <- df |> mutate(state = fct_inorder(state))
    groups <- intersect(input$race_groups, c("white","black","hispanic","asian"))
    lbl    <- c(white="White", black="Black", hispanic="Hispanic", asian="Asian")
    p <- plot_ly()
    for (g in groups) {
      p <- p |> add_trace(
        x = df[[g]] * 100, y = df$state, type = "bar", orientation = "h",
        name = lbl[g], marker = list(color = race_pal[g]),
        hovertemplate = paste0("<b>%{y}</b><br>", lbl[g], ": %{x:.1f}%<extra></extra>"))
    }
    p |> layout(barmode = "stack",
                xaxis = list(title = "Population Share (%)", gridcolor = "#ede8df"),
                yaxis = list(title = "", tickfont = list(size = 10)),
                legend = list(orientation = "h", y = -.05),
                plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
                font = list(family = "DM Sans", color = "#2d2520"),
                margin = list(l = 10, r = 10, t = 10, b = 60))
  })

  output$race_ob_scatter <- renderPlotly({
    req(input$race_ob_x)
    df <- dat$race |> filter(state != "United States") |>
      inner_join(dat$obesity, by = "state") |>
      filter(!is.na(.data[[input$race_ob_x]]), !is.na(obesity_pct)) |>
      mutate(race_pct = .data[[input$race_ob_x]] * 100)
    r  <- round(cor(df$race_pct, df$obesity_pct, use = "complete.obs"), 2)
    p  <- plot_ly(df, x = ~race_pct, y = ~obesity_pct,
      type = "scatter", mode = "markers", text = ~state,
      marker = list(color = race_pal[input$race_ob_x], size = 10, opacity = .8,
                    line = list(color = "white", width = 1)),
      hovertemplate = "<b>%{text}</b><br>%{x:.1f}% / %{y:.1f}%<extra></extra>")
    if (input$race_ob_lab)
      p <- p |> add_annotations(x = df$race_pct, y = df$obesity_pct, text = df$state,
        showarrow = FALSE, font = list(size = 8, color = "#8c7355"),
        xanchor = "left", xshift = 5)
    p |> layout(
      title = list(text = paste0("Pearson r = ", r),
                   font = list(size = 11, color = "#8c7355"), x = .01),
      xaxis = list(title = paste0(str_to_title(input$race_ob_x), " Pop. (%)"),
                   gridcolor = "#ede8df", zeroline = FALSE),
      yaxis = list(title = "Obesity Rate (%)", gridcolor = "#ede8df", zeroline = FALSE),
      plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
      font = list(family = "DM Sans", color = "#2d2520"),
      showlegend = FALSE, margin = list(l = 20, r = 20, t = 30, b = 50))
  })

  # ─────────────────────────────────────────────────────────
  #  MAP
  # ─────────────────────────────────────────────────────────
  active_chains <- reactiveVal(top_map_chains)
  clicked_state <- reactiveVal(NULL)

  locs_clean <- reactive({
    dat$locations |>
      mutate(name = str_replace(name, "^McDonalds$", "McDonald's"),
             name = str_replace(name, "^Sonic Drive-In$", "SONIC Drive In"))
  })

  locs_filtered <- reactive({
    ch <- active_chains()
    if (length(ch) > 0) filter(locs_clean(), name %in% ch) else locs_clean()
  })

  output$map_count <- renderText({
    paste0(format(nrow(locs_filtered()), big.mark = ","), " shown")
  })

  # chain toggle buttons
  output$map_chain_btns <- renderUI({
    tagList(
      lapply(top_map_chains, function(ch) {
        col       <- get_color(ch)
        is_active <- ch %in% active_chains()
        actionButton(
          inputId = paste0("mapbtn_", make.names(ch)),
          label   = ch,
          style   = paste0(
            "font-size:.58rem; letter-spacing:.06em; text-transform:uppercase;",
            "padding:.18rem .5rem; border-radius:2px; font-weight:700; margin:2px;",
            "font-family:'Space Mono',monospace; border:1.5px solid ", col, ";",
            if (is_active) paste0("background:", col, "22; color:", col, ";")
            else "background:transparent; color:#bbb; border-color:#ccc;"
          ),
          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                            "map_chain_toggle", ch)
        )
      }),
      actionButton("map_toggle_all", "All / None",
        style = paste("font-size:.58rem; letter-spacing:.06em; text-transform:uppercase;",
                      "padding:.18rem .5rem; border-radius:2px; margin:2px;",
                      "border:1px solid #8c7355; color:#8c7355; background:transparent;",
                      "font-family:'Space Mono',monospace;"))
    )
  })

  observeEvent(input$map_chain_toggle, {
    ch  <- input$map_chain_toggle
    cur <- active_chains()
    if (ch %in% cur) active_chains(setdiff(cur, ch))
    else             active_chains(union(cur, ch))
  })

  observeEvent(input$map_toggle_all, {
    if (length(active_chains()) > 0) active_chains(character(0))
    else                             active_chains(top_map_chains)
  })

  # Base map
  output$main_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addProviderTiles(providers$CartoDB.Positron,
        options = tileOptions(maxZoom = 18)) |>
      setView(lng = -96, lat = 38.5, zoom = 4) |>
      addScaleBar(position = "bottomleft")
  })

  # Update markers whenever filters change
  observeEvent(list(locs_filtered(), input$map_mode), {
    df    <- locs_filtered() |>
      mutate(
        col   = get_color(name),
        popup = paste0(
          "<div style='font-family:DM Sans,sans-serif; min-width:150px;'>",
          "<strong>", name, "</strong><br>",
          "<span style='color:#8c7355; font-size:.78rem;'>",
          city, ifelse(nchar(state) > 0, paste0(", ", state), ""),
          "</span></div>"
        )
      )
    proxy <- leafletProxy("main_map", session) |>
      clearMarkers() |>
      clearMarkerClusters()
    if (nrow(df) == 0) return()

    if (input$map_mode == "cluster") {
      for (ch in unique(df$name)) {
        sub <- df |> filter(name == ch)
        col <- get_color(ch)
        proxy <- proxy |> addCircleMarkers(
          data = sub, lng = ~longitude, lat = ~latitude,
          popup = ~popup, radius = 5,
          fillColor = col, fillOpacity = .85,
          color = "rgba(0,0,0,.2)", weight = .5,
          clusterOptions = markerClusterOptions(
            iconCreateFunction = JS(sprintf("
              function(cluster) {
                var n = cluster.getChildCount();
                var s = n > 100 ? 44 : n > 20 ? 34 : 26;
                return L.divIcon({
                  html: '<div style=\"background:%s;color:white;border-radius:50%%;'+
                        'width:\'+s+\'px;height:\'+s+\'px;display:flex;align-items:center;'+
                        'justify-content:center;font-family:Space Mono,monospace;'+
                        'font-size:10px;font-weight:700;border:2px solid white;'+
                        'box-shadow:0 1px 4px rgba(0,0,0,.3);\">'+n+\'</div>\',
                  className: \'\', iconSize: L.point(s, s)
                });
              }", col))
          )
        )
      }
    } else {
      proxy |> addCircleMarkers(
        data = df, lng = ~longitude, lat = ~latitude, popup = ~popup,
        radius = 4, fillColor = ~col, fillOpacity = .8,
        color = "rgba(0,0,0,.15)", weight = .4
      )
    }
  })

  # State overlay
  observeEvent(input$map_overlay, {
    proxy <- leafletProxy("main_map", session) |> clearShapes()
    if (input$map_overlay == "none") return()

    tryCatch({
      geo <- geojsonsf::geojson_sf(paste0(
        "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/",
        "master/data/geojson/us-states.json"))

      health <- dat$obesity |>
        left_join(dat$poverty, by = "state") |>
        rename(name = state)

      merged <- geo |> dplyr::left_join(health, by = "name")

      if (input$map_overlay == "obesity") {
        pal <- colorNumeric(c("#27ae60","#f39c12","#e67e22","#d4380d","#922b21"),
                            domain = c(18, 40), na.color = "transparent")
        proxy |>
          addPolygons(data = merged, fillColor = ~pal(obesity_pct),
            fillOpacity = .35, color = "rgba(0,0,0,.15)", weight = 1,
            layerId = ~name,
            label   = ~paste0(name, ": ", obesity_pct, "% obesity"),
            highlightOptions = highlightOptions(weight = 2, color = "#1a1612",
              fillOpacity = .55, bringToFront = TRUE)) |>
          addLegend("bottomright", pal = pal, values = merged$obesity_pct,
            title = "Obesity %", opacity = .9, layerId = "ol_legend")
      } else {
        pal <- colorNumeric(c("#16a085","#2980b9","#2c3e50","#8e44ad","#6c3483"),
                            domain = c(4, 16), na.color = "transparent")
        proxy |>
          addPolygons(data = merged, fillColor = ~pal(poverty_pct),
            fillOpacity = .35, color = "rgba(0,0,0,.15)", weight = 1,
            layerId = ~name,
            label   = ~paste0(name, ": ", poverty_pct, "% poverty"),
            highlightOptions = highlightOptions(weight = 2, color = "#1a1612",
              fillOpacity = .55, bringToFront = TRUE)) |>
          addLegend("bottomright", pal = pal, values = merged$poverty_pct,
            title = "Poverty %", opacity = .9, layerId = "ol_legend")
      }
    }, error = function(e) {
      showNotification(
        "Install geojsonsf for state overlays: install.packages('geojsonsf')",
        type = "warning", duration = 7)
    })
  })

  # Capture state click
  observeEvent(input$main_map_shape_click, {
    clicked_state(input$main_map_shape_click$id)
  })

  # State info panel (below map)
  output$map_state_panel <- renderUI({
    sname <- clicked_state()
    if (is.null(sname)) return(NULL)
    health <- dat$obesity |> left_join(dat$poverty, by = "state") |>
      filter(state == sname)
    if (nrow(health) == 0) return(NULL)

    abbr_map <- c(
      Alabama="AL", Alaska="AK", Arizona="AZ", Arkansas="AR", California="CA",
      Colorado="CO", Connecticut="CT", Delaware="DE", Florida="FL", Georgia="GA",
      Hawaii="HI", Idaho="ID", Illinois="IL", Indiana="IN", Iowa="IA",
      Kansas="KS", Kentucky="KY", Louisiana="LA", Maine="ME", Maryland="MD",
      Massachusetts="MA", Michigan="MI", Minnesota="MN", Mississippi="MS",
      Missouri="MO", Montana="MT", Nebraska="NE", Nevada="NV",
      `New Hampshire`="NH", `New Jersey`="NJ", `New Mexico`="NM",
      `New York`="NY", `North Carolina`="NC", `North Dakota`="ND",
      Ohio="OH", Oklahoma="OK", Oregon="OR", Pennsylvania="PA",
      `Rhode Island`="RI", `South Carolina`="SC", `South Dakota`="SD",
      Tennessee="TN", Texas="TX", Utah="UT", Vermont="VT",
      Virginia="VA", Washington="WA", `West Virginia`="WV",
      Wisconsin="WI", Wyoming="WY", `District of Columbia`="DC"
    )
    abbr      <- abbr_map[sname]
    state_loc <- locs_clean() |> filter(state %in% c(abbr, sname))
    top5      <- state_loc |> count(name, sort = TRUE) |> slice_head(n = 5)

    div(style = paste("border:1px solid #ede8df; border-left:4px solid #d4380d;",
                      "background:white; padding:1.2rem 1.5rem; margin-top:.5rem;"),
      div(style = "display:flex; gap:2.5rem; flex-wrap:wrap;",
        div(
          tags$h5(paste0("\U0001F4CD ", sname),
            style = "font-family:'DM Serif Display',serif; font-size:1.4rem; margin-bottom:.4rem;"),
          if (!is.na(health$obesity_pct[1]))
            tags$p(tags$strong(paste0(health$obesity_pct[1], "%"),
              style = "font-family:'Space Mono',monospace;"),
              " adult obesity rate", style = "font-size:.82rem; margin-bottom:.15rem;"),
          if (!is.na(health$poverty_pct[1]))
            tags$p(tags$strong(paste0(health$poverty_pct[1], "%"),
              style = "font-family:'Space Mono',monospace;"),
              " poverty rate", style = "font-size:.82rem;")
        ),
        div(
          tags$p("Top chains in this dataset:",
            style = paste("font-family:'Space Mono',monospace; font-size:.62rem;",
                          "color:#8c7355; text-transform:uppercase;",
                          "letter-spacing:.1em; margin-bottom:.4rem;")),
          tagList(lapply(seq_len(nrow(top5)), function(i) {
            ch  <- top5$name[i]
            col <- get_color(ch)
            tags$p(
              tags$span(style = paste0(
                "display:inline-block;width:8px;height:8px;border-radius:50%;",
                "background:", col, ";margin-right:5px;")),
              tags$strong(ch),
              paste0(" — ", top5$n[i], " locations"),
              style = "font-size:.78rem; margin-bottom:.15rem;"
            )
          })),
          tags$p(paste0("Total in dataset: ", nrow(state_loc)),
            style = "font-size:.7rem; color:#8c7355; font-family:'Space Mono',monospace;")
        )
      )
    )
  })

  # Map legend (chain colour key)
  output$map_legend <- renderUI({
    div(style = paste("display:flex; flex-wrap:wrap; gap:.5rem; padding:.7rem;",
                      "background:white; border:1px solid #ede8df;"),
      lapply(top_map_chains, function(ch) {
        col <- get_color(ch)
        tags$span(style = "display:flex; align-items:center; gap:4px; font-size:.62rem; font-family:'Space Mono',monospace;",
          tags$span(style = paste0("width:9px;height:9px;border-radius:50%;",
                                   "background:", col, ";display:inline-block;")),
          ch)
      })
    )
  })

  # ─────────────────────────────────────────────────────────
  #  DATA EXPLORER
  # ─────────────────────────────────────────────────────────
  exp_df <- reactive({
    switch(input$exp_ds,
      nutrition = dat$nutrition,
      chains    = dat$chains,
      obesity   = dat$obesity,
      poverty   = dat$poverty,
      race      = dat$race,
      locations = dat$locations
    )
  })

  output$exp_table <- renderDT({
    datatable(exp_df(), rownames = FALSE, filter = "top",
      class = "compact stripe",
      options = list(pageLength = 25, dom = "ftip", scrollX = TRUE))
  })

  output$exp_dl <- downloadHandler(
    filename = function() paste0(input$exp_ds, "_fastfood_health.csv"),
    content  = function(file) write_csv(exp_df(), file)
  )
}

# ─────────────────────────────────────────────────────────────
shinyApp(ui, server)
