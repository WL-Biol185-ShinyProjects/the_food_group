# ============================================================
#  Fast Food & Health in America
#  Single-file Shiny app — converted from HTML dashboard
#
#  HOW TO RUN:
#    1. install.packages(c("shiny","bslib","plotly","DT","dplyr","scales","forcats"))
#    2. shiny::runApp("app_single.R")
#       OR open in RStudio and click Run App
# ============================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(scales)
library(forcats)

# ── All data (embedded — no CSV files needed) ────────────────

NUTRITION <- list(
  Mcdonalds   = list(calories=640, sodium=1438, total_fat=31.8, protein=40.3, sugar=11.1),
  `Chick Fil-A`= list(calories=384, sodium=1152, total_fat=16.1, protein=31.7, sugar=4.1),
  Sonic       = list(calories=632, sodium=1351, total_fat=37.6, protein=29.2, sugar=6.5),
  Arbys       = list(calories=533, sodium=1515, total_fat=27.0, protein=29.3, sugar=7.6),
  `Burger King`= list(calories=609, sodium=1224, total_fat=36.8, protein=30.0, sugar=8.1),
  `Dairy Queen`= list(calories=520, sodium=1182, total_fat=28.9, protein=24.8, sugar=6.4),
  Subway      = list(calories=503, sodium=1273, total_fat=18.5, protein=30.3, sugar=10.1),
  `Taco Bell` = list(calories=444, sodium=1014, total_fat=20.9, protein=17.4, sugar=3.7)
)

nutr_df <- do.call(rbind, lapply(names(NUTRITION), function(r) {
  data.frame(restaurant=r, as.data.frame(NUTRITION[[r]]), stringsAsFactors=FALSE)
}))

OBESITY <- data.frame(
  state = c("Louisiana","Mississippi","West Virginia","Alabama","Kentucky","Arkansas",
            "Kansas","Oklahoma","Tennessee","Texas","Missouri","Iowa","South Carolina",
            "Nebraska","Indiana","Michigan","Ohio","Alaska","Georgia","Wisconsin",
            "Oregon","North Carolina","Illinois","South Dakota","Maine","Pennsylvania",
            "Delaware","North Dakota","Virginia","Wyoming","Idaho","New Mexico",
            "Maryland","Washington","Nevada","Minnesota","New Hampshire","Florida",
            "New Jersey","Connecticut","Vermont","New York","Utah","Massachusetts",
            "California","Montana","Hawaii","District of Columbia","Colorado"),
  obesity_pct = c(36.2,35.6,35.6,35.6,34.6,34.5,34.2,33.9,33.8,32.4,32.4,32.1,
                  31.7,31.4,31.3,31.2,29.8,29.8,30.7,30.7,30.1,30.1,30.8,30.4,
                  30.0,30.0,29.7,31.0,29.2,29.0,28.6,28.8,28.9,26.4,26.7,26.1,
                  26.3,26.8,25.6,25.3,25.1,25.0,24.5,24.3,24.2,23.6,22.7,22.1,20.2),
  stringsAsFactors = FALSE
)

POVERTY <- data.frame(
  state = c("Mississippi","Louisiana","New Mexico","West Virginia","Kentucky","Arkansas",
            "Alabama","Oklahoma","District of Columbia","Texas","South Carolina",
            "Georgia","Tennessee","New York","North Carolina","Ohio","Nevada",
            "Arizona","Florida","Michigan","California","Indiana","Missouri",
            "Illinois","Pennsylvania","Kansas","South Dakota","Delaware","Oregon","Montana"),
  poverty_pct = c(14.3,14.2,13.7,11.9,11.8,11.5,11.3,11.1,10.7,10.5,10.1,
                  9.9,9.9,9.8,9.4,9.2,9.0,8.9,8.9,8.8,8.4,8.4,8.4,
                  8.2,7.7,7.4,7.4,7.3,7.3,7.1),
  stringsAsFactors = FALSE
)

CHAINS <- data.frame(
  chain       = c("McDonald's","Starbucks","Chick-fil-A","Taco Bell","Wendy's",
                  "Dunkin'","Burger King","Subway","Domino's","Chipotle",
                  "Sonic Drive-In","Panera Bread","Pizza Hut","KFC","Popeyes",
                  "Dairy Queen","Arby's","Panda Express","Little Caesars","Jack in the Box",
                  "Wingstop","Five Guys","Jersey Mike's","Jimmy John's","Hardee's",
                  "Raising Cane's","Culver's","Whataburger","Zaxby's","In-N-Out Burger"),
  sales_m     = c(45960,24300,16700,12600,11111,10416,10033,9350,8641,7547,
                  5835,5650,5500,5100,4775,4494,4462,4452,4185,4077,
                  2278,2093,2203,2301,2100,2377,2489,3089,2233,1175),
  total_units = c(13438,15450,2732,7002,5938,9244,7105,21147,6560,2966,
                  3552,2080,6548,3953,2754,4339,3409,2334,4181,2218,
                  1534,1390,2100,2657,1734,567,837,873,908,370),
  unit_change = c(244,113,155,203,57,161,24,-1043,205,198,
                  26,-25,-13,10,146,-22,40,150,-28,-23,
                  175,8,246,48,-32,58,55,29,3,5),
  stringsAsFactors = FALSE
)

RACE_US <- data.frame(
  group = c("White","Hispanic","Black","Asian","Other"),
  pct   = c(56.4, 20.1, 11.6, 6.2, 5.7)
)

FF_BY_STATE <- data.frame(
  state = c("CA","TX","OH","FL","IN","IL","NC","GA","MO","KY","VA","PA","NY","MI","TN","SC","LA","AL","WA","OK"),
  count = c(676,634,543,471,379,363,358,347,334,332,327,283,269,251,245,238,237,236,209,208)
)

CHAIN_COUNTS <- data.frame(
  chain = c("McDonald's","Burger King","Taco Bell","Wendy's","Arby's","KFC",
            "Domino's Pizza","Subway","SONIC","Hardee's","Jack in the Box",
            "Chick-fil-A","Five Guys","Dairy Queen","Whataburger"),
  count = c(1882,1147,873,731,518,421,345,322,226,192,187,100,96,92,88)
)

# ── Theme ────────────────────────────────────────────────────
app_theme <- bs_theme(
  version      = 5,
  bg           = "#faf7f2",
  fg           = "#2d2520",
  primary      = "#d4380d",
  secondary    = "#8c7355",
  success      = "#27ae60",
  info         = "#2980b9",
  warning      = "#e67e22",
  "navbar-bg"  = "#1a1612",
  base_font    = font_google("DM Sans"),
  heading_font = font_google("DM Serif Display"),
  code_font    = font_google("Space Mono")
)

# ── CSS ──────────────────────────────────────────────────────
my_css <- "
  body { background:#faf7f2; }
  h1,h2,h3,h4,h5 { font-family:'DM Serif Display',serif; font-weight:400; }
  .navbar { border-bottom:3px solid #d4380d !important; }
  .nav-link { font-size:.75rem; letter-spacing:.06em; text-transform:uppercase; }

  .kpi-box {
    background:#1a1612; color:#f0ede6;
    padding:1.5rem 1.8rem; border-top:3px solid #d4380d;
    margin-bottom:1px;
  }
  .kpi-num {
    font-family:'DM Serif Display',serif;
    font-size:2.6rem; line-height:1; color:#e67e22;
  }
  .kpi-lbl {
    font-size:.68rem; color:rgba(240,237,230,.55);
    text-transform:uppercase; letter-spacing:.1em;
    font-family:'Space Mono',monospace;
  }

  .hero-wrap {
    background:#1a1612; color:#f0ede6;
    padding:3rem 2rem 2.5rem; margin-bottom:0;
    border-bottom:1px solid rgba(255,255,255,.08);
    background-image:
      repeating-linear-gradient(0deg,transparent,transparent 39px,rgba(255,255,255,.025) 39px,rgba(255,255,255,.025) 40px),
      repeating-linear-gradient(90deg,transparent,transparent 39px,rgba(255,255,255,.025) 39px,rgba(255,255,255,.025) 40px);
  }
  .hero-eyebrow {
    font-family:'Space Mono',monospace; font-size:.62rem;
    letter-spacing:.18em; text-transform:uppercase; color:#e67e22;
    border:1px solid rgba(230,126,34,.35); display:inline-block;
    padding:.25rem .7rem; margin-bottom:1rem;
  }
  .hero-title {
    font-family:'DM Serif Display',serif;
    font-size:clamp(2.5rem,6vw,5rem); line-height:.95;
    letter-spacing:-.02em; color:#f0ede6; margin-bottom:.8rem;
  }
  .hero-title em { color:#e67e22; font-style:italic; }
  .hero-desc { color:rgba(240,237,230,.6); font-size:.88rem; line-height:1.75; max-width:560px; }

  .section-label {
    font-family:'Space Mono',monospace; font-size:.6rem;
    letter-spacing:.2em; text-transform:uppercase; color:#d4380d;
  }
  .card { border:1px solid #ede8df; border-radius:0; }
  .card-header {
    background:#1a1612; color:#f0ede6; border-radius:0;
    font-family:'Space Mono',monospace; font-size:.7rem;
    letter-spacing:.1em; text-transform:uppercase;
  }
  .nav-tabs .nav-link { border-radius:0; font-size:.72rem;
    letter-spacing:.06em; text-transform:uppercase;
    font-family:'Space Mono',monospace; color:#8c7355; }
  .nav-tabs .nav-link.active { color:#1a1612 !important;
    border-bottom-color:#1a1612 !important; font-weight:700; }
  table.dataTable thead th {
    background:#1a1612 !important; color:#f0ede6 !important;
    font-family:'Space Mono',monospace; font-size:.65rem;
    letter-spacing:.1em; text-transform:uppercase;
  }
  table.dataTable tbody tr:hover { background:#f5f0e8 !important; }
  .modebar { display:none !important; }
  ::-webkit-scrollbar { width:5px; }
  ::-webkit-scrollbar-thumb { background:#d4c9b5; }
"

# ── Helper: plotly base layout ────────────────────────────────
pl <- function(p) {
  p |> layout(
    plot_bgcolor  = "#ffffff",
    paper_bgcolor = "#ffffff",
    font  = list(family="DM Sans", color="#2d2520"),
    margin = list(l=10, r=10, t=30, b=50)
  )
}

# ── UI ───────────────────────────────────────────────────────
ui <- page_navbar(
  title  = tags$span("Fast Food ", tags$em("& ", style="color:#e67e22;font-style:italic;"), "Health"),
  theme  = app_theme,
  bg     = "#1a1612",
  id     = "nav",
  header = tags$head(
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;700&family=DM+Serif+Display:ital@0;1&family=Space+Mono:wght@400;700&display=swap"),
    tags$style(HTML(my_css))
  ),

  # ── OVERVIEW ──────────────────────────────────────────────
  nav_panel("Overview", icon = icon("home"),
    div(class="hero-wrap",
      div(class="hero-eyebrow", "American Fast Food & Public Health · Data Explorer"),
      tags$h1(class="hero-title", "Fast Food,", tags$br(), tags$em("Health & America")),
      p(class="hero-desc",
        "An interactive look at how fast food chains, nutrition, obesity, poverty,
         and race intersect across the United States — all data embedded, no files needed.")
    ),
    br(),
    layout_columns(col_widths=c(3,3,3,3), gap="1px",
      div(class="kpi-box", div(class="kpi-num","50"),  div(class="kpi-lbl","Fast Food Chains")),
      div(class="kpi-box", div(class="kpi-num","9,950"),div(class="kpi-lbl","Restaurant Locations")),
      div(class="kpi-box", div(class="kpi-num","515"), div(class="kpi-lbl","Menu Items Analyzed")),
      div(class="kpi-box", div(class="kpi-num","49"),  div(class="kpi-lbl","States Covered"))
    ),
    br(),
    layout_columns(col_widths=c(4,4,4),
      card(card_header("Highest Obesity State"),
        card_body(tags$h3("Louisiana", style="color:#c0392b;"),
                  p("36.2% adult obesity rate", style="font-family:'Space Mono',monospace;font-size:.75rem;color:#8c7355;"))),
      card(card_header("Top Chain by Sales"),
        card_body(tags$h3("McDonald's", style="color:#e67e22;"),
                  p("$45.96B systemwide sales · 2021", style="font-family:'Space Mono',monospace;font-size:.75rem;color:#8c7355;"))),
      card(card_header("Highest Sodium Chain"),
        card_body(tags$h3("Arby's", style="color:#8e44ad;"),
                  p("Avg 1,515mg sodium per item", style="font-family:'Space Mono',monospace;font-size:.75rem;color:#8c7355;")))
    )
  ),

  # ── CHAINS ────────────────────────────────────────────────
  nav_panel("Chains", icon = icon("store"),
    br(),
    div(class="section-label", "Top 50 Fast Food Chains"),
    tags$h4("Sales & Market Presence", style="letter-spacing:-.02em;"),
    p("Source: QSR Magazine · 2021", style="font-family:'Space Mono',monospace;font-size:.65rem;color:#8c7355;"),
    tabsetPanel(type="tabs",
      tabPanel("Sales Chart", br(),
        layout_sidebar(
          sidebar = sidebar(width=220,
            sliderInput("n_chains","Show top N chains:",min=5,max=30,value=15,step=5),
            selectInput("chain_sort","Sort by:",
              choices=c("Sales ($M)"="sales_m","Total Units"="total_units","Unit Change"="unit_change"))
          ),
          card(full_screen=TRUE,
            card_body(plotlyOutput("chains_bar", height="550px")))
        )
      ),
      tabPanel("Bubble Chart", br(),
        card(full_screen=TRUE,
          card_body(plotlyOutput("chains_bubble", height="520px")))
      ),
      tabPanel("Full Table", br(),
        card(card_body(padding=0, DTOutput("chains_table")))
      )
    )
  ),

  # ── NUTRITION ─────────────────────────────────────────────
  nav_panel("Nutrition", icon = icon("utensils"),
    br(),
    div(class="section-label", "Menu Nutrition"),
    tags$h4("What's Really in Your Meal?", style="letter-spacing:-.02em;"),
    p("Source: FDA Menu Labeling Data · 515 items across 8 chains",
      style="font-family:'Space Mono',monospace;font-size:.65rem;color:#8c7355;"),
    tabsetPanel(type="tabs",
      tabPanel("Calories & Sodium", br(),
        layout_columns(col_widths=c(6,6),
          card(card_header("Average Calories per Item"),
            card_body(plotlyOutput("cal_bar", height="340px"))),
          card(card_header("Average Sodium per Item (mg)"),
            card_body(plotlyOutput("sodium_bar", height="340px")))
        )
      ),
      tabPanel("Fat & Protein", br(),
        layout_columns(col_widths=c(6,6),
          card(card_header("Average Total Fat (g)"),
            card_body(plotlyOutput("fat_bar", height="340px"))),
          card(card_header("Average Protein (g)"),
            card_body(plotlyOutput("protein_bar", height="340px")))
        )
      ),
      tabPanel("Nutrient Heatmap", br(),
        card(full_screen=TRUE,
          card_body(plotlyOutput("nutr_heatmap", height="480px")))
      ),
      tabPanel("Item Scatter", br(),
        layout_sidebar(
          sidebar = sidebar(width=200,
            selectInput("scatter_x","X axis:",
              choices=c("Calories"="calories","Sodium"="sodium","Fat"="total_fat","Protein"="protein","Sugar"="sugar")),
            selectInput("scatter_y","Y axis:",
              choices=c("Sodium"="sodium","Calories"="calories","Fat"="total_fat","Protein"="protein","Sugar"="sugar"))
          ),
          card(full_screen=TRUE,
            card_body(plotlyOutput("nutr_scatter", height="500px")))
        )
      )
    )
  ),

  # ── OBESITY ───────────────────────────────────────────────
  nav_panel("Obesity", icon = icon("heartbeat"),
    br(),
    div(class="section-label", "State Health Data"),
    tags$h4("Obesity & Poverty Across America", style="letter-spacing:-.02em;"),
    p("Sources: CDC BRFSS 2023 · U.S. Census SAIPE 2019–2023",
      style="font-family:'Space Mono',monospace;font-size:.65rem;color:#8c7355;"),
    tabsetPanel(type="tabs",
      tabPanel("Obesity Ranking", br(),
        layout_columns(col_widths=c(7,5),
          card(full_screen=TRUE, card_header("All States — Obesity Rate (%)"),
            card_body(plotlyOutput("obesity_lollipop", height="680px"))),
          card(card_header("Distribution"),
            card_body(plotlyOutput("obesity_hist", height="260px"),
                      hr(),
                      plotlyOutput("obesity_box",  height="180px")))
        )
      ),
      tabPanel("Poverty Ranking", br(),
        layout_columns(col_widths=c(6,6),
          card(full_screen=TRUE, card_header("Poverty Rate by State (%)"),
            card_body(plotlyOutput("poverty_bar", height="560px"))),
          card(full_screen=TRUE, card_header("Poverty vs Obesity Scatter"),
            card_body(plotlyOutput("pov_ob_scatter", height="560px")))
        )
      ),
      tabPanel("State Deep-Dive", br(),
        layout_sidebar(
          sidebar = sidebar(width=220,
            selectInput("state_pick","Choose a state:",
              choices=sort(OBESITY$state), selected="Louisiana")),
          layout_columns(col_widths=c(6,6),
            card(card_header("Health Gauges"),
              card_body(plotlyOutput("state_gauge", height="320px"))),
            card(card_header("Vs Top 20 States"),
              card_body(plotlyOutput("state_peer",  height="320px")))
          )
        )
      )
    )
  ),

  # ── RACE ──────────────────────────────────────────────────
  nav_panel("Demographics", icon = icon("users"),
    br(),
    div(class="section-label", "Demographics"),
    tags$h4("Race & Ethnicity by State", style="letter-spacing:-.02em;"),
    p("Source: KFF State Health Facts · 2024",
      style="font-family:'Space Mono',monospace;font-size:.65rem;color:#8c7355;"),
    tabsetPanel(type="tabs",
      tabPanel("National Overview", br(),
        layout_columns(col_widths=c(5,7),
          card(card_header("U.S. Racial Composition"),
            card_body(plotlyOutput("us_pie", height="380px"))),
          card(card_header("Fast Food Locations by State — Top 20"),
            card_body(plotlyOutput("ff_state_bar", height="380px")))
        )
      ),
      tabPanel("Chain Counts", br(),
        card(full_screen=TRUE, card_header("Top Chains by Location Count"),
          card_body(plotlyOutput("chain_counts_bar", height="480px")))
      )
    )
  ),

  # ── DATA TABLE ────────────────────────────────────────────
  nav_panel("Data", icon = icon("table"),
    br(),
    div(class="section-label", "Raw Data"),
    tags$h4("Browse All Datasets", style="letter-spacing:-.02em;"),
    br(),
    layout_sidebar(
      sidebar = sidebar(width=200,
        selectInput("data_choice","Dataset:",
          choices=c("Chains"="chains","Nutrition"="nutrition",
                    "Obesity"="obesity","Poverty"="poverty",
                    "Fast Food by State"="ff_state")),
        downloadButton("dl_csv","Download CSV",
          style="border-radius:0;background:#1a1612;color:#f0ede6;border:none;
                 font-family:'Space Mono',monospace;font-size:.7rem;width:100%;")
      ),
      card(full_screen=TRUE,
        card_body(padding=0, DTOutput("data_table", height="600px")))
    )
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── CHAINS ────────────────────────────────────────────────
  output$chains_bar <- renderPlotly({
    df <- CHAINS |>
      arrange(desc(.data[[input$chain_sort]])) |>
      slice_head(n=input$n_chains) |>
      mutate(chain=fct_reorder(chain, .data[[input$chain_sort]]))

    pal <- colorRampPalette(c("#f5cba7","#e67e22","#d4380d","#7b241c"))(nrow(df))

    plot_ly(df, x=~.data[[input$chain_sort]], y=~chain, type="bar",
            orientation="h",
            marker=list(color=pal),
            hovertemplate="<b>%{y}</b><br>%{x:,.0f}<extra></extra>") |>
      layout(xaxis=list(title=input$chain_sort, gridcolor="#ede8df", zeroline=FALSE),
             yaxis=list(title=""),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans", color="#2d2520"),
             margin=list(l=10,r=10,t=10,b=40))
  })

  output$chains_bubble <- renderPlotly({
    plot_ly(CHAINS, x=~total_units, y=~sales_m, size=~sales_m,
            text=~chain, type="scatter", mode="markers",
            marker=list(sizemode="area",
              sizeref=2*max(CHAINS$sales_m)/(60^2),
              color=~sales_m,
              colorscale=list(c(0,"#f5cba7"),c(0.5,"#e67e22"),c(1,"#7b241c")),
              showscale=TRUE, opacity=0.85,
              line=list(color="white",width=1)),
            hovertemplate="<b>%{text}</b><br>Units: %{x:,}<br>Sales: $%{y:,.0f}M<extra></extra>") |>
      layout(xaxis=list(title="Total Units", gridcolor="#ede8df", zeroline=FALSE),
             yaxis=list(title="Sales ($M)", gridcolor="#ede8df", zeroline=FALSE),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans", color="#2d2520"),
             showlegend=FALSE, margin=list(l=10,r=60,t=10,b=50))
  })

  output$chains_table <- renderDT({
    CHAINS |>
      mutate(sales_m=paste0("$",comma(sales_m),"M"),
             total_units=comma(total_units),
             unit_change=ifelse(unit_change>=0,paste0("+",unit_change),as.character(unit_change))) |>
      rename(Chain=chain, `Sales ($M)`=sales_m,
             `Total Units`=total_units, `Change '20->'21`=unit_change) |>
      datatable(rownames=FALSE,
                options=list(pageLength=20,dom="ftip",scrollX=TRUE),
                class="compact stripe") |>
      formatStyle("Change '20->'21",
        color=styleInterval(c(0),c("#c0392b","#27ae60")), fontWeight="bold")
  })

  # ── NUTRITION ─────────────────────────────────────────────
  make_nutr_bar <- function(metric, color_high, color_mid, color_low, suffix="") {
    df <- nutr_df |>
      group_by(restaurant) |>
      summarise(val=mean(.data[[metric]], na.rm=TRUE), .groups="drop") |>
      arrange(desc(val)) |>
      mutate(restaurant=fct_inorder(restaurant),
             color=ifelse(val==max(val), color_high,
                   ifelse(val >= median(val), color_mid, color_low)))
    plot_ly(df, x=~val, y=~restaurant, type="bar", orientation="h",
            marker=list(color=~color),
            hovertemplate=paste0("<b>%{y}</b><br>",metric,": %{x:.1f}",suffix,"<extra></extra>")) |>
      layout(xaxis=list(title=paste(metric, suffix), gridcolor="#ede8df", zeroline=FALSE),
             yaxis=list(title=""),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=10,t=5,b=40), showlegend=FALSE)
  }

  output$cal_bar    <- renderPlotly(make_nutr_bar("calories","#922b21","#e67e22","#27ae60"))
  output$sodium_bar <- renderPlotly(make_nutr_bar("sodium",  "#922b21","#e67e22","#16a085","mg"))
  output$fat_bar    <- renderPlotly(make_nutr_bar("total_fat","#922b21","#e67e22","#27ae60","g"))
  output$protein_bar<- renderPlotly(make_nutr_bar("protein", "#27ae60","#2980b9","#8c7355","g"))

  output$nutr_heatmap <- renderPlotly({
    metrics <- c("calories","sodium","total_fat","protein","sugar")
    df <- nutr_df |>
      group_by(restaurant) |>
      summarise(across(all_of(metrics), ~mean(.x,na.rm=TRUE)), .groups="drop")
    mat <- df |> select(all_of(metrics)) |>
      mutate(across(everything(),
        ~(.x-min(.x,na.rm=TRUE))/(max(.x,na.rm=TRUE)-min(.x,na.rm=TRUE)+1e-9)))
    plot_ly(x=c("Calories","Sodium","Total Fat","Protein","Sugar"),
            y=df$restaurant,
            z=as.matrix(mat), type="heatmap",
            colorscale=list(c(0,"#f9f3e8"),c(0.5,"#e67e22"),c(1,"#7b241c")),
            hovertemplate="<b>%{y}</b> — %{x}<br>Normalized: %{z:.2f}<extra></extra>") |>
      layout(xaxis=list(title="",tickangle=-20),
             yaxis=list(title=""),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=80,t=10,b=60))
  })

  output$nutr_scatter <- renderPlotly({
    pal <- c("#e8a000","#e63000","#7b2d8b","#c0392b","#8b2500","#00437a","#009a44","#e67e22")
    plot_ly(nutr_df, x=~.data[[input$scatter_x]], y=~.data[[input$scatter_y]],
            color=~restaurant, colors=pal,
            type="scatter", mode="markers", text=~item,
            marker=list(size=7,opacity=0.75),
            hovertemplate="<b>%{text}</b><br>%{x:.0f} / %{y:.0f}<extra></extra>") |>
      layout(xaxis=list(title=input$scatter_x,gridcolor="#ede8df"),
             yaxis=list(title=input$scatter_y,gridcolor="#ede8df"),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             legend=list(orientation="h",y=-0.15),
             margin=list(l=10,r=10,t=10,b=80))
  })

  # ── OBESITY ───────────────────────────────────────────────
  ob_color <- function(v) dplyr::case_when(
    v>=34 ~ "#922b21", v>=30 ~ "#d4380d",
    v>=26 ~ "#e67e22", TRUE  ~ "#27ae60"
  )

  output$obesity_lollipop <- renderPlotly({
    df <- OBESITY |> arrange(desc(obesity_pct)) |>
      mutate(state=fct_inorder(state), color=ob_color(obesity_pct))
    plot_ly(df) |>
      add_segments(x=18, xend=~obesity_pct, y=~state, yend=~state,
                   line=list(color="rgba(0,0,0,.08)",width=1)) |>
      add_markers(x=~obesity_pct, y=~state,
                  marker=list(color=~color,size=9,line=list(color="white",width=1)),
                  text=~paste0(state,": ",obesity_pct,"%"), hoverinfo="text") |>
      layout(xaxis=list(title="Obesity Rate (%)",range=c(18,40),gridcolor="#ede8df"),
             yaxis=list(title="",tickfont=list(size=10)),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             showlegend=FALSE, margin=list(l=10,r=10,t=10,b=40))
  })

  output$obesity_hist <- renderPlotly({
    plot_ly(OBESITY, x=~obesity_pct, type="histogram", nbinsx=12,
            marker=list(color="#d4380d",line=list(color="white",width=0.8))) |>
      layout(xaxis=list(title="Obesity Rate (%)",gridcolor="#ede8df"),
             yaxis=list(title="States",gridcolor="#ede8df"),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=10,t=5,b=40))
  })

  output$obesity_box <- renderPlotly({
    plot_ly(OBESITY, x=~obesity_pct, type="box",
            marker=list(color="#d4380d",size=4),
            line=list(color="#922b21"),
            fillcolor="rgba(212,56,13,0.15)") |>
      layout(xaxis=list(title="Obesity Rate (%)",gridcolor="#ede8df"),
             yaxis=list(showticklabels=FALSE),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=10,t=5,b=40))
  })

  output$poverty_bar <- renderPlotly({
    df <- POVERTY |> arrange(desc(poverty_pct)) |>
      mutate(state=fct_inorder(state),
             color=dplyr::case_when(
               poverty_pct>=13~"#6c3483",poverty_pct>=11~"#8e44ad",
               poverty_pct>=9~"#2c3e50",poverty_pct>=7~"#2980b9",TRUE~"#16a085"))
    plot_ly(df, x=~poverty_pct, y=~state, type="bar", orientation="h",
            marker=list(color=~color),
            hovertemplate="<b>%{y}</b><br>Poverty: %{x:.1f}%<extra></extra>") |>
      layout(xaxis=list(title="Poverty Rate (%)",gridcolor="#ede8df"),
             yaxis=list(title="",tickfont=list(size=10)),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=10,t=10,b=40))
  })

  output$pov_ob_scatter <- renderPlotly({
    df <- POVERTY |>
      dplyr::inner_join(OBESITY, by="state") |>
      mutate(color=ob_color(obesity_pct))
    cor_val <- round(cor(df$poverty_pct, df$obesity_pct), 2)
    plot_ly(df, x=~poverty_pct, y=~obesity_pct, type="scatter", mode="markers+text",
            text=~state, textposition="top right",
            textfont=list(size=8,color="#8c7355"),
            marker=list(color=~color,size=9,opacity=0.85),
            hovertemplate="<b>%{text}</b><br>Poverty:%{x:.1f}% Obesity:%{y:.1f}%<extra></extra>") |>
      layout(title=list(text=paste0("Pearson r = ",cor_val),
                        font=list(size=11,color="#8c7355"),x=0.01),
             xaxis=list(title="Poverty Rate (%)",gridcolor="#ede8df"),
             yaxis=list(title="Obesity Rate (%)",gridcolor="#ede8df"),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             showlegend=FALSE, margin=list(l=20,r=20,t=30,b=50))
  })

  output$state_gauge <- renderPlotly({
    req(input$state_pick)
    ob  <- OBESITY  |> dplyr::filter(state==input$state_pick)
    pov <- POVERTY  |> dplyr::filter(state==input$state_pick)
    avg_ob  <- mean(OBESITY$obesity_pct)
    avg_pov <- mean(POVERTY$poverty_pct)
    ob_val  <- if (nrow(ob)>0)  ob$obesity_pct[1]  else avg_ob
    pov_val <- if (nrow(pov)>0) pov$poverty_pct[1] else avg_pov

    plot_ly() |>
      add_trace(type="indicator", mode="gauge+number+delta",
        value=ob_val, title=list(text="Obesity Rate (%)"),
        delta=list(reference=avg_ob, valueformat=".1f",
                   increasing=list(color="#c0392b"), decreasing=list(color="#27ae60")),
        gauge=list(axis=list(range=list(18,40)), bar=list(color="#d4380d"),
          threshold=list(line=list(color="#2d2520",width=2),thickness=0.75,value=avg_ob)),
        domain=list(x=c(0,1),y=c(0.52,1))) |>
      add_trace(type="indicator", mode="gauge+number+delta",
        value=pov_val, title=list(text="Poverty Rate (%)"),
        delta=list(reference=avg_pov, valueformat=".1f",
                   increasing=list(color="#c0392b"), decreasing=list(color="#27ae60")),
        gauge=list(axis=list(range=list(0,16)), bar=list(color="#8e44ad"),
          threshold=list(line=list(color="#2d2520",width=2),thickness=0.75,value=avg_pov)),
        domain=list(x=c(0,1),y=c(0,0.45))) |>
      layout(plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=10,t=10,b=10))
  })

  output$state_peer <- renderPlotly({
    req(input$state_pick)
    df <- OBESITY |> arrange(desc(obesity_pct)) |>
      slice_head(n=20) |>
      mutate(color=ifelse(state==input$state_pick,"#d4380d","#d4c9b5"),
             state=fct_inorder(state))
    plot_ly(df, x=~obesity_pct, y=~state, type="bar", orientation="h",
            marker=list(color=~color),
            hovertemplate="<b>%{y}</b>: %{x:.1f}%<extra></extra>") |>
      layout(xaxis=list(title="Obesity Rate (%)",gridcolor="#ede8df"),
             yaxis=list(title="",tickfont=list(size=10)),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=10,t=10,b=40))
  })

  # ── RACE / LOCATIONS ──────────────────────────────────────
  output$us_pie <- renderPlotly({
    plot_ly(RACE_US, labels=~group, values=~pct, type="pie",
            marker=list(
              colors=c("#4e9af1","#54c066","#e87040","#f0c040","#94a3b8"),
              line=list(color="white",width=2)),
            textinfo="label+percent",
            hovertemplate="<b>%{label}</b><br>%{percent}<extra></extra>") |>
      layout(showlegend=FALSE,
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520",size=11),
             margin=list(l=10,r=10,t=10,b=10))
  })

  output$ff_state_bar <- renderPlotly({
    df <- FF_BY_STATE |> arrange(desc(count)) |>
      mutate(state=fct_inorder(state))
    pal <- colorRampPalette(c("#f5cba7","#d4380d"))(nrow(df))
    plot_ly(df, x=~count, y=~state, type="bar", orientation="h",
            marker=list(color=pal),
            hovertemplate="<b>%{y}</b>: %{x:,} locations<extra></extra>") |>
      layout(xaxis=list(title="Number of Locations",gridcolor="#ede8df",zeroline=FALSE),
             yaxis=list(title=""),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=10,t=10,b=40))
  })

  output$chain_counts_bar <- renderPlotly({
    df <- CHAIN_COUNTS |> arrange(desc(count)) |>
      mutate(chain=fct_inorder(chain))
    pal <- colorRampPalette(c("#f5cba7","#e67e22","#d4380d","#7b241c"))(nrow(df))
    plot_ly(df, x=~count, y=~chain, type="bar", orientation="h",
            marker=list(color=pal),
            hovertemplate="<b>%{y}</b>: %{x:,} locations<extra></extra>") |>
      layout(xaxis=list(title="Locations in Dataset",gridcolor="#ede8df",zeroline=FALSE),
             yaxis=list(title=""),
             plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
             font=list(family="DM Sans",color="#2d2520"),
             margin=list(l=10,r=10,t=10,b=40))
  })

  # ── DATA EXPLORER ─────────────────────────────────────────
  current_data <- reactive({
    switch(input$data_choice,
      chains    = CHAINS,
      nutrition = nutr_df,
      obesity   = OBESITY,
      poverty   = POVERTY,
      ff_state  = FF_BY_STATE
    )
  })

  output$data_table <- renderDT({
    datatable(current_data(), rownames=FALSE, filter="top",
              options=list(pageLength=25, dom="ftip", scrollX=TRUE),
              class="compact stripe")
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0(input$data_choice, "_data.csv"),
    content  = function(file) write.csv(current_data(), file, row.names=FALSE)
  )
}

shinyApp(ui, server)
