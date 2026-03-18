library(shiny)
library(leaflet)
library(plotly)
library(DT)

ui <- fluidPage(
  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;700&family=DM+Serif+Display:ital@0;1&family=Space+Mono:wght@400;700&display=swap"),
    tags$style(HTML("
      :root {
        --cream:#faf7f2; --warm:#f5f0e8; --paper:#ede8df; --tan:#d4c9b5;
        --brown:#8c7355; --dark:#1a1612; --ink:#2d2520;
        --accent:#d4380d; --orange:#e67e22; --green:#27ae60; --red:#c0392b;
        --sans:'DM Sans',sans-serif; --serif:'DM Serif Display',serif; --mono:'Space Mono',monospace;
      }
      *, *::before, *::after { box-sizing:border-box; }

      body {
        background:var(--cream);
        color:var(--ink);
        font-family:var(--sans);
        font-size:16px;
        margin:0; padding:0;
        overflow-x:hidden;
      }

      /* HERO */
      .hero {
        background:var(--dark); color:var(--cream);
        padding:3rem 3rem 2.5rem;
        border-bottom:3px solid var(--accent);
      }
      .hero-eyebrow {
        font-family:var(--mono); font-size:.75rem; letter-spacing:.15em;
        text-transform:uppercase; color:var(--orange); margin-bottom:.8rem;
        display:inline-block; padding:.3rem .7rem;
        border:1px solid rgba(230,126,34,.4);
      }
      .hero h1 {
        font-family:var(--serif);
        font-size:clamp(2.5rem,5vw,5rem);
        font-weight:400; line-height:.95;
        letter-spacing:-.02em; margin-bottom:1rem;
      }
      .hero h1 em { color:var(--orange); font-style:italic; }
      .hero-desc {
        color:rgba(250,247,242,.65); font-size:1rem;
        max-width:600px; line-height:1.75; margin-bottom:2rem;
      }
      .hero-kpis {
        display:flex; gap:3rem; flex-wrap:wrap;
        padding-top:1.5rem; border-top:1px solid rgba(255,255,255,.1);
      }
      .kpi-num { font-family:var(--serif); font-size:2.8rem; color:var(--orange); line-height:1; }
      .kpi-label { font-size:.78rem; text-transform:uppercase; letter-spacing:.08em; color:rgba(250,247,242,.5); margin-top:.3rem; }

      /* NAVBAR */
      .navbar { background:var(--dark) !important; border:none !important; border-radius:0 !important; margin-bottom:0 !important; border-bottom:2px solid var(--accent) !important; min-height:52px !important; }
      .navbar-default .navbar-nav>li>a {
        color:rgba(250,247,242,.6) !important;
        font-family:var(--mono) !important;
        font-size:.75rem !important;
        letter-spacing:.1em !important;
        text-transform:uppercase !important;
        padding:16px 18px !important;
        line-height:1.2 !important;
      }
      .navbar-default .navbar-nav>.active>a,
      .navbar-default .navbar-nav>li>a:hover {
        color:var(--orange) !important;
        background:rgba(255,255,255,.07) !important;
      }
      .navbar-brand { display:none !important; }
      .navbar-collapse { padding:0 !important; }

      /* TAB CONTENT — full width, proper scroll */
      .tab-content { width:100%; }
      .tab-pane {
        padding:2.5rem 3rem;
        min-height:600px;
        overflow-y:auto;
      }

      /* INNER SUBTABS */
      .nav-tabs { border-bottom:2px solid var(--paper); margin-bottom:1.8rem; }
      .nav-tabs>li>a {
        font-family:var(--mono) !important;
        font-size:.72rem !important;
        letter-spacing:.06em !important;
        text-transform:uppercase !important;
        color:var(--brown) !important;
        border:none !important;
        border-bottom:2px solid transparent !important;
        margin-bottom:-2px !important;
        padding:.6rem 1.2rem !important;
      }
      .nav-tabs>li.active>a {
        color:var(--dark) !important;
        border-bottom-color:var(--dark) !important;
        font-weight:700 !important;
        background:none !important;
      }
      .nav-tabs>li>a:hover { color:var(--ink) !important; background:none !important; }

      /* SECTION HEADERS */
      .section-header {
        display:flex; justify-content:space-between; align-items:baseline;
        border-bottom:2px solid var(--dark);
        padding-bottom:1rem; margin-bottom:2rem;
      }
      .section-label {
        font-family:var(--mono); font-size:.72rem; letter-spacing:.18em;
        text-transform:uppercase; color:var(--accent); margin-bottom:.35rem;
      }
      .section-title {
        font-family:var(--serif);
        font-size:clamp(1.6rem,2.5vw,2.4rem);
        font-weight:400; letter-spacing:-.02em; line-height:1.1;
      }
      .section-meta { font-size:.75rem; color:var(--brown); font-family:var(--mono); }

      /* CHART BOXES */
      .chart-box {
        background:white; border:1px solid var(--paper);
        padding:1.6rem; margin-bottom:1.8rem;
      }
      .chart-box-title { font-weight:700; font-size:.9rem; margin-bottom:.25rem; color:var(--dark); }
      .chart-box-sub   { font-size:.72rem; color:var(--brown); font-family:var(--mono); margin-bottom:1.2rem; }

      /* CHAIN CARDS */
      .chain-grid {
        display:grid;
        grid-template-columns:repeat(auto-fill,minmax(210px,1fr));
        gap:1px; background:var(--paper); margin-bottom:1.5rem;
      }
      .chain-card { background:white; padding:1.2rem 1.4rem; transition:background .15s; }
      .chain-card:hover { background:var(--warm); }
      .chain-rank  { font-family:var(--mono); font-size:.65rem; color:var(--brown); }
      .chain-name  { font-weight:700; font-size:.95rem; margin:.3rem 0 .7rem; line-height:1.2; }
      .chain-sales { font-family:var(--serif); font-size:1.6rem; color:var(--accent); line-height:1; }
      .chain-sales-label { font-size:.62rem; color:var(--brown); text-transform:uppercase; letter-spacing:.08em; font-family:var(--mono); }
      .chain-units { font-size:.76rem; color:var(--brown); margin-top:.35rem; }
      .chain-change { font-family:var(--mono); font-size:.72rem; margin-top:.25rem; }
      .chain-pos { color:#27ae60; } .chain-neg { color:#c0392b; }

      /* NUTRITION CARDS */
      .nutr-grid {
        display:grid;
        grid-template-columns:repeat(auto-fill,minmax(190px,1fr));
        gap:1px; background:var(--paper);
      }
      .nutr-card { background:white; padding:1.2rem; }
      .nutr-card-name { font-weight:700; font-size:.88rem; margin-bottom:.8rem; }
      .nutr-metric { display:flex; justify-content:space-between; font-size:.76rem; padding:.25rem 0; border-bottom:1px solid var(--warm); }
      .nutr-metric-label { color:var(--brown); }
      .nutr-metric-val { font-family:var(--mono); font-weight:700; }

      /* MAP */
      #ffMap { height:580px; width:100%; border:1px solid var(--paper); }
      .map-control-row { display:flex; align-items:center; gap:1.2rem; margin-bottom:1rem; flex-wrap:wrap; }
      .map-counter-text { font-family:var(--mono); font-size:.78rem; color:var(--brown); }

      /* FILTER PANEL */
      .filter-panel { background:white; border:1px solid var(--paper); padding:1.2rem; }
      .filter-panel .checkbox label { font-size:.82rem; }

      /* DT TABLES */
      .dataTables_wrapper { font-size:.82rem; }
      table.dataTable thead { background:var(--dark) !important; color:var(--cream) !important; }
      table.dataTable thead th {
        font-family:var(--mono) !important; font-size:.65rem !important;
        letter-spacing:.08em !important; text-transform:uppercase !important;
        border-bottom:none !important; padding:10px 12px !important;
      }
      table.dataTable tbody td { padding:8px 12px !important; font-size:.82rem !important; }
      tbody tr:hover { background:var(--warm) !important; }

      /* SELECT INPUTS */
      .selectize-input { border-radius:0 !important; font-size:.82rem !important; }

      /* SLIDER */
      .irs--shiny .irs-bar { background:var(--accent) !important; }
      .irs--shiny .irs-handle { border-color:var(--accent) !important; }

      /* LEGEND BADGE */
      .overlay-legend {
        background:white; border:1px solid var(--paper);
        padding:.8rem 1rem; margin-top:.8rem; font-size:.75rem;
        font-family:var(--mono);
      }
      .legend-row { display:flex; align-items:center; gap:.5rem; margin:.3rem 0; }
      .legend-swatch { width:16px; height:16px; border:1px solid rgba(0,0,0,.1); flex-shrink:0; }
      /* GRADIENT LEGEND */
      .gradient-legend { margin-top:.5rem; }
      .gradient-bar { height:14px; border-radius:2px; border:1px solid rgba(0,0,0,.1); margin:.4rem 0 .2rem; }
      .gradient-labels { display:flex; justify-content:space-between; font-size:.68rem; color:var(--brown); }

      /* FOOTER */
      .dash-footer {
        background:var(--dark); color:rgba(250,247,242,.4);
        font-family:var(--mono); font-size:.7rem;
        padding:1.5rem 3rem;
        display:flex; justify-content:space-between; align-items:center;
        margin-top:2rem;
      }
      .dash-footer strong { color:var(--cream); }

      /* RESPONSIVE */
      @media(max-width:900px) {
        .tab-pane { padding:1.5rem; }
        .hero { padding:2rem 1.5rem; }
        .hero-kpis { gap:1.5rem; }
      }
    "))
  ),

  # ── HERO ────────────────────────────────────────────────────
  div(class="hero",
    div(class="hero-eyebrow", "American Fast Food & Public Health · Data Explorer"),
    h1(HTML("Fast Food, <em>Health & America</em>")),
    p(class="hero-desc",
      "An interactive look at how fast food chains, nutritional choices, obesity rates, poverty,
       and demographics intersect across the United States — powered by your real datasets."),
    div(class="hero-kpis",
      div(div(class="kpi-num","50"),   div(class="kpi-label","Fast Food Chains")),
      div(div(class="kpi-num","9,999"),div(class="kpi-label","Restaurant Locations")),
      div(div(class="kpi-num","515"),  div(class="kpi-label","Menu Items Analyzed")),
      div(div(class="kpi-num","51"),   div(class="kpi-label","States + DC"))
    )
  ),

  # ── MAIN TABS ───────────────────────────────────────────────
  navbarPage(title="", id="nav",

    # ── 0. ABOUT ───────────────────────────────────────────────
    tabPanel("About",
      div(class="section-header",
        div(div(class="section-label","About This Dashboard"),
            div(class="section-title","Fast Food & Health in America")),
        div(class="section-meta","")
      )
    ),

    # ── 1. CHAINS ──────────────────────────────────────────────
    tabPanel("Chains",
      div(class="section-header",
        div(div(class="section-label","Top 50 Fast Food Chains"),
            div(class="section-title","Sales & Market Presence")),
        div(class="section-meta","Source: QSR Magazine · 2021")
      ),
      div(class="chart-box",
        div(class="chart-box-title","Systemwide Sales — Top 20 Chains"),
        div(class="chart-box-sub","U.S. Sales in Millions USD · 2021"),
        plotlyOutput("salesChart", height="540px")
      ),
      div(class="chart-box",
        div(class="chart-box-title","Total Units vs. Average Sales per Unit"),
        div(class="chart-box-sub","Bubble size = total units · Each bubble = one chain · 2021"),
        plotlyOutput("chainScatterChart", height="420px")
      )
    ),

    # ── 2. NUTRITION ───────────────────────────────────────────
    tabPanel("Nutrition",
      div(class="section-header",
        div(div(class="section-label","Menu Nutrition"),
            div(class="section-title","What's Really in Your Meal?")),
        div(class="section-meta","Source: FDA · 515 Menu Items")
      ),
      tabsetPanel(
        tabPanel("By Restaurant", br(), uiOutput("nutrCards")),
        tabPanel("Calories & Sodium", br(),
          fluidRow(
            column(6, div(class="chart-box",
              div(class="chart-box-title","Avg Calories per Menu Item"),
              div(class="chart-box-sub","Mean across all items per restaurant"),
              plotlyOutput("calChart", height="320px"))),
            column(6, div(class="chart-box",
              div(class="chart-box-title","Avg Sodium per Menu Item (mg)"),
              div(class="chart-box-sub","Mean sodium content"),
              plotlyOutput("sodChart", height="320px")))
          )
        ),
        tabPanel("Protein & Fat", br(),
          fluidRow(
            column(6, div(class="chart-box",
              div(class="chart-box-title","Avg Protein per Menu Item (g)"),
              plotlyOutput("protChart", height="320px"))),
            column(6, div(class="chart-box",
              div(class="chart-box-title","Avg Total Fat per Menu Item (g)"),
              plotlyOutput("fatChart", height="320px")))
          )
        )
      )
    ),

    # ── 3. OBESITY ─────────────────────────────────────────────
    tabPanel("📊 Obesity",
      div(class="section-header",
        div(div(class="section-label","State Health Data"),
            div(class="section-title","Obesity Rates Across America")),
        div(class="section-meta","Source: CDC BRFSS · 2023")
      ),
      tabsetPanel(
        tabPanel("State Ranking", br(),
          fluidRow(
            column(6, div(class="chart-box",
              div(class="chart-box-title","Highest Obesity Rates — Top 15 States"),
              div(class="chart-box-sub","Adult obesity prevalence (%)"),
              plotlyOutput("obHighChart", height="440px"))),
            column(6, div(class="chart-box",
              div(class="chart-box-title","Lowest Obesity Rates — Bottom 10 States"),
              div(class="chart-box-sub","Adult obesity prevalence (%)"),
              plotlyOutput("obLowChart", height="440px")))
          )
        ),
        tabPanel("All States Table", br(), DTOutput("obTable"))
      )
    ),

    # ── 4. POVERTY ─────────────────────────────────────────────
    tabPanel("Poverty",
      div(class="section-header",
        div(div(class="section-label","Economic Context"),
            div(class="section-title","Poverty Rates by State")),
        div(class="section-meta","Source: USDA SAIPE · 2023")
      ),
      fluidRow(
        column(6, div(class="chart-box",
          div(class="chart-box-title","Highest Poverty Rates — Top 15 States"),
          div(class="chart-box-sub","% of all people in poverty · 2023"),
          plotlyOutput("povChart", height="440px"))),
        column(6, div(class="chart-box",
          div(class="chart-box-title","Poverty vs. Obesity Correlation"),
          div(class="chart-box-sub","Each dot = one state. Hover for details."),
          plotlyOutput("scatterChart", height="440px")))
      )
    ),

    # ── 5. DEMOGRAPHICS ────────────────────────────────────────
    tabPanel("Demographics",
      div(class="section-header",
        div(div(class="section-label","Demographics & Fast Food"),
            div(class="section-title","Fast Food Density by Racial Majority")),
        div(class="section-meta","Sources: KFF State Health Facts 2024 · Datafiniti")
      ),
      div(class="chart-box",
        div(class="chart-box-title","Fast Food Locations per 100k People — by State"),
        div(class="chart-box-sub","States colored by their largest racial/ethnic group · Hover for details"),
        plotlyOutput("demoFFChart", height="620px")
      )
    ),

    # ── 6. MAP ─────────────────────────────────────────────────
    tabPanel("Map",
      div(class="section-header",
        div(div(class="section-label","Interactive Map"),
            div(class="section-title","Fast Food Across America")),
        div(class="section-meta","Source: Datafiniti / Kaggle · 9,999 locations")
      ),
      fluidRow(
        column(9,
          div(class="map-control-row",
            div(style="display:flex;align-items:center;gap:.6rem;",
              tags$label(style="font-family:var(--mono);font-size:.75rem;color:var(--brown);margin:0;","Map Overlay:"),
              selectInput("mapOverlay", NULL,
                choices=c("None (Chain Colors)"="none",
                          "Obesity Rate by County"="obesity",
                          "Poverty Rate by County"="poverty"),
                width="220px")
            ),
            div(class="map-counter-text", textOutput("mapCounter", inline=TRUE))
          ),
          leafletOutput("ffMap", height="580px"),
          uiOutput("overlayLegend")
        ),
        column(3,
          div(class="filter-panel",
            div(class="chart-box-title","Filter Chains"),
            div(class="chart-box-sub","Click to toggle chains on/off"),
            uiOutput("chainFilterUI")
          )
        )
      )
    )
  ),

  # ── FOOTER ──────────────────────────────────────────────────
  div(class="dash-footer",
    div(HTML("<strong>Fast Food & Health in America</strong> — Interactive Shiny Dashboard")),
    div("CDC · USDA · FDA · US Census · QSR Magazine · KFF · BRFSS · Datafiniti")
  )
)
