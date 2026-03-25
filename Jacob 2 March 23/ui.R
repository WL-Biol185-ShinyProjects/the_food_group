library(shiny)
library(leaflet)
library(plotly)
library(DT)

#seperate function for about page which is called further down "tabpanel("about")
aboutTabUI <- function() {
  tagList(
    # ── Page-scoped CSS ───────────────────────────────────────────────────────
    tags$style(HTML("

      /* ── Google Fonts ── */
      @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=Playfair+Display:wght@700;900&display=swap');

      /* ── About wrapper ── */
      .about-page {
        background : #faf7f2;
        font-family: 'DM Sans', sans-serif;
        color      : #2c2318;
        min-height : 100vh;
        padding    : 0 0 80px;
      }

      /* ── Hero banner ── */
      .about-hero {
        background   : linear-gradient(135deg, #1a0f00 0%, #3b1f00 55%, #6b3a10 100%);
        padding      : 72px 5% 64px;
        position     : relative;
        overflow     : hidden;
      }
      .about-hero::before {
        content    : '';
        position   : absolute;
        inset      : 0;
        background : repeating-linear-gradient(
                       -45deg,
                       transparent,
                       transparent 18px,
                       rgba(255,199,44,.04) 18px,
                       rgba(255,199,44,.04) 19px
                     );
        pointer-events: none;
      }
      .about-hero-eyebrow {
        font-family   : 'DM Sans', sans-serif;
        font-size     : .78rem;
        font-weight   : 600;
        letter-spacing: .18em;
        text-transform: uppercase;
        color         : #FFC72C;
        margin-bottom : 14px;
      }
      .about-hero h1 {
        font-family : 'Playfair Display', Georgia, serif;
        font-size   : clamp(2.4rem, 5vw, 4rem);
        font-weight : 900;
        color       : #fff;
        line-height : 1.08;
        margin      : 0 0 20px;
      }
      .about-hero h1 span {
        color: #FFC72C;
      }
      .about-hero-sub {
        font-size  : 1.05rem;
        color      : rgba(255,255,255,.72);
        max-width  : 560px;
        line-height: 1.7;
        margin     : 0;
      }

      /* ── Content section ── */
      .about-content {
        max-width: 860px;
        margin   : 0 auto;
        padding  : 60px 5% 0;
      }

      /* ── Stat strip ── */
      .stat-strip {
        display              : grid;
        grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
        gap                  : 16px;
        margin               : 0 auto 56px;
        max-width            : 860px;
        padding              : 0 5%;
      }
      .stat-card {
        background   : #fff;
        border       : 1px solid #e8dfd0;
        border-radius: 10px;
        padding      : 22px 20px 18px;
        text-align   : center;
        box-shadow   : 0 2px 8px rgba(0,0,0,.04);
        transition   : transform .18s ease, box-shadow .18s ease;
      }
      .stat-card:hover {
        transform : translateY(-3px);
        box-shadow: 0 6px 18px rgba(0,0,0,.08);
      }
      .stat-number {
        font-family: 'Playfair Display', Georgia, serif;
        font-size  : 2.1rem;
        font-weight: 700;
        color      : #d4380d;
        line-height: 1;
        margin     : 0 0 6px;
      }
      .stat-label {
        font-size  : .78rem;
        font-weight: 500;
        color      : #7a6a57;
        line-height: 1.35;
      }

      /* ── Body copy ── */
      .about-section {
        margin-bottom: 48px;
      }
      .about-section h2 {
        font-family  : 'Playfair Display', Georgia, serif;
        font-size    : 1.55rem;
        font-weight  : 700;
        color        : #1a0f00;
        margin       : 0 0 16px;
        padding-left : 16px;
        border-left  : 4px solid #FFC72C;
      }
      .about-section p {
        font-size  : 1rem;
        line-height: 1.82;
        color      : #3d2e1e;
        margin     : 0 0 16px;
      }

      /* ── Divider ── */
      .about-divider {
        border     : none;
        border-top : 1px solid #e8dfd0;
        margin     : 0 0 48px;
      }

      /* ── Topic pills ── */
      .topic-row {
        display  : flex;
        flex-wrap: wrap;
        gap      : 10px;
        margin-top: 18px;
      }
      .topic-pill {
        background   : #fff3d6;
        border       : 1px solid #e0c97a;
        border-radius: 999px;
        padding      : 5px 16px;
        font-size    : .82rem;
        font-weight  : 500;
        color        : #7a4f00;
      }

      /* ── Team / footer strip ── */
      .about-footer-note {
        background   : #fff;
        border       : 1px solid #e8dfd0;
        border-radius: 10px;
        padding      : 24px 28px;
        font-size    : .88rem;
        color        : #7a6a57;
        line-height  : 1.7;
        margin-top   : 12px;
      }
      .about-footer-note strong {
        color: #2c2318;
      }
    ")),
    
    # ── Markup ────────────────────────────────────────────────────────────────
    div(class = "about-page",
      
        # Stat strip
        tags$div(style = "height:40px;"),  # spacer
        div(class = "stat-strip",
            div(class = "stat-card",
                div(class = "stat-number", "200K+"),
                div(class = "stat-label",  "Fast-food locations\nacross the U.S.")
            ),
            div(class = "stat-card",
                div(class = "stat-number", "~36%"),
                div(class = "stat-label",  "Adults who eat\nfast food daily")
            ),
            div(class = "stat-card",
                div(class = "stat-number", "50"),
                div(class = "stat-label",  "Top chains\nanalyzed")
            ),
            div(class = "stat-card",
                div(class = "stat-number", "$331B"),
                div(class = "stat-label",  "U.S. fast-food industry\nrevenue (2023)")
            )
        ),
        
        # Body content
        div(class = "about-content",
            
            # ── About the project
            div(class = "about-section",
                tags$h2("About Our Project"),
                tags$p(
                  "Our project explores the growth, impact, and consequences of the
             fast-food industry in the United States. What began in the early
             20th century as a convenient and affordable dining option has
             evolved into a dominant force in American culture, shaping how
             people eat, spend money, and experience food."
                ),
                tags$p(
                  "The rise of chains like White Castle, followed by industry leaders
             such as McDonald's and Burger King, introduced a model centered on
             speed, consistency, and low cost. While this innovation made food
             more accessible, it also contributed to a shift away from fresh,
             home-cooked meals toward highly processed options that are often
             high in calories, sodium, and fat."
                )
            ),
            
            tags$hr(class = "about-divider"),
            
            # ── What we examine
            div(class = "about-section",
                tags$h2("What We Examine"),
                tags$p(
                  "This project uses interactive data visualizations to examine the
             relationship between fast food and public health. By analyzing
             nutritional information, restaurant distribution, and state-level
             data on obesity and poverty, this project highlights how fast food
             consumption is connected to broader social and economic patterns."
                ),
                div(class = "topic-row",
                    span(class = "topic-pill", HTML("&#x1F354; Nutritional Data")),
                    span(class = "topic-pill", HTML("&#x1F4CD; Restaurant Distribution")),
                    span(class = "topic-pill", HTML("&#x1F4CA; Obesity by State")),
                    span(class = "topic-pill", HTML("&#x1F4B8; Poverty & Economics")),
                    span(class = "topic-pill", HTML("&#x1F465; Race & Demographics"))
                )
            ),
            
            tags$hr(class = "about-divider"),
            
            # ── Data sources / footer note
            div(class = "about-section",
                tags$h2("Data & Methods"),
                div(class = "about-footer-note",
                    tags$strong("Datasets used in this project include: "),
                    tags$br(),
                    HTML(
                      "&#x2022; Top 50 Fast-Food Chains in the USA (systemwide sales & unit counts)<br>
               &#x2022; Fast-food nutritional information by menu item<br>
               &#x2022; National Obesity by State (CDC / BRFSS)<br>
               &#x2022; Poverty rates by state (U.S. Census Bureau, 2023)<br>
               &#x2022; Race &amp; ethnicity demographics by state<br>
               &#x2022; Fast-food restaurant geolocation data"
                    ),
                    tags$br(), tags$br(),
                    "All visualizations are built with ",
                    tags$strong("R Shiny"), ", ",
                    tags$strong("Leaflet"), ", ",
                    tags$strong("Plotly"), ", and ",
                    tags$strong("DT"),
                    ". Data reflects the most recent available reporting year for each source."
                )
            )
            
        ) # /.about-content
    )   # /.about-page
  )
}

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
      body { background:var(--cream); color:var(--ink); font-family:var(--sans); font-size:16px; margin:0; padding:0; overflow-x:hidden; }

      .hero { background:var(--dark); color:var(--cream); padding:3rem 3rem 2.5rem; border-bottom:3px solid var(--accent); }
      .hero-eyebrow { font-family:var(--mono); font-size:.75rem; letter-spacing:.15em; text-transform:uppercase; color:var(--orange); margin-bottom:.8rem; display:inline-block; padding:.3rem .7rem; border:1px solid rgba(230,126,34,.4); }
      .hero h1 { font-family:var(--serif); font-size:clamp(2.5rem,5vw,5rem); font-weight:400; line-height:.95; letter-spacing:-.02em; margin-bottom:1rem; }
      .hero h1 em { color:var(--orange); font-style:italic; }
      .hero-desc { color:rgba(250,247,242,.65); font-size:1rem; max-width:600px; line-height:1.75; margin-bottom:2rem; }
      .hero-kpis { display:flex; gap:3rem; flex-wrap:wrap; padding-top:1.5rem; border-top:1px solid rgba(255,255,255,.1); }
      .kpi-num { font-family:var(--serif); font-size:2.8rem; color:var(--orange); line-height:1; }
      .kpi-label { font-size:.78rem; text-transform:uppercase; letter-spacing:.08em; color:rgba(250,247,242,.5); margin-top:.3rem; }

      .navbar { background:var(--dark) !important; border:none !important; border-radius:0 !important; margin-bottom:0 !important; border-bottom:2px solid var(--accent) !important; min-height:52px !important; }
      .navbar-default .navbar-nav>li>a { color:rgba(250,247,242,.6) !important; font-family:var(--mono) !important; font-size:.75rem !important; letter-spacing:.1em !important; text-transform:uppercase !important; padding:16px 18px !important; line-height:1.2 !important; }
      .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>li>a:hover { color:var(--orange) !important; background:rgba(255,255,255,.07) !important; }
      .navbar-brand { display:none !important; }
      .navbar-collapse { padding:0 !important; }

      .tab-content { width:100%; }
      .tab-pane { padding:2.5rem 3rem; min-height:600px; overflow-y:auto; }

      .nav-tabs { border-bottom:2px solid var(--paper); margin-bottom:1.8rem; }
      .nav-tabs>li>a { font-family:var(--mono) !important; font-size:.72rem !important; letter-spacing:.06em !important; text-transform:uppercase !important; color:var(--brown) !important; border:none !important; border-bottom:2px solid transparent !important; margin-bottom:-2px !important; padding:.6rem 1.2rem !important; }
      .nav-tabs>li.active>a { color:var(--dark) !important; border-bottom-color:var(--dark) !important; font-weight:700 !important; background:none !important; }
      .nav-tabs>li>a:hover { color:var(--ink) !important; background:none !important; }

      .section-header { display:flex; justify-content:space-between; align-items:baseline; border-bottom:2px solid var(--dark); padding-bottom:1rem; margin-bottom:2rem; }
      .section-label { font-family:var(--mono); font-size:.72rem; letter-spacing:.18em; text-transform:uppercase; color:var(--accent); margin-bottom:.35rem; }
      .section-title { font-family:var(--serif); font-size:clamp(1.6rem,2.5vw,2.4rem); font-weight:400; letter-spacing:-.02em; line-height:1.1; }
      .section-meta { font-size:.75rem; color:var(--brown); font-family:var(--mono); }

      .chart-box { background:white; border:1px solid var(--paper); padding:1.6rem; margin-bottom:1.8rem; }
      .chart-box-title { font-weight:700; font-size:.9rem; margin-bottom:.25rem; color:var(--dark); }
      .chart-box-sub { font-size:.72rem; color:var(--brown); font-family:var(--mono); margin-bottom:1.2rem; }

      .chain-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(210px,1fr)); gap:1px; background:var(--paper); margin-bottom:1.5rem; }
      .chain-card { background:white; padding:1.2rem 1.4rem; transition:background .15s; }
      .chain-card:hover { background:var(--warm); }
      .chain-rank { font-family:var(--mono); font-size:.65rem; color:var(--brown); }
      .chain-name { font-weight:700; font-size:.95rem; margin:.3rem 0 .7rem; line-height:1.2; }
      .chain-sales { font-family:var(--serif); font-size:1.6rem; color:var(--accent); line-height:1; }
      .chain-sales-label { font-size:.62rem; color:var(--brown); text-transform:uppercase; letter-spacing:.08em; font-family:var(--mono); }
      .chain-units { font-size:.76rem; color:var(--brown); margin-top:.35rem; }
      .chain-change { font-family:var(--mono); font-size:.72rem; margin-top:.25rem; }
      .chain-pos { color:#27ae60; } .chain-neg { color:#c0392b; }

      .nutr-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(190px,1fr)); gap:1px; background:var(--paper); }
      .nutr-card { background:white; padding:1.2rem; }
      .nutr-card-name { font-weight:700; font-size:.88rem; margin-bottom:.8rem; }
      .nutr-metric { display:flex; justify-content:space-between; font-size:.76rem; padding:.25rem 0; border-bottom:1px solid var(--warm); }
      .nutr-metric-label { color:var(--brown); }
      .nutr-metric-val { font-family:var(--mono); font-weight:700; }

      #ffMap { height:580px; width:100%; border:1px solid var(--paper); }
      .map-control-row { display:flex; align-items:center; gap:1.2rem; margin-bottom:1rem; flex-wrap:wrap; }
      .map-counter-text { font-family:var(--mono); font-size:.78rem; color:var(--brown); }

      .filter-panel { background:white; border:1px solid var(--paper); padding:1.2rem; }
      .filter-panel .checkbox label { font-size:.82rem; }

      .btn-filter-toggle { font-family:var(--mono) !important; font-size:.68rem !important; letter-spacing:.06em !important; text-transform:uppercase !important; background:var(--warm) !important; border:1px solid var(--tan) !important; color:var(--brown) !important; padding:.3rem .8rem !important; border-radius:0 !important; margin-bottom:.8rem !important; width:100% !important; }
      .btn-filter-toggle:hover { background:var(--paper) !important; color:var(--dark) !important; }

      .dataTables_wrapper { font-size:.82rem; }
      table.dataTable thead { background:var(--dark) !important; color:var(--cream) !important; }
      table.dataTable thead th { font-family:var(--mono) !important; font-size:.65rem !important; letter-spacing:.08em !important; text-transform:uppercase !important; border-bottom:none !important; padding:10px 12px !important; }
      table.dataTable tbody td { padding:8px 12px !important; font-size:.82rem !important; }
      tbody tr:hover { background:var(--warm) !important; }

      .selectize-input { border-radius:0 !important; font-size:.82rem !important; }
      .irs--shiny .irs-bar { background:var(--accent) !important; }
      .irs--shiny .irs-handle { border-color:var(--accent) !important; }

      .overlay-legend { background:white; border:1px solid var(--paper); padding:.8rem 1rem; margin-top:.8rem; font-size:.75rem; font-family:var(--mono); }
      .legend-row { display:flex; align-items:center; gap:.5rem; margin:.3rem 0; }
      .legend-swatch { width:16px; height:16px; border:1px solid rgba(0,0,0,.1); flex-shrink:0; }
      .gradient-legend { margin-top:.5rem; }
      .gradient-bar { height:14px; border-radius:2px; border:1px solid rgba(0,0,0,.1); margin:.4rem 0 .2rem; }
      .gradient-labels { display:flex; justify-content:space-between; font-size:.68rem; color:var(--brown); }

      /* ── COMPARE TAB ── */
      .compare-pickers {
        display:grid; grid-template-columns:1fr 48px 1fr;
        gap:.8rem; align-items:center; margin-bottom:1.4rem;
      }
      .compare-picker-box {
        background:white; border:1px solid var(--paper); padding:1rem 1.2rem;
      }
      .compare-map-label {
        font-family:var(--mono); font-size:.7rem; letter-spacing:.1em;
        text-transform:uppercase; margin-bottom:.5rem; display:block;
      }
      .compare-map-label.left  { color:#1a73e8; border-left:3px solid #1a73e8; padding-left:.5rem; }
      .compare-map-label.right { color:#c0392b; border-left:3px solid #c0392b; padding-left:.5rem; }
      .compare-vs { font-family:var(--serif); font-size:1.8rem; color:var(--tan); text-align:center; }

      .compare-maps-wrap {
        display:grid; grid-template-columns:1fr 1fr;
        gap:1.2rem; margin-bottom:1.4rem;
      }
      @media(max-width:860px) { .compare-maps-wrap { grid-template-columns:1fr; } }
      .compare-map-panel { background:white; border:1px solid var(--paper); padding:1rem; }
      .compare-legend-title { font-family:var(--mono); font-size:.67rem; color:var(--brown); margin-bottom:.3rem; }
      .compare-grad-bar { height:10px; border-radius:2px; margin:.3rem 0 .2rem; }
      .compare-grad-labels { display:flex; justify-content:space-between; font-family:var(--mono); font-size:.63rem; color:var(--brown); }

      .compare-scatter-box { background:white; border:1px solid var(--paper); padding:1.4rem; }
      .compare-scatter-title { font-weight:700; font-size:.9rem; margin-bottom:.2rem; }
      .compare-scatter-sub { font-family:var(--mono); font-size:.7rem; color:var(--brown); margin-bottom:1rem; }

      .dash-footer { background:var(--dark); color:rgba(250,247,242,.4); font-family:var(--mono); font-size:.7rem; padding:1.5rem 3rem; display:flex; justify-content:space-between; align-items:center; margin-top:2rem; }
      .dash-footer strong { color:var(--cream); }

      @media(max-width:900px) { .tab-pane { padding:1.5rem; } .hero { padding:2rem 1.5rem; } .hero-kpis { gap:1.5rem; } }
    "))
  ),
  
  # ── HERO ────────────────────────────────────────────────────
  div(class="hero",
      div(class="hero-eyebrow", "American Fast Food & Public Health · Data Explorer"),
      h1(HTML("Fast Food, Health, and  Poverty in America")),
      p(class="hero-desc",
        "An interactive look at how fast food chains, nutritional choices, obesity rates, poverty,
       and demographics intersect across the United States — powered by real datasets."),
      div(class="hero-kpis",
          div(div(class="kpi-num","9,999"),div(class="kpi-label","Restaurant Locations")),
          div(div(class="kpi-num","515"),  div(class="kpi-label","Menu Items Analyzed")),
          div(div(class="kpi-num","51"),   div(class="kpi-label","States + DC"))
      )
  ),
  
  # ── MAIN TABS ───────────────────────────────────────────────
  navbarPage(title="", id="nav",
             
             tabPanel("About",
                      aboutTabUI()
                      ),
             
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
                                   column(6, div(class="chart-box", div(class="chart-box-title","Avg Calories per Menu Item"), div(class="chart-box-sub","Mean across all items per restaurant"), plotlyOutput("calChart", height="320px"))),
                                   column(6, div(class="chart-box", div(class="chart-box-title","Avg Sodium per Menu Item (mg)"), div(class="chart-box-sub","Mean sodium content"), plotlyOutput("sodChart", height="320px")))
                                 )
                        ),
                        tabPanel("Protein & Fat", br(),
                                 fluidRow(
                                   column(6, div(class="chart-box", div(class="chart-box-title","Avg Protein per Menu Item (g)"), plotlyOutput("protChart", height="320px"))),
                                   column(6, div(class="chart-box", div(class="chart-box-title","Avg Total Fat per Menu Item (g)"), plotlyOutput("fatChart", height="320px")))
                                 )
                        )
                      )
             ),
             
             tabPanel("\U0001f4ca Obesity",
                      div(class="section-header",
                          div(div(class="section-label","State Health Data"),
                              div(class="section-title","Obesity Rates Across America")),
                          div(class="section-meta","Source: CDC BRFSS · 2023")
                      ),
                      tabsetPanel(
                        tabPanel("State Ranking", br(),
                                 fluidRow(
                                   column(6, div(class="chart-box", div(class="chart-box-title","Highest Obesity Rates — Top 15 States"), div(class="chart-box-sub","Adult obesity prevalence (%)"), plotlyOutput("obHighChart", height="440px"))),
                                   column(6, div(class="chart-box", div(class="chart-box-title","Lowest Obesity Rates — Bottom 10 States"), div(class="chart-box-sub","Adult obesity prevalence (%)"), plotlyOutput("obLowChart", height="440px")))
                                 )
                        ),
                        tabPanel("All States Table", br(), DTOutput("obTable"))
                      )
             ),
             
             tabPanel("Poverty",
                      div(class="section-header",
                          div(div(class="section-label","Economic Context"),
                              div(class="section-title","Poverty Rates by State")),
                          div(class="section-meta","Source: USDA SAIPE · 2023")
                      ),
                      fluidRow(
                        column(6, div(class="chart-box", div(class="chart-box-title","Highest Poverty Rates — Top 15 States"), div(class="chart-box-sub","% of all people in poverty · 2023"), plotlyOutput("povChart", height="440px"))),
                        column(6, div(class="chart-box", div(class="chart-box-title","Poverty vs. Obesity Correlation"), div(class="chart-box-sub","Each dot = one state. Hover for details."), plotlyOutput("scatterChart", height="440px")))
                      )
             ),
             
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
                                   actionButton("toggleAllChains", "Deselect All", class="btn-filter-toggle"),
                                   uiOutput("chainFilterUI")
                               )
                        )
                      )
             ),
             
             # ── COMPARE TAB ────────────────────────────────────────────
             tabPanel("Compare",
                      div(class="section-header",
                          div(div(class="section-label","Geographic Comparison · USDA Food Environment Atlas 2025"),
                              div(class="section-title","Compare Any Two Variables, County by County")),
                          div(class="section-meta","3,153 U.S. counties · Obesity, Diabetes, Poverty, Food Security & more")
                      ),
                      
                      # Variable pickers
                      div(class="compare-pickers",
                          div(class="compare-picker-box",
                              span(class="compare-map-label left", "Left Map — Variable A"),
                              selectInput("compareVarA", NULL, width="100%",
                                          choices = list(
                                            "Health" = c(
                                              "Obesity Rate % (2022)"              = "ObesityRate2022",
                                              "Diabetes Rate % (2019)"             = "DiabetesRate2019",
                                              "Physically Active HS % (2021)"      = "PctHSPhysActive2021"
                                            ),
                                            "Food Security" = c(
                                              "Food Insecurity % (2021-23)"        = "FoodInsecPct2123",
                                              "Very Low Food Security % (2021-23)" = "VeryLowFoodSecPct2123",
                                              "Low Food Access % (2019)"           = "PctLowAccess2019",
                                              "Low Income + Low Access %"          = "PctLowIncLowAccess2019",
                                              "Children Low Access %"              = "PctChildLowAccess2019"
                                            ),
                                            "Economics" = c(
                                              "Poverty Rate % (2021)"              = "PovRate2021",
                                              "Child Poverty Rate % (2021)"        = "ChildPovRate2021",
                                              "Deep Poverty Rate % (2021)"         = "DeepPovRate2021",
                                              "Median HH Income $ (2021)"          = "MedianHHInc2021",
                                              "SNAP Participation % (2022)"        = "SNAPPct2022",
                                              "School Lunch % (2021)"              = "SchoolLunchPct2021"
                                            ),
                                            "Food Environment" = c(
                                              "Fast Food per 1k (2020)"            = "FFRPer1k2020",
                                              "Grocery Stores per 1k (2020)"       = "GrocPer1k2020",
                                              "Dollar Stores per 1k (2020)"        = "DollarPer1k2020",
                                              "Conv Stores per 1k (2020)"          = "ConvPer1k2020",
                                              "Rec Facilities per 1k (2020)"       = "RecFacPer1k2020",
                                              "Farmers Markets per 1k (2018)"      = "FarmMktPer1k2018"
                                            ),
                                            "Demographics" = c(
                                              "% White (2020)"                     = "PctWhite2020",
                                              "% Black (2020)"                     = "PctBlack2020",
                                              "% Hispanic (2020)"                  = "PctHisp2020",
                                              "% Age 65+ (2020)"                   = "Pct65Plus2020",
                                              "% Under 18 (2020)"                  = "PctUnder182020"
                                            )
                                          ),
                                          selected = "ObesityRate2022"
                              )
                          ),
                          div(class="compare-vs", "vs"),
                          div(class="compare-picker-box",
                              span(class="compare-map-label right", "Right Map — Variable B"),
                              selectInput("compareVarB", NULL, width="100%",
                                          choices = list(
                                            "Health" = c(
                                              "Obesity Rate % (2022)"              = "ObesityRate2022",
                                              "Diabetes Rate % (2019)"             = "DiabetesRate2019",
                                              "Physically Active HS % (2021)"      = "PctHSPhysActive2021"
                                            ),
                                            "Food Security" = c(
                                              "Food Insecurity % (2021-23)"        = "FoodInsecPct2123",
                                              "Very Low Food Security % (2021-23)" = "VeryLowFoodSecPct2123",
                                              "Low Food Access % (2019)"           = "PctLowAccess2019",
                                              "Low Income + Low Access %"          = "PctLowIncLowAccess2019",
                                              "Children Low Access %"              = "PctChildLowAccess2019"
                                            ),
                                            "Economics" = c(
                                              "Poverty Rate % (2021)"              = "PovRate2021",
                                              "Child Poverty Rate % (2021)"        = "ChildPovRate2021",
                                              "Deep Poverty Rate % (2021)"         = "DeepPovRate2021",
                                              "Median HH Income $ (2021)"          = "MedianHHInc2021",
                                              "SNAP Participation % (2022)"        = "SNAPPct2022",
                                              "School Lunch % (2021)"              = "SchoolLunchPct2021"
                                            ),
                                            "Food Environment" = c(
                                              "Fast Food per 1k (2020)"            = "FFRPer1k2020",
                                              "Grocery Stores per 1k (2020)"       = "GrocPer1k2020",
                                              "Dollar Stores per 1k (2020)"        = "DollarPer1k2020",
                                              "Conv Stores per 1k (2020)"          = "ConvPer1k2020",
                                              "Rec Facilities per 1k (2020)"       = "RecFacPer1k2020",
                                              "Farmers Markets per 1k (2018)"      = "FarmMktPer1k2018"
                                            ),
                                            "Demographics" = c(
                                              "% White (2020)"                     = "PctWhite2020",
                                              "% Black (2020)"                     = "PctBlack2020",
                                              "% Hispanic (2020)"                  = "PctHisp2020",
                                              "% Age 65+ (2020)"                   = "Pct65Plus2020",
                                              "% Under 18 (2020)"                  = "PctUnder182020"
                                            )
                                          ),
                                          selected = "PovRate2021"
                              )
                          )
                      ),
                      
                      # Dual choropleth maps
                      div(class="compare-maps-wrap",
                          div(class="compare-map-panel",
                              span(class="compare-map-label left", textOutput("compareLabelA", inline=TRUE)),
                              br(),
                              leafletOutput("compareMapA", height="420px"),
                              br(),
                              uiOutput("compareLegendA")
                          ),
                          div(class="compare-map-panel",
                              span(class="compare-map-label right", textOutput("compareLabelB", inline=TRUE)),
                              br(),
                              leafletOutput("compareMapB", height="420px"),
                              br(),
                              uiOutput("compareLegendB")
                          )
                      ),
                      
                      # Scatter: A vs B, one point per county
                      div(class="compare-scatter-box",
                          div(class="compare-scatter-title", textOutput("compareScatterTitle")),
                          div(class="compare-scatter-sub", "Each point = one county · hover for details · color = U.S. Census region"),
                          plotlyOutput("compareScatter", height="420px")
                      )
             )
  ),
  
  div(class="dash-footer",
      div(HTML("<strong>Fast Food & Health in America</strong> — Interactive Shiny Dashboard")),
      div("CDC · USDA ERS · FDA · US Census · QSR Magazine · KFF · BRFSS · Datafiniti")
  )
)