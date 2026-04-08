library(shiny)
library(leaflet)
library(plotly)
library(DT)

# ── ABOUT PAGE UI ────────────────────────────────────────────
aboutTabUI <- function() {
  tagList(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=Playfair+Display:wght@700;900&display=swap');
      .about-page { background:#faf7f2; font-family:'DM Sans',sans-serif; color:#2c2318; min-height:100vh; padding:0 0 80px; }
      .stat-strip { display:grid; grid-template-columns:repeat(auto-fit,minmax(170px,1fr)); gap:16px; margin:0 auto 56px; max-width:860px; padding:0 5%; }
      .stat-card { background:#fff; border:1px solid #e8dfd0; border-radius:10px; padding:22px 20px 18px; text-align:center; box-shadow:0 2px 8px rgba(0,0,0,.04); transition:transform .18s ease,box-shadow .18s ease; }
      .stat-card:hover { transform:translateY(-3px); box-shadow:0 6px 18px rgba(0,0,0,.08); }
      .stat-number { font-family:'Playfair Display',Georgia,serif; font-size:2.5rem; font-weight:700; color:#d4380d; line-height:1; margin:0 0 6px; }
      .stat-label { font-size:1.2rem; font-weight:500; color:#7a6a57; line-height:1.35; }
      .about-content { max-width:860px; margin:0 auto; padding:60px 5% 0; }
      .about-section { margin-bottom:48px; }
      .about-section h2 { font-family:'Playfair Display',Georgia,serif; font-size:1.55rem; font-weight:700; color:#1a0f00; margin:0 0 16px; padding-left:16px; border-left:4px solid #FFC72C; }
      .about-section p { font-size:1.6rem; line-height:1.82; color:#3d2e1e; margin:0 0 16px; }
      .about-divider { border:none; border-top:1px solid #e8dfd0; margin:0 0 48px; }
      .topic-row { display:flex; flex-wrap:wrap; gap:10px; margin-top:18px; }
      .topic-pill { background:#fff3d6; border:1px solid #e0c97a; border-radius:999px; padding:5px 16px; font-size:.82rem; font-weight:500; color:#7a4f00; }
      .about-footer-note { background:#fff; border:1px solid #e8dfd0; border-radius:10px; padding:24px 28px; font-size:.88rem; color:#7a6a57; line-height:1.7; margin-top:12px; }
      .about-footer-note strong { color:#2c2318; }
    ")),
    div(class="about-page",
        # Stat strip
        tags$div(style = "height:40px;"), # spacer
        
        
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
        
        tags$div(
          style = "text-align:center; margin:0 auto 48px; max-width:860px; padding:0 5%;",
          tags$img(
            src = "https://i.imgur.com/VDs1rUp.png",
            alt = "Fast food image",
            style = "width:100%; max-width:860px; border-radius:10px; object-fit:cover;"
          )
        ),
        
        
        div(class="about-content",
            div(class="about-section",
                tags$h2("About Our Project"),
                tags$p("Our project explores the growth, impact, and consequences of the fast-food industry in the United States. What began in the early 20th century as a convenient and affordable dining option has evolved into a dominant force in American culture, shaping how people eat, spend money, and experience food."),
                tags$p("The rise of chains like White Castle, followed by industry leaders such as McDonald's and Burger King, introduced a model centered on speed, consistency, and low cost. While this innovation made food more accessible, it also contributed to a shift away from fresh, home-cooked meals toward highly processed options that are often high in calories, sodium, and fat.")
            ),
            tags$hr(class="about-divider"),
            div(class="about-section",
                tags$h2("What We Examine"),
                tags$p("This project uses interactive data visualizations to examine the relationship between fast food and public health. By analyzing nutritional information, restaurant distribution, and state-level data on obesity and poverty, this project highlights how fast food consumption is connected to broader social and economic patterns."),
                div(class="topic-row",
                    span(class="topic-pill", HTML("Nutritional Data")),
                    span(class="topic-pill", HTML("Restaurant Distribution")),
                    span(class="topic-pill", HTML("Obesity by State")),
                    span(class="topic-pill", HTML("Poverty & Economics")),
                    span(class="topic-pill", HTML("Race & Demographics"))
                )
            ),
            tags$hr(class="about-divider"),
            div(class="about-section",
                tags$h2("Data & Methods"),
                div(class="about-footer-note",
                    tags$strong("Datasets used in this project include:"),
                    tags$br(),
                    HTML("&#x2022; Top 50 Fast-Food Chains in the USA (systemwide sales & unit counts)<br>&#x2022; Fast-food nutritional information by menu item<br>&#x2022; National Obesity by State (CDC / BRFSS)<br>&#x2022; Poverty rates by state (U.S. Census Bureau, 2023)<br>&#x2022; Race &amp; ethnicity demographics by state<br>&#x2022; Fast-food restaurant geolocation data"),
                    tags$br(), tags$br(),
                    "All visualizations are built with ", tags$strong("R Shiny"), ", ",
                    tags$strong("Leaflet"), ", ", tags$strong("Plotly"), ", and ",
                    tags$strong("DT"), ". Data reflects the most recent available reporting year for each source."
                )
            )
        )
    )
  )
}

# ── MEET THE TEAM PAGE UI ────────────────────────────────────
meetTeamUI <- function() {
  tagList(
    tags$style(HTML("
      .team-page { background:#faf7f2; font-family:'DM Sans',sans-serif; color:#2c2318; min-height:100vh; padding:60px 5% 80px; }
      .team-page-title { font-family:'Playfair Display',Georgia,serif; font-size:2.2rem; font-weight:700; color:#1a0f00; margin:0 0 8px; }
      .team-page-sub { font-size:1.6rem; color:#7a6a57; margin:0 0 48px; line-height:1.6; max-width:560px; }
      .team-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(260px,1fr)); gap:28px; max-width:960px; }
      .team-card { background:#fff; border:1px solid #e8dfd0; border-radius:12px; overflow:hidden; box-shadow:0 2px 10px rgba(0,0,0,.05); transition:transform .2s ease,box-shadow .2s ease; }
      .team-card:hover { transform:translateY(-4px); box-shadow:0 8px 24px rgba(0,0,0,.1); }
      .team-photo-wrap { width:100%; aspect-ratio:1/1; overflow:hidden; background:#ede8df; }
      .team-photo-wrap img { width:100%; height:100%; object-fit:cover; display:block; }
      .team-photo-placeholder { width:100%; height:100%; display:flex; align-items:center; justify-content:center; font-size:4rem; background:linear-gradient(135deg,#ede8df,#d4c9b5); }
      .team-card-body { padding:22px 24px 26px; }
      .team-name { font-family:'Playfair Display',Georgia,serif; font-size:1.6rem; font-weight:700; color:#1a0f00; margin:0 0 4px; }
      .team-role { font-size:1.4rem; font-weight:600; letter-spacing:.1em; text-transform:uppercase; color:#d4380d; margin:0 0 14px; }
      .team-bio { font-size:.1.2rem; line-height:1.75; color:#3d2e1e; margin:0; }
      .team-divider { border:none; border-top:1px solid #e8dfd0; margin:0 0 48px; max-width:960px; }
    ")),
    div(class="team-page",
        tags$h2(class="team-page-title", "Meet the Team"),
        tags$p(class="team-page-sub",
               "Fast food isn't just about burgers. We built this to show how one industry quietly connects to poverty, race, health, and geography — all at once."),
        tags$hr(class="team-divider"),
        div(class="team-grid",
            
            # ── Member 1 ──────────────────────────────────────
            div(class="team-card",
                div(class="team-photo-wrap",
                    # Replace the src below with a real photo URL or relative path
                    tags$img(src="https://columns.wlu.edu/wp-content/uploads/2024/12/Mellanese-Barlow-2-350x233.jpg", alt="Team Member 1")
                ),
                div(class="team-card-body",
                    tags$h3(class="team-name",  "Mellanese Barlow"),
                    tags$p(class="team-role",   "B.S. Biology and Minor in Poverty Studies"),
                    tags$p(class="team-bio",
                           "Hi! I am a Pre-Vet Biology major with a minor in Poverty and Human Capabilty Studies.")
                )
            ),
            
            # ── Member 2 ──────────────────────────────────────
            div(class="team-card",
                div(class="team-photo-wrap",
                    tags$img(src="https://imglink.cc/cdn/_zYIttOe2u.jpeg", alt="Team Member 2")
                ),
                div(class="team-card-body",
                    tags$h3(class="team-name",  "Gracie Jorgensen"),
                    tags$p(class="team-role" , "B.S. Biology, Philosophy Minor"),
                    tags$p(class="team-bio",
                           "Hi there! I am from Macon, Georgia. I'm a junior at Washington and Lee University on the pre-vet track. ")
                )
            ),
            
            # ── Member 3 ──────────────────────────────────────
            div(class="team-card",
                div(class="team-photo-wrap",
                    tags$img(src="https://generalssports.com/images/2025/11/3/Wright.jpg?width=300", alt="Team Member 3")
                ),
                div(class="team-card-body",
                    tags$h3(class="team-name",  "Jacob Wright"),
                    tags$p(class="team-role",   "B.S. Neuroscience"),
                    tags$p(class="team-bio",
                           "Hello! I am a from Fredericksburg, VA. Hope you enjoy our app! Thank you to the Washington and Lee Data Science program for making this happen!"),

                    tags$h3(class="team-name",  ""),
                    tags$p(class="team-role", ),
                    tags$p(class="team-bio",
                           "
")
                )
            )
            
        ) # end team-grid
    )   # end team-page
  )
}

# ── UI ───────────────────────────────────────────────────────
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

      .hero { background:var(--dark); color:var(--cream); padding:3rem 3rem 2.5rem; border-bottom:3px solid var(--accent); text-align:center; display:flex; flex-direction:column; align-items:center; }
      .hero-eyebrow { font-family:var(--mono); font-size:.75rem; letter-spacing:.15em; text-transform:uppercase; color:var(--orange); margin-bottom:.8rem; display:inline-block; padding:.3rem .7rem; border:1px solid rgba(230,126,34,.4); }
      .hero h1 { font-family:var(--serif); font-size:clamp(2.5rem,5vw,5rem); font-weight:400; line-height:.95; letter-spacing:-.02em; margin-bottom:1rem; }
      .hero h1 em { color:var(--orange); font-style:italic; }
      .hero-desc { color:rgba(250,247,242,.65); font-size:1.6rem; max-width:600px; line-height:1.75; margin-bottom:2rem; }
      .hero-kpis { display:flex; gap:3rem; flex-wrap:wrap; padding-top:1.5rem; border-top:1px solid rgba(255,255,255,.1); }
      .kpi-num { font-family:var(--serif); font-size:2.8rem; color:var(--orange); line-height:1; }
      .kpi-label { font-size:1.6rem; text-transform:uppercase; letter-spacing:.08em; color:rgba(250,247,242,.5); margin-top:.3rem; }

      .navbar { background:var(--dark) !important; border:none !important; border-radius:0 !important; margin-bottom:0 !important; border-bottom:2px solid var(--accent) !important; min-height:52px !important; }
      .navbar-default .navbar-nav>li>a { color:rgba(250,247,242,.6) !important; font-family:var(--mono) !important; font-size:1.3rem !important; letter-spacing:.1em !important; text-transform:uppercase !important; padding:16px 18px !important; line-height:1.2 !important; }
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

      .bubble-controls { display:flex; align-items:center; gap:2rem; margin-bottom:1rem; padding:.8rem 1.2rem; background:var(--warm); border:1px solid var(--paper); flex-wrap:wrap; }
      .bubble-controls .control-label { font-family:var(--mono); font-size:.72rem; letter-spacing:.08em; text-transform:uppercase; color:var(--brown); margin-bottom:0; white-space:nowrap; }
      .bubble-controls .irs { margin-bottom:0; }
      .bubble-legend { display:flex; gap:1.4rem; align-items:center; flex-wrap:wrap; font-size:.78rem; font-family:var(--mono); color:var(--brown); }
      .bubble-legend-item { display:flex; align-items:center; gap:.4rem; }
      .bubble-legend-swatch { width:13px; height:13px; border-radius:50%; border:1px solid rgba(0,0,0,.12); flex-shrink:0; }

      .overlay-legend { background:white; border:1px solid var(--paper); padding:.8rem 1rem; margin-top:.8rem; font-size:.75rem; font-family:var(--mono); }
      .gradient-legend { margin-top:.5rem; }
      .gradient-bar { height:14px; border-radius:2px; border:1px solid rgba(0,0,0,.1); margin:.4rem 0 .2rem; }
      .gradient-labels { display:flex; justify-content:space-between; font-size:.68rem; color:var(--brown); }

      .compare-pickers { display:grid; grid-template-columns:1fr 48px 1fr; gap:.8rem; align-items:center; margin-bottom:1.4rem; }
      .compare-picker-box { background:white; border:1px solid var(--paper); padding:1rem 1.2rem; }
      .compare-map-label { font-family:var(--mono); font-size:.7rem; letter-spacing:.1em; text-transform:uppercase; margin-bottom:.5rem; display:block; }
      .compare-map-label.left  { color:#1a73e8; border-left:3px solid #1a73e8; padding-left:.5rem; }
      .compare-map-label.right { color:#c0392b; border-left:3px solid #c0392b; padding-left:.5rem; }
      .compare-vs { font-family:var(--serif); font-size:1.8rem; color:var(--tan); text-align:center; }
      .compare-maps-wrap { display:grid; grid-template-columns:1fr 1fr; gap:1.2rem; margin-bottom:1.4rem; }
      @media(max-width:860px) { .compare-maps-wrap { grid-template-columns:1fr; } }
      .compare-map-panel { background:white; border:1px solid var(--paper); padding:1rem; }
      .compare-legend-title { font-family:var(--mono); font-size:.67rem; color:var(--brown); margin-bottom:.3rem; }
      .compare-grad-bar { height:10px; border-radius:2px; margin:.3rem 0 .2rem; }
      .compare-grad-labels { display:flex; justify-content:space-between; font-family:var(--mono); font-size:.63rem; color:var(--brown); }

      .dash-footer { background:var(--dark); color:rgba(250,247,242,.4); font-family:var(--mono); font-size:.7rem; padding:1.5rem 3rem; display:flex; justify-content:space-between; align-items:center; margin-top:2rem; }
      .dash-footer strong { color:var(--cream); }

      .page-blurb { font-size:1.6rem; line-height:1.78; max-width:860px; color:var(--ink); margin-bottom:2rem; }

      @media(max-width:900px) { .tab-pane { padding:1.5rem; } .hero { padding:2rem 1.5rem; } }
    "))
  ),
  
  div(class="hero",
      div(class="hero-eyebrow", "American Fast Food & Public Health · Data Explorer"),
      h1(HTML("Fast Food, Health, and Poverty in America")),
      p(class="hero-desc",
        "Visualizing the relationships between fast food access, nutritional habits, obesity, and income across U.S. communities."),
      div(class="hero-kpis")
  ),
  
  navbarPage(title="", id="nav",
             
             tabPanel("About", aboutTabUI()),
             
             tabPanel("Chains",
                      div(class="section-header",
                          div(div(class="section-label","Top 50 Fast Food Chains"),
                              div(class="section-title","Sales & Market Presence")),
                          div(class="section-meta",
                              tags$a(href = "https://www.kaggle.com/datasets/iamsouravbanerjee/top-50-fastfood-chains-in-usa", 
                                     "Data: Top 50 Fast Food Chains in USA", 
                                     target = "_blank"))
                      ),
                      p(class="page-blurb",
                        "The bar chart below ranks the top 20 chains by total U.S. systemwide sales in 2021, revealing just how dominant a handful of brands are — McDonald's alone outpaces most competitors by billions. The bubble chart plots each chain by its average sales per unit against total systemwide revenue, with bubble size reflecting total number of locations. Together, these two views separate chains that win through sheer volume of locations from those that generate outsized revenue per restaurant."),
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
             
             # ── COMBINED NUTRITION & OBESITY ─────────────────────────
             tabPanel("Nutrition",
                      div(class="section-header",
                          div(div(class="section-label","Nutrition & Health"),
                              div(class="section-title","What We Eat & What It Costs Us")),
                          div(class="section-meta",
                              tags$a(href = "https://www.kaggle.com/datasets/ulrikthygepedersen/fastfood-nutrition?resource=download",
                                     "Fast Food Nutrition",
                                     target = "_blank") )
                      ),
                      p(class="page-blurb",
                        "A breakdown of the average nutritional content of menu items across 8 major chains. The numbers reveal stark differences: a typical McDonald's item carries over 640 calories and nearly 1,440mg of sodium, which is close to 60% of the recommended daily limit in a single item. Even chains perceived as \u201chealthier\u201d options like Subway still average over 1,270mg of sodium per item. Across the board, high sodium stands out as a consistent concern, regardless of calorie count."),
                      
                      # Row 1: Healthiness score + Nutrient heatmap
                      fluidRow(
                        column(7,
                               div(class="chart-box",
                                   div(class="chart-box-title","Chain Healthiness Score"),
                                   div(class="chart-box-sub","Composite score based on avg calories, sodium, sat. fat, sugar, protein & fiber · Higher = healthier"),
                                   plotlyOutput("nutrHealthChart", height="380px"),
                                   tags$div(
                                     style="margin-top:1rem;padding:1rem 1.1rem;background:#faf7f2;border:1px solid #e8dfd0;border-radius:8px;",
                                     tags$p(
                                       style="font-family:'Space Mono',monospace;font-size:.68rem;letter-spacing:.12em;text-transform:uppercase;color:#d4380d;margin:0 0 6px;",
                                       "How to read this chart"
                                     ),
                                     tags$p(
                                       style="font-size:.82rem;color:#3d2e1e;margin:0 0 12px;line-height:1.65;",
                                       "Each chain is scored 0\u2013100 based on the average nutritional content of its menu items. ",
                                       tags$strong("A higher score means fewer of the bad things (calories, sodium, saturated fat, sugar) and more of the good things (protein, fiber)."),
                                       " Scores are relative \u2014 a chain scoring 70 is healthier than average, not necessarily healthy in an absolute sense."
                                     ),
                                     tags$div(
                                       style="display:flex;gap:8px;flex-wrap:wrap;margin-bottom:10px;",
                                       tags$div(
                                         style="flex:1;min-width:140px;background:white;border:1px solid #e8dfd0;border-radius:6px;padding:8px 10px;border-top:3px solid #c0392b;",
                                         tags$div(style="font-size:.72rem;font-weight:700;color:#c0392b;margin-bottom:4px;", "Penalizes \u2193"),
                                         tags$div(style="font-size:.75rem;color:#3d2e1e;line-height:1.6;", "High calories (22%)"),
                                         tags$div(style="font-size:.75rem;color:#3d2e1e;line-height:1.6;", "High sodium (22%)"),
                                         tags$div(style="font-size:.75rem;color:#3d2e1e;line-height:1.6;", "High saturated fat (18%)"),
                                         tags$div(style="font-size:.75rem;color:#3d2e1e;line-height:1.6;", "High sugar (13%)")
                                       ),
                                       tags$div(
                                         style="flex:1;min-width:140px;background:white;border:1px solid #e8dfd0;border-radius:6px;padding:8px 10px;border-top:3px solid #27ae60;",
                                         tags$div(style="font-size:.72rem;font-weight:700;color:#27ae60;margin-bottom:4px;", "Rewards \u2191"),
                                         tags$div(style="font-size:.75rem;color:#3d2e1e;line-height:1.6;", "High protein (15%)"),
                                         tags$div(style="font-size:.75rem;color:#3d2e1e;line-height:1.6;", "High fiber (10%)")
                                       )
                                     ),
                                     tags$p(
                                       style="font-size:.74rem;color:#7a6a57;margin:0;line-height:1.5;border-top:1px solid #e8dfd0;padding-top:8px;",
                                       "Percentages reflect each nutrient's weight in the final score, based on the ",
                                       tags$a(
                                         href="https://www.fao.org/nutrition/requirements/en/",
                                         target="_blank",
                                         style="color:#d4380d;text-decoration:underline;",
                                         "Nutrient Profiling Index (NPI)"
                                       ),
                                       " model used in public health research."
                                     )
                                   )
                               )
                        ),
                        column(5,
                               div(class="chart-box",
                                   div(class="chart-box-title","Nutrient Comparison by Chain"),
                                   div(class="chart-box-sub","Average per menu item · Normalized 0\u2013100 · Darker = higher value"),
                                   plotlyOutput("nutrHeatmapChart", height="380px")
                               )
                        )
                      ),
                      
             ),
             
             tabPanel("Obesity",
                      div(class="section-header",
                          div(div(class="section-label","Obesity & Public Health"),
                              div(class="section-title","Adult Obesity Rates Across America")),
                          div(class="section-meta",
                              tags$a(href = "https://www.cdc.gov/brfss/index.html",
                                     "Data: CDC BRFSS 2023",
                                     target = "_blank"))
                      ),
                      p(class="page-blurb",
                        "Adult obesity rates vary dramatically by state and region. The South consistently records the highest rates, while the Northeast and West tend to fare better. The ANOVA analysis below confirms these regional differences are statistically significant, not a product of chance — region alone explains a meaningful share of the variation in county-level obesity rates across all 3,143 U.S. counties."),
                      
                      # Row 2: State obesity ranked + ANOVA boxplot
                      fluidRow(
                        column(7,
                               div(class="chart-box",
                                   div(class="chart-box-title","Adult Obesity Rate by State"),
                                   div(class="chart-box-sub","CDC BRFSS 2023 · % adults with obesity · All 50 states + DC"),
                                   plotlyOutput("obStateChart", height="640px")
                               )
                        ),
                        column(5,
                               div(class="chart-box",
                                   div(class="chart-box-title","Obesity Rate by Census Region"),
                                   div(class="chart-box-sub","One-Way ANOVA · County-level data (n = 3,143) · USDA Food Environment Atlas 2025"),
                                   plotlyOutput("anovaBoxplot", height="400px")
                               ),
                               uiOutput("anovaResultStrip")
                        )
                               )
                        ),
             
             
             tabPanel("Poverty",
                      div(class="section-header",
                          div(div(class="section-label","Economic Context"),
                              div(class="section-title","Poverty Rates by State")),
                          div(class="section-meta",
                              tags$a(href = "https://www.census.gov/data-tools/demo/saipe/#/?map_yearSelector=2023&x_tableYears=2024,2023",
                              "USA Census Bureau: Small Area Income and Poverty Estimates (SAIPE)",
                              target = "_blank") )
                          ),
                      p(class="page-blurb",
                        "Poverty and poor health outcomes tend to have correlation in America. States with the highest poverty rates (Louisiana, Mississippi, New Mexico, and West Virginia) also consistently rank among the highest for obesity. The scatter plot to the right makes this relationship visible: as poverty rates rise, obesity rates tend to rise with them. This is no coincidence. In high-poverty areas, fast food is often the most affordable and accessible source of calories, fresh grocery options are scarce, and the chronic stress of financial insecurity compounds poor health outcomes."),
                      fluidRow(
                        column(6, div(class="chart-box", div(class="chart-box-title","Highest Poverty Rates — Top 15 States"), div(class="chart-box-sub","% of all people in poverty · 2023"), plotlyOutput("povChart", height="440px"))),
                        column(6, div(class="chart-box", div(class="chart-box-title","Poverty vs. Obesity Correlation"), div(class="chart-box-sub","Each dot = one state. Hover for details."), plotlyOutput("scatterChart", height="440px")))
                      )
             )
             ,
             
             tabPanel("Demographics",
                      div(class="section-header",
                          div(div(class="section-label","Demographics · Food Environment Atlas 2025"),
                              div(class="section-title","Food Insecurity, Income & Fast Food Density by Race")),
                          div(class="section-meta",tags$a(href = "https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads",
                                                          "USDA: Food Environment Atlas",
                                                          target = "_blank"))
                      ),
                      p(class="page-blurb",
                        "Research consistently links fast food density to higher rates of obesity and diet-related illness, particularly in lower-income and minority communities where access to fresh, affordable food is limited. At the same time, poverty shapes food choices in complex ways: fast food is often the most accessible and affordable option available."),
                      div(class="bubble-controls",
                          div(
                            tags$label("Race Group", class="control-label"),
                            selectInput("raceGroup", label=NULL,
                                        choices=c("% White"="white", "% Black"="black", "% Hispanic"="hispanic"),
                                        selected="white", width="200px")
                          ),
                          div(class="bubble-legend",
                              tags$span(style="font-family:var(--mono);font-size:.7rem;color:var(--brown);",
                                        HTML(""))
                          )
                      ),
                      div(class="chart-box",
                          div(class="chart-box-title", "Food Insecurity vs. Median Household Income — State Level"),
                          plotlyOutput("demoBubbleChart", height="620px")
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
                                                   selected="obesity",
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
                                   actionButton("toggleAllChains","Select All", class="btn-filter-toggle"),
                                   uiOutput("chainFilterUI")
                               )
                        )
                      )
             ),
             
             tabPanel("Compare",
                      div(class="section-header",
                          div(div(class="section-label","Geographic Comparison · USDA Food Environment Atlas 2025"),
                              div(class="section-title","Compare Any Two Variables, County by County")),
                          div(class="section-meta","3,153 U.S. counties · Obesity, Diabetes, Poverty, Food Security & more")
                      ),
                      div(class="compare-pickers",
                          div(class="compare-picker-box",
                              span(class="compare-map-label left","Left Map — Variable A"),
                              selectInput("compareVarA", NULL, width="100%",
                                          choices=list(
                                            "Health"        = c("Obesity Rate by County % (2023)"="ObesityRate2022","Diabetes Rate % (2019)"="DiabetesRate2019","Physically Active HS % (2021)"="PctHSPhysActive2021"),
                                            "Food Security" = c("Food Insecurity % (2021-23)"="FoodInsecPct2123","Very Low Food Security % (2021-23)"="VeryLowFoodSecPct2123","Low Food Access % (2019)"="PctLowAccess2019","Low Income + Low Access %"="PctLowIncLowAccess2019","Children Low Access %"="PctChildLowAccess2019"),
                                            "Economics"     = c("Poverty Rate % (2021)"="PovRate2021","Child Poverty Rate % (2021)"="ChildPovRate2021","Deep Poverty Rate % (2021)"="DeepPovRate2021","Median HH Income $ (2021)"="MedianHHInc2021","SNAP Participation % (2022)"="SNAPPct2022","School Lunch % (2021)"="SchoolLunchPct2021"),
                                            "Food Env."     = c("Fast Food per 1k (2020)"="FFRPer1k2020","Grocery Stores per 1k (2020)"="GrocPer1k2020","Dollar Stores per 1k (2020)"="DollarPer1k2020","Conv Stores per 1k (2020)"="ConvPer1k2020","Rec Facilities per 1k (2020)"="RecFacPer1k2020","Farmers Markets per 1k (2018)"="FarmMktPer1k2018"),
                                            "Demographics"  = c("% White (2020)"="PctWhite2020","% Black (2020)"="PctBlack2020","% Hispanic (2020)"="PctHisp2020","% Age 65+ (2020)"="Pct65Plus2020","% Under 18 (2020)"="PctUnder182020")
                                          ), selected="ObesityRate2022")
                          ),
                          div(class="compare-vs","vs"),
                          div(class="compare-picker-box",
                              span(class="compare-map-label right","Right Map — Variable B"),
                              selectInput("compareVarB", NULL, width="100%",
                                          choices=list(
                                            "Health"        = c("Obesity Rate by County % (2023)"="ObesityRate2022","Diabetes Rate % (2019)"="DiabetesRate2019","Physically Active HS % (2021)"="PctHSPhysActive2021"),
                                            "Food Security" = c("Food Insecurity % (2021-23)"="FoodInsecPct2123","Very Low Food Security % (2021-23)"="VeryLowFoodSecPct2123","Low Food Access % (2019)"="PctLowAccess2019","Low Income + Low Access %"="PctLowIncLowAccess2019","Children Low Access %"="PctChildLowAccess2019"),
                                            "Economics"     = c("Poverty Rate % (2021)"="PovRate2021","Child Poverty Rate % (2021)"="ChildPovRate2021","Deep Poverty Rate % (2021)"="DeepPovRate2021","Median HH Income $ (2021)"="MedianHHInc2021","SNAP Participation % (2022)"="SNAPPct2022","School Lunch % (2021)"="SchoolLunchPct2021"),
                                            "Food Env."     = c("Fast Food per 1k (2020)"="FFRPer1k2020","Grocery Stores per 1k (2020)"="GrocPer1k2020","Dollar Stores per 1k (2020)"="DollarPer1k2020","Conv Stores per 1k (2020)"="ConvPer1k2020","Rec Facilities per 1k (2020)"="RecFacPer1k2020","Farmers Markets per 1k (2018)"="FarmMktPer1k2018"),
                                            "Demographics"  = c("% White (2020)"="PctWhite2020","% Black (2020)"="PctBlack2020","% Hispanic (2020)"="PctHisp2020","% Age 65+ (2020)"="Pct65Plus2020","% Under 18 (2020)"="PctUnder182020")
                                          ), selected="PovRate2021")
                          )
                      ),
                      div(class="compare-maps-wrap",
                          div(class="compare-map-panel",
                              span(class="compare-map-label left", textOutput("compareLabelA", inline=TRUE)),
                              br(), leafletOutput("compareMapA", height="420px"), br(),
                              uiOutput("compareLegendA")
                          ),
                          div(class="compare-map-panel",
                              span(class="compare-map-label right", textOutput("compareLabelB", inline=TRUE)),
                              br(), leafletOutput("compareMapB", height="420px"), br(),
                              uiOutput("compareLegendB")
                          )
                      )
             ),
             
             tabPanel("Meet the Team", meetTeamUI())
  ),
  
  div(class="dash-footer",
      div(HTML("<strong>Fast Food & Health in America</strong> — Interactive Shiny Dashboard")),
      div("CDC · USDA ERS · FDA · US Census · QSR Magazine · KFF · BRFSS · Datafiniti")
  )
)
  