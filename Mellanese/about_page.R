# ─────────────────────────────────────────────────────────────────────────────
#  ABOUT PAGE  –  Fast Food in America
#
#  HOW TO INTEGRATE:
#  1. Copy the aboutTabUI() call into your navbarPage() / tabsetPanel() in ui.R
#     e.g.  tabPanel("About", aboutTabUI())
#
#  2. The about page is purely UI — no server-side code is needed.
#     Simply source this file at the top of your ui.R (or app.R):
#     source("about_page.R")
# ─────────────────────────────────────────────────────────────────────────────

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

      # Hero
      div(class = "about-hero",
        div(class = "about-hero-eyebrow", "Data Exploration Project"),
        tags$h1(HTML("Fast Food <span>in America</span>")),
        tags$p(class = "about-hero-sub",
          "An interactive look at the growth, impact, and consequences of the
           fast-food industry across the United States."
        )
      ),

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
