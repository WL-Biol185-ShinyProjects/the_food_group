`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && length(a) > 0) a else b

library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)

# ── DATA DIRECTORY ───────────────────────────────────────────
DATA_DIR <- "."  # change if CSVs are elsewhere

read_csv_safe <- function(fname, skip=0, ...) {
  path <- file.path(DATA_DIR, fname)
  if (!file.exists(path)) { message("Missing: ", fname); return(NULL) }
  read.csv(path, stringsAsFactors=FALSE, skip=skip, ...)
}

# ── LOAD ALL DATA ────────────────────────────────────────────

# Top 50 chains
chains_raw <- read_csv_safe("Top 50 Fast-Food Chains in USA.csv")
if (!is.null(chains_raw)) {
  names(chains_raw) <- c("name","sales","avg_unit","franchised","company","units","change")
  chains_df <- chains_raw %>% arrange(desc(sales))
} else chains_df <- NULL

# Nutrition items
nutr_raw <- read_csv_safe("fastfood.csv")
if (!is.null(nutr_raw)) {
  nutr_df <- nutr_raw
  nutr_summary <- nutr_df %>%
    group_by(restaurant) %>%
    summarise(calories=round(mean(calories,na.rm=TRUE),1),
              sodium=round(mean(sodium,na.rm=TRUE),1),
              total_fat=round(mean(total_fat,na.rm=TRUE),1),
              protein=round(mean(protein,na.rm=TRUE),1),
              sugar=round(mean(sugar,na.rm=TRUE),1), .groups="drop") %>%
    arrange(desc(calories))
} else { nutr_df <- NULL; nutr_summary <- NULL }

# Obesity by state
ob_raw <- read_csv_safe("National Obesity by State.csv")
if (!is.null(ob_raw)) {
  ob_df <- ob_raw %>%
    select(state=NAME, obesity=Obesity) %>%
    filter(!is.na(obesity)) %>%
    arrange(desc(obesity))
} else ob_df <- NULL

# Poverty by state
pov_raw <- read_csv_safe("Poverty2023.csv", skip=4)
if (!is.null(pov_raw)) {
  pov_df <- pov_raw[, c(1,2,3,11)]
  names(pov_df) <- c("fips","abbr","state","pct_poverty")
  pov_df <- pov_df %>%
    filter(grepl("000$", as.character(fips)), fips != "00000",
           !is.na(pct_poverty), pct_poverty != "") %>%
    mutate(pct_poverty = as.numeric(gsub(",","", pct_poverty))) %>%
    filter(!is.na(pct_poverty)) %>%
    arrange(desc(pct_poverty))
} else pov_df <- NULL

# Race / ethnicity
race_raw <- read_csv_safe("raw_data.csv", skip=2)
if (!is.null(race_raw)) {
  names(race_raw)[1:5] <- c("state","white","black","hispanic","asian")
  race_df <- race_raw %>%
    select(state, white, black, hispanic, asian) %>%
    filter(!is.na(state), state != "", state != "Location") %>%
    mutate(across(c(white,black,hispanic,asian), suppressWarnings(as.numeric))) %>%
    filter(!is.na(white))
} else race_df <- NULL

# Map locations
locs_raw <- read_csv_safe("FastFoodRestaurants.csv")
if (is.null(locs_raw)) locs_raw <- read_csv_safe("Fast_Food_Restaurants_US.csv")
if (!is.null(locs_raw)) {
  names(locs_raw) <- tolower(names(locs_raw))
  map_df <- locs_raw %>%
    filter(!is.na(latitude), !is.na(longitude),
           latitude != 0, longitude != 0,
           latitude > 24, latitude < 50,
           longitude > -130, longitude < -60) %>%
    mutate(chain = case_when(
      grepl("mcdonald",        name, ignore.case=TRUE) ~ "McDonald's",
      grepl("burger king",     name, ignore.case=TRUE) ~ "Burger King",
      grepl("wendy",           name, ignore.case=TRUE) ~ "Wendy's",
      grepl("taco bell",       name, ignore.case=TRUE) ~ "Taco Bell",
      grepl("subway",          name, ignore.case=TRUE) ~ "Subway",
      grepl("chick.fil",       name, ignore.case=TRUE) ~ "Chick-fil-A",
      grepl("popeye",          name, ignore.case=TRUE) ~ "Popeyes",
      grepl("\\bkfc\\b",       name, ignore.case=TRUE) ~ "KFC",
      grepl("domino",          name, ignore.case=TRUE) ~ "Domino's",
      grepl("pizza hut",       name, ignore.case=TRUE) ~ "Pizza Hut",
      grepl("chipotle",        name, ignore.case=TRUE) ~ "Chipotle",
      grepl("sonic",           name, ignore.case=TRUE) ~ "Sonic",
      grepl("arby",            name, ignore.case=TRUE) ~ "Arby's",
      grepl("five guys",       name, ignore.case=TRUE) ~ "Five Guys",
      grepl("hardee",          name, ignore.case=TRUE) ~ "Hardee's",
      grepl("jack in the box", name, ignore.case=TRUE) ~ "Jack in the Box",
      grepl("dairy queen|\\bdq\\b", name, ignore.case=TRUE) ~ "Dairy Queen",
      grepl("whataburger",     name, ignore.case=TRUE) ~ "Whataburger",
      grepl("panda",           name, ignore.case=TRUE) ~ "Panda Express",
      TRUE ~ "Other"
    ))
} else map_df <- NULL

# County-level obesity (Obesity_Prevalence__2023.csv)
# geoId format: "us-al-001" -> FIPS "01001"
ob_county_raw <- read_csv_safe("Obesity_Prevalence__2023.csv")
if (!is.null(ob_county_raw)) {
  # Correct USPS → FIPS state code lookup (non-sequential, must be exact)
  STATE_FIPS_LOOKUP <- c(
    AL="01",AK="02",AZ="04",AR="05",CA="06",CO="08",CT="09",DE="10",
    DC="11",FL="12",GA="13",HI="15",ID="16",IL="17",IN="18",IA="19",
    KS="20",KY="21",LA="22",ME="23",MD="24",MA="25",MI="26",MN="27",
    MS="28",MO="29",MT="30",NE="31",NV="32",NH="33",NJ="34",NM="35",
    NY="36",NC="37",ND="38",OH="39",OK="40",OR="41",PA="42",RI="44",
    SC="45",SD="46",TN="47",TX="48",UT="49",VT="50",VA="51",WA="53",
    WV="54",WI="55",WY="56"
  )
  ob_county_df <- ob_county_raw %>%
    filter(!is.na(value), !is.na(geoId)) %>%
    mutate(
      geo_clean   = sub("^us-", "", geoId),         # "al-001"
      state_ab    = toupper(sub("-.*", "", geo_clean)), # "AL"
      county_part = sub(".*-", "", geo_clean),       # "001"
      state_fips  = STATE_FIPS_LOOKUP[state_ab],
      FIPS        = paste0(state_fips,
                           formatC(as.integer(county_part), width=3, flag="0")),
      obesity_pct = round(value * 100, 1)
    ) %>%
    filter(!is.na(state_fips), !is.na(obesity_pct)) %>%
    select(FIPS, obesity_pct)
} else ob_county_df <- NULL

# County-level poverty (Poverty2023.csv, skip=4)
pov_county_raw <- read_csv_safe("Poverty2023.csv", skip=4)
if (!is.null(pov_county_raw)) {
  names(pov_county_raw)[1:11] <- c("fips","abbr","area_name","rucc13","uic13","rucc23","uic24",
                                    "povall","ci90lb","ci90ub","pct_poverty")
  pov_county_df <- pov_county_raw %>%
    mutate(fips = formatC(as.integer(gsub(",","",as.character(fips))), width=5, flag="0")) %>%
    filter(nchar(fips)==5, !grepl("000$", fips), fips != "00000") %>%
    mutate(pct_poverty = suppressWarnings(as.numeric(gsub(",","", as.character(pct_poverty))))) %>%
    filter(!is.na(pct_poverty)) %>%
    select(FIPS=fips, pct_poverty)
} else pov_county_df <- NULL

# ── CONSTANTS ────────────────────────────────────────────────
CHAIN_COLORS <- c(
  "McDonald's"="#FFC72C","Burger King"="#FF7F00","Wendy's"="#E2231A",
  "Taco Bell"="#702082","Subway"="#008C15","Chick-fil-A"="#DD0031",
  "Popeyes"="#F47920","KFC"="#F40027","Domino's"="#006491","Pizza Hut"="#EE3224",
  "Chipotle"="#441700","Sonic"="#FFCC00","Arby's"="#CC2529","Five Guys"="#CE1126",
  "Hardee's"="#D62300","Jack in the Box"="#EF7B10","Dairy Queen"="#CC2529",
  "Whataburger"="#F77F00","Panda Express"="#CC0000","Other"="#888888"
)

TOP_CHAINS <- c("McDonald's","Burger King","Wendy's","Taco Bell","Subway",
                "Chick-fil-A","KFC","Domino's","Pizza Hut","Sonic","Arby's","Dairy Queen")

ob_color  <- function(v) ifelse(v>=34,"#c0392b",ifelse(v>=30,"#e67e22",ifelse(v>=26,"#f39c12","#27ae60")))
pov_color <- function(v) ifelse(v>=14,"#c0392b",ifelse(v>=11,"#e67e22",ifelse(v>=9,"#f39c12","#2980b9")))

# State name → abbreviation lookup
STATE_ABBR <- c(
  Alabama="AL",Alaska="AK",Arizona="AZ",Arkansas="AR",California="CA",
  Colorado="CO",Connecticut="CT",Delaware="DE","District of Columbia"="DC",
  Florida="FL",Georgia="GA",Hawaii="HI",Idaho="ID",Illinois="IL",Indiana="IN",
  Iowa="IA",Kansas="KS",Kentucky="KY",Louisiana="LA",Maine="ME",Maryland="MD",
  Massachusetts="MA",Michigan="MI",Minnesota="MN",Mississippi="MS",Missouri="MO",
  Montana="MT",Nebraska="NE",Nevada="NV","New Hampshire"="NH","New Jersey"="NJ",
  "New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND",
  Ohio="OH",Oklahoma="OK",Oregon="OR",Pennsylvania="PA","Rhode Island"="RI",
  "South Carolina"="SC","South Dakota"="SD",Tennessee="TN",Texas="TX",
  Utah="UT",Vermont="VT",Virginia="VA",Washington="WA","West Virginia"="WV",
  Wisconsin="WI",Wyoming="WY"
)

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── CHAINS ─────────────────────────────────────────────────
  output$salesChart <- renderPlotly({
    req(chains_df)
    df <- head(chains_df, 20) %>% arrange(sales)
    plot_ly(df, x=~sales, y=~reorder(name, sales), type="bar", orientation="h",
            marker=list(color=colorRampPalette(c("#d4380d","#e67e22","#f39c12"))(20)),
            hovertemplate="%{y}: $%{x}M<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Sales (Millions USD)", gridcolor="#ede8df", tickfont=list(size=12), fixedrange=TRUE),
             yaxis=list(title="", tickfont=list(size=12), fixedrange=TRUE),
             margin=list(l=130,r=30,t=20,b=50),
             font=list(family="DM Sans", size=13)) %>%
      config(displayModeBar=FALSE)
  })

  output$chainScatterChart <- renderPlotly({
    req(chains_df)
    plot_ly(chains_df, x=~avg_unit, y=~sales, text=~name,
            type="scatter", mode="markers",
            marker=list(size=~(8 + 24 * (units - min(units, na.rm=TRUE)) / (max(units, na.rm=TRUE) - min(units, na.rm=TRUE))),
                        color=colorRampPalette(c("#d4380d","#e67e22","#f39c12"))(nrow(chains_df)),
                        opacity=0.8, line=list(color="white", width=1)),
            hovertemplate="<b>%{text}</b><br>Avg/Unit: $%{x}K<br>Total Sales: $%{y}M<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Avg Sales per Unit ($K)", gridcolor="#ede8df", tickfont=list(size=12), fixedrange=TRUE),
             yaxis=list(title="Systemwide Sales ($M)", gridcolor="#ede8df", tickfont=list(size=12), fixedrange=TRUE),
             margin=list(l=70,r=30,t=20,b=60),
             font=list(family="DM Sans", size=13)) %>%
      config(displayModeBar=FALSE)
  })

  # ── NUTRITION ───────────────────────────────────────────────
  output$nutrCards <- renderUI({
    req(nutr_summary)
    cards <- lapply(seq_len(nrow(nutr_summary)), function(i) {
      r <- nutr_summary[i,]
      div(class="nutr-card",
        div(class="nutr-card-name", r$restaurant),
        div(class="nutr-metric", span(class="nutr-metric-label","Calories"), span(class="nutr-metric-val",paste0(r$calories," kcal"))),
        div(class="nutr-metric", span(class="nutr-metric-label","Sodium"),   span(class="nutr-metric-val",paste0(r$sodium," mg"))),
        div(class="nutr-metric", span(class="nutr-metric-label","Fat"),      span(class="nutr-metric-val",paste0(r$total_fat,"g"))),
        div(class="nutr-metric", span(class="nutr-metric-label","Protein"),  span(class="nutr-metric-val",paste0(r$protein,"g"))),
        div(class="nutr-metric", span(class="nutr-metric-label","Sugar"),    span(class="nutr-metric-val",paste0(r$sugar,"g")))
      )
    })
    div(class="nutr-grid", tagList(cards))
  })

  nutr_bar <- function(df, col, color_fn, fmt) {
    d <- df %>% arrange(desc(.data[[col]]))
    plot_ly(d, x=d[[col]], y=~reorder(restaurant, d[[col]]),
            type="bar", orientation="h",
            marker=list(color=sapply(d[[col]], color_fn)),
            hovertemplate=paste0("%{y}: %{x}", fmt, "<extra></extra>")) %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(gridcolor="#ede8df", tickfont=list(size=12), fixedrange=TRUE),
             yaxis=list(title="", tickfont=list(size=12), fixedrange=TRUE),
             margin=list(l=120,r=20,t=10,b=30),
             font=list(family="DM Sans", size=13)) %>%
      config(displayModeBar=FALSE)
  }

  output$calChart  <- renderPlotly({ req(nutr_summary); nutr_bar(nutr_summary,"calories",  function(v) ifelse(v>600,"#c0392b",ifelse(v>480,"#e67e22","#27ae60")), " kcal") })
  output$sodChart  <- renderPlotly({ req(nutr_summary); nutr_bar(nutr_summary,"sodium",    function(v) ifelse(v>1400,"#c0392b",ifelse(v>1100,"#e67e22","#16a085")), " mg") })
  output$protChart <- renderPlotly({ req(nutr_summary); nutr_bar(nutr_summary,"protein",   function(v) "#27ae60", "g") })
  output$fatChart  <- renderPlotly({ req(nutr_summary); nutr_bar(nutr_summary,"total_fat", function(v) ifelse(v>30,"#c0392b",ifelse(v>20,"#e67e22","#27ae60")), "g") })


  # ── OBESITY ─────────────────────────────────────────────────
  output$obHighChart <- renderPlotly({
    req(ob_df)
    d <- head(ob_df,15) %>% arrange(obesity)
    plot_ly(d, x=~obesity, y=~reorder(state,obesity), type="bar", orientation="h",
            marker=list(color=ob_color(d$obesity)),
            hovertemplate="%{y}: %{x}%<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Obesity Rate (%)", gridcolor="#ede8df", range=c(0,40), tickfont=list(size=12), fixedrange=TRUE),
             yaxis=list(title="", tickfont=list(size=12), fixedrange=TRUE),
             margin=list(l=160,r=20,t=10,b=50),
             font=list(family="DM Sans", size=13)) %>%
      config(displayModeBar=FALSE)
  })

  output$obLowChart <- renderPlotly({
    req(ob_df)
    d <- tail(ob_df,10) %>% arrange(obesity)
    plot_ly(d, x=~obesity, y=~reorder(state,obesity), type="bar", orientation="h",
            marker=list(color=ob_color(d$obesity)),
            hovertemplate="%{y}: %{x}%<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Obesity Rate (%)", gridcolor="#ede8df", range=c(0,40), tickfont=list(size=12), fixedrange=TRUE),
             yaxis=list(title="", tickfont=list(size=12), fixedrange=TRUE),
             margin=list(l=160,r=20,t=10,b=50),
             font=list(family="DM Sans", size=13)) %>%
      config(displayModeBar=FALSE)
  })

  output$obTable <- renderDT({
    req(ob_df)
    ob_df %>% arrange(desc(obesity)) %>%
      mutate(Rank=row_number(), `Obesity Rate`=paste0(obesity,"%")) %>%
      select(Rank, State=state, `Obesity Rate`) %>%
      datatable(rownames=FALSE, options=list(pageLength=25), class="stripe hover")
  })

  # ── POVERTY ─────────────────────────────────────────────────
  output$povChart <- renderPlotly({
    req(pov_df)
    d <- head(pov_df,15) %>% arrange(pct_poverty)
    plot_ly(d, x=~pct_poverty, y=~reorder(state,pct_poverty), type="bar", orientation="h",
            marker=list(color=pov_color(d$pct_poverty)),
            hovertemplate="%{y}: %{x}%<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Poverty Rate (%)", gridcolor="#ede8df", tickfont=list(size=12), fixedrange=TRUE),
             yaxis=list(title="", tickfont=list(size=12), fixedrange=TRUE),
             margin=list(l=160,r=20,t=10,b=50),
             font=list(family="DM Sans", size=13)) %>%
      config(displayModeBar=FALSE)
  })

  output$scatterChart <- renderPlotly({
    req(ob_df, pov_df)
    merged <- inner_join(ob_df %>% select(state,obesity),
                         pov_df %>% select(state,pct_poverty), by="state")
    plot_ly(merged, x=~pct_poverty, y=~obesity, text=~state,
            type="scatter", mode="markers",
            marker=list(size=11, color=ob_color(merged$obesity), opacity=0.85,
                        line=list(color="white",width=1.5)),
            hovertemplate="<b>%{text}</b><br>Poverty: %{x}%<br>Obesity: %{y}%<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Poverty Rate (%)", gridcolor="#ede8df", tickfont=list(size=12)),
             yaxis=list(title="Obesity Rate (%)", gridcolor="#ede8df", tickfont=list(size=12)),
             margin=list(l=60,r=20,t=20,b=60),
             font=list(family="DM Sans", size=13))
  })

  # ── DEMOGRAPHICS ────────────────────────────────────────────
  output$demoFFChart <- renderPlotly({
    req(race_df, map_df)

    # State population (most recent year available)
    pop_raw <- read_csv_safe("historical_state_population_by_year.csv")
    pop_df <- NULL
    if (!is.null(pop_raw)) {
      names(pop_raw) <- c("abbr","year","population")
      pop_df <- pop_raw %>%
        filter(!is.na(population)) %>%
        group_by(abbr) %>%
        filter(year == max(year)) %>%
        ungroup() %>%
        select(abbr, population)
    }

    # Fast food counts per state abbreviation
    ff_counts <- map_df %>%
      count(province, name="ff_count") %>%
      rename(abbr = province)

    # Race data: determine majority group per state
    STATE_ABBR_REV <- setNames(names(STATE_ABBR), STATE_ABBR)
    race_clean <- race_df %>%
      filter(state != "United States", !is.na(white)) %>%
      mutate(
        abbr = STATE_ABBR[state],
        majority = case_when(
          white    == pmax(white, black, hispanic, asian, na.rm=TRUE) ~ "White",
          hispanic == pmax(white, black, hispanic, asian, na.rm=TRUE) ~ "Hispanic",
          black    == pmax(white, black, hispanic, asian, na.rm=TRUE) ~ "Black",
          asian    == pmax(white, black, hispanic, asian, na.rm=TRUE) ~ "Asian",
          TRUE ~ "Other"
        ),
        white_pct    = round(white    * 100, 1),
        black_pct    = round(black    * 100, 1),
        hispanic_pct = round(hispanic * 100, 1),
        asian_pct    = round(asian    * 100, 1)
      ) %>%
      filter(!is.na(abbr))

    # Merge everything
    merged <- race_clean %>%
      inner_join(ff_counts, by="abbr")

    if (!is.null(pop_df)) {
      merged <- merged %>% left_join(pop_df, by="abbr") %>%
        mutate(ff_per_100k = round(ff_count / population * 100000, 1))
    } else {
      merged <- merged %>% mutate(ff_per_100k = ff_count, population = NA)
    }

    merged <- merged %>%
      filter(!is.na(ff_per_100k)) %>%
      arrange(desc(ff_per_100k))

    maj_colors <- c(
      "White"    = "#4e9af1",
      "Hispanic" = "#54c066",
      "Black"    = "#e87040",
      "Asian"    = "#f0c040",
      "Other"    = "#aaaaaa"
    )

    merged$color <- maj_colors[merged$majority]

    hover_txt <- paste0(
      "<b>", merged$state, " (", merged$abbr, ")</b><br>",
      "FF per 100k: <b>", merged$ff_per_100k, "</b><br>",
      "Majority group: <b>", merged$majority, "</b><br>",
      "White: ", merged$white_pct, "% · ",
      "Black: ", merged$black_pct, "% · ",
      "Hispanic: ", merged$hispanic_pct, "% · ",
      "Asian: ", merged$asian_pct, "%"
    )

    # One trace per majority group so legend works
    groups <- unique(merged$majority)
    p <- plot_ly()
    for (grp in groups) {
      d <- merged %>% filter(majority == grp)
      p <- p %>% add_trace(
        data        = d,
        x           = ~reorder(abbr, -ff_per_100k),
        y           = ~ff_per_100k,
        type        = "bar",
        name        = grp,
        marker      = list(color = maj_colors[grp]),
        hovertext   = hover_txt[merged$majority == grp],
        hoverinfo   = "text"
      )
    }

    p %>% layout(
      barmode      = "overlay",
      paper_bgcolor = "#faf7f2",
      plot_bgcolor  = "white",
      xaxis = list(
        title       = "State",
        tickfont    = list(size=11),
        fixedrange  = TRUE,
        categoryorder = "array",
        categoryarray = merged$abbr
      ),
      yaxis = list(
        title      = "Fast Food Locations per 100k People",
        gridcolor  = "#ede8df",
        tickfont   = list(size=12),
        fixedrange = TRUE
      ),
      legend = list(
        title       = list(text="Majority Group"),
        orientation = "h",
        y           = 1.06,
        font        = list(size=13)
      ),
      margin = list(l=70, r=20, t=40, b=80),
      font   = list(family="DM Sans", size=13)
    ) %>%
    config(displayModeBar=FALSE)
  })

  # ── MAP ─────────────────────────────────────────────────────

  # Gradient color helpers — interpolate a value within a palette
  gradient_color <- function(v, lo, hi, palette_colors, alpha=0.72) {
    if (is.na(v)) return("rgba(200,200,200,0.20)")
    t <- max(0, min(1, (v - lo) / (hi - lo)))
    n <- length(palette_colors)
    idx_f <- t * (n - 1) + 1
    idx_lo <- floor(idx_f);  idx_hi <- ceiling(idx_f)
    frac <- idx_f - idx_lo
    col_lo <- col2rgb(palette_colors[idx_lo])
    col_hi <- col2rgb(palette_colors[idx_hi])
    r <- round(col_lo[1] + frac * (col_hi[1] - col_lo[1]))
    g <- round(col_lo[2] + frac * (col_hi[2] - col_lo[2]))
    b <- round(col_lo[3] + frac * (col_hi[3] - col_lo[3]))
    sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
  }

  OB_PALETTE  <- c("#2ecc71","#f1c40f","#e67e22","#c0392b","#7b0000")
  POV_PALETTE <- c("#aed6f1","#2980b9","#1a5276","#7d3c98","#4a235a")

  active_chains <- reactiveVal(TOP_CHAINS)

  output$chainFilterUI <- renderUI({
    chains <- if (!is.null(map_df)) sort(unique(map_df$chain[map_df$chain != "Other"])) else TOP_CHAINS
    checkboxGroupInput("chainFilter", "", choices=chains, selected=chains)
  })

  observe({ req(input$chainFilter); active_chains(input$chainFilter) })

  map_filtered <- reactive({
    req(map_df, active_chains())
    map_df %>% filter(chain %in% active_chains())
  })

  output$mapCounter <- renderText({
    paste0(formatC(nrow(map_filtered()), format="d", big.mark=","), " locations shown")
  })

  # Gradient legend UI
  output$overlayLegend <- renderUI({
    ov <- input$mapOverlay
    if (is.null(ov) || ov == "none") return(NULL)

    if (ov == "obesity") {
      pal    <- OB_PALETTE
      lo_lbl <- "~20%"
      hi_lbl <- "~40%+"
      ttl    <- "Adult Obesity Rate (by County)"
      grad   <- paste0("linear-gradient(to right, ",
                        paste(pal, collapse=", "), ")")
    } else {
      pal    <- POV_PALETTE
      lo_lbl <- "Low"
      hi_lbl <- "High"
      ttl    <- "Poverty Rate (by County)"
      grad   <- paste0("linear-gradient(to right, ",
                        paste(pal, collapse=", "), ")")
    }

    div(class="overlay-legend",
      strong(style="font-size:.78rem;display:block;margin-bottom:.5rem;", ttl),
      div(class="gradient-legend",
        div(class="gradient-bar", style=paste0("background:", grad, ";")),
        div(class="gradient-labels",
          span(lo_lbl),
          span(hi_lbl)
        )
      ),
      span(style="font-size:.68rem;color:#8c7355;margin-top:.5rem;display:block;",
           "Hover a county for its value")
    )
  })

  # Load county GeoJSON once at startup
  county_sf <- tryCatch({
    if (!requireNamespace("sf", quietly=TRUE)) stop("sf not installed")
    sf::read_sf("https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json") %>%
      dplyr::rename(FIPS = id)
  }, error = function(e) {
    message("county sf load failed: ", e$message); NULL
  })

  # Render base map once
  output$ffMap <- renderLeaflet({
    leaflet(options=leafletOptions(preferCanvas=TRUE, zoomControl=TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron,    group="Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter,  group="Dark") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group="Street") %>%
      setView(lng=-96, lat=38, zoom=4) %>%
      addLayersControl(baseGroups=c("Light","Dark","Street"),
                       options=layersControlOptions(collapsed=TRUE)) %>%
      addScaleBar(position="bottomleft")
  })

  # Redraw everything when filters or overlay changes
  observeEvent(list(map_filtered(), input$mapOverlay), {
    df  <- map_filtered()
    ov  <- input$mapOverlay %||% "none"
    proxy <- leafletProxy("ffMap")
    proxy %>% clearMarkers() %>% clearShapes() %>% clearControls()

    # ── COUNTY CHOROPLETH OVERLAY ─────────────────────────────
    if (ov != "none" && !is.null(county_sf)) {

      if (ov == "obesity" && !is.null(ob_county_df)) {
        geo <- dplyr::left_join(county_sf, ob_county_df, by="FIPS")
        vals <- geo$obesity_pct
        lo <- floor(min(vals, na.rm=TRUE))
        hi <- ceiling(max(vals, na.rm=TRUE))
        fill_colors <- sapply(vals, gradient_color,
                              lo=lo, hi=hi, palette_colors=OB_PALETTE)
        label_txt <- paste0(
          ifelse(is.na(geo$NAME), "County", geo$NAME), ": ",
          ifelse(is.na(vals), "No data", paste0(vals, "% obesity"))
        )
      } else if (ov == "poverty" && !is.null(pov_county_df)) {
        geo <- dplyr::left_join(county_sf, pov_county_df, by="FIPS")
        vals <- geo$pct_poverty
        lo <- floor(min(vals, na.rm=TRUE))
        hi <- ceiling(max(vals, na.rm=TRUE))
        fill_colors <- sapply(vals, gradient_color,
                              lo=lo, hi=hi, palette_colors=POV_PALETTE)
        label_txt <- paste0(
          ifelse(is.na(geo$NAME), "County", geo$NAME), ": ",
          ifelse(is.na(vals), "No data", paste0(vals, "% poverty"))
        )
      } else {
        geo <- NULL
      }

      if (!is.null(geo)) {
        proxy %>% addPolygons(
          data             = geo,
          fillColor        = fill_colors,
          fillOpacity      = 0.70,
          color            = "#444444",
          weight           = 0.3,
          opacity          = 0.4,
          label            = label_txt,
          labelOptions     = labelOptions(
            style     = list("font-family"="Space Mono, monospace",
                             "font-size"="12px",
                             "background"="white",
                             "padding"="4px 8px"),
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            weight      = 1.5,
            color       = "#222",
            fillOpacity = 0.85,
            bringToFront = FALSE
          )
        )
      }
    }

    # ── CIRCLE MARKERS ────────────────────────────────────────
    cols <- unname(sapply(df$chain, function(ch) {
      v <- CHAIN_COLORS[ch]; if (is.na(v)) "#888888" else v
    }))

    proxy %>% addCircleMarkers(
      data        = df,
      lng         = ~longitude,
      lat         = ~latitude,
      radius      = 3,
      color       = cols,
      fillColor   = cols,
      fillOpacity = 0.82,
      weight      = 0.5,
      opacity     = 0.9,
      popup = ~paste0(
        "<div style='font-family:DM Sans,sans-serif;min-width:150px;'>",
        "<strong style='font-size:13px;'>", name, "</strong><br>",
        "<span style='color:#888;font-size:11px;'>", chain, "</span><br>",
        "<span style='font-size:11px;'>", city, ", ", province, "</span>",
        "</div>"
      ),
      label = ~name
    )
  })
}
