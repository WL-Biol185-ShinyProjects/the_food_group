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
  output$chainCards <- renderUI({
    req(chains_df)
    top20 <- head(chains_df, 20)
    cards <- lapply(seq_len(nrow(top20)), function(i) {
      r <- top20[i,]
      cls <- if (r$change >= 0) "chain-pos" else "chain-neg"
      sign <- if (r$change >= 0) "+" else ""
      div(class="chain-card",
        div(class="chain-rank", paste0("#", i)),
        div(class="chain-name", r$name),
        div(class="chain-sales", paste0("$", formatC(r$sales, format="d", big.mark=","), "M")),
        div(class="chain-sales-label","Systemwide Sales"),
        div(class="chain-units", paste0(formatC(r$units, format="d", big.mark=","), " total units")),
        div(class=paste("chain-change", cls), paste0(sign, r$change, " units vs 2020"))
      )
    })
    div(class="chain-grid", tagList(cards))
  })

  output$salesChart <- renderPlotly({
    req(chains_df)
    df <- head(chains_df, 20) %>% arrange(sales)
    plot_ly(df, x=~sales, y=~reorder(name, sales), type="bar", orientation="h",
            marker=list(color=colorRampPalette(c("#d4380d","#e67e22","#f39c12"))(20)),
            hovertemplate="%{y}: $%{x}M<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Sales (Millions USD)", gridcolor="#ede8df", tickfont=list(size=12)),
             yaxis=list(title="", tickfont=list(size=12)),
             margin=list(l=130,r=30,t=20,b=50),
             font=list(family="DM Sans", size=13))
  })

  output$chainsTable <- renderDT({
    req(chains_df)
    chains_df %>%
      mutate(sales=paste0("$",formatC(sales,format="d",big.mark=","),"M"),
             avg_unit=paste0("$",formatC(avg_unit,format="d",big.mark=","),"K"),
             units=formatC(units,format="d",big.mark=","),
             franchised=formatC(franchised,format="d",big.mark=","),
             company=formatC(company,format="d",big.mark=","),
             change=ifelse(change>=0,paste0("+",change),as.character(change))) %>%
      rename(Chain=name,Sales=sales,`Avg/Unit`=avg_unit,
             Franchised=franchised,Company=company,
             `Total Units`=units,`Change '20→'21`=change) %>%
      datatable(rownames=FALSE, options=list(pageLength=25,scrollX=TRUE), class="stripe hover")
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
             xaxis=list(gridcolor="#ede8df", tickfont=list(size=12)),
             yaxis=list(title="", tickfont=list(size=12)),
             margin=list(l=120,r=20,t=10,b=30),
             font=list(family="DM Sans", size=13))
  }

  output$calChart  <- renderPlotly({ req(nutr_summary); nutr_bar(nutr_summary,"calories",  function(v) ifelse(v>600,"#c0392b",ifelse(v>480,"#e67e22","#27ae60")), " kcal") })
  output$sodChart  <- renderPlotly({ req(nutr_summary); nutr_bar(nutr_summary,"sodium",    function(v) ifelse(v>1400,"#c0392b",ifelse(v>1100,"#e67e22","#16a085")), " mg") })
  output$protChart <- renderPlotly({ req(nutr_summary); nutr_bar(nutr_summary,"protein",   function(v) "#27ae60", "g") })
  output$fatChart  <- renderPlotly({ req(nutr_summary); nutr_bar(nutr_summary,"total_fat", function(v) ifelse(v>30,"#c0392b",ifelse(v>20,"#e67e22","#27ae60")), "g") })

  observe({
    req(nutr_df)
    updateSelectInput(session,"nutrRestaurant",
      choices=c("All"="", sort(unique(nutr_df$restaurant))), selected="")
  })

  nutr_filtered <- reactive({
    req(nutr_df)
    df <- nutr_df %>% filter(calories<=input$nutrCal, sodium<=input$nutrSod)
    if (!is.null(input$nutrRestaurant) && any(nchar(input$nutrRestaurant)>0))
      df <- df %>% filter(restaurant %in% input$nutrRestaurant)
    df
  })

  output$nutrTable <- renderDT({
    nutr_filtered() %>%
      select(Restaurant=restaurant, Item=item, Calories=calories,
             Sodium=sodium, Fat=total_fat, Protein=protein, Sugar=sugar) %>%
      datatable(rownames=FALSE, options=list(pageLength=15,scrollX=TRUE), class="stripe hover")
  })

  # ── OBESITY ─────────────────────────────────────────────────
  output$obHighChart <- renderPlotly({
    req(ob_df)
    d <- head(ob_df,15) %>% arrange(obesity)
    plot_ly(d, x=~obesity, y=~reorder(state,obesity), type="bar", orientation="h",
            marker=list(color=ob_color(d$obesity)),
            hovertemplate="%{y}: %{x}%<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Obesity Rate (%)", gridcolor="#ede8df", range=c(0,40), tickfont=list(size=12)),
             yaxis=list(title="", tickfont=list(size=12)),
             margin=list(l=160,r=20,t=10,b=50),
             font=list(family="DM Sans", size=13))
  })

  output$obLowChart <- renderPlotly({
    req(ob_df)
    d <- tail(ob_df,10) %>% arrange(obesity)
    plot_ly(d, x=~obesity, y=~reorder(state,obesity), type="bar", orientation="h",
            marker=list(color=ob_color(d$obesity)),
            hovertemplate="%{y}: %{x}%<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Obesity Rate (%)", gridcolor="#ede8df", range=c(0,40), tickfont=list(size=12)),
             yaxis=list(title="", tickfont=list(size=12)),
             margin=list(l=160,r=20,t=10,b=50),
             font=list(family="DM Sans", size=13))
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
             xaxis=list(title="Poverty Rate (%)", gridcolor="#ede8df", tickfont=list(size=12)),
             yaxis=list(title="", tickfont=list(size=12)),
             margin=list(l=160,r=20,t=10,b=50),
             font=list(family="DM Sans", size=13))
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
  output$donutChart <- renderPlotly({
    req(race_df)
    us <- race_df %>% filter(state=="United States")
    if (nrow(us)==0) us <- race_df[1,]
    vals   <- c(us$white, us$hispanic, us$black, us$asian,
                pmax(0, 1-us$white-us$hispanic-us$black-us$asian))
    labels <- c("White","Hispanic","Black","Asian","Other")
    cols   <- c("#4e9af1","#54c066","#e87040","#f0c040","#aaaaaa")
    plot_ly(labels=labels, values=round(as.numeric(vals)*100,1), type="pie",
            hole=0.45, marker=list(colors=cols, line=list(color="white",width=2)),
            textinfo="label+percent",
            hovertemplate="%{label}: %{value}%<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", showlegend=TRUE,
             legend=list(font=list(size=13)),
             margin=list(l=10,r=10,t=20,b=10),
             font=list(family="DM Sans", size=13))
  })

  output$ffStateChart <- renderPlotly({
    req(map_df)
    d <- map_df %>% count(province, sort=TRUE) %>% head(20) %>% arrange(n)
    plot_ly(d, x=~n, y=~reorder(province,n), type="bar", orientation="h",
            marker=list(color="#d4380d"),
            hovertemplate="%{y}: %{x} locations<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="# Locations", gridcolor="#ede8df", tickfont=list(size=12)),
             yaxis=list(title="", tickfont=list(size=12)),
             margin=list(l=50,r=20,t=10,b=40),
             font=list(family="DM Sans", size=13))
  })

  output$raceStackChart <- renderPlotly({
    req(race_df)
    d <- race_df %>% filter(state!="United States") %>% arrange(desc(white))
    plot_ly(d, y=~state, x=~round(white*100,1),    type="bar", orientation="h", name="White",    marker=list(color="#4e9af1")) %>%
      add_trace(x=~round(black*100,1),    name="Black",    marker=list(color="#e87040")) %>%
      add_trace(x=~round(hispanic*100,1), name="Hispanic", marker=list(color="#54c066")) %>%
      add_trace(x=~round(asian*100,1),    name="Asian",    marker=list(color="#f0c040")) %>%
      layout(barmode="stack",
             paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Population Share (%)", gridcolor="#ede8df", tickfont=list(size=12)),
             yaxis=list(title="", tickfont=list(size=11)),
             margin=list(l=140,r=20,t=40,b=50),
             legend=list(orientation="h", y=1.04, font=list(size=13)),
             font=list(family="DM Sans", size=13))
  })

  # ── MAP ─────────────────────────────────────────────────────

  # Obesity lookup — derived directly from National Obesity by State.csv
  OBESITY_DATA <- if (!is.null(ob_df)) setNames(ob_df$obesity, ob_df$state) else c()

  # Poverty lookup — derived directly from Poverty2023.csv
  POVERTY_DATA <- if (!is.null(pov_df)) setNames(pov_df$pct_poverty, pov_df$state) else c()

  obesity_fill <- function(v) {
    if (is.na(v)) return("rgba(200,200,200,0.25)")
    if (v >= 34) "rgba(192,57,43,0.35)"
    else if (v >= 32) "rgba(230,81,0,0.30)"
    else if (v >= 30) "rgba(230,126,34,0.28)"
    else if (v >= 27) "rgba(241,196,15,0.25)"
    else "rgba(39,174,96,0.22)"
  }

  poverty_fill <- function(v) {
    if (is.na(v)) return("rgba(200,200,200,0.25)")
    if (v >= 13) "rgba(142,68,173,0.35)"
    else if (v >= 11) "rgba(155,89,182,0.30)"
    else if (v >= 9)  "rgba(52,73,94,0.28)"
    else if (v >= 7)  "rgba(52,152,219,0.25)"
    else "rgba(22,160,133,0.22)"
  }

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

  # Overlay legend
  output$overlayLegend <- renderUI({
    ov <- input$mapOverlay
    if (is.null(ov) || ov == "none") return(NULL)
    if (ov == "obesity") {
      rows <- list(
        list(col="rgba(192,57,43,0.5)",  lbl="≥ 34%"),
        list(col="rgba(230,81,0,0.45)",  lbl="32 – 34%"),
        list(col="rgba(230,126,34,0.4)", lbl="30 – 32%"),
        list(col="rgba(241,196,15,0.4)", lbl="27 – 30%"),
        list(col="rgba(39,174,96,0.35)", lbl="< 27%")
      )
      ttl <- "Adult Obesity Rate"
    } else {
      rows <- list(
        list(col="rgba(142,68,173,0.5)",  lbl="≥ 13%"),
        list(col="rgba(155,89,182,0.45)", lbl="11 – 13%"),
        list(col="rgba(52,73,94,0.4)",    lbl="9 – 11%"),
        list(col="rgba(52,152,219,0.4)",  lbl="7 – 9%"),
        list(col="rgba(22,160,133,0.35)", lbl="< 7%")
      )
      ttl <- "Poverty Rate"
    }
    div(class="overlay-legend",
      strong(style="font-size:.78rem;display:block;margin-bottom:.4rem;", ttl),
      tagList(lapply(rows, function(r)
        div(class="legend-row",
          div(class="legend-swatch", style=paste0("background:",r$col,";")),
          span(r$lbl)
        )
      )),
      span(style="font-size:.68rem;color:#8c7355;margin-top:.5rem;display:block;",
           "Hover a state for its value")
    )
  })

  # Load state shapes once at startup using sf
  # sf is bundled with many R spatial installs; if missing: install.packages("sf")
  state_sf <- tryCatch({
    if (!requireNamespace("sf", quietly=TRUE)) stop("sf not installed")
    sf::read_sf("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json") %>%
      dplyr::select(name)
  }, error = function(e) {
    message("sf load failed: ", e$message)
    NULL
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
    df <- map_filtered()
    ov <- input$mapOverlay %||% "none"

    proxy <- leafletProxy("ffMap")
    proxy %>% clearMarkers() %>% clearShapes() %>% clearControls()

    # ── STATE CHOROPLETH OVERLAY ──────────────────────────────
    if (ov != "none" && !is.null(state_sf)) {

      # Join the right data column onto the sf object
      if (ov == "obesity") {
        ob_lookup <- data.frame(
          name  = names(OBESITY_DATA),
          value = as.numeric(OBESITY_DATA),
          stringsAsFactors = FALSE
        )
        geo <- dplyr::left_join(state_sf, ob_lookup, by="name")
        fill_colors <- sapply(geo$value, obesity_fill)
        label_txt   <- paste0(geo$name, ": ", ifelse(is.na(geo$value), "N/A", paste0(geo$value, "%")),
                              " obesity rate")
      } else {
        pov_lookup <- data.frame(
          name  = names(POVERTY_DATA),
          value = as.numeric(POVERTY_DATA),
          stringsAsFactors = FALSE
        )
        geo <- dplyr::left_join(state_sf, pov_lookup, by="name")
        fill_colors <- sapply(geo$value, poverty_fill)
        label_txt   <- paste0(geo$name, ": ", ifelse(is.na(geo$value), "N/A", paste0(geo$value, "%")),
                              " poverty rate")
      }

      proxy %>% addPolygons(
        data        = geo,
        fillColor   = fill_colors,
        fillOpacity = 0.65,
        color       = "#555555",
        weight      = 0.8,
        opacity     = 0.6,
        label       = label_txt,
        labelOptions = labelOptions(
          style    = list("font-family"="Space Mono, monospace", "font-size"="12px",
                          "background"="white", "padding"="4px 8px"),
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#333",
          fillOpacity = 0.8,
          bringToFront = FALSE   # keep markers on top
        )
      )
    }

    # ── INDIVIDUAL CIRCLE MARKERS (no clustering) ─────────────
    cols <- unname(sapply(df$chain, function(ch) {
      v <- CHAIN_COLORS[ch]
      if (is.na(v)) "#888888" else v
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
