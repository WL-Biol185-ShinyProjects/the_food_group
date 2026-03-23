`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && length(a) > 0) a else b

library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)

# ── DATA DIRECTORY ───────────────────────────────────────────
DATA_DIR <- "."

read_csv_safe <- function(fname, skip=0, ...) {
  path <- file.path(DATA_DIR, fname)
  if (!file.exists(path)) { message("Missing: ", fname); return(NULL) }
  read.csv(path, stringsAsFactors=FALSE, skip=skip, ...)
}

# ── LOAD ALL DATA ────────────────────────────────────────────

chains_raw <- read_csv_safe("Top 50 Fast-Food Chains in USA.csv")
if (!is.null(chains_raw)) {
  names(chains_raw) <- c("name","sales","avg_unit","franchised","company","units","change")
  chains_df <- chains_raw %>% arrange(desc(sales))
} else chains_df <- NULL

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

ob_raw <- read_csv_safe("National Obesity by State.csv")
if (!is.null(ob_raw)) {
  ob_df <- ob_raw %>%
    select(state=NAME, obesity=Obesity) %>%
    filter(!is.na(obesity)) %>%
    arrange(desc(obesity))
} else ob_df <- NULL

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

race_raw <- read_csv_safe("raw_data.csv", skip=2)
if (!is.null(race_raw)) {
  names(race_raw)[1:5] <- c("state","white","black","hispanic","asian")
  race_df <- race_raw %>%
    select(state, white, black, hispanic, asian) %>%
    filter(!is.na(state), state != "", state != "Location") %>%
    mutate(across(c(white,black,hispanic,asian), suppressWarnings(as.numeric))) %>%
    filter(!is.na(white))
} else race_df <- NULL

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

ob_county_raw <- read_csv_safe("Obesity_Prevalence__2023.csv")
if (!is.null(ob_county_raw)) {
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
      geo_clean   = sub("^us-", "", geoId),
      state_ab    = toupper(sub("-.*", "", geo_clean)),
      county_part = sub(".*-", "", geo_clean),
      state_fips  = STATE_FIPS_LOOKUP[state_ab],
      FIPS        = paste0(state_fips,
                           formatC(as.integer(county_part), width=3, flag="0")),
      obesity_pct = round(value * 100, 1)
    ) %>%
    filter(!is.na(state_fips), !is.na(obesity_pct)) %>%
    select(FIPS, obesity_pct)
} else ob_county_df <- NULL

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

# ── FOOD ENVIRONMENT ATLAS ───────────────────────────────────
atlas_raw <- read_csv_safe("2025_food_environment_atlas_data.csv")
if (!is.null(atlas_raw)) {
  # Pad FIPS to 5 chars, replace -8888 sentinel with NA
  atlas_df <- atlas_raw %>%
    mutate(FIPS = formatC(as.integer(FIPS), width=5, flag="0")) %>%
    mutate(across(where(is.numeric), ~ ifelse(. == -8888, NA_real_, .)))
} else atlas_df <- NULL

# Census region lookup (state abbreviation → region name)
CENSUS_REGION <- c(
  CT="Northeast", ME="Northeast", MA="Northeast", NH="Northeast",
  RI="Northeast", VT="Northeast", NJ="Northeast", NY="Northeast", PA="Northeast",
  IL="Midwest",  IN="Midwest",   MI="Midwest",   OH="Midwest",   WI="Midwest",
  IA="Midwest",  KS="Midwest",   MN="Midwest",   MO="Midwest",   NE="Midwest",
  ND="Midwest",  SD="Midwest",
  DE="South",    FL="South",     GA="South",     MD="South",     NC="South",
  SC="South",    VA="South",     WV="South",     AL="South",     KY="South",
  MS="South",    TN="South",     AR="South",     LA="South",     OK="South",
  TX="South",    DC="South",
  AZ="West",     CO="West",      ID="West",      MT="West",      NV="West",
  NM="West",     UT="West",      WY="West",      AK="West",      CA="West",
  HI="West",     OR="West",      WA="West"
)
REGION_COLORS <- c(
  "Northeast" = "#1a73e8",
  "Midwest"   = "#27ae60",
  "South"     = "#e8711a",
  "West"      = "#8e44ad"
)

# Human-readable labels for atlas variables
ATLAS_LABELS <- c(
  ObesityRate2022          = "State-Wide Obesity Rate % (2022)",
  DiabetesRate2019         = "Diabetes Rate % (2019)",
  PctHSPhysActive2021      = "Physically Active HS % (2021)",
  FoodInsecPct2123         = "Food Insecurity % (2021-23)",
  VeryLowFoodSecPct2123    = "Very Low Food Security % (2021-23)",
  PctLowAccess2019         = "Low Food Access % (2019)",
  PctLowIncLowAccess2019   = "Low Income + Low Access %",
  PctChildLowAccess2019    = "Children Low Access %",
  PovRate2021              = "Poverty Rate % (2021)",
  ChildPovRate2021         = "Child Poverty Rate % (2021)",
  DeepPovRate2021          = "Deep Poverty Rate % (2021)",
  MedianHHInc2021          = "Median Household Income $ (2021)",
  SNAPPct2022              = "SNAP Participation % (2022)",
  SchoolLunchPct2021       = "School Lunch % (2021)",
  FFRPer1k2020             = "Fast Food Restaurants per 1k (2020)",
  GrocPer1k2020            = "Grocery Stores per 1k (2020)",
  DollarPer1k2020          = "Dollar Stores per 1k (2020)",
  ConvPer1k2020            = "Conv Stores per 1k (2020)",
  RecFacPer1k2020          = "Rec Facilities per 1k (2020)",
  FarmMktPer1k2018         = "Farmers Markets per 1k (2018)",
  PctWhite2020             = "% White (2020)",
  PctBlack2020             = "% Black (2020)",
  PctHisp2020              = "% Hispanic (2020)",
  Pct65Plus2020            = "% Age 65+ (2020)",
  PctUnder182020           = "% Under 18 (2020)"
)

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

# ── CHOROPLETH HELPERS ───────────────────────────────────────
gradient_color <- function(v, lo, hi, palette_colors, alpha=0.80) {
  if (is.na(v)) return("rgba(200,200,200,0.15)")
  t <- max(0, min(1, (v - lo) / max(hi - lo, .Machine$double.eps)))
  n <- length(palette_colors)
  idx_f  <- t * (n - 1) + 1
  idx_lo <- floor(idx_f);  idx_hi <- ceiling(idx_f)
  frac   <- idx_f - idx_lo
  cl <- col2rgb(palette_colors[idx_lo])
  ch <- col2rgb(palette_colors[idx_hi])
  r  <- round(cl[1] + frac*(ch[1]-cl[1]))
  g  <- round(cl[2] + frac*(ch[2]-cl[2]))
  b  <- round(cl[3] + frac*(ch[3]-cl[3]))
  sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
}

OB_PALETTE  <- c("#2ecc71","#f1c40f","#e67e22","#c0392b","#7b0000")
POV_PALETTE <- c("#aed6f1","#2980b9","#1a5276","#7d3c98","#4a235a")

# Generic sequential palettes keyed to variable "direction"
# Most vars: higher = more intense (warm)  |  income: higher = better (cool-to-green)
WARM_PAL  <- c("#fffde4","#fee08b","#fc8d59","#d73027","#7b0000")  # yellow → red
COOL_PAL  <- c("#f7fbff","#9ecae1","#3182bd","#08519c","#08306b")  # light → dark blue
GREEN_PAL <- c("#f7fcf5","#a1d99b","#41ab5d","#238b45","#00441b")  # white → dark green

var_palette <- function(varname) {
  if (varname %in% c("MedianHHInc2021","PctHSPhysActive2021",
                     "GrocPer1k2020","RecFacPer1k2020","FarmMktPer1k2018")) {
    GREEN_PAL   # higher = better
  } else if (varname %in% c("PctWhite2020","PctBlack2020","PctHisp2020",
                            "Pct65Plus2020","PctUnder182020")) {
    COOL_PAL
  } else {
    WARM_PAL    # higher = worse / more intense
  }
}

make_choropleth <- function(proxy, geo_sf, vals, pal, label_txt, clear=TRUE) {
  if (clear) proxy <- proxy %>% clearShapes() %>% clearControls()
  valid <- vals[!is.na(vals)]
  lo <- if (length(valid)) quantile(valid, .02) else 0
  hi <- if (length(valid)) quantile(valid, .98) else 1
  fill_colors <- sapply(vals, gradient_color, lo=lo, hi=hi, palette_colors=pal)
  proxy %>% addPolygons(
    data             = geo_sf,
    fillColor        = fill_colors,
    fillOpacity      = 0.78,
    color            = "#555555",
    weight           = 0.25,
    opacity          = 0.4,
    label            = label_txt,
    labelOptions     = labelOptions(
      style     = list("font-family"="Space Mono, monospace",
                       "font-size"  ="11px",
                       "background" ="rgba(255,255,255,.92)",
                       "padding"    ="3px 7px"),
      direction = "auto"
    ),
    highlightOptions = highlightOptions(
      weight=1.2, color="#111", fillOpacity=0.92, bringToFront=TRUE
    )
  )
}

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── CHAINS ──────────────────────────────────────────────────
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
    pop_raw2 <- read_csv_safe("historical_state_population_by_year.csv")
    pop_df <- NULL
    if (!is.null(pop_raw2)) {
      names(pop_raw2) <- c("abbr","year","population")
      pop_df <- pop_raw2 %>%
        filter(!is.na(population)) %>%
        group_by(abbr) %>%
        filter(year == max(year)) %>%
        ungroup() %>%
        select(abbr, population)
    }
    ff_counts <- map_df %>%
      count(province, name="ff_count") %>%
      rename(abbr = province)
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
    merged <- race_clean %>% inner_join(ff_counts, by="abbr")
    if (!is.null(pop_df)) {
      merged <- merged %>% left_join(pop_df, by="abbr") %>%
        mutate(ff_per_100k = round(ff_count / population * 100000, 1))
    } else {
      merged <- merged %>% mutate(ff_per_100k = ff_count, population = NA)
    }
    merged <- merged %>% filter(!is.na(ff_per_100k)) %>% arrange(desc(ff_per_100k))
    maj_colors <- c("White"="#4e9af1","Hispanic"="#54c066","Black"="#e87040","Asian"="#f0c040","Other"="#aaaaaa")
    merged$color <- maj_colors[merged$majority]
    hover_txt <- paste0("<b>",merged$state," (",merged$abbr,")</b><br>","FF per 100k: <b>",merged$ff_per_100k,"</b><br>","Majority group: <b>",merged$majority,"</b><br>","White: ",merged$white_pct,"% · ","Black: ",merged$black_pct,"% · ","Hispanic: ",merged$hispanic_pct,"% · ","Asian: ",merged$asian_pct,"%")
    groups <- unique(merged$majority)
    p <- plot_ly()
    for (grp in groups) {
      d <- merged %>% filter(majority == grp)
      p <- p %>% add_trace(data=d, x=~reorder(abbr,-ff_per_100k), y=~ff_per_100k,
                           type="bar", name=grp, marker=list(color=maj_colors[grp]),
                           hovertext=hover_txt[merged$majority==grp], hoverinfo="text")
    }
    p %>% layout(barmode="overlay", paper_bgcolor="#faf7f2", plot_bgcolor="white",
                 xaxis=list(title="State",tickfont=list(size=11),fixedrange=TRUE,
                            categoryorder="array",categoryarray=merged$abbr),
                 yaxis=list(title="Fast Food Locations per 100k People",gridcolor="#ede8df",tickfont=list(size=12),fixedrange=TRUE),
                 legend=list(title=list(text="Majority Group"),orientation="h",y=1.06,font=list(size=13)),
                 margin=list(l=70,r=20,t=40,b=80), font=list(family="DM Sans",size=13)) %>%
      config(displayModeBar=FALSE)
  })
  
  # ── MAP ──────────────────────────────────────────────────────
  all_chains_available <- reactive({
    if (!is.null(map_df)) sort(unique(map_df$chain[map_df$chain != "Other"]))
    else TOP_CHAINS
  })
  active_chains <- reactiveVal(NULL)
  observe({
    ch <- all_chains_available()
    if (is.null(isolate(active_chains()))) active_chains(ch)
  })
  all_selected <- reactiveVal(TRUE)
  observeEvent(input$toggleAllChains, {
    chains <- all_chains_available()
    if (all_selected()) {
      updateCheckboxGroupInput(session, "chainFilter", selected=character(0))
      active_chains(character(0))
      updateActionButton(session, "toggleAllChains", label="Select All")
      all_selected(FALSE)
    } else {
      updateCheckboxGroupInput(session, "chainFilter", selected=chains)
      active_chains(chains)
      updateActionButton(session, "toggleAllChains", label="Deselect All")
      all_selected(TRUE)
    }
  })
  observeEvent(input$chainFilter, {
    chains <- all_chains_available()
    active_chains(input$chainFilter)
    if (length(input$chainFilter) == length(chains)) {
      updateActionButton(session, "toggleAllChains", label="Deselect All"); all_selected(TRUE)
    } else {
      updateActionButton(session, "toggleAllChains", label="Select All");   all_selected(FALSE)
    }
  }, ignoreNULL=FALSE)
  output$chainFilterUI <- renderUI({
    chains <- all_chains_available()
    checkboxGroupInput("chainFilter", "", choices=chains, selected=chains)
  })
  map_filtered <- reactive({
    req(map_df)
    ch <- active_chains()
    if (is.null(ch) || length(ch)==0) return(map_df[0,])
    map_df %>% filter(chain %in% ch)
  })
  output$mapCounter <- renderText({
    paste0(formatC(nrow(map_filtered()), format="d", big.mark=","), " locations shown")
  })
  output$overlayLegend <- renderUI({
    ov <- input$mapOverlay
    if (is.null(ov) || ov == "none") return(NULL)
    if (ov == "obesity") {
      pal <- OB_PALETTE; lo_lbl <- "~20%"; hi_lbl <- "~40%+"; ttl <- "Adult Obesity Rate (by County)"
      grad <- paste0("linear-gradient(to right, ", paste(pal, collapse=", "), ")")
    } else {
      pal <- POV_PALETTE; lo_lbl <- "Low"; hi_lbl <- "High"; ttl <- "Poverty Rate (by County)"
      grad <- paste0("linear-gradient(to right, ", paste(pal, collapse=", "), ")")
    }
    div(class="overlay-legend",
        strong(style="font-size:.78rem;display:block;margin-bottom:.5rem;", ttl),
        div(class="gradient-legend",
            div(class="gradient-bar", style=paste0("background:", grad, ";")),
            div(class="gradient-labels", span(lo_lbl), span(hi_lbl))
        ),
        span(style="font-size:.68rem;color:#8c7355;margin-top:.5rem;display:block;",
             "Hover a county for its value")
    )
  })
  
  county_sf <- tryCatch({
    if (!requireNamespace("sf", quietly=TRUE)) stop("sf not installed")
    sf::read_sf("https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json") %>%
      dplyr::rename(FIPS = id)
  }, error=function(e) { message("county sf load failed: ", e$message); NULL })
  
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
  
  observeEvent(list(map_filtered(), input$mapOverlay), {
    df  <- map_filtered()
    ov  <- input$mapOverlay %||% "none"
    proxy <- leafletProxy("ffMap")
    proxy %>% clearMarkers() %>% clearShapes() %>% clearControls()
    if (ov != "none" && !is.null(county_sf)) {
      if (ov == "obesity" && !is.null(ob_county_df)) {
        geo <- dplyr::left_join(county_sf, ob_county_df, by="FIPS")
        vals <- geo$obesity_pct
        lo <- floor(min(vals, na.rm=TRUE)); hi <- ceiling(max(vals, na.rm=TRUE))
        fill_colors <- sapply(vals, gradient_color, lo=lo, hi=hi, palette_colors=OB_PALETTE)
        label_txt <- paste0(ifelse(is.na(geo$NAME),"County",geo$NAME),": ",
                            ifelse(is.na(vals),"No data",paste0(vals,"% obesity")))
      } else if (ov == "poverty" && !is.null(pov_county_df)) {
        geo <- dplyr::left_join(county_sf, pov_county_df, by="FIPS")
        vals <- geo$pct_poverty
        lo <- floor(min(vals, na.rm=TRUE)); hi <- ceiling(max(vals, na.rm=TRUE))
        fill_colors <- sapply(vals, gradient_color, lo=lo, hi=hi, palette_colors=POV_PALETTE)
        label_txt <- paste0(ifelse(is.na(geo$NAME),"County",geo$NAME),": ",
                            ifelse(is.na(vals),"No data",paste0(vals,"% poverty")))
      } else { geo <- NULL }
      if (!is.null(geo)) {
        proxy %>% addPolygons(
          data=geo, fillColor=fill_colors, fillOpacity=0.70, color="#444444",
          weight=0.3, opacity=0.4, label=label_txt,
          labelOptions=labelOptions(style=list("font-family"="Space Mono, monospace",
                                               "font-size"="12px","background"="white","padding"="4px 8px"),
                                    direction="auto"),
          highlightOptions=highlightOptions(weight=1.5,color="#222",fillOpacity=0.85,bringToFront=FALSE)
        )
      }
    }
    cols <- unname(sapply(df$chain, function(ch) { v <- CHAIN_COLORS[ch]; if (is.na(v)) "#888888" else v }))
    if (nrow(df) > 0) {
      proxy %>% addCircleMarkers(
        data=df, lng=~longitude, lat=~latitude,
        radius=3, color=cols, fillColor=cols, fillOpacity=0.82, weight=0.5, opacity=0.9,
        popup=~paste0("<div style='font-family:DM Sans,sans-serif;min-width:150px;'>",
                      "<strong style='font-size:13px;'>",name,"</strong><br>",
                      "<span style='color:#888;font-size:11px;'>",chain,"</span><br>",
                      "<span style='font-size:11px;'>",city,", ",province,"</span></div>"),
        label=~name
      )
    }
  })
  
  # ── COMPARE TAB ─────────────────────────────────────────────
  # Shared: load county GeoJSON once (reuse county_sf from above)
  # Atlas data joined to geo for each variable selection
  
  # Helper: join atlas column to county_sf and build labels
  atlas_geo <- function(varname) {
    req(atlas_df, county_sf)
    col_sym <- as.name(varname)
    sub_df  <- atlas_df %>% select(FIPS, value = !!col_sym) %>% filter(!is.na(value))
    geo     <- dplyr::left_join(county_sf, sub_df, by="FIPS")
    list(geo=geo, vals=geo$value)
  }
  
  # Gradient legend UI helper
  grad_legend_ui <- function(varname, pal, vals) {
    valid <- vals[!is.na(vals)]
    lo <- if (length(valid)) round(quantile(valid,.02),1) else 0
    hi <- if (length(valid)) round(quantile(valid,.98),1) else 1
    grad <- paste0("linear-gradient(to right, ", paste(pal, collapse=", "), ")")
    div(
      div(class="compare-legend-title", ATLAS_LABELS[varname] %||% varname),
      div(class="compare-grad-bar", style=paste0("background:", grad, ";")),
      div(class="compare-grad-labels", span(lo), span(paste0(hi, "+")))
    )
  }
  
  # ── Left map ──────────────────────────────────────
  output$compareLabelA <- renderText({
    ATLAS_LABELS[input$compareVarA] %||% input$compareVarA
  })
  output$compareMapA <- renderLeaflet({
    leaflet(options=leafletOptions(preferCanvas=TRUE, zoomControl=FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=-96, lat=38, zoom=3)
  })
  observeEvent(input$compareVarA, {
    req(county_sf, atlas_df)
    varname <- input$compareVarA
    pal     <- var_palette(varname)
    ag      <- atlas_geo(varname)
    geo     <- ag$geo;  vals <- ag$vals
    valid   <- vals[!is.na(vals)]
    lo      <- if (length(valid)) quantile(valid,.02) else 0
    hi      <- if (length(valid)) quantile(valid,.98) else 1
    fill_colors <- sapply(vals, gradient_color, lo=lo, hi=hi, palette_colors=pal)
    lbl <- ATLAS_LABELS[varname] %||% varname
    fmt_val <- function(v) if (is.na(v)) "No data" else {
      if (grepl("Inc", varname)) paste0("$", formatC(v, format="d", big.mark=","))
      else paste0(round(v,1))
    }
    label_txt <- paste0(ifelse(is.na(geo$NAME),"County",paste0(geo$NAME,", ",geo$State)),
                        ": ", sapply(vals, fmt_val))
    leafletProxy("compareMapA") %>% clearShapes() %>%
      addPolygons(data=geo, fillColor=fill_colors, fillOpacity=0.78,
                  color="#555", weight=0.25, opacity=0.35,
                  label=label_txt,
                  labelOptions=labelOptions(style=list("font-family"="Space Mono, monospace",
                                                       "font-size"="11px","background"="rgba(255,255,255,.9)",
                                                       "padding"="3px 7px"), direction="auto"),
                  highlightOptions=highlightOptions(weight=1.2,color="#111",fillOpacity=0.92,bringToFront=TRUE))
  }, ignoreNULL=FALSE)
  output$compareLegendA <- renderUI({
    req(atlas_df)
    varname <- input$compareVarA
    pal     <- var_palette(varname)
    ag      <- atlas_geo(varname)
    grad_legend_ui(varname, pal, ag$vals)
  })
  
  # ── Right map ─────────────────────────────────────
  output$compareLabelB <- renderText({
    ATLAS_LABELS[input$compareVarB] %||% input$compareVarB
  })
  output$compareMapB <- renderLeaflet({
    leaflet(options=leafletOptions(preferCanvas=TRUE, zoomControl=FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=-96, lat=38, zoom=3)
  })
  observeEvent(input$compareVarB, {
    req(county_sf, atlas_df)
    varname <- input$compareVarB
    pal     <- var_palette(varname)
    ag      <- atlas_geo(varname)
    geo     <- ag$geo;  vals <- ag$vals
    valid   <- vals[!is.na(vals)]
    lo      <- if (length(valid)) quantile(valid,.02) else 0
    hi      <- if (length(valid)) quantile(valid,.98) else 1
    fill_colors <- sapply(vals, gradient_color, lo=lo, hi=hi, palette_colors=pal)
    fmt_val <- function(v) if (is.na(v)) "No data" else {
      if (grepl("Inc", varname)) paste0("$", formatC(v, format="d", big.mark=","))
      else paste0(round(v,1))
    }
    label_txt <- paste0(ifelse(is.na(geo$NAME),"County",paste0(geo$NAME,", ",geo$State)),
                        ": ", sapply(vals, fmt_val))
    leafletProxy("compareMapB") %>% clearShapes() %>%
      addPolygons(data=geo, fillColor=fill_colors, fillOpacity=0.78,
                  color="#555", weight=0.25, opacity=0.35,
                  label=label_txt,
                  labelOptions=labelOptions(style=list("font-family"="Space Mono, monospace",
                                                       "font-size"="11px","background"="rgba(255,255,255,.9)",
                                                       "padding"="3px 7px"), direction="auto"),
                  highlightOptions=highlightOptions(weight=1.2,color="#111",fillOpacity=0.92,bringToFront=TRUE))
  }, ignoreNULL=FALSE)
  output$compareLegendB <- renderUI({
    req(atlas_df)
    varname <- input$compareVarB
    pal     <- var_palette(varname)
    ag      <- atlas_geo(varname)
    grad_legend_ui(varname, pal, ag$vals)
  })
  
  # ── Scatter: A vs B per county ────────────────────
  output$compareScatterTitle <- renderText({
    la <- ATLAS_LABELS[input$compareVarA] %||% input$compareVarA
    lb <- ATLAS_LABELS[input$compareVarB] %||% input$compareVarB
    paste(la, "vs.", lb, "— by County")
  })
  output$compareScatter <- renderPlotly({
    req(atlas_df)
    va <- input$compareVarA
    vb <- input$compareVarB
    la <- ATLAS_LABELS[va] %||% va
    lb <- ATLAS_LABELS[vb] %||% vb
    
    plot_df <- atlas_df %>%
      select(FIPS, State, County, x=all_of(va), y=all_of(vb)) %>%
      filter(!is.na(x), !is.na(y)) %>%
      mutate(
        region = CENSUS_REGION[State] %||% "Other",
        color  = REGION_COLORS[region] %||% "#888888",
        hover  = paste0("<b>", County, ", ", State, "</b><br>",
                        la, ": ", round(x,1), "<br>",
                        lb, ": ", round(y,1))
      )
    
    # Correlation
    cor_val <- if (nrow(plot_df) > 10)
      round(cor(plot_df$x, plot_df$y, use="complete.obs"), 3) else NA
    
    regions <- unique(plot_df$region)
    p <- plot_ly()
    for (rg in regions) {
      d <- plot_df %>% filter(region == rg)
      p <- p %>% add_trace(
        data=d, x=~x, y=~y, type="scatter", mode="markers",
        name=rg,
        marker=list(size=5, color=REGION_COLORS[rg], opacity=0.55,
                    line=list(color="white", width=0.3)),
        hovertext=~hover, hoverinfo="text"
      )
    }
    
    subtitle_txt <- if (!is.na(cor_val))
      paste0("Pearson r = ", cor_val, " · n = ", nrow(plot_df), " counties")
    else paste0("n = ", nrow(plot_df), " counties")
    
    p %>% layout(
      paper_bgcolor = "#faf7f2",
      plot_bgcolor  = "white",
      xaxis = list(title=la, gridcolor="#ede8df", tickfont=list(size=11), zeroline=FALSE),
      yaxis = list(title=lb, gridcolor="#ede8df", tickfont=list(size=11), zeroline=FALSE),
      legend= list(title=list(text="Region"), orientation="h", y=1.05,
                   font=list(size=12)),
      margin= list(l=70, r=20, t=50, b=60),
      font  = list(family="DM Sans", size=13),
      annotations = list(list(
        x=0.5, y=1.04, xref="paper", yref="paper",
        text=subtitle_txt, showarrow=FALSE,
        font=list(family="Space Mono", size=11, color="#8c7355")
      ))
    ) %>%
      config(displayModeBar=FALSE)
  })
  
}