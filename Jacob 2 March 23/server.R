`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && length(a) > 0) a else b

library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(scales)

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
  message("Atlas columns: ", paste(names(atlas_raw), collapse=", "))
  atlas_df <- atlas_raw %>%
    mutate(FIPS = formatC(as.integer(FIPS), width=5, flag="0")) %>%
    mutate(across(where(is.numeric), ~ ifelse(. == -8888, NA_real_, .)))
} else atlas_df <- NULL

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
REGION_COLORS <- c("Northeast"="#1a73e8","Midwest"="#27ae60","South"="#e8711a","West"="#8e44ad")

ATLAS_LABELS <- c(
  ObesityRate2022="Obesity Rate by County % (2023)", DiabetesRate2019="Diabetes Rate % (2019)",
  PctHSPhysActive2021="Physically Active HS % (2021)", FoodInsecPct2123="Food Insecurity % (2021-23)",
  VeryLowFoodSecPct2123="Very Low Food Security % (2021-23)", PctLowAccess2019="Low Food Access % (2019)",
  PctLowIncLowAccess2019="Low Income + Low Access %", PctChildLowAccess2019="Children Low Access %",
  PovRate2021="Poverty Rate % (2021)", ChildPovRate2021="Child Poverty Rate % (2021)",
  DeepPovRate2021="Deep Poverty Rate % (2021)", MedianHHInc2021="Median Household Income $ (2021)",
  SNAPPct2022="SNAP Participation % (2022)", SchoolLunchPct2021="School Lunch % (2021)",
  FFRPer1k2020="Fast Food Restaurants per 1k (2020)", GrocPer1k2020="Grocery Stores per 1k (2020)",
  DollarPer1k2020="Dollar Stores per 1k (2020)", ConvPer1k2020="Conv Stores per 1k (2020)",
  RecFacPer1k2020="Rec Facilities per 1k (2020)", FarmMktPer1k2018="Farmers Markets per 1k (2018)",
  PctWhite2020="% White (2020)", PctBlack2020="% Black (2020)", PctHisp2020="% Hispanic (2020)",
  Pct65Plus2020="% Age 65+ (2020)", PctUnder182020="% Under 18 (2020)"
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
WARM_PAL    <- c("#fffde4","#fee08b","#fc8d59","#d73027","#7b0000")
COOL_PAL    <- c("#f7fbff","#9ecae1","#3182bd","#08519c","#08306b")
GREEN_PAL   <- c("#f7fcf5","#a1d99b","#41ab5d","#238b45","#00441b")

var_palette <- function(varname) {
  if (varname %in% c("MedianHHInc2021","PctHSPhysActive2021","GrocPer1k2020","RecFacPer1k2020","FarmMktPer1k2018")) GREEN_PAL
  else if (varname %in% c("PctWhite2020","PctBlack2020","PctHisp2020","Pct65Plus2020","PctUnder182020")) COOL_PAL
  else WARM_PAL
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
             margin=list(l=130,r=30,t=20,b=50), font=list(family="DM Sans", size=13)) %>%
      config(displayModeBar=FALSE)
  })
  
  output$chainScatterChart <- renderPlotly({
    req(chains_df)
    plot_ly(chains_df, x=~avg_unit, y=~sales, text=~name, type="scatter", mode="markers",
            marker=list(size=~(8 + 24*(units-min(units,na.rm=TRUE))/(max(units,na.rm=TRUE)-min(units,na.rm=TRUE))),
                        color=colorRampPalette(c("#d4380d","#e67e22","#f39c12"))(nrow(chains_df)),
                        opacity=0.8, line=list(color="white",width=1)),
            hovertemplate="<b>%{text}</b><br>Avg/Unit: $%{x}K<br>Total Sales: $%{y}M<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Avg Sales per Unit ($K)", gridcolor="#ede8df", tickfont=list(size=12), fixedrange=TRUE),
             yaxis=list(title="Systemwide Sales ($M)", gridcolor="#ede8df", tickfont=list(size=12), fixedrange=TRUE),
             margin=list(l=70,r=30,t=20,b=60), font=list(family="DM Sans", size=13)) %>%
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
    plot_ly(d, x=d[[col]], y=~reorder(restaurant, d[[col]]), type="bar", orientation="h",
            marker=list(color=sapply(d[[col]], color_fn)),
            hovertemplate=paste0("%{y}: %{x}", fmt, "<extra></extra>")) %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(gridcolor="#ede8df", tickfont=list(size=12), fixedrange=TRUE),
             yaxis=list(title="", tickfont=list(size=12), fixedrange=TRUE),
             margin=list(l=120,r=20,t=10,b=30), font=list(family="DM Sans", size=13)) %>%
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
             margin=list(l=160,r=20,t=10,b=50), font=list(family="DM Sans", size=13)) %>%
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
             margin=list(l=160,r=20,t=10,b=50), font=list(family="DM Sans", size=13)) %>%
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
             margin=list(l=160,r=20,t=10,b=50), font=list(family="DM Sans", size=13)) %>%
      config(displayModeBar=FALSE)
  })
  output$scatterChart <- renderPlotly({
    req(ob_df, pov_df)
    merged <- inner_join(ob_df %>% select(state,obesity),
                         pov_df %>% select(state,pct_poverty), by="state")
    plot_ly(merged, x=~pct_poverty, y=~obesity, text=~state, type="scatter", mode="markers",
            marker=list(size=11, color=ob_color(merged$obesity), opacity=0.85,
                        line=list(color="white",width=1.5)),
            hovertemplate="<b>%{text}</b><br>Poverty: %{x}%<br>Obesity: %{y}%<extra></extra>") %>%
      layout(paper_bgcolor="#faf7f2", plot_bgcolor="white",
             xaxis=list(title="Poverty Rate (%)", gridcolor="#ede8df", tickfont=list(size=12)),
             yaxis=list(title="Obesity Rate (%)", gridcolor="#ede8df", tickfont=list(size=12)),
             margin=list(l=60,r=20,t=20,b=60), font=list(family="DM Sans", size=13))
  })
  
  # ── DEMOGRAPHICS — bubble plot ───────────────────────────────
  # ── DEMOGRAPHICS — STATE-LEVEL STORY BUBBLE ───────────────────
  output$demoBubbleChart <- renderPlotly({
    req(atlas_df)
    
    # Detect state column
    state_col <- names(atlas_df)[tolower(names(atlas_df)) %in%
                                   c("state","state_name","statename","st")]
    req(length(state_col) > 0)
    
    # ── STATE AGGREGATION ──────────────────────────────────────
    state_df <- atlas_df %>%
      rename(state = !!state_col[1]) %>%
      group_by(state) %>%
      summarise(
        food_insec = mean(FoodInsecPct2123, na.rm=TRUE),
        median_inc = mean(MedianHHInc2021, na.rm=TRUE),
        ff_per_1k  = mean(FFRPer1k2020, na.rm=TRUE),
        pct_black  = mean(PctBlack2020, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(food_insec), !is.na(median_inc), !is.na(ff_per_1k))
    
    # ── MEDIANS FOR QUADRANTS ──────────────────────────────────
    x_mid <- median(state_df$food_insec, na.rm=TRUE)
    y_mid <- median(state_df$median_inc, na.rm=TRUE)
    
    # ── BUBBLE SIZE SCALING ────────────────────────────────────
    sizeref <- max(state_df$ff_per_1k, na.rm=TRUE) / 20
    
    # ── PLOT ───────────────────────────────────────────────────
    plot_ly(
      state_df,
      x = ~food_insec,
      y = ~median_inc,
      type = "scatter",
      mode = "markers+text",
      text = ~state,
      textposition = "top center",
      hovertemplate = paste0(
        "<b>%{text}</b><br>",
        "Food Insecurity: %{x:.1f}%<br>",
        "Median Income: $%{y:,}<br>",
        "Fast Food / 1k: %{marker.size:.2f}<extra></extra>"
      ),
      marker = list(
        size = ~ff_per_1k,
        sizemode = "diameter",
        sizeref = sizeref,
        sizemin = 6,
        color = ~pct_black,
        colorscale = list(c(0, "#f7fbff"), c(1, "#08306b")),
        showscale = TRUE,
        line = list(color = "white", width = 1),
        opacity = 0.75
      )
    ) %>%
      layout(
        paper_bgcolor = "#faf7f2",
        plot_bgcolor = "white",
        
        xaxis = list(
          title = "Food Insecurity Rate (%)",
          gridcolor = "#ede8df",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Median Household Income ($)",
          gridcolor = "#ede8df",
          tickformat = "$,",
          zeroline = FALSE
        ),
        

        
        # ── STORY ANNOTATIONS ──────────────────────────────────
        annotations = list(
          list(
            text = "Most vulnerable<br>(low income, high insecurity)",
            x = min(state_df$food_insec),
            y = min(state_df$median_inc),
            showarrow = FALSE,
            xanchor = "left",
            font = list(size = 11)
          ),
          list(
            text = "Wealthy & secure",
            x = max(state_df$food_insec),
            y = max(state_df$median_inc),
            showarrow = FALSE,
            xanchor = "right",
            font = list(size = 11)
          ),
          list(
            text = "Bubble size = fast food density • Color = % Black population",
            x = 0.5, y = -0.15,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 10, color = "#8c7355")
          )
        ),
        
        margin = list(l=80, r=40, t=30, b=80),
        font = list(family="DM Sans", size=13)
      ) %>%
      config(displayModeBar = FALSE)
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
    df    <- map_filtered()
    ov    <- input$mapOverlay %||% "none"
    proxy <- leafletProxy("ffMap")
    proxy %>% clearMarkers() %>% clearShapes() %>% clearControls()
    if (ov != "none" && !is.null(county_sf)) {
      if (ov == "obesity" && !is.null(ob_county_df)) {
        geo <- dplyr::left_join(county_sf, ob_county_df, by="FIPS")
        vals <- geo$obesity_pct
        lo <- floor(min(vals,na.rm=TRUE)); hi <- ceiling(max(vals,na.rm=TRUE))
        fill_colors <- sapply(vals, gradient_color, lo=lo, hi=hi, palette_colors=OB_PALETTE)
        label_txt <- paste0(ifelse(is.na(geo$NAME),"County",geo$NAME),": ",
                            ifelse(is.na(vals),"No data",paste0(vals,"% obesity")))
      } else if (ov == "poverty" && !is.null(pov_county_df)) {
        geo <- dplyr::left_join(county_sf, pov_county_df, by="FIPS")
        vals <- geo$pct_poverty
        lo <- floor(min(vals,na.rm=TRUE)); hi <- ceiling(max(vals,na.rm=TRUE))
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
  atlas_geo <- function(varname) {
    req(county_sf)
    if (varname == "ObesityRate2022") {
      req(ob_county_df)
      geo <- dplyr::left_join(county_sf, ob_county_df, by="FIPS")
      list(geo=geo, vals=geo$obesity_pct)
    } else {
      req(atlas_df)
      col_sym <- as.name(varname)
      sub_df  <- atlas_df %>% select(FIPS, value=!!col_sym) %>% filter(!is.na(value))
      geo     <- dplyr::left_join(county_sf, sub_df, by="FIPS")
      list(geo=geo, vals=geo$value)
    }
  }
  
  build_label <- function(geo, vals, varname, atlas_df_ref) {
    county_name <- if (!is.null(geo$NAME)) geo$NAME else rep("County", nrow(geo))
    fmt_val <- function(v) {
      if (is.na(v)) return("No data")
      if (grepl("Inc", varname)) paste0("$", formatC(as.integer(v), format="d", big.mark=","))
      else paste0(round(v, 1), "%")
    }
    if (varname == "ObesityRate2022") {
      paste0(ifelse(is.na(county_name),"County",county_name), ": ", sapply(vals,fmt_val), " obesity")
    } else {
      state_col <- names(atlas_df_ref)[tolower(names(atlas_df_ref)) %in% c("state","state_name","statename","st")]
      if (length(state_col) > 0) {
        state_lookup <- atlas_df_ref %>% select(FIPS, state_val=!!as.name(state_col[1])) %>% distinct(FIPS,.keep_all=TRUE)
        geo_with_state <- dplyr::left_join(data.frame(FIPS=geo$FIPS,stringsAsFactors=FALSE), state_lookup, by="FIPS")
        state_name <- geo_with_state$state_val
      } else { state_name <- rep(NA_character_, nrow(geo)) }
      location_label <- ifelse(!is.na(state_name),
                               paste0(ifelse(is.na(county_name),"County",county_name),", ",state_name),
                               ifelse(is.na(county_name),"County",county_name))
      paste0(location_label, ": ", sapply(vals, fmt_val))
    }
  }
  
  grad_legend_ui <- function(varname, pal, vals) {
    valid <- vals[!is.na(vals)]
    lo <- if (length(valid)) round(quantile(valid,.02),1) else 0
    hi <- if (length(valid)) round(quantile(valid,.98),1) else 1
    grad <- paste0("linear-gradient(to right, ", paste(pal, collapse=", "), ")")
    div(
      div(class="compare-legend-title", ATLAS_LABELS[varname] %||% varname),
      div(class="compare-grad-bar", style=paste0("background:", grad, ";")),
      div(class="compare-grad-labels", span(lo), span(paste0(hi,"+")))
    )
  }
  
  # Left map
  output$compareLabelA <- renderText({ ATLAS_LABELS[input$compareVarA] %||% input$compareVarA })
  output$compareMapA <- renderLeaflet({
    leaflet(options=leafletOptions(preferCanvas=TRUE, zoomControl=FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% setView(lng=-96, lat=38, zoom=3)
  })
  observeEvent(input$compareVarA, {
    req(county_sf); varname <- input$compareVarA
    if (varname != "ObesityRate2022") req(atlas_df)
    pal <- var_palette(varname); ag <- atlas_geo(varname)
    geo <- ag$geo; vals <- ag$vals; valid <- vals[!is.na(vals)]
    lo <- if (length(valid)) quantile(valid,.02) else 0
    hi <- if (length(valid)) quantile(valid,.98) else 1
    final_pal   <- if (varname == "ObesityRate2022") OB_PALETTE else pal
    fill_colors <- sapply(vals, gradient_color, lo=lo, hi=hi, palette_colors=final_pal)
    atlas_ref   <- if (varname == "ObesityRate2022") data.frame() else atlas_df
    label_txt   <- build_label(geo, vals, varname, atlas_ref)
    leafletProxy("compareMapA") %>% clearShapes() %>%
      addPolygons(data=geo, fillColor=fill_colors, fillOpacity=0.78,
                  color="#555", weight=0.25, opacity=0.35, label=label_txt,
                  labelOptions=labelOptions(style=list("font-family"="Space Mono, monospace","font-size"="11px","background"="rgba(255,255,255,.9)","padding"="3px 7px"), direction="auto"),
                  highlightOptions=highlightOptions(weight=1.2,color="#111",fillOpacity=0.92,bringToFront=TRUE))
  }, ignoreNULL=FALSE)
  output$compareLegendA <- renderUI({
    varname <- input$compareVarA
    if (varname != "ObesityRate2022") req(atlas_df)
    pal <- if (varname == "ObesityRate2022") OB_PALETTE else var_palette(varname)
    grad_legend_ui(varname, pal, atlas_geo(varname)$vals)
  })
  
  # Right map
  output$compareLabelB <- renderText({ ATLAS_LABELS[input$compareVarB] %||% input$compareVarB })
  output$compareMapB <- renderLeaflet({
    leaflet(options=leafletOptions(preferCanvas=TRUE, zoomControl=FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% setView(lng=-96, lat=38, zoom=3)
  })
  observeEvent(input$compareVarB, {
    req(county_sf); varname <- input$compareVarB
    if (varname != "ObesityRate2022") req(atlas_df)
    pal <- var_palette(varname); ag <- atlas_geo(varname)
    geo <- ag$geo; vals <- ag$vals; valid <- vals[!is.na(vals)]
    lo <- if (length(valid)) quantile(valid,.02) else 0
    hi <- if (length(valid)) quantile(valid,.98) else 1
    final_pal   <- if (varname == "ObesityRate2022") OB_PALETTE else pal
    fill_colors <- sapply(vals, gradient_color, lo=lo, hi=hi, palette_colors=final_pal)
    atlas_ref   <- if (varname == "ObesityRate2022") data.frame() else atlas_df
    label_txt   <- build_label(geo, vals, varname, atlas_ref)
    leafletProxy("compareMapB") %>% clearShapes() %>%
      addPolygons(data=geo, fillColor=fill_colors, fillOpacity=0.78,
                  color="#555", weight=0.25, opacity=0.35, label=label_txt,
                  labelOptions=labelOptions(style=list("font-family"="Space Mono, monospace","font-size"="11px","background"="rgba(255,255,255,.9)","padding"="3px 7px"), direction="auto"),
                  highlightOptions=highlightOptions(weight=1.2,color="#111",fillOpacity=0.92,bringToFront=TRUE))
  }, ignoreNULL=FALSE)
  output$compareLegendB <- renderUI({
    varname <- input$compareVarB
    if (varname != "ObesityRate2022") req(atlas_df)
    pal <- if (varname == "ObesityRate2022") OB_PALETTE else var_palette(varname)
    grad_legend_ui(varname, pal, atlas_geo(varname)$vals)
  })
}