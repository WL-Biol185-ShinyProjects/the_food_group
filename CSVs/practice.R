# ============================================================
#  Fast Food Locations Interactive Map
#  Data source: Kaggle - "Fast Food Restaurants Across America"
#               https://www.kaggle.com/datasets/datafiniti/fast-food-restaurants
#
#  SETUP INSTRUCTIONS:
#  1. Go to https://www.kaggle.com/datasets/datafiniti/fast-food-restaurants
#  2. Click "Download" (free Kaggle account required)
#  3. Unzip the file — you'll get a CSV called something like
#     "FastFoodRestaurants.csv"
#  4. Set the path below to wherever you saved it
# ============================================================

# ── 0. SET YOUR FILE PATH HERE ───────────────────────────────
csv_path <- "FastFoodRestaurants.csv"   # <-- change this if needed


# ── 1. Install & load packages ───────────────────────────────
pkgs <- c("leaflet", "dplyr", "htmlwidgets")

install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
invisible(lapply(pkgs, install_if_missing))
invisible(lapply(pkgs, library, character.only = TRUE))


# ── 2. Load & clean the data ─────────────────────────────────
message("Loading dataset...")

csv_path <- "CSVs/FastFoodRestaurants.csv"
raw <- read.csv(csv_path, stringsAsFactors = FALSE)

# Peek at column names so we can adapt if needed
message("Columns found: ", paste(names(raw), collapse = ", "), "\n")

# Standardise column names to lowercase
names(raw) <- tolower(names(raw))

# The Kaggle dataset uses these columns:
#   name, address, city, country, latitude, longitude
df <- raw %>%
  filter(
    !is.na(latitude), !is.na(longitude),
    latitude  != 0,   longitude != 0,
    # Keep only US locations
    grepl("US|United States", country, ignore.case = TRUE) |
      (longitude < -60 & longitude > -130 &
         latitude  >  24 & latitude  <  50)
  ) %>%
  mutate(
    # Normalise chain names into a clean "chain" column
    chain = case_when(
      grepl("mcdonald",        name, ignore.case = TRUE) ~ "McDonald's",
      grepl("burger king",     name, ignore.case = TRUE) ~ "Burger King",
      grepl("wendy",           name, ignore.case = TRUE) ~ "Wendy's",
      grepl("taco bell",       name, ignore.case = TRUE) ~ "Taco Bell",
      grepl("subway",          name, ignore.case = TRUE) ~ "Subway",
      grepl("chick.fil",       name, ignore.case = TRUE) ~ "Chick-fil-A",
      grepl("popeye",          name, ignore.case = TRUE) ~ "Popeyes",
      grepl("\\bkfc\\b",       name, ignore.case = TRUE) ~ "KFC",
      grepl("domino",          name, ignore.case = TRUE) ~ "Domino's",
      grepl("pizza hut",       name, ignore.case = TRUE) ~ "Pizza Hut",
      grepl("chipotle",        name, ignore.case = TRUE) ~ "Chipotle",
      grepl("sonic",           name, ignore.case = TRUE) ~ "Sonic",
      grepl("arby",            name, ignore.case = TRUE) ~ "Arby's",
      grepl("five guys",       name, ignore.case = TRUE) ~ "Five Guys",
      grepl("shake shack",     name, ignore.case = TRUE) ~ "Shake Shack",
      grepl("panda",           name, ignore.case = TRUE) ~ "Panda Express",
      grepl("whataburger",     name, ignore.case = TRUE) ~ "Whataburger",
      grepl("jack in the box", name, ignore.case = TRUE) ~ "Jack in the Box",
      grepl("hardee",          name, ignore.case = TRUE) ~ "Hardee's",
      grepl("carl",            name, ignore.case = TRUE) ~ "Carl's Jr",
      grepl("starbucks",       name, ignore.case = TRUE) ~ "Starbucks",
      grepl("dunkin",          name, ignore.case = TRUE) ~ "Dunkin'",
      grepl("panera",          name, ignore.case = TRUE) ~ "Panera Bread",
      grepl("in-n-out|in n out", name, ignore.case = TRUE) ~ "In-N-Out",
      TRUE ~ "Other"
    ),
    full_address = paste0(
      ifelse(!is.na(address) & address != "", paste0(address, ", "), ""),
      ifelse(!is.na(city)    & city    != "", paste0(city,    ", "), ""),
      ifelse(!is.na(province) & province != "", province, "")
    )
  ) %>%
  select(lat = latitude, lon = longitude, name, chain, full_address)

message(sprintf("Loaded %s US locations across %d chains.\n",
                format(nrow(df), big.mark = ","), n_distinct(df$chain)))


# ── 3. Colour palette ────────────────────────────────────────
palette_colors <- c(
  "McDonald's"      = "#FFC72C",
  "Burger King"     = "#FF7F00",
  "Wendy's"         = "#E2231A",
  "Taco Bell"       = "#702082",
  "Subway"          = "#008C15",
  "Chick-fil-A"     = "#DD0031",
  "Popeyes"         = "#F47920",
  "KFC"             = "#F40027",
  "Domino's"        = "#006491",
  "Pizza Hut"       = "#EE3224",
  "Chipotle"        = "#441700",
  "Sonic"           = "#FFCC00",
  "Arby's"          = "#CC2529",
  "Five Guys"       = "#CE1126",
  "Shake Shack"     = "#6EBD44",
  "Panda Express"   = "#CC0000",
  "Whataburger"     = "#F77F00",
  "Jack in the Box" = "#EF7B10",
  "Hardee's"        = "#D62300",
  "Carl's Jr"       = "#D62300",
  "Starbucks"       = "#00704A",
  "Dunkin'"         = "#FF671F",
  "Panera Bread"    = "#6A9E3F",
  "In-N-Out"        = "#E31837",
  "Other"           = "#888888"
)

df <- df %>%
  mutate(color = ifelse(chain %in% names(palette_colors),
                        palette_colors[chain], "#888888"))

chain_levels <- sort(unique(df$chain))


# ── 4. Build the interactive leaflet map ─────────────────────
message("Building interactive map...")

map <- leaflet(df, options = leafletOptions(preferCanvas = TRUE)) %>%
  
  addProviderTiles(providers$CartoDB.DarkMatter,  group = "Dark")   %>%
  addProviderTiles(providers$CartoDB.Positron,    group = "Light")  %>%
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Street") %>%
  
  setView(lng = -96, lat = 38, zoom = 4) %>%
  
  addCircleMarkers(
    lng         = ~lon,
    lat         = ~lat,
    radius      = 5,
    color       = ~color,
    fillColor   = ~color,
    fillOpacity = 0.8,
    weight      = 1,
    opacity     = 0.9,
    popup = ~paste0(
      "<b>", name, "</b><br>",
      "<span style='color:#888'>", chain, "</span><br>",
      ifelse(nchar(trimws(full_address)) > 2,
             paste0("<small>", full_address, "</small>"), "")
    ),
    label = ~name,
    clusterOptions = markerClusterOptions(
      maxClusterRadius        = 40,
      disableClusteringAtZoom = 13
    ),
    group = ~chain
  ) %>%
  
  addLayersControl(
    baseGroups    = c("Dark", "Light", "Street"),
    overlayGroups = chain_levels,
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addScaleBar(position = "bottomleft") %>%
  
  addControl(
    html = paste0(
      "<div style='background:rgba(0,0,0,0.75);color:white;padding:8px 14px;",
      "border-radius:6px;font-family:sans-serif;font-size:14px;font-weight:bold'>",
      "🍔 US Fast Food Locations<br>",
      "<span style='font-size:11px;font-weight:normal;color:#aaa'>",
      format(nrow(df), big.mark = ","), " locations · Kaggle / Datafiniti</span>",
      "</div>"
    ),
    position = "topleft"
  )

message(sprintf("Map built with %s locations.", format(nrow(df), big.mark = ",")))


# ── 5. Save outputs ──────────────────────────────────────────
saveWidget(map, file = "fast_food_map.html", selfcontained = TRUE,
           title = "US Fast Food Locations")
message("Map saved -> fast_food_map.html")

write.csv(df, "fast_food_locations_clean.csv", row.names = FALSE)
message(sprintf("Clean dataset saved -> fast_food_locations_clean.csv  (%s rows)\n",
                format(nrow(df), big.mark = ",")))


# ── 6. Summary ───────────────────────────────────────────────
message("── Location counts by chain ──────────────────────")
df %>%
  count(chain, sort = TRUE) %>%
  mutate(bar = strrep("\u2588", pmin(round(n / max(n) * 30), 30))) %>%
  with(invisible(mapply(function(c, n, b)
    message(sprintf("  %-20s %5s  %s", c, format(n, big.mark = ","), b)),
    chain, n, bar)))
message("──────────────────────────────────────────────────\n")
message("Done! Open fast_food_map.html in your browser.")

file.exists("CSVs/FastFoodRestaurants.csv")
getwd()
