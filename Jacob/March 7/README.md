# Fast Food, Obesity & Poverty in America — R Shiny App
## Setup & Run Instructions

---

## Project Structure

```
your-app-folder/
├── app.R                    ← Main Shiny app (copy this file here)
├── atlas_wide.csv           ← USDA Food Environment Atlas (processed)
├── historical_cpi.csv       ← USDA ERS food price inflation data
├── poverty_by_state.csv     ← Census ACS state poverty rates
```

---

## Step 1: Install Required R Packages

Run this once in your R console:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "leaflet",
  "ggplot2",
  "dplyr",
  "tidyr",
  "sf",
  "tigris",
  "scales",
  "DT",
  "plotly",
  "htmltools"
))
```

---

## Step 2: Place Data Files

Put all four files (`app.R`, `atlas_wide.csv`, `historical_cpi.csv`,
`poverty_by_state.csv`) in the **same folder**.

---

## Step 3: Run the App

In RStudio, open `app.R` and click **Run App**, or from the R console:

```r
setwd("path/to/your-app-folder")
shiny::runApp()
```

> **Note:** The first run will download U.S. county shapefiles via the
> `tigris` package (~10 MB). These are cached automatically so
> subsequent runs are fast.

---

## App Tabs

| Tab | What It Shows |
|-----|--------------|
| **County Map** | Interactive choropleth — 15 variables to choose from |
| **Scatter Explorer** | Any variable vs. any variable, colored by metro/state |
| **State Rankings** | Bar chart of top/bottom states for any variable |
| **Food Price Trends** | CPI inflation time series (1974–2024) |
| **Demographics** | Outcome variables vs. racial/ethnic composition by county |
| **Data Table** | Full searchable/downloadable county data table |

---

## Data Sources

| Dataset | Source | Variables |
|---------|--------|-----------|
| Food Environment Atlas | USDA ERS | Fast food density, food access, SNAP, poverty, obesity, diabetes |
| Food CPI | USDA ERS | Annual food inflation 1974–2024 |
| State Poverty | Census ACS / HDPulse | Family poverty rates by state |
| County Shapefiles | U.S. Census Bureau (via `tigris`) | Geographic boundaries |

---

## Troubleshooting

- **Map is blank:** Check that `atlas_wide.csv` is in the same directory as `app.R`.
- **tigris error:** Ensure you have internet access on first run for shapefile download.
- **Slow first load:** Normal — shapefile joins ~3,100 counties take a few seconds.
- **-8888 or -9999 values:** These are suppressed/missing values, already converted to `NA`.
