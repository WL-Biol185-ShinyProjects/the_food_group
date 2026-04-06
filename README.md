# 🍔 Fast Food, Health, and Poverty in America
### An Interactive R Shiny Dashboard

---

## Overview

This dashboard explores the relationship between the fast food industry and public health outcomes across the United States. Using interactive visualizations, it connects data on restaurant distribution, nutritional content, obesity rates, poverty, food insecurity, and racial demographics — revealing how access to fast food intersects with broader social and economic patterns.

---

## App Tabs

### 🏠 About
Introduces the project and its goals. Provides context on the rise of the fast food industry in America, key statistics (200K+ locations, $331B industry revenue), and a summary of the datasets used.

### 🏪 Chains
Explores the Top 50 fast food chains in the U.S. by systemwide sales and unit count.
- **Bar chart** — Systemwide sales for the top 20 chains (2021)
- **Bubble chart** — Total units vs. average sales per unit, sized by number of locations

### 🥗 Nutrition & Obesity
Breaks down the nutritional content of menu items across 8 major chains and connects diet to health outcomes at the state level.
- **Healthiness Score** — Composite score per chain based on calories, sodium, saturated fat, sugar, protein, and fiber
- **Nutrient Heatmap** — Normalized comparison of key nutrients across all chains
- **Obesity by State** — Ranked bar chart of adult obesity rates (CDC BRFSS 2023)
- **Obesity by Race/Ethnicity** — National breakdown by demographic group
- **Physical Inactivity vs. Obesity** — State-level scatter plot

### 💸 Poverty
Examines the relationship between poverty and poor health outcomes.
- **Top 15 Highest Poverty States** — % of population below the poverty line (2023)
- **Poverty vs. Obesity Scatter** — Each dot is a state; shows the correlation between economic hardship and obesity rates

### 👥 Demographics
Visualizes food insecurity, income, and fast food density through the lens of race and ethnicity using USDA Food Environment Atlas data.
- **Interactive bubble chart** — X axis: food insecurity rate · Y axis: median household income · Bubble size: fast food restaurants per 1,000 people · Color: share of selected race group (White / Black / Hispanic)
- Selectable race group filter

### 🗺️ Map
Places 9,999 fast food restaurant locations on an interactive U.S. map.
- Toggle individual chains on/off
- Overlay county-level **obesity rate** or **poverty rate** as a choropleth
- Color-coded markers by chain brand

### ⚖️ Compare
Side-by-side county-level maps for any two variables from the USDA Food Environment Atlas (3,153 counties).
- Choose from 25+ variables across Health, Food Security, Economics, Food Environment, and Demographics
- Useful for visually identifying geographic correlations (e.g., fast food density vs. diabetes rate)

### 👋 Meet the Team
Introduces the project contributors with photos, roles, and bios.

---

## Data Sources

| Dataset | Source |
|---|---|
| Top 50 Fast Food Chains (sales & units) | [Kaggle — Sourav Banerjee](https://www.kaggle.com/datasets/iamsouravbanerjee/top-50-fastfood-chains-in-usa) |
| Fast food nutritional information | FDA Menu Labeling Data |
| Adult Obesity Rate by State | CDC BRFSS 2023 |
| Poverty rates by state | [U.S. Census Bureau — SAIPE 2023](https://www.census.gov/data-tools/demo/saipe/) |
| Race & ethnicity by state | KFF State Health Facts 2024 |
| Food Environment Atlas (food insecurity, income, fast food density, county-level) | [USDA ERS 2025](https://www.ers.usda.gov/data-products/food-environment-atlas/) |
| Restaurant geolocation data | Datafiniti / Kaggle (9,999 U.S. locations) |

---

## Tech Stack

| Tool | Purpose |
|---|---|
| **R Shiny** | Web application framework |
| **Leaflet** | Interactive maps |
| **Plotly** | Interactive charts and visualizations |
| **DT** | Interactive data tables |
| **dplyr / tidyr** | Data wrangling |
| **bslib** | UI theming |

---

## How to Run

**1. Install required packages:**
```r
install.packages(c("shiny", "leaflet", "plotly", "DT", "dplyr",
                   "tidyr", "scales", "bslib", "sf"))
```

**2. Clone or download this repository.**

**3. Open `ui.R` or `server.R` in RStudio and click ▶ Run App**, or run from the console:
```r
shiny::runApp()
```

---

## Project Structure

```
project/
├── ui.R              # All UI layout, styling, and tab structure
├── server.R          # All reactive logic, chart rendering, and map code
├── www/              # Static assets (images, photos)
│   └── *.png / *.jpg
└── README.md
```

---

## A Note on AI Assistance

This project was developed with the assistance of **Claude (Anthropic)**, an AI assistant. Claude was used to help with:

- Writing and debugging R Shiny code (`ui.R` and `server.R`)
- Designing the dashboard layout and CSS styling
- Building interactive Plotly charts and Leaflet maps
- Converting and structuring data for visualization
- Generating this README

All data, research framing, project direction, and final decisions were made by the team. AI served as a coding and development tool — the ideas, analysis, and interpretation are our own.

---

## Team

- **Mellanese Barlow** — Pre-Vet Biology, Minor in Poverty & Human Capability Studies
- *(Additional team members — see Meet the Team tab)*

---

*CDC · USDA ERS · FDA · U.S. Census · QSR Magazine · KFF · BRFSS · Datafiniti*
