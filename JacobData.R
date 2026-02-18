library(tidyverse)

read.table("FoodData.txt")

View("FoodData.txt")

file.info("FoodData.txt")

FoodData <- haven::read_xpt("FoodData.txt")
FoodData

str(FoodData)
names(FoodData)
nrow(FoodData)

fifty_fastfood <- read_csv("Top 50 Fast-Food Chains in USA.csv")
fifty_fastfood

View(fifty_fastfood)


# app_fixed.R - Fast Food Analytics Dashboard (CORRECTED VERSION)

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(lubridate)
library(scales)

# ============================================================================
# DATA LOADING (assumes CSV files already exist)
# ============================================================================

# Check if data files exist
required_files <- c("transactions.csv", "store_locations.csv", 
                    "menu_sales.csv", "employee_data.csv")

if(!all(file.exists(required_files))) {
  stop("Data files not found! Please run 'generate_data.R' first to create the datasets.")
}

# Load data with progress messages
cat("Loading datasets...\n")
transactions <- read_csv("transactions.csv", show_col_types = FALSE)
store_locations <- read_csv("store_locations.csv", show_col_types = FALSE)
menu_sales <- read_csv("menu_sales.csv", show_col_types = FALSE)
employee_data <- read_csv("employee_data.csv", show_col_types = FALSE)
cat("Data loaded successfully!\n")

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "Fast Food Analytics USA"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Transaction Analysis", tabName = "transactions", icon = icon("receipt")),
      menuItem("Store Performance", tabName = "stores", icon = icon("store")),
      menuItem("Menu Analytics", tabName = "menu", icon = icon("hamburger")),
      menuItem("Employee Insights", tabName = "employees", icon = icon("users")),
      menuItem("Time Patterns", tabName = "timepatterns", icon = icon("clock")),
      menuItem("Geographic Analysis", tabName = "geography", icon = icon("map")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .box { border-top: 3px solid #dd4b39; }
        .shiny-output-error-validation { color: red; font-weight: bold; }
      "))
    ),
    
    tabItems(
      # ========== DASHBOARD TAB ==========
      tabItem(
        tabName = "dashboard",
        h2("Executive Dashboard"),
        
        fluidRow(
          valueBoxOutput("total_revenue"),
          valueBoxOutput("total_transactions"),
          valueBoxOutput("avg_transaction")
        ),
        
        fluidRow(
          valueBoxOutput("total_stores_box"),
          valueBoxOutput("total_employees"),
          valueBoxOutput("avg_rating")
        ),
        
        fluidRow(
          box(
            title = "Daily Revenue Trend",
            status = "danger",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("revenue_trend", height = 350)
          ),
          box(
            title = "Top 5 Chains by Revenue",
            status = "danger",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("top_chains", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Transaction Distribution by Hour",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("hourly_dist", height = 300)
          ),
          box(
            title = "Order Type Breakdown",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("order_type_pie", height = 300)
          )
        )
      ),
      
      # ========== TRANSACTION ANALYSIS TAB ==========
      tabItem(
        tabName = "transactions",
        h2("Transaction Analysis (100,000 Transactions)"),
        
        fluidRow(
          box(
            title = "Filters",
            status = "warning",
            width = 12,
            column(3, selectInput("trans_chain", "Chain:", 
                                  choices = c("All", sort(unique(transactions$chain))),
                                  selected = "All")),
            column(3, selectInput("trans_meal", "Meal Period:",
                                  choices = c("All", unique(transactions$meal_period)),
                                  selected = "All")),
            column(3, selectInput("trans_payment", "Payment Method:",
                                  choices = c("All", unique(transactions$payment_method)),
                                  selected = "All")),
            column(3, dateRangeInput("trans_dates", "Date Range:",
                                     start = min(transactions$date),
                                     end = max(transactions$date)))
          )
        ),
        
        fluidRow(
          box(
            title = "Transaction Amount Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("trans_amount_hist", height = 350)
          ),
          box(
            title = "Average Transaction by Day of Week",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("trans_dow", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Wait Time Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("wait_time_box", height = 350)
          ),
          box(
            title = "Customer Satisfaction Ratings",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("rating_bar", height = 350)
          )
        )
      ),
      
      # ========== STORE PERFORMANCE TAB ==========
      tabItem(
        tabName = "stores",
        h2("Store Performance (15,000+ Stores)"),
        
        fluidRow(
          box(
            title = "Store Comparison",
            status = "primary",
            width = 12,
            selectInput("store_metric", "Select Metric:",
                        choices = c("Average Daily Customers" = "avg_daily_customers",
                                    "Average Transaction Value" = "avg_transaction_value",
                                    "Google Rating" = "google_rating",
                                    "Employee Count" = "employee_count")),
            plotlyOutput("store_comparison", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Drive-Thru vs No Drive-Thru",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("drivethu_analysis", height = 300)
          ),
          box(
            title = "Store Size Distribution",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("sqft_dist", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "Store Growth by Year",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("store_growth_year", height = 350)
          )
        )
      ),
      
      # ========== MENU ANALYTICS TAB ==========
      tabItem(
        tabName = "menu",
        h2("Menu Item Analytics (25,000+ Sales Records)"),
        
        fluidRow(
          box(
            title = "Filters",
            status = "info",
            width = 12,
            column(4, selectInput("menu_chain_filter", "Chain:",
                                  choices = c("All", sort(unique(menu_sales$chain))),
                                  selected = "All")),
            column(4, selectInput("menu_category_filter", "Category:",
                                  choices = c("All", unique(menu_sales$category)),
                                  selected = "All")),
            column(4, selectInput("menu_city_filter", "City:",
                                  choices = c("All", sort(unique(menu_sales$city))),
                                  selected = "All"))
          )
        ),
        
        fluidRow(
          box(
            title = "Top 20 Menu Items by Revenue",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("top_menu_items", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Price vs Quantity Sold",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("price_quantity_scatter", height = 350)
          ),
          box(
            title = "Category Performance",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("category_revenue", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Calorie Distribution by Category",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("calorie_box", height = 350)
          )
        )
      ),
      
      # ========== EMPLOYEE INSIGHTS TAB ==========
      tabItem(
        tabName = "employees",
        h2("Employee Analytics (12,000+ Employees)"),
        
        fluidRow(
          valueBoxOutput("total_emp_box"),
          valueBoxOutput("avg_wage"),
          valueBoxOutput("turnover_rate")
        ),
        
        fluidRow(
          box(
            title = "Employee Distribution by Position",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("position_dist", height = 300)
          ),
          box(
            title = "Wage Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("wage_hist", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "Experience vs Wage",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("exp_wage_scatter", height = 350)
          ),
          box(
            title = "Performance Rating Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("performance_dist", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Full-Time vs Part-Time by Chain",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("ft_pt_chain", height = 350)
          )
        )
      ),
      
      # ========== TIME PATTERNS TAB ==========
      tabItem(
        tabName = "timepatterns",
        h2("Time-Based Patterns"),
        
        fluidRow(
          box(
            title = "Hourly Transaction Volume",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            checkboxGroupInput("time_chains", "Select Chains:",
                               choices = sort(unique(transactions$chain))[1:8],
                               selected = sort(unique(transactions$chain))[1:4],
                               inline = TRUE),
            plotlyOutput("hourly_volume", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Weekend vs Weekday Revenue",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("weekend_weekday", height = 350)
          ),
          box(
            title = "Monthly Seasonality",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("monthly_pattern", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Meal Period Performance",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("meal_period_perf", height = 350)
          )
        )
      ),
      
      # ========== GEOGRAPHIC ANALYSIS TAB ==========
      tabItem(
        tabName = "geography",
        h2("Geographic Distribution"),
        
        fluidRow(
          box(
            title = "Store Locations Map",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("map_chain_filter", "Filter by Chain:",
                        choices = c("All", sort(unique(store_locations$chain))),
                        selected = "All"),
            leafletOutput("store_map", height = 600)
          )
        ),
        
        fluidRow(
          box(
            title = "Top 15 Cities by Store Count",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("city_store_count", height = 350)
          ),
          box(
            title = "Revenue by Top 15 Cities",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("city_revenue", height = 350)
          )
        )
      ),
      
      # ========== DATA EXPLORER TAB ==========
      tabItem(
        tabName = "data",
        h2("Raw Data Explorer"),
        
        fluidRow(
          box(
            title = "Select Dataset",
            status = "primary",
            width = 12,
            radioButtons("dataset_choice", "Choose Dataset:",
                         choices = c("Transactions (100K rows)" = "transactions",
                                     "Store Locations (15K rows)" = "stores",
                                     "Menu Sales (25K rows)" = "menu",
                                     "Employee Data (12K rows)" = "employees"),
                         selected = "transactions",
                         inline = TRUE),
            downloadButton("download_data", "Download Selected Data", class = "btn-success")
          )
        ),
        
        fluidRow(
          box(
            title = "Data Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table", height = 600)
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # ========== DASHBOARD TAB ==========
  output$total_revenue <- renderValueBox({
    total_rev <- sum(transactions$transaction_amount, na.rm = TRUE)
    valueBox(
      paste0("$", format(round(total_rev / 1000000, 2), big.mark = ","), "M"),
      "Total Revenue (2023)",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$total_transactions <- renderValueBox({
    valueBox(
      format(nrow(transactions), big.mark = ","),
      "Total Transactions",
      icon = icon("receipt"),
      color = "blue"
    )
  })
  
  output$avg_transaction <- renderValueBox({
    avg_trans <- mean(transactions$transaction_amount, na.rm = TRUE)
    valueBox(
      paste0("$", round(avg_trans, 2)),
      "Avg Transaction Value",
      icon = icon("shopping-cart"),
      color = "yellow"
    )
  })
  
  output$total_stores_box <- renderValueBox({
    valueBox(
      format(nrow(store_locations), big.mark = ","),
      "Total Stores",
      icon = icon("store"),
      color = "red"
    )
  })
  
  output$total_employees <- renderValueBox({
    valueBox(
      format(nrow(employee_data), big.mark = ","),
      "Total Employees",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$avg_rating <- renderValueBox({
    avg_rat <- mean(store_locations$google_rating, na.rm = TRUE)
    valueBox(
      paste0(round(avg_rat, 2), " / 5.0"),
      "Avg Google Rating",
      icon = icon("star"),
      color = "orange"
    )
  })
  
  output$revenue_trend <- renderPlotly({
    validate(
      need(nrow(transactions) > 0, "No transaction data available")
    )
    
    trend_data <- transactions %>%
      group_by(date) %>%
      summarise(daily_revenue = sum(transaction_amount, na.rm = TRUE), .groups = "drop")
    
    plot_ly(trend_data, x = ~date, y = ~daily_revenue,
            type = 'scatter', mode = 'lines',
            line = list(color = '#dd4b39', width = 2)) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Daily Revenue ($)"),
             hovermode = "x unified")
  })
  
  output$top_chains <- renderPlotly({
    chain_rev <- transactions %>%
      group_by(chain) %>%
      summarise(total_rev = sum(transaction_amount, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_rev)) %>%
      head(5)
    
    plot_ly(chain_rev, x = ~reorder(chain, total_rev), y = ~total_rev,
            type = 'bar', marker = list(color = '#dd4b39')) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Revenue ($)"))
  })
  
  output$hourly_dist <- renderPlotly({
    hourly_data <- transactions %>%
      count(hour)
    
    plot_ly(hourly_data, x = ~hour, y = ~n, type = 'bar',
            marker = list(color = '#3c8dbc')) %>%
      layout(xaxis = list(title = "Hour of Day"),
             yaxis = list(title = "Number of Transactions"))
  })
  
  output$order_type_pie <- renderPlotly({
    order_data <- transactions %>%
      count(order_type)
    
    plot_ly(order_data, labels = ~order_type, values = ~n, type = 'pie') %>%
      layout(title = "")
  })
  
  # ========== TRANSACTION ANALYSIS TAB ==========
  filtered_transactions <- reactive({
    data <- transactions
    
    if(input$trans_chain != "All") {
      data <- data %>% filter(chain == input$trans_chain)
    }
    if(input$trans_meal != "All") {
      data <- data %>% filter(meal_period == input$trans_meal)
    }
    if(input$trans_payment != "All") {
      data <- data %>% filter(payment_method == input$trans_payment)
    }
    
    data <- data %>%
      filter(date >= input$trans_dates[1] & date <= input$trans_dates[2])
    
    validate(
      need(nrow(data) > 0, "No transactions match the selected filters")
    )
    
    data
  })
  
  output$trans_amount_hist <- renderPlotly({
    plot_ly(filtered_transactions(), x = ~transaction_amount, type = "histogram",
            marker = list(color = '#00a65a')) %>%
      layout(xaxis = list(title = "Transaction Amount ($)"),
             yaxis = list(title = "Frequency"))
  })
  
  output$trans_dow <- renderPlotly({
    dow_data <- filtered_transactions() %>%
      group_by(day_of_week) %>%
      summarise(avg_amount = mean(transaction_amount, na.rm = TRUE), .groups = "drop")
    
    plot_ly(dow_data, x = ~day_of_week, y = ~avg_amount, type = 'bar',
            marker = list(color = '#f39c12')) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Avg Transaction ($)"))
  })
  
  output$wait_time_box <- renderPlotly({
    plot_ly(filtered_transactions(), y = ~wait_time_minutes, color = ~order_type,
            type = "box") %>%
      layout(yaxis = list(title = "Wait Time (minutes)"))
  })
  
  output$rating_bar <- renderPlotly({
    rating_data <- filtered_transactions() %>%
      count(rating)
    
    plot_ly(rating_data, x = ~rating, y = ~n, type = 'bar',
            marker = list(color = '#dd4b39')) %>%
      layout(xaxis = list(title = "Rating (stars)"),
             yaxis = list(title = "Count"))
  })
  
  # ========== STORE PERFORMANCE TAB ==========
  output$store_comparison <- renderPlotly({
    metric_data <- store_locations %>%
      group_by(chain) %>%
      summarise(avg_metric = mean(.data[[input$store_metric]], na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_metric)) %>%
      head(15)
    
    plot_ly(metric_data, x = ~reorder(chain, avg_metric), y = ~avg_metric,
            type = 'bar', marker = list(color = '#3c8dbc')) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = gsub("_", " ", tools::toTitleCase(input$store_metric))))
  })
  
  output$drivethu_analysis <- renderPlotly({
    dt_data <- store_locations %>%
      group_by(has_drive_thru) %>%
      summarise(avg_customers = mean(avg_daily_customers, na.rm = TRUE), .groups = "drop") %>%
      mutate(has_drive_thru = ifelse(has_drive_thru, "Has Drive-Thru", "No Drive-Thru"))
    
    plot_ly(dt_data, x = ~has_drive_thru, y = ~avg_customers, type = 'bar',
            marker = list(color = c('#dd4b39', '#00a65a'))) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Avg Daily Customers"))
  })
  
  output$sqft_dist <- renderPlotly({
    plot_ly(store_locations, x = ~sq_footage, type = "histogram",
            marker = list(color = '#f39c12')) %>%
      layout(xaxis = list(title = "Square Footage"),
             yaxis = list(title = "Frequency"))
  })
  
  output$store_growth_year <- renderPlotly({
    growth_data <- store_locations %>%
      count(year_opened, chain) %>%
      filter(chain %in% c("McDonald's", "Starbucks", "Subway", "Chick-fil-A", "Taco Bell"))
    
    plot_ly(growth_data, x = ~year_opened, y = ~n, color = ~chain,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Stores Opened"))
  })
  
  # ========== MENU ANALYTICS TAB ==========
  filtered_menu <- reactive({
    data <- menu_sales
    
    if(input$menu_chain_filter != "All") {
      data <- data %>% filter(chain == input$menu_chain_filter)
    }
    if(input$menu_category_filter != "All") {
      data <- data %>% filter(category == input$menu_category_filter)
    }
    if(input$menu_city_filter != "All") {
      data <- data %>% filter(city == input$menu_city_filter)
    }
    
    validate(
      need(nrow(data) > 0, "No menu items match the selected filters")
    )
    
    data
  })
  
  output$top_menu_items <- renderPlotly({
    top_items <- filtered_menu() %>%
      group_by(item_name, chain) %>%
      summarise(total_revenue = sum(daily_revenue, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_revenue)) %>%
      head(20)
    
    plot_ly(top_items, x = ~reorder(item_name, total_revenue), y = ~total_revenue,
            color = ~chain, type = 'bar') %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Revenue ($)"),
             barmode = 'stack')
  })
  
  output$price_quantity_scatter <- renderPlotly({
    scatter_data <- filtered_menu() %>%
      group_by(item_name, price) %>%
      summarise(total_qty = sum(quantity_sold, na.rm = TRUE), .groups = "drop")
    
    plot_ly(scatter_data, x = ~price, y = ~total_qty,
            type = 'scatter', mode = 'markers',
            text = ~item_name, marker = list(size = 8, color = '#dd4b39')) %>%
      layout(xaxis = list(title = "Price ($)"),
             yaxis = list(title = "Total Quantity Sold"))
  })
  
  output$category_revenue <- renderPlotly({
    cat_data <- filtered_menu() %>%
      group_by(category) %>%
      summarise(total_revenue = sum(daily_revenue, na.rm = TRUE), .groups = "drop")
    
    plot_ly(cat_data, labels = ~category, values = ~total_revenue, type = 'pie') %>%
      layout(title = "")
  })
  
  output$calorie_box <- renderPlotly({
    plot_ly(filtered_menu(), y = ~calories, color = ~category, type = "box") %>%
      layout(yaxis = list(title = "Calories"))
  })
  
  # ========== EMPLOYEE INSIGHTS TAB ==========
  output$total_emp_box <- renderValueBox({
    valueBox(
      format(nrow(employee_data), big.mark = ","),
      "Total Employees",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_wage <- renderValueBox({
    avg_w <- mean(employee_data$hourly_wage, na.rm = TRUE)
    valueBox(
      paste0("$", round(avg_w, 2), "/hr"),
      "Average Hourly Wage",
      icon = icon("money-bill"),
      color = "green"
    )
  })
  
  output$turnover_rate <- renderValueBox({
    # Simple estimate: employees with < 1 year experience
    turnover <- mean(employee_data$years_experience < 1, na.rm = TRUE) * 100
    valueBox(
      paste0(round(turnover), "%"),
      "Est. Annual Turnover",
      icon = icon("exchange-alt"),
      color = "red"
    )
  })
  
  output$position_dist <- renderPlotly({
    pos_data <- employee_data %>%
      count(position)
    
    plot_ly(pos_data, labels = ~position, values = ~n, type = 'pie') %>%
      layout(title = "")
  })
  
  output$wage_hist <- renderPlotly({
    plot_ly(employee_data, x = ~hourly_wage, type = "histogram",
            marker = list(color = '#00a65a')) %>%
      layout(xaxis = list(title = "Hourly Wage ($)"),
             yaxis = list(title = "Frequency"))
  })
  
  output$exp_wage_scatter <- renderPlotly({
    plot_ly(employee_data, x = ~years_experience, y = ~hourly_wage,
            color = ~position, type = 'scatter', mode = 'markers',
            marker = list(size = 6)) %>%
      layout(xaxis = list(title = "Years of Experience"),
             yaxis = list(title = "Hourly Wage ($)"))
  })
  
  output$performance_dist <- renderPlotly({
    perf_data <- employee_data %>%
      count(performance_rating)
    
    plot_ly(perf_data, x = ~performance_rating, y = ~n, type = 'bar',
            marker = list(color = '#f39c12')) %>%
      layout(xaxis = list(title = "Performance Rating (1-5)"),
             yaxis = list(title = "Number of Employees"))
  })
  
  output$ft_pt_chain <- renderPlotly({
    ft_data <- employee_data %>%
      group_by(chain, is_full_time) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(employment_type = ifelse(is_full_time, "Full-Time", "Part-Time")) %>%
      group_by(chain) %>%
      mutate(total = sum(count),
             percentage = count / total * 100) %>%
      filter(chain %in% c("McDonald's", "Starbucks", "Subway", "Chick-fil-A", 
                          "Taco Bell", "Burger King", "Wendy's", "Dunkin'"))
    
    plot_ly(ft_data, x = ~chain, y = ~percentage, color = ~employment_type,
            type = 'bar') %>%
      layout(barmode = 'stack',
             xaxis = list(title = ""),
             yaxis = list(title = "Percentage (%)"))
  })
  
  # ========== TIME PATTERNS TAB ==========
  output$hourly_volume <- renderPlotly({
    validate(
      need(length(input$time_chains) > 0, "Please select at least one chain")
    )
    
    hourly_data <- transactions %>%
      filter(chain %in% input$time_chains) %>%
      group_by(hour, chain) %>%
      summarise(count = n(), .groups = "drop")
    
    plot_ly(hourly_data, x = ~hour, y = ~count, color = ~chain,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Hour of Day"),
             yaxis = list(title = "Number of Transactions"))
  })
  
  output$weekend_weekday <- renderPlotly({
    ww_data <- transactions %>%
      group_by(is_weekend) %>%
      summarise(total_revenue = sum(transaction_amount, na.rm = TRUE), .groups = "drop") %>%
      mutate(day_type = ifelse(is_weekend, "Weekend", "Weekday"))
    
    plot_ly(ww_data, x = ~day_type, y = ~total_revenue, type = 'bar',
            marker = list(color = c('#dd4b39', '#00a65a'))) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Revenue ($)"))
  })
  
  output$monthly_pattern <- renderPlotly({
    monthly_data <- transactions %>%
      group_by(month) %>%
      summarise(avg_daily_revenue = mean(transaction_amount, na.rm = TRUE), .groups = "drop")
    
    plot_ly(monthly_data, x = ~month, y = ~avg_daily_revenue, type = 'bar',
            marker = list(color = '#f39c12')) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Avg Transaction Amount ($)"))
  })
  
  output$meal_period_perf <- renderPlotly({
    meal_data <- transactions %>%
      group_by(meal_period, chain) %>%
      summarise(total_revenue = sum(transaction_amount, na.rm = TRUE), .groups = "drop") %>%
      filter(chain %in% c("McDonald's", "Starbucks", "Subway", "Chick-fil-A", "Taco Bell"))
    
    plot_ly(meal_data, x = ~meal_period, y = ~total_revenue, color = ~chain,
            type = 'bar') %>%
      layout(barmode = 'group',
             xaxis = list(title = "Meal Period"),
             yaxis = list(title = "Total Revenue ($)"))
  })
  
  # ========== GEOGRAPHIC ANALYSIS TAB ==========
  output$store_map <- renderLeaflet({
    map_data <- store_locations
    
    if(input$map_chain_filter != "All") {
      map_data <- map_data %>% filter(chain == input$map_chain_filter)
    }
    
    # Sample for performance (show max 1000 points with clustering)
    if(nrow(map_data) > 1000) {
      map_data <- map_data %>% sample_n(1000)
    }
    
    leaflet(map_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lon, lat = ~lat,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0("<strong>", chain, "</strong><br>",
                        "City: ", city, ", ", state, "<br>",
                        "Rating: ", google_rating)
      )
  })
  
  output$city_store_count <- renderPlotly({
    city_data <- store_locations %>%
      count(city, state) %>%
      arrange(desc(n)) %>%
      head(15) %>%
      mutate(city_state = paste0(city, ", ", state))
    
    plot_ly(city_data, x = ~reorder(city_state, n), y = ~n, type = 'bar',
            marker = list(color = '#3c8dbc')) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Number of Stores"))
  })
  
  output$city_revenue <- renderPlotly({
    # Get top cities by store count
    top_cities <- store_locations %>%
      count(city) %>%
      arrange(desc(n)) %>%
      head(15) %>%
      pull(city)
    
    # Calculate revenue by aggregating transactions by chain, then matching to cities
    city_rev <- transactions %>%
      group_by(chain) %>%
      summarise(total_revenue = sum(transaction_amount, na.rm = TRUE), .groups = "drop") %>%
      left_join(
        store_locations %>%
          filter(city %in% top_cities) %>%
          group_by(city, chain) %>%
          summarise(store_count = n(), .groups = "drop"),
        by = "chain"
      ) %>%
      filter(!is.na(city)) %>%
      mutate(allocated_revenue = total_revenue * store_count / sum(store_count)) %>%
      group_by(city) %>%
      summarise(total_revenue = sum(allocated_revenue, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_revenue))
    
    plot_ly(city_rev, x = ~reorder(city, total_revenue), y = ~total_revenue,
            type = 'bar', marker = list(color = '#00a65a')) %>%
      layout(xaxis = list(title = "City"),
             yaxis = list(title = "Estimated Revenue ($)"))
  })
  
  # ========== DATA EXPLORER TAB ==========
  selected_dataset <- reactive({
    switch(input$dataset_choice,
           "transactions" = transactions,
           "stores" = store_locations,
           "menu" = menu_sales,
           "employees" = employee_data)
  })
  
  output$data_table <- renderDT({
    datatable(selected_dataset(),
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                scrollY = "500px"
              ),
              filter = 'top')
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(selected_dataset(), file, row.names = FALSE)
    }
  )
}

# ============================================================================
# RUN THE APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)