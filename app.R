# Collection of all R Visualizations over the semester

# install.packages(c("shiny","shinythemes","tidyverse","plotly","treemapify","sf","scales"))

library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggthemes)
library(treemapify)
library(scales)
library(forcats)
library(sf)
library(tigris)
library(highcharter)

# Load debt data
median_assets_debts <- read_csv("data/debt/median_assets_debt.csv", show_col_types = FALSE)
# Load unemployment data
avg_unemployment <- read_csv("data/unemployment/avg_unemployment.csv", show_col_types = FALSE)
bw_unemployment <- read_csv("data/unemployment/bw_unemployment.csv", show_col_types = FALSE)
bm_unemployment <- read_csv("data/unemployment/bm_unemployment.csv", show_col_types = FALSE)
black_unemployment <- read_csv("data/unemployment/black_unemployment.csv", show_col_types = FALSE)
ww_unemployment <- read_csv("data/unemployment/ww_unemployment.csv", show_col_types = FALSE)
wm_unemployment <- read_csv("data/unemployment/wm_unemployment.csv", show_col_types = FALSE)
white_unemployment <- read_csv("data/unemployment/white_unemployment.csv", show_col_types = FALSE)
hw_unemployment <- read_csv("data/unemployment/hw_unemployment.csv", show_col_types = FALSE)
hm_unemployment <- read_csv("data/unemployment/hm_unemployment.csv", show_col_types = FALSE)
hispanic_unemployment <- read_csv("data/unemployment/hispanic_unemployment.csv", show_col_types = FALSE)
#Load wages data 
high_violation_2023 <- read_csv("data/wages/low-wage-high-violation.csv", show_col_types = FALSE)
industry_pop <- read_csv("data/wages/industry_pop.csv", show_col_types = FALSE)
# Load map data 
minimum_wage_data <- read_csv("data/maps/minimum_wage_data.csv", show_col_types = FALSE)
pop_data <- read_csv("data/maps/pop_by_state.csv", show_col_types = FALSE)


usmap <- states()

# Prepare Minimum Wage Map Data (2020)
min_wage_2020 <- minimum_wage_data %>%
  filter(Year == 2020)

min_wage_map_data <- left_join(usmap, min_wage_2020, by = c("NAME" = "State")) %>%
  shift_geometry() %>%
  filter(STATEFP <= 56) %>%
  mutate(wage_category = case_when(
    State.Minimum.Wage <= 7.25 ~ "Federal ($7.25 or less)",
    State.Minimum.Wage > 7.25 & State.Minimum.Wage < 10 ~ "$7.26 - $9.99",
    State.Minimum.Wage >= 10 & State.Minimum.Wage < 12 ~ "$10.00 - $11.99",
    State.Minimum.Wage >= 12 ~ "$12.00 or Higher"
  )) %>%
  mutate(wage_category = factor(wage_category, levels = c(
    "Federal ($7.25 or less)",
    "$7.26 - $9.99",
    "$10.00 - $11.99",
    "$12.00 or Higher"
  )))

# Prepare Right to Work map data
rtw_data <- tibble(
  State = c("Alaska", "California", "Colorado", "Connecticut", "Delaware", 
            "District of Columbia", "Hawaii", "Illinois", "Maine", 
            "Maryland", "Massachusetts", "Michigan", "Minnesota", "Missouri", "Montana", 
            "New Hampshire", "New Jersey", "New Mexico", "New York", "Ohio", 
            "Oregon", "Pennsylvania", "Rhode Island", "Vermont", "Washington",
            "Kansas", "Alabama", "Arizona", "Arkansas", "Florida", "Georgia", "Idaho", 
            "Iowa", "Louisiana", "Mississippi", "Nebraska", "Nevada", 
            "North Carolina", "North Dakota", "Oklahoma", "South Carolina", 
            "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Wyoming",
            "Indiana", "Kentucky", "West Virginia", "Wisconsin"),
  RTW_Status = c("Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW",
                 "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW",
                 "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW",
                 "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW",
                 "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW", "Non-RTW",
                 "RTW", "RTW", "RTW", "RTW", "RTW", "RTW", "RTW",
                 "RTW", "RTW", "RTW", "RTW", "RTW", "RTW", "RTW",
                 "RTW", "RTW", "RTW", "RTW", "RTW", "RTW", "RTW",
                 "RTW", "RTW", "RTW", "RTW", "RTW")
)

rtw_map_data <- left_join(usmap, rtw_data, by = c("NAME" = "State")) %>%
  shift_geometry() %>%
  filter(STATEFP <= 56) %>%
  mutate(RTW_Status = factor(RTW_Status, levels = c("Non-RTW", "RTW")))

# Prepare Black population quartile map data
blk_state_pop <- pop_data %>%
  filter(geo_level == "state") %>%
  select(state_name, pop_total, pop_black_alone, 
         pct_black_alone, median_income_black_alone, quartiles_pct_black_alone)

blk_quartile_map_data <- left_join(usmap, blk_state_pop, by = c("NAME" = "state_name")) %>%
  shift_geometry() %>%
  filter(STATEFP <= 56) %>%
  mutate(quartiles_pct_black_alone = factor(quartiles_pct_black_alone, levels = c(
    "Top 25% of geographies",
    "51-75% of geographies",
    "26-50% of geographies",
    "Bottom 25% of geographies"
  )))

# Prepare Black median income map data
blk_income_map_data <- left_join(usmap, blk_state_pop, by = c("NAME" = "state_name")) %>%
  shift_geometry() %>%
  filter(STATEFP <= 56)


# Prepare data for education debt plot
all_race_edu_debt <- median_assets_debts %>%
  filter(metric == "Education Loans", racecl4 != "All")

# Filtering debt data for interactive plot
median_assets_debts <- median_assets_debts %>%
  mutate(racecl4 = as.character(racecl4)) %>%
  filter(racecl4 != "All") %>%
  mutate(racecl4 = factor(racecl4)) %>%
  mutate(label = paste0("Year: ", year, "\nRace: ", racecl4, "\nMedian: $", scales::comma(median)))

# Prepare unemployment data
all_unemployment_data <- bind_rows(
  avg_unemployment %>% mutate(Group = "National Average"),
  bw_unemployment %>% mutate(Group = "Black Women"),
  bm_unemployment %>% mutate(Group = "Black Men"),
  black_unemployment %>% mutate(Group = "Black Overall"),
  ww_unemployment %>% mutate(Group = "White Women"),
  wm_unemployment %>% mutate(Group = "White Men"),
  white_unemployment %>% mutate(Group = "White Overall"),
  hw_unemployment %>% mutate(Group = "Hispanic Women"),
  hm_unemployment %>% mutate(Group = "Hispanic Men"),
  hispanic_unemployment %>% mutate(Group = "Hispanic Overall")
) %>%
  mutate(
    Month_Num = as.numeric(gsub("M", "", Period)),
    Date = as.Date(paste(Year, Month_Num, "01", sep = "-"))
  )

# Define color palette for all demographics
unemployment_colors <- c(
  "National Average" = "#000000",
  "Black Women" = "#618825",
  "Black Men" = "#7BA840",
  "Black Overall" = "#96C85B",
  "White Women" = "#052E2A",
  "White Men" = "#0A4A45",
  "White Overall" = "#0F6660",
  "Hispanic Women" = "#4D8AB3",
  "Hispanic Men" = "#6AA3C8",
  "Hispanic Overall" = "#87BCDD"
)

# Prepare violations and industry data
violations_clean <- high_violation_2023 %>%
  select(`Low Wage, High Violation Industries`, `...7`) %>%
  rename(
    industry_label = `Low Wage, High Violation Industries`,
    BackWages = `...7`
  ) %>%
  filter(industry_label != "Low Wage, High Violation Industries") %>%
  mutate(BackWages = as.numeric(gsub("\\$|,", "", BackWages))) %>%
  arrange(desc(BackWages))

top_3_industries <- industry_pop %>%
  filter(sub_occupation_2 == "all") %>%
  filter(
    (occupation == "Service occupations" & sub_occupation_1 == "Healthcare support occupations") |
      (occupation == "Service occupations" & sub_occupation_1 == "Food preparation and serving related occupations") |
      (occupation == "Natural resources, construction, and maintenance occupations" & sub_occupation_1 == "Construction and extraction occupations")
  ) %>%
  mutate(industry_label = case_when(
    sub_occupation_1 == "Healthcare support occupations" ~ "Health Care",
    sub_occupation_1 == "Food preparation and serving related occupations" ~ "Food Services",
    sub_occupation_1 == "Construction and extraction occupations" ~ "Construction",
    TRUE ~ NA_character_
  ))

top3_backwages <- violations_clean %>%
  arrange(desc(BackWages)) %>%
  slice_head(n = 3)

top3_names <- top3_backwages$industry_label

df_treemap_prep <- top_3_industries %>%
  left_join(top3_backwages, by = "industry_label") %>%
  select(
    industry_label, BackWages,
    number_employed_white_per_100,
    number_employed_black_per_100,
    number_employed_asian_per_100,
    number_employed_hispanic_per_100
  ) %>%
  distinct()

df_treemap <- df_treemap_prep %>%
  pivot_longer(
    cols = ends_with("_per_100"),
    names_to = "race",
    values_to = "per_100"
  ) %>%
  mutate(race = case_when(
    race == "number_employed_white_per_100" ~ "White",
    race == "number_employed_black_per_100" ~ "Black",
    race == "number_employed_asian_per_100" ~ "Asian",
    race == "number_employed_hispanic_per_100" ~ "Hispanic",
    TRUE ~ race
  )) %>%
  group_by(industry_label) %>%
  mutate(
    total_per_100 = sum(per_100, na.rm = TRUE),
    race_share = per_100 / total_per_100
  ) %>%
  ungroup() %>%
  mutate(
    tile_area = BackWages * per_100,
    label_text = str_c(
      "Race / Ethnicity: ", race, "\n",
      "Total Population Employed per 100 People: ", per_100
    )
  ) %>%
  mutate(
    industry_label = factor(industry_label, levels = top3_names),
    race = factor(race, levels = c("Black", "White", "Hispanic", "Asian"))
  )

# get race options
race_choices <- sort(unique(median_assets_debts$racecl4))

# get debt categories
debt_types <- c(
  "Credit Card Balances after Last Payment" = "Credit Card Balances after Last Payment",
  "Debt Secured by Primary Residence" = "Debt Secured by Primary Residence",
  "Education Loans" = "Education Loans",
  "Installment Loans" = "Installment Loans",
  "Other Debts" = "Other Debts",
  "Total Debt" = "Total Debt"
)

# UI portion
ui <- fluidPage(
  theme = shinytheme("united"),
  
  # Header
  titlePanel(
    div(
      p("Visualizing Black Wealth and Economic Inequality in the United States ", 
        style = "margin: 5px 0 0 0; color: #7f8c8d; font-size: 16px;")
    )
  ),
  
  # Main content with tabs
  tabsetPanel(
    # Tab 0: Project Overview (Full Page)
    tabPanel(
      "Project Overview",
      div(
        style = "padding: 40px;",
        
        # The Four Posts Section
        div(
          style = "margin-bottom: 50px;",
          h2("Visualizing Black Wealth and Economic Inequality in the United States", style = "color: #2c3e50; font-size: 36px; font-weight: bold; margin-bottom: 30px; text-align: center;"),
          
          p("Since our nation’s founding, control of Black labor has shaped who has access to stable, well-paid work in the United States. Occupational segregation continues to place Black workers in lower-wage and less secure jobs, widening a racial wealth gap that has cost trillions in lost economic potential and billions in unpaid wages, especially for Black women. In the context of recent rollbacks to worker protections, labor enforcement, and DEI initiatives, this interactive dashboard examines how labor policy, wage violations, and economic structures continue to shape Black economic outcomes across the United States.",
            style = "font-size: 16px; color: #555; line-height: 1.8; max-width: 900px; margin: 0 auto 40px auto; text-align: left;"),
          
          fluidRow(
            column(
              6,
              div(
                style = "background: white; padding: 30px; border-left: 5px solid #DD6B20; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); min-height: 220px;",
                h4(tags$b("Debt Burdens"), style = "color: #DD6B20; margin-top: 0;"),
                p("Six Median Debt Metrics by Race (2007–2022)", style = "font-size: 16px; color: #555; line-height: 1.6;"),
                p("Examining how debt burdens differ across racial and ethnic groups.",
                  style = "font-size: 14px; color: #777;")
              )
            ),
            column(
              6,
              div(
                style = "background: white; padding: 30px; border-left: 5px solid #DD6B20; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); min-height: 220px;",
                h4(tags$b("Wage Violations"), style = "color: #DD6B20; margin-top: 0;"),
                p("Back Wages Owed in Low-Wage, High-Violation Industries (2023)", style = "font-size: 16px; color: #555; line-height: 1.6;"),
                p("Analyzing the racial composition of workers in industries with the highest wage violations and back wages owed.",
                  style = "font-size: 14px; color: #777;")
              )
            )
          ),
          
          fluidRow(
            column(
              6,
              div(
                style = "background: white; padding: 30px; border-left: 5px solid #DD6B20; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); min-height: 220px;",
                h4(tags$b("Federal Rollbacks on Unemployment"), style = "color: #DD6B20; margin-top: 0;"),
                p("Unemployment Rates by Sex and Race (2020-2025)", style = "font-size: 16px; color: #555; line-height: 1.6;"),
                p("Tracking unemployment trends across racial and gender groups during federal policy changes.",
                  style = "font-size: 14px; color: #777;")
              )
            ),
            column(
              6,
              div(
                style = "background: white; padding: 30px; border-left: 5px solid #DD6B20; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); min-height: 220px;",
                h4(tags$b("State Policy and the South"), style = "color: #DD6B20; margin-top: 0;"),
                p("Four State Policy Maps (2020 & 2023)", style = "font-size: 16px; color: #555; line-height: 1.6;"),
                p("Mapping black population density and their intersection with minimum wage policies, right-to-work laws, and median income for Black workers.",
                  style = "font-size: 14px; color: #777;")
              )
            )
          )
        ),
        
        # Data Sources Section
        div(
          style = "background: #f8f9fa; padding: 40px; border-radius: 8px; margin-bottom: 40px;",
          h2("Data Sources", style = "color: #2c3e50; font-size: 32px; font-weight: bold; margin-bottom: 25px; text-align: center;"),
          
          div(
            style = "display: flex; justify-content: center;",
            div(
              style = "max-width: 1000px;",
              fluidRow(
                column(
                  4,
                  div(
                    style = "text-align: center;",
                    h4("Black Wealth Data Center", style = "color: #DD6B20; font-size: 16px; font-weight: bold;"),
                    p("Calculations from Bureau of Labor Statistics, Labor Force Statistics from the Current Population Survey, U.S. Census Bureau, U.S. Federal Reserve, Survey of Consumer Finances (SCF)", style = "font-size: 13px; color: #666;")
                  )
                ),
                column(
                  4,
                  div(
                    style = "text-align: center;",
                    h4("U.S. Department of Labor", style = "color: #DD6B20; font-size: 16px; font-weight: bold;"),
                    p("Wage and Hour Division", style = "font-size: 13px; color: #666;")
                  )
                ),
                column(
                  4,
                  div(
                    style = "text-align: center;",
                    h4("Economic Policy Institute", style = "color: #DD6B20; font-size: 16px; font-weight: bold;"),
                    p("Right-to-Work States, National Conference of State Legislatures", style = "font-size: 13px; color: #666;")
                  )
                )
              )
            )
          ),
          div(
            style = "text-align: center; margin-top: 20px;",
            tags$i("Special thanks to the Workforce Policy team at the Joint Center for Political and Economic Studies for the support and opportunity to expand on the Labor Loopholes project.")
          )
        )
      )
    ),
    
    # Tab 1: Interactive Debt Comparison
        tabPanel(
          "Debt Burdens",
          fluidRow(
            column(
              4,
              wellPanel(
                h4("Compare Debt Types"),
                p("Select debt type, race/ethnicity, and time range to explore disparities.", 
                  style = "font-size: 12px; color: #666;"),
                hr(),
                selectInput("metric_from_data",
                  label = h5("Debt Type:"),
                  choices = debt_types,
                  selected = "Education Loans"
                ),
                checkboxGroupInput("race_from_data",
                  label = h5("Race or Ethnicity:"),
                  choices = race_choices,
                  selected = c("Black", "White")
                ),
                sliderInput("year_range",
                  label = h5("Range of Years:"),
                  min = min(median_assets_debts$year, na.rm = TRUE),
                  max = max(median_assets_debts$year, na.rm = TRUE),
                  value = c(min(median_assets_debts$year, na.rm = TRUE), max(median_assets_debts$year, na.rm = TRUE)),
                  step = 3,
                  sep = ""
                )
              )
            ),
            column(
              8,
              h3(textOutput("output_race_years")),
              highchartOutput("ggplot_variable_vs_race", height = "500px"),
              p("Data Source: Black Wealth Data Center calculations from U.S. Federal Reserve, Survey of Consumer Finances (SCF) years from 2007 to 2022. 'Other' group includes Asian, American Indian, Alaska Native, Native Hawaiian, Pacific Islander, and other races.",
                style = "font-size: 12px; color: #666; margin-top: 20px;"
              )
            )
          )
        ),

    # Tab 2: Wage Violations - Stacked Bar
    tabPanel(
      "Wage Violations",
      fluidRow(
        column(
          12,
          h3("Back Wages Owed in Low-Wage, High-Violation Industries (2023)"),
          highchartOutput("violations_bar_hc", height = "500px"),
          p("Data Source: U.S. Department of Labor Wage and Hour Division, Low Wage, High Violation Industries, 2023",
            style = "font-size: 12px; color: #666; margin-top: 10px; text-align: center;"),
          h3("Top 3 Low-Wage, High-Violation Industries by Race (2023)"),
          highchartOutput("stacked_bar_hc", height = "700px"),
          p("Data Source: Black Wealth Data Center calculations from Bureau of Labor Statistics, Labor Force Statistics from the Current Population Survey, 2023. 2023 is the most recent table publication year.",
            style = "font-size: 12px; color: #666; margin-top: 10px; text-align: center;"),
          div(
            style = "background-color: #DD6B2015; border-left: 4px solid #DD6B20; padding: 15px; border-radius: 5px; margin: 20px 0;",
            p(strong("Key Insights"), style = "font-size: 14px; margin-bottom: 10px; color: #2c3e50;"),
            tags$ul(
              style = "font-size: 12px; margin: 0; padding-left: 18px; color: #2c3e50; line-height: 1.6;",
              tags$li("Occupational segregation is defined as the overrepresentation or underrepresentation of a demographic group in a certain occupation or field. As a result of occupational segregation, Black and Latinx workers are overrepresented in dangerous jobs, underpaying jobs, and jobs with few benefits."),
              tags$li("U.S. Department of Labor reveals that Black women lost $42.7 billion in wages compared to white men in 2023, and Hispanic women lost $53.3 billion in wages. These losses are driven entirely by the fact that Black and Hispanic women are concentrated disproportionately in jobs that, on average, pay lower wages than those held by white men.")
            )
          )
        )
      )
    ),

    # Tab 3: Unemployment Rates
    tabPanel(
      "Federal Rollbacks on Unemployment",
      fluidRow(
        column(
          3,
          wellPanel(
            h4("Select Groups to Display"),
            checkboxInput("show_national", "National Average", value = TRUE),
            hr(),
            h5("Unemployment by Race"),
            checkboxGroupInput("show_race", NULL,
              choices = c("Black", "White", "Hispanic"),
              selected = c("Black", "White")
            ),
            hr(),
            h5("Unemployment by Sex"),
            checkboxGroupInput("show_sex", NULL,
              choices = c("Women", "Men", "Overall"),
              selected = c("Women")
            )
          ),
          div(
            style = "background-color: #DD6B2015; border-left: 4px solid #DD6B20; padding: 15px; border-radius: 5px; margin-top: 15px;",
            p(strong("Key Insights"), style = "font-size: 14px; margin-bottom: 10px; color: #2c3e50;"),
            tags$ul(
              style = "font-size: 12px; margin: 0; padding-left: 18px; color: #2c3e50; line-height: 1.6;",
              tags$li("Recent federal rollbacks have weakened labor protections, reshaping how workplace rights are defined and enforced."),
              tags$li("Black women experience disproportionately higher unemployment compared to other demographic groups at 7.5%.")
              )
          )
        ),
        column(
          9,
          h3("Unemployment Rate by Race and Sex (2020-2025)", style = "color: #2c3e50; font-weight: bold; margin-bottom: 15px;"),
          highchartOutput("unemployment_plot_hc", height = "600px"),
          p("Data Source: Bureau of Labor Statistics, Current Population Survey (Seasonally Adjusted Rates for Workers 20 and Older)",
            style = "font-size: 12px; color: #666; margin-top: 20px; text-align: center;"
          )
        )
      )
    ),

    # Tab 6: Education Attainment Map (Moved from Tab 4 position logic) -> Actually it' 6 in code
    # Adding Post 4 Tab before Map

    tabPanel(
      "State Policy and the South",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Select Map", style = "color: #34495e; margin-top: 0;"),
          p("Choose a map to explore different aspects of state labor policy and demographics.",
            style = "font-size: 13px; color: #666; line-height: 1.5;"),
          hr(),
          radioButtons(
            "map_selection",
            NULL,
            choices = c(
              "Black Population Distribution" = "black_pop",
              "Right-to-Work Status" = "rtw",
              "State Minimum Wage Laws" = "min_wage",
              "Black Median Income" = "black_income"
            ),
            selected = "black_pop"
          ),
          hr(),
          uiOutput("map_insights")
        ),
        mainPanel(
          width = 9,
          uiOutput("map_title"),
          plotOutput("selected_map", height = "700px", width = "100%"),
          uiOutput("map_caption")
        )
      )
    ),
    
    # Tab 5: Next Steps
    tabPanel(
      "Next Steps",
      div(
        style = "padding: 40px;",
        p("With additional time and resources, this work could be expanded in the following ways to gain better clarity and granularity of Black economic status:",
          style = "font-size: 17px; color: #555; margin-bottom: 30px;"),
        div(
          style = "background: #fff; border-left: 6px solid #DD6B20; padding: 28px 32px; border-radius: 8px; margin-bottom: 32px; box-shadow: 0 2px 8px rgba(0,0,0,0.07);",
          h3("State-Level Data on Union Activity by Race", style = "color: #DD6B20; font-size: 22px; font-weight: bold; margin-top: 0; margin-bottom: 18px;"),
          tags$ul(
            style = "font-size: 16px; line-height: 1.8; color: #555; margin-bottom: 0;",
            tags$li("Suprisingly, there is a lack of intersectional data on Black union density per state. The U.S. Bureau of Labor Statistics only provides union density by Race on a national scale."),
            tags$li("In understanding union density at the state-level, we could analyze whether higher Black union participation is associated with lower wage violations and median wages across Right-to-Work and non–Right-to-Work states clearly.")
          )
        ),
        div(
          style = "background: #fff; border-left: 6px solid #DD6B20; padding: 28px 32px; border-radius: 8px; margin-bottom: 32px; box-shadow: 0 2px 8px rgba(0,0,0,0.07);",
          h3("Intersectional Data on Federal Rollback Policies", style = "color: #DD6B20; font-size: 22px; font-weight: bold; margin-top: 0; margin-bottom: 18px;"),
          tags$ul(
            style = "font-size: 16px; line-height: 1.8; color: #555; margin-bottom: 0;",
            tags$li("A majority of the effect observed by the federal rollbacks have been seen through unemployment; however, it would be nice to see race and gender splits in this data as well."),
            tags$li("We could use longitudinal analysis to identify short- and long-term economic effects of policy changes across states and demographic groups as time goes on.")
          )
        )
      )
    )
  )  # Close tabsetPanel
)    # Close fluidPage


# Server portion
server <- function(input, output, session) {
  # Tab 1: Interactive debt comparison
  two_race_data <- reactive({
    req(input$race_from_data, input$metric_from_data)
    if (length(input$race_from_data) == 0) {
      return(NULL)
    }

    median_assets_debts %>%
      select(racecl4, year, metric, median, label) %>%
      filter(
        racecl4 %in% input$race_from_data,
        metric == input$metric_from_data,
        year >= min(input$year_range),
        year <= max(input$year_range)
      )
  })

  output$output_race_years <- renderText({
    req(input$race_from_data, input$metric_from_data)
    races_str <- paste(input$race_from_data, collapse = ", ")
    paste(
      races_str, "|",
      input$metric_from_data, "|",
      min(input$year_range), "-", max(input$year_range)
    )
  })

  output$ggplot_variable_vs_race <- renderHighchart({
    two_race_data_output <- two_race_data()
    if (is.null(two_race_data_output) || nrow(two_race_data_output) == 0) {
      return(NULL)
    }

    y_axis_label <- switch(input$metric_from_data,
      "Credit Card Balances after Last Payment" = "Credit Card Balances after Last Payment",
      "Debt Secured by Primary Residence" = "Debt Secured by Primary Residence",
      "Education Loans" = "Education Loans",
      "Installment Loans" = "Installment Loans",
      "Other Debts" = "Other Debts",
      "Total Debt" = "Total Debt",
      input$metric_from_data
    )

    # Define color mapping
    race_colors <- c(
      "Black" = "#618825", 
      "White" = "#052E2A",
      "Hispanic" = "#4D8AB3", 
      "Other" = "#64AFA7"
    )

    # Create highchart manually
    hc <- highchart() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""),
               labels = list(formatter = JS(
                 "function() { 
                   return '$' + Highcharts.numberFormat(this.value, 0, '.', ',');
                 }"
               ))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(
          "function() {
            return '<b>' + this.series.name + '</b><br/>' +
                   'Year: ' + this.x + '<br/>' +
                   'Value: $' + Highcharts.numberFormat(this.y, 0, '.', ',');
          }"
        )
      ) %>%
      hc_legend(align = "right", verticalAlign = "top", layout = "vertical") %>%
      hc_plotOptions(
        series = list(
          marker = list(enabled = TRUE, radius = 4)
        )
      )

    # Add series for each race
    for (race in unique(two_race_data_output$racecl4)) {
      race_data <- two_race_data_output %>%
        filter(racecl4 == race) %>%
        arrange(year)
      
      hc <- hc %>%
        hc_add_series(
          name = race,
          type = "line",
          data = lapply(1:nrow(race_data), function(i) {
            list(x = race_data$year[i], y = race_data$median[i])
          }),
          color = race_colors[[race]],
          lineWidth = 3
        )
    }
    
    hc
  })

  # Tab 3: Unemployment plot

  # Reactive data for Highcharts
  filtered_unemployment <- reactive({
    selected_groups <- c()

    if (input$show_national) {
      selected_groups <- c(selected_groups, "National Average")
    }

    # Construct demographic groups based on checkboxes
    races <- input$show_race
    sexes <- input$show_sex

    if (length(races) > 0 && length(sexes) > 0) {
      for (r in races) {
        for (s in sexes) {
          # Construct group name matching the data
          group_name <- paste(r, s)
          selected_groups <- c(selected_groups, group_name)
        }
      }
    }

    all_unemployment_data %>%
      filter(Group %in% selected_groups)
  })

  output$unemployment_plot_hc <- renderHighchart({
    data_to_plot <- filtered_unemployment()

    # validate(need(nrow(data_to_plot) > 0, "No data selected"))
    if (nrow(data_to_plot) == 0) {
      return(highchart())
    }

    # Ensure colors match the groups in the data
    # We join the colors to the data or extract them in the correct order
    # hchart groups by the 'group' aesthetic. The order of series is typically order of appearance.

    # Force Group to be a factor with levels in the order of unemployment_colors
    data_to_plot$Group <- factor(data_to_plot$Group, levels = names(unemployment_colors))
    # Only keep colors for groups present in the data, in the correct order
    color_map <- unemployment_colors[levels(droplevels(data_to_plot$Group))]
    color_map[is.na(color_map)] <- "#000000"

    hchart(data_to_plot, "line", hcaes(x = Date, y = Value, group = Group)) %>%
      hc_colors(color_map) %>%
      hc_xAxis(title = list(text = NULL), type = "datetime") %>%
      hc_yAxis(
        title = list(text = "Unemployment Rate (%)"),
        labels = list(format = "{value}%")
      ) %>%
      hc_tooltip(
        shared = TRUE,
        xDateFormat = "%B %Y",
        pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>{point.y}%</b><br/>"
      ) %>%
      hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal") %>%
      hc_add_theme(hc_theme_smpl())
  })

  # Tab 4: Treemap plot
  output$treemap_plot <- renderPlot({
    ggplot(df_treemap, aes(area = tile_area, fill = race, subgroup = industry_label, text = label_text)) +
      geom_treemap(colour = "white", size = 2) +
      geom_treemap_subgroup_border(colour = "white", size = 4) +
      geom_treemap_text(aes(label = paste0(race, "\n", scales::percent(race_share, accuracy = 0.1))),
        colour = "white",
        place = "bottomleft", grow = FALSE,
        size = 10,
        min.size = 2
      ) +
      geom_treemap_subgroup_text(
        label = paste0(df_treemap$industry_label, "\n", scales::dollar(df_treemap$BackWages, scale = 1e-6, suffix = "M")),
        place = "topleft",
        colour = "white", fontface = "bold",
        reflow = TRUE,
        size = 16,
        min.size = 5
      ) +
      scale_fill_manual(values = c(
        "Black" = "#618825", "White" = "#052E2A",
        "Hispanic" = "#4D8AB3", "Asian" = "#64AFA7"
      )) +
      labs(fill = "Race / Ethnicity") +
      theme_minimal() +
      theme(
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold")
      )
  })

  # Tab 5: Top 3 Low Wage, High Violation Industries
  output$stacked_bar_hc <- renderHighchart({
    # Prepare data for plotting: calculate absolute wage value per race
    data_for_stack <- df_treemap %>%
      mutate(calculated_wage = BackWages * race_share)

    hchart(data_for_stack, "column", hcaes(x = industry_label, y = calculated_wage, group = race)) %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      hc_yAxis(
        title = list(text = ""),
        labels = list(format = "${value:,.0f}")
      ) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_tooltip(pointFormat = "<b>{series.name}</b><br>Amount: ${point.y:,.0f}<br>Share: {point.percentage:.1f}%") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c("#618825", "#052E2A", "#4D8AB3", "#64AFA7"))
  })

  # Tab 4: Dynamic Map Selection
  output$selected_map <- renderPlot({
    req(input$map_selection)
    
    if (input$map_selection == "min_wage") {
      ggplot(min_wage_map_data) +
        geom_sf(aes(fill = wage_category), size = 0.25, color = "white") +
        scale_fill_manual(
          values = c(
            "Federal ($7.25 or less)" = "#00441b",
            "$7.26 - $9.99" = "#238b45",
            "$10.00 - $11.99" = "#74c476",
            "$12.00 or Higher" = "#c7e9c0"
          ),
          name = "Minimum Wage"
        ) +
        theme_minimal(base_size = 14, base_family = "sans") +
        theme(
          legend.title = element_text(face = "bold", size = 13),
          legend.text = element_text(size = 11),
          legend.position = "right",
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
        )
    } else if (input$map_selection == "rtw") {
      ggplot(rtw_map_data) +
        geom_sf(aes(fill = RTW_Status), size = 0.25, color = "white") +
        scale_fill_manual(
          values = c(
            "Non-RTW" = "#2c7bb6",
            "RTW" = "#d73027"
          ),
          name = "RTW Status"
        ) +
        theme_minimal(base_size = 14, base_family = "sans") +
        theme(
          legend.title = element_text(face = "bold", size = 13),
          legend.text = element_text(size = 11),
          legend.position = "right",
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
        )
    } else if (input$map_selection == "black_pop") {
      ggplot(blk_quartile_map_data) +
        geom_sf(aes(fill = quartiles_pct_black_alone), size = 0.25, color = "white") +
        scale_fill_manual(
          values = c(
            "Top 25% of geographies" = "#DD6B20",
            "51-75% of geographies" = "#FD8D3C",
            "26-50% of geographies" = "#FDBE85",
            "Bottom 25% of geographies" = "#FEE5D9"
          ),
          name = "Black Population\nQuartile",
          na.value = "grey90"
        ) +
        theme_minimal(base_size = 14, base_family = "sans") +
        theme(
          legend.title = element_text(face = "bold", size = 13),
          legend.text = element_text(size = 11),
          legend.position = "right",
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
        )
    } else if (input$map_selection == "black_income") {
      ggplot(blk_income_map_data) +
        geom_sf(aes(fill = median_income_black_alone), size = 0.25, color = "white") +
        scale_fill_gradient(
          low = "#DAE7F4",
          high = "#0D6DD0",
          labels = scales::dollar_format(),
          name = "Median Income"
        ) +
        theme_minimal(base_size = 14, base_family = "sans") +
        theme(
          legend.title = element_text(face = "bold", size = 13),
          legend.text = element_text(size = 11),
          legend.position = "right",
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
        )
    }
  })
  
  # Dynamic title based on map selection
  output$map_title <- renderUI({
    req(input$map_selection)
    
    title_info <- if (input$map_selection == "min_wage") {
      list(
        title = "State Minimum Wage Laws",
        subtitle = "Lighter colors indicate higher minimum wage, darker colors indicate lower than the Federal miminum wage of $7.25."
      )
    } else if (input$map_selection == "rtw") {
      list(
        title = "Right-to-Work States in the United States",
        subtitle = "States with statutory restrictions on workers' collective bargaining rights"
      )
    } else if (input$map_selection == "black_pop") {
      list(
        title = "Black Population Distribution by State",
        subtitle = "States ranked by percentage of Black residents (quartiles)"
      )
    } else if (input$map_selection == "black_income") {
      list(
        title = "Black Median Income by State",
        subtitle = "State-level median household income for Black residents"
      )
    }
    
    div(
      style = "margin-bottom: 10px;",
      h3(title_info$title, style = "color: #2c3e50; font-weight: bold; margin-bottom: 5px; text-align: center;"),
      p(title_info$subtitle, style = "color: #666; font-size: 14px; font-style: italic; text-align: center; margin-top: 0; margin-bottom: 5px;")
    )
  })
  
  # Dynamic caption based on map selection
  output$map_caption <- renderUI({
    req(input$map_selection)
    
    caption_text <- if (input$map_selection == "min_wage") {
      "Data Source: U.S. Department of Labor State Minimum Wage Rates, 2020"
    } else if (input$map_selection == "rtw") {
      "Data Source: Economic Policy Institute, National Conference of State Legislatures, 2024| Note: Michigan repealed RTW in 2024"
    } else if (input$map_selection == "black_pop") {
      "Data Source: Black Wealth Data Center calculations from U.S. Census Bureau, 2023."
    } else if (input$map_selection == "black_income") {
      "Data Source: Black Wealth Data Center calculations from U.S. Census Bureau, 2023."
    }
    
    p(caption_text,
      style = "font-size: 12px; color: #666; margin-top: 20px; text-align: center;")
  })
  
  # Dynamic insights panel based on map selection
  output$map_insights <- renderUI({
    req(input$map_selection)
    
    insight_content <- if (input$map_selection == "black_pop") {
      list(
        title = "Key Insights:",
        color = "#DD6B20",
        points = c(
          "Southern states have the highest concentrations of Black residents, with Mississippi, Louisiana, and Georgia in the top quartile.",
          "States with larger Black populations often have different labor market dynamics and policy impacts.",
          "Understanding population distribution is essential for assessing the reach and impact of labor policies on Black workers."
        )
      )
    } else if (input$map_selection == "rtw") {
      list(
        title = "Key Insights:",
        color = "#525150",
        points = c(
          "'Right-to-work' refers to state laws in the U.S. that guarantee an employee's right to work for an employer without being compelled to join a labor union or pay union dues as a condition of employment.",
          "RTW states typically have lower union membership rates and weaker worker bargaining power, affecting wages and benefits.",
          "27 states currently have Right-to-Work laws that restrict union organizing and collective bargaining, mostly gathered in the Southern region of the U.S.",
          "Michigan became the first state in nearly 60 years to repeal RTW laws in 2024, restoring worker protections."
        )
      )
    } else if (input$map_selection == "min_wage") {
      list(
        title = "Key Insights:",
        color = "#74c476",
        points = c(
          "21 states still maintain the federal minimum wage of $7.25, unchanged since 2009.",
          "States with higher minimum wages (dark green) tend to have stronger worker protections and lower poverty rates.",
          "Many states with federal minimum wage overlap with Right-to-Work states, compounding challenges for low-wage workers."
        )
      )
    } else if (input$map_selection == "black_income") {
      list(
        title = "Key Insights:",
        color = "#0D6DD0",
        points = c(
          "Significant income disparities exist across states, with median incomes for Black households ranging from under $35K to over $60K.",
          "States with higher minimum wages and stronger labor protections tend to have higher median incomes for Black workers.",
          "Geographic patterns show lower incomes in states with weaker labor policies, highlighting the intersection of policy and economic outcomes."
        )
      )
    }
    
    div(
      style = paste0("background-color: ", insight_content$color, "15; border-left: 4px solid ", 
                     insight_content$color, "; padding: 12px; border-radius: 5px; margin-top: 0px;"),
      p(strong(insight_content$title), style = "font-size: 13px; margin-bottom: 10px; color: #2c3e50;"),
      tags$ul(
        style = "font-size: 11px; margin: 0; padding-left: 18px; color: #2c3e50; line-height: 1.6;",
        lapply(insight_content$points, function(point) {
          tags$li(point)
        })
      )
    )
  })

  # Tab 5: Low Wage, High Violation Industries
  output$violations_bar_hc <- renderHighchart({
    violations_clean_sorted <- violations_clean %>%
      arrange(desc(BackWages)) %>%
      mutate(color = case_when(
        industry_label %in% c("Construction", "Health Care", "Food Services") ~ "#618825",
        TRUE ~ "#000000"
      ))

    hchart(violations_clean_sorted, "bar", hcaes(x = industry_label, y = BackWages, color = color)) %>%
      hc_xAxis(title = list(text = ""), categories = violations_clean_sorted$industry_label) %>%
      hc_yAxis(
        title = list(text = ""),
        labels = list(
          formatter = JS(
            "function() {
              return '$' + Highcharts.numberFormat(this.value, 0, '.', ',');
            }"
          )
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(
          "function() {
            return '<b>' + this.point.industry_label + '</b><br/>' +
                   'Back Wages: <b>$' + Highcharts.numberFormat(this.y, 0, '.', ',') + '</b>';
          }"
        )
      ) %>%
      hc_add_theme(hc_theme_smpl())
  })

  # Tab 6: Education attainment map
  output$edu_map_plot <- renderPlot({
    ggplot(degree_map) +
      geom_sf(aes(fill = fct_relevel(
        `quartiles_pct_bach_degree_or_higher`,
        "Top 25% of geographies",
        "51-75% of geographies",
        "26-50% of geographies",
        "Bottom 25% of geographies"
      )), size = 0.25, color = "lightgrey") +
      scale_fill_brewer(
        palette = "YlOrBr",
        direction = -1,
        name = "Bachelor's Degree or Higher",
        labels = c(
          "Top 25% (Most Educated)",
          "51–75% (Above Average)",
          "26–50% (Below Average)",
          "Bottom 25% (Least Educated)"
        )
      ) +
      theme_minimal(base_size = 14, base_family = "sans") +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      labs(fill = "")
  })
}

# Run App
shinyApp(ui = ui, server = server)