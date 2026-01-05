# Data Viz Final Project: Black Economic Inequality Shiny App

## Overview
This is my final project for PPOL 5202 Data Visualization for Data Science, taught by the brilliant professor, Alex Lundry. This is an interactive data visualization dashboard built with R and Shiny. I previously worked as a workforce policy intern for the Joint Center for Political and Economic Studies, on the Labor Loopholes project. This work inspired me to explore various datasets related to economic and social indicators that affect the Black labor force in the United States, including minimum wage, right-to-work laws, debt, median income, and more. I did publish it here, please take a look here: [jw2293.shinyapps.io/Final_Project](https://jw2293.shinyapps.io/Final_Project/)

## Features
- Interactive maps and plots for key indicators
- Data filtering and selection tools
- Clean, modern UI with custom color schemes using Highcharts
- Key Insight for exploration and understanding


## Folder Structure
```
Final_Project/
├── app.R                               # Main Shiny app 
├── README.md                           # This file 
├── Final_Project_Proposal.docx         # Project Proposal Outline document (project rationale)
├── static_plots.Rmd                    # R markdown of all static plots before implementing interactive components in Shiny
├── .Rhistory
├── rsconnect/
├── data/
│   ├── debt/
│   │   └── median_assets_debt.csv      # Data Source:[Black Wealth Data Center](https://blackwealthdata.org/sign-up/accounts?gad_source=1&gad_campaignid=21453939514&gbraid=0AAAAAp3cdIGlOGxp7AHou_S2PQAYfvMMS&gclid=CjwKCAiAmKnKBhBrEiwAaqAnZ9VM_JmWc-rMy4iGX-sexiyX3pfByJrrkD9OEniC3XFbo8T60hC_8RoCOz8QAvD_BwE) 
│   ├── maps/
│   │   ├── minimum_wage_data.csv       # Data Source:[U.S. Department of Labor's table of minimum wage by state](https://www.dol.gov/whd/state/stateMinWageHis.htm) 
│   │   └── pop_by_state.csv            # Data Source: [Black Wealth Data Center](https://blackwealthdata.org/sign-up/accounts?gad_source=1&gad_campaignid=21453939514&gbraid=0AAAAAp3cdIGlOGxp7AHou_S2PQAYfvMMS&gclid=CjwKCAiAmKnKBhBrEiwAaqAnZ9VM_JmWc-rMy4iGX-sexiyX3pfByJrrkD9OEniC3XFbo8T60hC_8RoCOz8QAvD_BwE) 
│   ├── unemployment/                   # Data Source: [U.S Bureau of Labor Statistics, Seasonally Adjusted Unemployment Rate 20 yrs. over by race and sex](https://data.bls.gov/dataQuery/find?q=seasonally+adjusted+unemployment+)
│   │   ├── avg_unemployment.csv        
│   │   ├── black_unemployment.csv
│   │   ├── bm_unemployment.csv
│   │   ├── bw_unemployment.csv
│   │   ├── hispanic_unemployment.csv
│   │   ├── hm_unemployment.csv
│   │   ├── hw_unemployment.csv
│   │   ├── unemployment_by_reason.csv
│   │   ├── white_unemployment.csv
│   │   ├── wm_unemployment.csv
│   │   └── ww_unemployment.csv
│   └── wages/
│       ├── industry_pop.csv            # Data Source: [Black Wealth Data Center](https://blackwealthdata.org/sign-up/accounts?gad_source=1&gad_campaignid=21453939514&gbraid=0AAAAAp3cdIGlOGxp7AHou_S2PQAYfvMMS&gclid=CjwKCAiAmKnKBhBrEiwAaqAnZ9VM_JmWc-rMy4iGX-sexiyX3pfByJrrkD9OEniC3XFbo8T60hC_8RoCOz8QAvD_BwE)
│       └── low-wage-high-violation.csv # Data Source: [U.S. Department of Labor, Wage and Hour Division](https://www.dol.gov/agencies/whd/data/charts/low-wage-high-violation-industries)
└── ...
```

## Getting Started
1. **Install R and RStudio** (if not already installed)
2. **Install required R packages:**
   - shiny
   - shinythemes
   - tidyverse
   - plotly
   - treemapify
   - sf
   - scales
   - highcharter
   - (Install with `install.packages("<package>")`)
3. **Run the app:**
   - Open `app.R` in RStudio
   - Click "Run App" or use `shiny::runApp()`

## Data Sources
- All data files are located in the `data/` subfolders, organized by topic.
- Citation is on each plot in the app, as well. 

## Deployment
- The app is ready for deployment on [jw2293.shinyapps.io/Final_Project](https://jw2293.shinyapps.io/Final_Project/)
- Ensure all data files are included and referenced with relative paths provided. 

## Contributing
Pull requests and suggestions are welcome! Please open an issue or submit a PR for improvements.

## License
This project is for educational/exploratory purposes. See individual data sources for their respective licenses.

## Author
Joya Wheatfall-Melvin

## Acknowledgements 
Special thanks to the Joint Center for Political and Economic Studies for allowing me to expand on the Labor Loopholes project. 

