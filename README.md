# US-Hurricane-Data-Dashboard
This is my project of UBC-ECON573 Environmental Economics
An interactive Shiny dashboard that visualizes how county-level home values evolve after hurricane strikes in the United States.

**Dashboard:** [louie20021016.shinyapps.io/econ_573]([https://louie20021016.shinyapps.io/econ_573](https://louie20021016.shinyapps.io/econ_573/))

## Features

- Interactive map showing all U.S. counties with both Zillow ZHVI coverage and at least one FEMA hurricane disaster declaration since 2000
- Counties color-coded by number of hurricane declarations
- Click any county to view its full ZHVI time series
- Red shaded regions mark the one-year period following each hurricane strike
- Hover tooltips showing exact date and home value

## Data Sources

- **Home values:** [Zillow Home Value Index (ZHVI)](https://www.zillow.com/research/data/) — county-level monthly home values
- **Hurricane declarations:** [FEMA Disaster Declarations](https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2) — major disaster declarations filtered to hurricane incidents since 2000

## Project Structure

```
├── app.R                  # Shiny app (UI + server)
├── data_prep.R            # Data cleaning and joining script
└── data/
    ├── zhvi_clean.csv     # Cleaned Zillow ZHVI data
    ├── hurricanes_clean.csv  # Cleaned FEMA hurricane declarations
    └── county_info.csv    # County-level summary info
```

## Running Locally

1. Clone this repository
2. Install required R packages:
```r
install.packages(c("shiny", "tidyverse", "lubridate", "leaflet",
                   "sf", "tigris", "plotly"))
```
3. Run the data preparation script (if raw data is not included):
```r
source("data_prep.R")
```
4. Launch the app:
```r
shiny::runApp("app.R")
```
