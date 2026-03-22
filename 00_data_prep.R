# Data Preparation

library(tidyverse)
library(lubridate)

# Data Catalog
dir.create("data", showWarnings = FALSE)


# 1. Download Zillow ZHVI (incl. County-level, All Homes, Smoothed, Seasonally Adjusted)


zhvi_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

zhvi_raw <- read_csv(zhvi_url, show_col_types = FALSE)

# Transform Date to format "YYYY-MM-DD"
date_cols <- names(zhvi_raw)[grepl("^\\d{4}-\\d{2}-\\d{2}$", names(zhvi_raw))]

zhvi_long <- zhvi_raw %>%
  select(RegionID, RegionName, StateCodeFIPS, MunicipalCodeFIPS, State, StateName,
         all_of(date_cols)) %>%
  # Construct 5-digit FIPS Code
  mutate(
    fips = sprintf("%02d%03d", as.integer(StateCodeFIPS), as.integer(MunicipalCodeFIPS))
  ) %>%
  pivot_longer(
    cols = all_of(date_cols),
    names_to = "date",
    values_to = "zhvi"
  ) %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(zhvi)) %>%
  # Keep 2000 and after

  filter(date >= ymd("2000-01-01")) %>%
  select(fips, county = RegionName, state = StateName, date, zhvi)


# 2. Download FEMA Catastrophe Announcement


fema_url <- "https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries.csv"

fema_raw <- read_csv(fema_url, show_col_types = FALSE)

# Filter Hurricane and Major Disaster
hurricanes <- fema_raw %>%
  filter(
    incidentType == "Hurricane",
    declarationType == "DR"  # DR = Major Disaster Declaration
  ) %>%
  mutate(
    fips = sprintf("%02d%03d", as.integer(fipsStateCode), as.integer(fipsCountyCode)),
    declaration_date = as.Date(declarationDate),
    begin_date = as.Date(incidentBeginDate),
    end_date = as.Date(incidentEndDate)
  ) %>%
  filter(begin_date >= ymd("2000-01-01")) %>%
  # Drop Duplication
  select(fips, disaster_number = disasterNumber, disaster_name = declarationTitle,
         declaration_date, begin_date, end_date) %>%
  distinct(fips, disaster_number, .keep_all = TRUE)

cat("Sample：", nrow(hurricanes), "Rows\n")

# 3. Find Common Samples including both ZHVI and Hurricane

zhvi_counties <- zhvi_long %>% distinct(fips)
hurricane_counties <- hurricanes %>% distinct(fips)

common_fips <- inner_join(zhvi_counties, hurricane_counties, by = "fips")

cat("Common Sample：", nrow(common_fips), "\n")

# Filter Sample
zhvi_final <- zhvi_long %>%
  semi_join(common_fips, by = "fips")

hurricanes_final <- hurricanes %>%
  semi_join(common_fips, by = "fips")


# 4. Aggregate Data
county_info <- zhvi_final %>%
  group_by(fips, county, state) %>%
  summarise(
    date_min = min(date),
    date_max = max(date),
    n_months = n(),
    .groups = "drop"
  ) %>%
  left_join(
    hurricanes_final %>%
      group_by(fips) %>%
      summarise(
        n_hurricanes = n_distinct(disaster_number),
        hurricane_names = paste(unique(disaster_name), collapse = "; "),
        .groups = "drop"
      ),
    by = "fips"
  )



# 5. Save
write_csv(zhvi_final, "data/zhvi_clean.csv")
write_csv(hurricanes_final, "data/hurricanes_clean.csv")
write_csv(county_info, "data/county_info.csv")

cat("  - zhvi_clean.csv (", nrow(zhvi_final), " Rows)\n")
cat("  - hurricanes_clean.csv (", nrow(hurricanes_final), " Rows)\n")
cat("  - county_info.csv (", nrow(county_info), " Rows)\n")
cat("\n You can now run the Shiny app \n")
