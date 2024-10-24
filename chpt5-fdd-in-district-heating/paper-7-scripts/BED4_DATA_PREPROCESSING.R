# FDD PAPER - DATA PREPROCESSING

# -------------------------------------------------------------------------
### LIBRARY OF PACKAGES
# -------------------------------------------------------------------------

library(data.table)
library(lubridate)
library(dplyr)
library(caTools)
library(scales)
library(forcats)
library(xts)
library(imputeTS)
library(openxlsx)
library(readxl)




# -------------------------------------------------------------------------
### SET WORKING DIRECTORY
# -------------------------------------------------------------------------

setwd("C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis")  # Set working (main) directory
Sys.setenv(LANG = "en")                                                                                                   # Set system's language to English
Sys.setlocale("LC_ALL", "English")



# -------------------------------------------------------------------------
### LOAD DATASETS
# -------------------------------------------------------------------------

# Load the dataset from the specified file path.
dt_SHM_Hour_1 <- fread(file = "~/Analysis - BED4/shm_processed/Raw SHM data/Customer_contacts.csv")
dt_SHM_Hour_2 <- fread(file = "~/Analysis - BED4/shm_processed/Raw SHM data/Error_resolved.csv")
dt_SHM_Hour_3 <- fread(file = "~/Analysis - BED4/shm_processed/Raw SHM data/No_action.csv")

dt_SHM_Hour <- rbind(dt_SHM_Hour_1, dt_SHM_Hour_2, dt_SHM_Hour_3)

# Load the faults reports - excel
fdd_reports <- read_excel("~/Analysis - BED4/shm_processed/Raw SHM data/FDD_reports.xlsx", sheet = "Report")



# -------------------------------------------------------------------------
### IMPROVE DATA TABLE OF THE FAULT REPORTS
# -------------------------------------------------------------------------

df_fdd_reports <- as.data.frame(fdd_reports) %>% 
  dplyr::select(-Failure_type, -Faults, -Fault_Labels) %>% 
  filter(is.na(Duplicates) & is.na(Missing))

setDT(df_fdd_reports)

# Convert time column of the dates to Danish time zone
df_fdd_reports[, Intervention_date := as.Date(Intervention_date, tz = "Europe/Copenhagen")]
df_fdd_reports[, Failure_start_date := as.Date(Failure_start_date, tz = "Europe/Copenhagen")]
df_fdd_reports[, Failure_end_date := as.Date(Failure_end_date, tz = "Europe/Copenhagen")]
df_fdd_reports[, Visit_date := as.Date(Visit_date, tz = "Europe/Copenhagen")]

df_fdd_reports <- as.data.frame(df_fdd_reports)



# -------------------------------------------------------------------------
### FILTER MEASUREMENTS FROM 2022
# -------------------------------------------------------------------------

# Convert time column of the measurements to Danish time zone
dt_SHM_Hour[, time_rounded := with_tz(time_rounded, tzone = "Europe/Copenhagen")]

# Filter measurements from 2022 on
dt_SHM_Hour <- dt_SHM_Hour[time_rounded > as.POSIXct("2021-01-01 00:00:00", tz = "Europe/Copenhagen")]


# -------------------------------------------------------------------------
### IMPROVE DATA TABLE OF THE MEASUREMENTS
# -------------------------------------------------------------------------

# Remove extra columns
dt_SHM_Hour[, c("id",
                "customer_id",
                "period",
                "heat_energy_kwh",
                "heat_energy_kwh_spms",
                "volume_m3",
                "flow_x_temp_supply_m3c",
                "flow_x_temp_return_m3c",
                "was_missing",
                "demand_spms") := NULL]

# Change name of columns
setnames(dt_SHM_Hour,
         old = c("time_rounded",
                 "heat_meter_id",
                 "heat_energy_kwh_demand",
                 "volume_m3_demand",
                 "flow_x_temp_supply_m3c_demand",
                 "flow_x_temp_return_m3c_demand"),
         new = c("Time",
                 "heat_meter_id",
                 "Energy",
                 "Volume",
                 "Vol_x_Tsupply",
                 "Vol_x_Treturn"))


dt_SHM_Hour[, `:=` (Date = as.Date(Time, tz = "Europe/Copenhagen"))]
# dt_SHM_Hour[, Date := as.POSIXct(as.Date(Time, tz = "Europe/Copenhagen"))]




# -------------------------------------------------------------------------
### CONVERT HOURLY MEASUREMENTS TO DAILY
# -------------------------------------------------------------------------

dt_SHM_Daily <- dt_SHM_Hour[, .(Energy = sum(Energy),
                                Volume = sum(Volume),
                                Vol_x_Tsupply = sum(Vol_x_Tsupply),
                                Vol_x_Treturn = sum(Vol_x_Treturn)),
                            by = .(heat_meter_id, Date)
][, `:=`(Tsupply = Vol_x_Tsupply/Volume,
         Treturn = Vol_x_Treturn/Volume)]

dt_SHM_Daily[, `:=` (Temp_difference = Tsupply - Treturn)]


# Remove extra columns
dt_SHM_Daily[, c("Vol_x_Tsupply",
                 "Vol_x_Treturn") := NULL]


# -------------------------------------------------------------------------
### DELETE IMPOSSIBLE T_SUPPLY AND t_RETURN VALUES
# -------------------------------------------------------------------------

# Energy conditions
dt_SHM_Daily[, Energy := ifelse(Volume == 0, 0, Energy)]


# Volume conditions
dt_SHM_Daily[, Volume := ifelse(Volume < 0, NA, Volume)]
dt_SHM_Daily[, Tsupply := ifelse(Volume < 0, NA, Tsupply)]
dt_SHM_Daily[, Treturn := ifelse(Volume < 0, NA, Treturn)]
dt_SHM_Daily[, Temp_difference := ifelse(Volume < 0, NA, Temp_difference)]


# Temperature difference conditions
dt_SHM_Daily[, Tsupply := ifelse(Temp_difference < 1, NA, Tsupply)]
dt_SHM_Daily[, Treturn := ifelse(Temp_difference < 1, NA, Treturn)]
dt_SHM_Daily[, Temp_difference := ifelse(Temp_difference < 1, NA, Temp_difference)]


# Supply temperature conditions
dt_SHM_Daily[, Tsupply := ifelse(Treturn < 1, NA, Tsupply)]
dt_SHM_Daily[, Tsupply := ifelse(Treturn > 90, NA, Tsupply)]
dt_SHM_Daily[, Treturn := ifelse(Treturn < 1, NA, Treturn)]
dt_SHM_Daily[, Treturn := ifelse(Treturn > 90, NA, Treturn)]
dt_SHM_Daily[, Temp_difference := ifelse(Treturn < 1, NA, Temp_difference)]
dt_SHM_Daily[, Temp_difference := ifelse(Treturn > 90, NA, Temp_difference)]


# Return temperature conditions
dt_SHM_Daily[, Tsupply := ifelse(Tsupply < 1, NA, Tsupply)]
dt_SHM_Daily[, Tsupply := ifelse(Tsupply > 90, NA, Tsupply)]
dt_SHM_Daily[, Treturn := ifelse(Tsupply < 1, NA, Treturn)]
dt_SHM_Daily[, Treturn := ifelse(Tsupply > 90, NA, Treturn)]
dt_SHM_Daily[, Temp_difference := ifelse(Tsupply < 1, NA, Temp_difference)]
dt_SHM_Daily[, Temp_difference := ifelse(Tsupply > 90, NA, Temp_difference)]

# Other conditions
dt_SHM_Daily[sapply(dt_SHM_Daily, is.infinite)] <- NA
dt_SHM_Daily[sapply(dt_SHM_Daily, is.nan)] <- NA



# Convert your dataframe to a data.table if it's not already
# setDT(df_90.days)

# Identify the heat_meter_ids with any NA in the specified columns
ids_with_na <- dt_SHM_Daily[, if(any(is.na(Treturn) | is.na(Tsupply) | is.na(Temp_difference) | is.na(Volume) | is.na(Energy))) .(heat_meter_id), by = heat_meter_id]$heat_meter_id


# Filter out the rows with those heat_meter_ids
dt_SHM_Daily <- dt_SHM_Daily[!heat_meter_id %in% ids_with_na]



# -------------------------------------------------------------------------
### COMBINE DATASETS
# -------------------------------------------------------------------------

df_SHM_Daily <- as.data.frame(dt_SHM_Daily)
                            
df_SHM_Daily <- left_join(df_SHM_Daily, df_fdd_reports, by = "heat_meter_id") %>% 
  arrange(heat_meter_id, Date)



# -------------------------------------------------------------------------
### DATA FILTERING (NOISE SMOOTHING)
# -------------------------------------------------------------------------

# Convert the 'dt_SHM_Daily' dataframe into a data table for faster manipulation and efficient memory usage.
dt_SHM_Daily = data.table(df_SHM_Daily)

# Calculate 7-day moving average for Treturn, Volume, and Energy
dt_SHM_Daily[, Treturn_MA := rollapply(Treturn, width = 5, FUN = mean, fill = NA, align = "right"), by = heat_meter_id]
dt_SHM_Daily[, Volume_MA := rollapply(Volume, width = 5, FUN = mean, fill = NA, align = "right"), by = heat_meter_id]
dt_SHM_Daily[, Energy_MA := rollapply(Energy, width = 5, FUN = mean, fill = NA, align = "right"), by = heat_meter_id]




# -------------------------------------------------------------------------
### TIME FRAME REDUCTION - 90 DAYS BEFORE INTERVENTATION DATE
# -------------------------------------------------------------------------

# Split the 'dt_SHM_Daily' data table into a list of smaller data tables based on each unique 'heat_meter_id'.
# This operation groups the data by 'heat_meter_id', facilitating individual operations on each group.
dt_split <- split(dt_SHM_Daily, dt_SHM_Daily$heat_meter_id)

# Define a function 'filter_days' to process and filter data frames for a specific time frame.
filter_days <- function(df){
  # Filter the data frame to only include rows where the 'Date' is less than or equal to a predefined 'timestamp'.
  # This step assumes that 'timestamp' is defined elsewhere in your environment as the upper date limit.
  df <- df %>% filter(Date >= Start & Date <= End) %>% 
    arrange(Date)
  
  # Add a new column 'Row_number' to the data frame, which is a sequence from 1 to the number of rows in the data frame.
  # This can help in further indexing or identifying rows after filtering.
  df$Row_number <- 1:nrow(df)
  
  # Return the modified data frame with the added 'Row_number' column and filtered rows.
  return(df)
}


# Apply the 'filter_days' function to each data table in 'dt_split'.
# This step filters and transforms each group of data based on the specified 'heat_meter_id'.
dt_split <- lapply(dt_split, filter_days)

# Combine all the individually processed data tables back into one large data table.
# This reassembles the split and individually filtered groups into a single cohesive data structure.
dt_90.days <- rbindlist(dt_split)



# -------------------------------------------------------------------------
### MEASUREMENTS Z-NORMALIZATION
# -------------------------------------------------------------------------


# Max-min normalization
min_max_normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# Normalize the measurements
dt_90.days[, Treturn_scaled := scale(smooth(Treturn)), by = heat_meter_id]
dt_90.days[, Volume_scaled := scale(smooth(Volume)), by = heat_meter_id]
dt_90.days[, Energy_scaled := scale(smooth(Energy)), by = heat_meter_id]




# -------------------------------------------------------------------------
### FILTER METERS REGARDING SPECIFIC CONDITIONS
# -------------------------------------------------------------------------

df_90.days <- as.data.frame(dt_90.days)

df_90.days <- df_90.days %>%
  filter(
    Contact_type == "Visit"  &
      Assessment == "Proven" &
      !(Fault_category == "No details" |
          Fault_category == "Miscellaneous")
    )


# Remove meters with less than 90 days measurements
df_90.days <- df_90.days %>%
  filter(!is.na(Start))



# -------------------------------------------------------------------------
### COUNT REPORTS THAT HAVE MEASUREMENTS
# -------------------------------------------------------------------------

Meter_IDs = length(unique(df_90.days$heat_meter_id))


# -------------------------------------------------------------------------
### CONVERT METER IDS TO BETTER NUMBERS (INSTEAD OF HUGE STRINGS)
# -------------------------------------------------------------------------

df_90.days$meter_id = as.numeric(as.factor(df_90.days$heat_meter_id))


# -------------------------------------------------------------------------
### SAVE DATASET
# -------------------------------------------------------------------------

dt_SHM_segmented <- df_90.days

ids_preprocessed <- unique(dt_SHM_segmented$heat_meter_id)

dt_SHM_preprocessed <- dt_SHM_Daily[heat_meter_id %in% ids_preprocessed]

dt_SHM_preprocessed$meter_id = as.numeric(as.factor(dt_SHM_preprocessed$heat_meter_id))


save(dt_SHM_segmented, file = "SHM_processed_BED4.rda")
save(dt_SHM_preprocessed, file = "SHM_processed_BED4_full.rda")




