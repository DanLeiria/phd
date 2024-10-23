# FDD PAPER - DATA PREPROCESSING - BED4 DATASET

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
dt_SHM_Hour_1 <- fread(file = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis - BED4/shm_processed/Raw SHM data/Customer_contacts.csv")
dt_SHM_Hour_2 <- fread(file = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis - BED4/shm_processed/Raw SHM data/Error_resolved.csv")
dt_SHM_Hour_3 <- fread(file = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis - BED4/shm_processed/Raw SHM data/No_action.csv")

dt_SHM_Hour <- rbind(dt_SHM_Hour_1, dt_SHM_Hour_2, dt_SHM_Hour_3)

# Load the faults reports - excel
fdd_reports <- read_excel("C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis - BED4/shm_processed/Raw SHM data/FDD_reports.xlsx", sheet = "Report")



# -------------------------------------------------------------------------
### IMPROVE DATA TABLE OF THE FAULT REPORTS
# -------------------------------------------------------------------------



df_fdd_reports <- as.data.frame(fdd_reports) %>% 
  dplyr::select(-Failure_type, -Faults, -Fault_Labels) %>% 
  filter(is.na(Duplicates) & is.na(Missing))
  # dplyr::select(-Duplicates)

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
# ids_with_na <- dt_SHM_Daily[, if(any(is.na(Treturn_MA) | is.na(Volume_MA) | is.na(Energy_MA))) .(heat_meter_id), by = heat_meter_id]$heat_meter_id


# Filter out the rows with those heat_meter_ids
dt_SHM_Daily <- dt_SHM_Daily[!heat_meter_id %in% ids_with_na]




# -------------------------------------------------------------------------
### IMPUTE MISSING VALUES
# -------------------------------------------------------------------------

df_SHM_Daily <- as.data.frame(dt_SHM_Daily)
# 
# # Split 'dt_SHM_Daily' into a list of smaller data tables based on unique 'heat_meter_id' values.
# # This allows for operations to be performed on each group of data individually.
# df_split <- split(df_SHM_Daily, df_SHM_Daily$heat_meter_id)
# 
# # Define a function 'variable_impute' to impute missing values for specific variables in a data frame.
# variable_impute <- function(df){
#   # Convert specified columns in the data frame to time series objects using xts(), ordering by the 'Date'.
#   # This transformation is necessary for time series analysis and missing data imputation.
#   Temp_ts <- xts(df$Temp_difference, order.by=df$Date)
#   Supply_ts <- xts(df$Tsupply, order.by=df$Date)
#   Return_ts <- xts(df$Treturn, order.by=df$Date)
#   Volume_ts <- xts(df$Volume, order.by=df$Date)
#   
#   # Impute missing values using moving average (MA) with k=3 and exponential weighting.
#   # The na_ma() function is used here, likely from a time series package, to fill in missing values.
#   # The imputed values are then reassigned to their respective columns in the data frame.
#   # df[, c("Volume","Temp_difference", "Tsupply", "Treturn") :=
#   #      list(
#   #        as.numeric(na_ma(as,ts(Volume_ts), k = 3, weighting = "exponential", maxgap = 3)),
#   #        as.numeric(na_ma(as,ts(Temp_ts), k = 3, weighting = "exponential", maxgap = 3)),
#   #        as.numeric(na_ma(as,ts(Supply_ts), k = 3, weighting = "exponential", maxgap = 3)),
#   #        as.numeric(na_ma(as,ts(Return_ts), k = 3, weighting = "exponential", maxgap = 3))
#   #      )]
#   
#   # longestNAstrech <- function(x) {
#   #   with(rle(is.na(x)), max(lengths[values]))  
#   # }
#   # 
#   # z = longestNAstrech(as.ts(Volume_ts))
#   
#   df <- df %>%
#     mutate(
#       Volume = as.numeric(na_ma(as.ts(Volume_ts), k = 3, weighting = "exponential", maxgap = 5)),
#       Temp_difference = as.numeric(na_ma(as.ts(Temp_ts), k = 3, weighting = "exponential", maxgap = 5)),
#       Tsupply = as.numeric(na_ma(as.ts(Supply_ts), k = 3, weighting = "exponential", maxgap = 5)),
#       Treturn = as.numeric(na_ma(as.ts(Return_ts), k = 3, weighting = "exponential", maxgap = 5))
#     )
#   
#   # Return the modified data frame with imputed values.
#   return(df)
#   # return(z)
#   
# }
# 
# # Apply the 'variable_impute' function to each data table in 'dt_split', effectively imputing missing values for each subset of data.
# df_split <- lapply(df_split, variable_impute)
# 
# 
# 
# # Re-combine the list of imputed data tables into a single data table.
# df_SHM_Daily <- rbindlist(df_split)
# 
# 
# 
# -------------------------------------------------------------------------
### COMBINE DATASETS
# -------------------------------------------------------------------------


df_SHM_Daily <- left_join(df_SHM_Daily, df_fdd_reports, by = "heat_meter_id") %>% 
  arrange(heat_meter_id, Date)



### __PLOT GRAPHS___________________________________________________________
# 
# library(dygraphs)
# 
# x_id = "b2f64224e8e7e5546c7de2f90f68c0ea0dea2da50f60da3e8eb8eb50aea4e4cf"
# df_id = df_SHM_Daily %>% filter(heat_meter_id == x_id)
# 
# Return_ts <- xts(df_id$Treturn, order.by=df_id$Date)
# Date_intervention = unique(df_id$Intervention_date)
# Date_End = unique(df_id$Failure_end_date)
# 
# 
# dygraph(Return_ts, main = "Return temperature", ylab = "Temp (C)") %>%
#   dyRangeSelector() %>%
#   dyRoller(rollPeriod = 7) %>%
#   dyEvent(Date_intervention, "Intervention", labelLoc = "bottom", color = "red") %>%
#   dyEvent(Date_End, "Failure end", labelLoc = "bottom", color = "blue")
# 
# 




library(dygraphs)

x_id = "a9a57ad594884b252949f46bf637118c95d5992826448b6b81b9ff9639daae6a"
df_id = df_SHM_Daily %>% filter(heat_meter_id == x_id)

Return_ts <- xts(df_id$Treturn, order.by=df_id$Date)
Volume_ts <- xts(df_id$Volume, order.by=df_id$Date)
Energy_ts <- xts(df_id$Energy, order.by=df_id$Date)

Date_visit = unique(df_id$Visit_date)
Date_Start = unique(df_id$Start)
Date_End = unique(df_id$End)


FDD_start = unique(df_id$FDD_start)
FDD_end = unique(df_id$FDD_end)


# a <- dygraph((Return_ts), 
#         main = "Return", ylab = "Temperature", group = "DH") %>%
#   dyRangeSelector() %>%
#   dyRoller(rollPeriod = 1) %>%
#   dyEvent(Date_visit, "Intervention", labelLoc = "bottom", color = "red") %>%
#   dyEvent(Date_End, "End", labelLoc = "bottom", color = "blue") %>%
#   dyEvent(Date_Start, "Start", labelLoc = "bottom", color = "green") %>% 
#   dyEvent(FDD_start, "FDD_1", labelLoc = "bottom", color = "grey") %>%
#   dyEvent(FDD_end, "FDD_2", labelLoc = "bottom", color = "black")
# 
# b <- dygraph((Volume_ts), 
#         main = "Volume", ylab = "Volume", group = "DH") %>%
#   dyRangeSelector() %>%
#   dyRoller(rollPeriod = 1) %>%
#   dyEvent(Date_visit, "Intervention", labelLoc = "bottom", color = "red") %>%
#   dyEvent(Date_End, "End", labelLoc = "bottom", color = "blue") %>%
#   dyEvent(Date_Start, "Start", labelLoc = "bottom", color = "green") %>% 
#   dyEvent(FDD_start, "FDD_1", labelLoc = "bottom", color = "grey") %>%
#   dyEvent(FDD_end, "FDD_2", labelLoc = "bottom", color = "black")
# 
# c <- dygraph((Energy_ts), 
#         main = "Energy", ylab = "Energy", group = "DH") %>%
#   dyRangeSelector() %>%
#   dyRoller(rollPeriod = 1) %>%
#   dyEvent(Date_visit, "Intervention", labelLoc = "bottom", color = "red") %>%
#   dyEvent(Date_End, "End", labelLoc = "bottom", color = "blue") %>%
#   dyEvent(Date_Start, "Start", labelLoc = "bottom", color = "green") %>% 
#   dyEvent(FDD_start, "FDD_1", labelLoc = "bottom", color = "grey") %>%
#   dyEvent(FDD_end, "FDD_2", labelLoc = "bottom", color = "black")

library(htmltools)

browsable(
  tagList(
    dygraph(Return_ts, main = "Return", ylab = "Return", group = "DH", height = 200, width = "100%") %>%
              dyRangeSelector() %>%
              dyRoller(rollPeriod = 1) %>%
              dyEvent(Date_visit, "Intervention", labelLoc = "bottom", color = "red") %>%
              dyEvent(Date_End, "End", labelLoc = "bottom", color = "blue") %>%
              dyEvent(Date_Start, "Start", labelLoc = "bottom", color = "green") %>% 
              dyEvent(FDD_start, "FDD_1", labelLoc = "bottom", color = "grey") %>%
              dyEvent(FDD_end, "FDD_2", labelLoc = "bottom", color = "black"),
    dygraph(Volume_ts, main = "Volume", ylab = "Volume", group = "DH", height = 200, width = "100%") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 1) %>%
      dyEvent(Date_visit, "Intervention", labelLoc = "bottom", color = "red") %>%
      dyEvent(Date_End, "End", labelLoc = "bottom", color = "blue") %>%
      dyEvent(Date_Start, "Start", labelLoc = "bottom", color = "green") %>% 
      dyEvent(FDD_start, "FDD_1", labelLoc = "bottom", color = "grey") %>%
      dyEvent(FDD_end, "FDD_2", labelLoc = "bottom", color = "black"),
    dygraph(Energy_ts, main = "Energy", ylab = "Energy", group = "DH", height = 200, width = "100%") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 1) %>%
      dyEvent(Date_visit, "Intervention", labelLoc = "bottom", color = "red") %>%
      dyEvent(Date_End, "End", labelLoc = "bottom", color = "blue") %>%
      dyEvent(Date_Start, "Start", labelLoc = "bottom", color = "green") %>% 
      dyEvent(FDD_start, "FDD_1", labelLoc = "bottom", color = "grey") %>%
      dyEvent(FDD_end, "FDD_2", labelLoc = "bottom", color = "black")
    
  )
)

### _____________________________________________________________



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

# filter_days <- function(df){
#   # Filter the data frame to only include rows where the 'Date' is less than or equal to a predefined 'timestamp'.
#   # This step assumes that 'timestamp' is defined elsewhere in your environment as the upper date limit.
#   df <- df %>% filter(Date <= Visit_date) %>% 
#     arrange(Date)
#   
#   # Define the number of days to consider in the time frame as 90 days.
#   n_days = 90
#   
#   # Select only the last 'n_days' rows of the data frame, focusing on the most recent data.
#   df <- tail(df, n = n_days)
#   
#   # Add a new column 'Row_number' to the data frame, which is a sequence from 1 to the number of rows in the data frame.
#   # This can help in further indexing or identifying rows after filtering.
#   df$Row_number <- 1:nrow(df)
#   
#   # Return the modified data frame with the added 'Row_number' column and filtered rows.
#   return(df)
# }

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
# dt_90.days[, Treturn_scaled := (min_max_normalize(Treturn)), by = heat_meter_id]
# dt_90.days[, Volume_scaled := (min_max_normalize(Volume)), by = heat_meter_id]
# dt_90.days[, Energy_scaled := (min_max_normalize(Energy)), by = heat_meter_id]

# dt_90.days[, Treturn_scaled := scale(min_max_normalize(Treturn_MA)), by = heat_meter_id]
# dt_90.days[, Volume_scaled := scale(min_max_normalize(Volume_MA)), by = heat_meter_id]
# dt_90.days[, Energy_scaled := scale(min_max_normalize(Energy_MA)), by = heat_meter_id]

# dt_90.days[, Treturn_scaled := scale((Treturn_MA)), by = heat_meter_id]
# dt_90.days[, Volume_scaled := scale((Volume_MA)), by = heat_meter_id]
# dt_90.days[, Energy_scaled := scale((Energy_MA)), by = heat_meter_id]

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

# # Remove meters with less than 90 days measurements
# df_90.days <- df_90.days %>%
#   group_by(heat_meter_id) %>%
#   dplyr::mutate(n = n()) %>% 
#   ungroup() %>% 
#   filter(n == 90) %>% 
#   dplyr::select(-n)

# REMOVE -----

# Remove meters with less than 90 days measurements
# df_90.days_z <- df_90.days %>%
#   group_by(heat_meter_id) %>%
#   summarise(n_NA = mean(Treturn) + mean(Tsupply) + mean(Temp_difference) + mean(Volume))
# %>%
#   ungroup() %>% 
#   filter(n_NA == 0) %>% 
#   dplyr::select(-n_NA)


# # Convert your dataframe to a data.table if it's not already
# setDT(df_90.days)
# 
# # Identify the heat_meter_ids with any NA in the specified columns
# # ids_with_na <- df_90.days[, if(any(is.na(Treturn) | is.na(Tsupply) | is.na(Temp_difference) | is.na(Volume) | is.na(Energy))) .(heat_meter_id), by = heat_meter_id]$heat_meter_id
# ids_with_na <- df_90.days[, if(any(is.na(Treturn_MA) | is.na(Volume_MA) | is.na(Energy_MA))) .(heat_meter_id), by = heat_meter_id]$heat_meter_id
# 
# 
# # Filter out the rows with those heat_meter_ids
# df_90.days <- df_90.days[!heat_meter_id %in% ids_with_na]
# 
# df_90.days <- as.data.frame(df_90.days)


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




# library(dygraphs)
# 
# x_id = 5
# df_id = dt_SHM_Daily %>% filter(meter_id == x_id)
# 
# Return_ts <- xts(df_id$Treturn, order.by=df_id$Date)
# 
# Date_visit = unique(df_id$Visit_date)
# Date_Start = unique(df_id$Start)
# Date_End = unique(df_id$End)
# 
# 
# dygraph(Return_ts, main = paste(unique(df_id$Final_pattern)), ylab = "Temp (C)") %>%
#   dyRangeSelector() %>%
#   dyRoller(rollPeriod = 1) %>%
#   dyEvent(Date_visit, "Intervention", labelLoc = "bottom", color = "red") %>%
#   dyEvent(Date_End, "End", labelLoc = "bottom", color = "blue") %>%
#   dyEvent(Date_Start, "Start", labelLoc = "bottom", color = "green")


# for (i in 1:50){
#   df_id = dt_SHM_Daily %>% filter(meter_id == i)
# 
#   pattern <- unique(df_id$Final_pattern)
# 
#   ts_original <- zoo(x = df_id$Treturn, order = df_id$Date)
#   ts_lowfilter <- zoo( x = smooth(ts_original,
#                                   kind = "3RSS",
#                                   twiceit = F),
#                        order = df_id$Date)
# 
#   plot(ts_original, type = "l", col = "black",
#        ylab = "Value",
#        xlab = "Time",
#        main = paste("Meter:", i, "-", pattern))
#   # Add the low-pass filtered time series
#   lines(ts_lowfilter, type = "l", col = "red")
# }




# 
# ggplot(df_id, aes(x = Treturn)) +
#   geom_density() +
#   labs(title = paste(unique(df_id$Pattern_Return)))




