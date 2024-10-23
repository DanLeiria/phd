# -------------------------------------------------------------------------
### LIBRARY OF PACKAGES
# -------------------------------------------------------------------------
# Load necessary libraries for data manipulation, plotting, statistical analysis, and machine learning.

library(ggfortify)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(viridis)
library(ggExtra)
library(reshape2)
library(ggdendro) 
library(gplots) 
library(tseries) 
library(TSclust) 
library(dtwclust) 
library(factoextra)
library(scales)
library(gridExtra)
library(forcats)
library(tsfeatures)
library(GGally)
library(xts)
library(imputeTS)
library(openxlsx)
library(psych)
library(devtools)
library(ggbiplot)
library(umap)
library(TSdist)
library(dendextend)
library(forecast)
library(tsoutliers)
library(ggbeeswarm)
library(ggalluvial)
library(ggforce)
library(ggpubr)
library(kohonen)
library(aweSOM)
library(yasomi)
library(waffle)


# -------------------------------------------------------------------------
### SET WORKING DIRECTORY
# -------------------------------------------------------------------------
# This section sets the working directory and system language/locale for consistent data processing and analysis.

setwd("C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis")  # Define the main directory for the analysis workflow.
Sys.setenv(LANG = "en")                                                                                                   # Ensure system language is set to English for consistency in processing and output.
Sys.setlocale("LC_ALL", "English")                                                                                        # Set all locale settings to English.

# -------------------------------------------------------------------------
### LOAD DATASET
# -------------------------------------------------------------------------
# Load the processed dataset and prepare it for analysis.

load(file = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis/SHM_processed_BED4.rda") # Load the dataset from a .rda file.

dt_SHM_segmented <- dt_SHM_segmented %>% 
  arrange(meter_id, Date)  # Sort the segmented dataset by meter_id and Date for orderly analysis.



# Perform grouping and summarization to analyze fault detection and diagnosis (FDD) groups within the dataset.
FDD_groups <- dt_SHM_segmented %>% 
  group_by(meter_id, FDD_category, Treturn_Pattern, Treturn_Volatility, Volume_Pattern, Volume_Volatility, Energy_Pattern, Energy_Volatility, Fault_Repetition) %>% 
  dplyr::summarise(
    Start = mean(Start),
    End = mean(End),
    Failure_start = mean(FDD_start),
    Failure_end = mean(FDD_end),
    Failure_days = mean(Number_days),
    Failure_period = mean(FDD_month),
    Min_TR = min(Treturn[Date >= Failure_start & Date <= End]),
    Max_TR = max(Treturn[Date >= Failure_start & Date <= End]),
    Min_Vol = min(Volume[Date >= Failure_start & Date <= End]),
    Max_Vol = max(Volume[Date >= Failure_start & Date <= End]),
    Min_Ener = min(Energy[Date >= Failure_start & Date <= End]),
    Max_Ener = max(Energy[Date >= Failure_start & Date <= End]),
    
    First_TR = Treturn[Date == Failure_start],
    Last_TR = Treturn[Date == End],
    First_Vol = Volume[Date == Failure_start],
    Last_Vol = Volume[Date == End],
    First_Ener = Energy[Date == Failure_start],
    Last_Ener = Energy[Date == End]
  )


# Count the number of cases for each FDD category to understand the distribution of faults.
FDD_cases <- FDD_groups %>% 
  group_by(FDD_category) %>% 
  dplyr::summarise(n_count = n())

FDD_cases  # Display the count of FDD cases by category.






# -------------------------------------------------------------------------
### FAULT PERIOD
# -------------------------------------------------------------------------

# Summarize fault occurrences by meter ID, fault category, and the period of failure. This aggregation helps to quantify faults within specified periods.

Failure_period <- FDD_groups %>% 
  group_by(meter_id, FDD_category, Failure_period) %>% 
  dplyr::summarise(n_count = n())  # Count the number of occurrences for each combination of meter ID, fault category, and failure period.


# Create a beeswarm plot to visualize the distribution of fault categories over different failure periods.
plt.failure_month <- ggplot(Failure_period, aes(x = Failure_period, y = FDD_category, fill = FDD_category)) +
  geom_beeswarm(priority = "random", size = 3, shape = 21, cex = 3, show.legend = F) +  # Use geom_beeswarm for a non-overlapping scatter plot. 
  theme_bw() +  # Use a minimalistic theme for the plot background.
  theme(
    legend.position = "none",  # Remove the legend to declutter the plot.
    panel.spacing = unit(0.1, "lines"),  # Adjust spacing between panels.
    axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
    axis.text.y = element_text(size = 14),  # Increase size of y-axis labels
    axis.title = element_text(size = 16),   # Increase size of axis titles (xlab and ylab)
    text = element_text(size = 16)          # Increase size of other text (annotations, etc.)
  ) +
  xlab("Month of the year") +  # Label the x-axis as "Months".
  ylab("Fault label") +  # Label the y-axis as "Fault label".
  geom_vline(xintercept = 4.7, linetype = "dashed") +  # Add a dashed vertical line at 4.7 months as a significant marker.
  geom_vline(xintercept = 9.3, linetype = "dashed") +  # Add another dashed vertical line at 9.3 months as a significant marker.
  scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +  # Set the x-axis to display months 1 through 12.
  scale_y_discrete(expand = c(0.1,0.1)) +
  annotate("rect", xmin = 4.7, xmax = 9.3, ymin = -Inf, ymax = Inf, alpha = .3, fill = "grey")  # Highlight the area between the two vertical lines to emphasize a specific period.

plt.failure_month

ggsave(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/plt.failure_month.png",
       plot = plt.failure_month,
       width = 3000,
       height = 1500,
       units = "px")


# -------------------------------------------------------------------------
### PATTERN TYPES
# -------------------------------------------------------------------------


# FOUR PATTERNS

ids_patterns <- c(10, 4, 6, 30)

dt_segmented <- as.data.table(dt_SHM_segmented)

dt_pattern_types <- dt_segmented[meter_id %in% ids_patterns]



ggplot(dt_pattern_types, aes(x = Date, y = Treturn)) +
  geom_line(color = "black") +
  geom_smooth(color = "red", size = 1.5, se = F, span = 0.4) +
  theme_bw() +
  facet_wrap(.~factor(Treturn_Pattern, levels = c("Degradation", 
                                                  "Stroke",
                                                  "Level shift",
                                                  "Level shift with degradation")), scales = "free") +
  ylab("Return temperature") +
  xlab("Time")





# PATTERNS PER FAULT LABEL

TR_patterns <- FDD_groups %>%
  group_by(FDD_category, Treturn_Pattern) %>% 
  dplyr::summarise(n_count = n()) %>%
  transmute(FDD_category, 
            Pattern_type = Treturn_Pattern, 
            n_count, 
            Var_type = "Return temperature")

Vol_patterns <- FDD_groups %>% 
  group_by(FDD_category, Volume_Pattern) %>% 
  dplyr::summarise(n_count = n()) %>%
  transmute(FDD_category, 
            Pattern_type = Volume_Pattern, 
            n_count, 
            Var_type = "Volume")

Ener_patterns <- FDD_groups %>% 
  group_by(FDD_category, Energy_Pattern) %>% 
  dplyr::summarise(n_count = n()) %>%
  transmute(FDD_category, 
            Pattern_type = Energy_Pattern, 
            n_count, 
            Var_type = "Energy")

Combined_patterns <- rbind(TR_patterns, Vol_patterns) %>% 
  rbind(Ener_patterns)

Combined_patterns$Var_type <- factor(Combined_patterns$Var_type,
                                     levels = c("Return temperature", 
                                                "Volume", 
                                                "Energy"))


plt.pattern_fault_label <- ggplot(Combined_patterns, aes(fill = Pattern_type, y = n_count, x = FDD_category)) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
    axis.text.y = element_text(size = 14),  # Increase size of y-axis labels
    axis.title = element_text(size = 16),   # Increase size of axis titles (xlab and ylab)
    text = element_text(size = 16)          # Increase size of other text (annotations, etc.)
  ) +
  ylab("Number of cases") +
  xlab("Fault label") +
  labs(fill = "Pattern type:") +
  facet_wrap(.~Var_type)




plt.pattern_fault_label

ggsave(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/plt.pattern_fault_label.png",
       plot = plt.pattern_fault_label,
       width = 3500,
       height = 1500,
       units = "px")





# -------------------------------------------------------------------------
### ASSOCIATION OF PATTERNS AND FAULT LABELS
# -------------------------------------------------------------------------

Associated_patterns <- FDD_groups %>%
  dplyr::select(meter_id, FDD_category, Treturn_Pattern, Volume_Pattern, Energy_Pattern) %>%
  mutate(Associated_Pattern = paste(Treturn_Pattern, "-", Volume_Pattern, "-", Energy_Pattern)) %>%
  group_by(FDD_category, Associated_Pattern) %>%
  dplyr::summarise(Freq = n())

FDD_groups$Main_system[FDD_groups$FDD_category == "DHW HEX - Defective"] <- "DHW HEX"
FDD_groups$Main_system[FDD_groups$FDD_category == "DHW HEX - High settings"] <- "DHW HEX"
FDD_groups$Main_system[FDD_groups$FDD_category == "DHW HEX - No details"] <- "DHW HEX"

FDD_groups$Main_system[FDD_groups$FDD_category == "DHW Tank - High settings"] <- "DHW Tank"
FDD_groups$Main_system[FDD_groups$FDD_category == "DHW Tank - No details"] <- "DHW Tank"

FDD_groups$Main_system[FDD_groups$FDD_category == "Radiator - Defective"] <- "Radiator"
FDD_groups$Main_system[FDD_groups$FDD_category == "Radiator - High settings"] <- "Radiator"
FDD_groups$Main_system[FDD_groups$FDD_category == "Radiator - No details"] <- "Radiator"

FDD_groups$Main_system[FDD_groups$FDD_category == "UFH - Defective"] <- "UFH"
FDD_groups$Main_system[FDD_groups$FDD_category == "UFH - High settings"] <- "UFH"
FDD_groups$Main_system[FDD_groups$FDD_category == "UFH - No details"] <- "UFH"

FDD_groups$Main_system[FDD_groups$FDD_category == "Towel dryer - High settings"] <- "Towel dryer"

Associated_patterns <- FDD_groups %>%
  group_by(Main_system, Treturn_Pattern, Volume_Pattern, Energy_Pattern) %>%
  dplyr::summarise(Freq = n()) %>%
  ungroup()  # Always a good practice to ungroup after you're done

Associated_patterns$Main_system <- factor(Associated_patterns$Main_system,
                                          levels = c("Radiator",
                                                     "UFH",
                                                     "Towel dryer",
                                                     "DHW Tank",
                                                     "DHW HEX"))

Associated_patterns$Energy_Pattern <- factor(Associated_patterns$Energy_Pattern,
                                          levels = c("Stroke",
                                                     "Level shift",
                                                     "Degradation",
                                                     "Not observed"),
                                          labels = c("S",
                                                     "LS",
                                                     "D",
                                                     "NO"))

Associated_patterns$Volume_Pattern <- factor(Associated_patterns$Volume_Pattern,
                                             levels = c("Stroke",
                                                        "Level shift",
                                                        "Degradation",
                                                        "Level shift with degradation",
                                                        "Not observed"),
                                             labels = c("S",
                                                        "LS",
                                                        "D",
                                                        "LS + D",
                                                        "NO"))

Associated_patterns$Treturn_Pattern <- factor(Associated_patterns$Treturn_Pattern,
                                             levels = c("Stroke",
                                                        "Level shift",
                                                        "Degradation",
                                                        "Level shift with degradation"),
                                             labels = c("S",
                                                        "LS",
                                                        "D",
                                                        "LS + D"))


data <- reshape2::melt(Associated_patterns)
data <- gather_set_data(data, 1:4)

data$x <- factor(data$x, levels=c(1,2,3,4), labels = c("System",
                                                       "Return temp.",
                                                       "Volume",
                                                       "Energy"))

### RADIATOR

size_labels = 4

plt.radiator <- ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Main_system), alpha = 0.5, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size = size_labels, nudge_x = 0.1, hjust = 0) +
  theme_void() +
  scale_fill_manual(values = c(`Radiator` = '#440154', 
                               `UFH` = 'grey', 
                               `Towel dryer` = 'grey',
                               `DHW Tank` = 'grey',
                               `DHW HEX` = 'grey'
  )) +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text()
        )
plt.radiator

### UFH

plt.ufh <- ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Main_system), alpha = 0.5, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'black', angle = 0, nudge_x = 0.1, hjust = 0, size = size_labels) +
  theme_void() +
  scale_fill_manual(values = c(`Radiator` = 'grey', 
                               `UFH` = '#3b528b', 
                               `Towel dryer` = 'grey',
                               `DHW Tank` = 'grey',
                               `DHW HEX` = 'grey'
  )) +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text()
        )
  


### Towel dryer

plt.td <- ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Main_system), alpha = 0.5, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'black', angle = 0, nudge_x = 0.1, hjust = 0, size = size_labels) +
  theme_void() +
  scale_fill_manual(values = c(`Radiator` = 'grey', 
                               `UFH` = 'grey', 
                               `Towel dryer` = '#21918c',
                               `DHW Tank` = 'grey',
                               `DHW HEX` = 'grey'
  )) +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text()
        )


### DHW Tank

plt.dhw_tank <- ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Main_system), alpha = 0.5, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'black', angle = 0, nudge_x = 0.1, hjust = 0, size = size_labels) +
  theme_void() +
  scale_fill_manual(values = c(`Radiator` = 'grey', 
                               `UFH` = 'grey', 
                               `Towel dryer` = 'grey',
                               `DHW Tank` = '#5ec962',
                               `DHW HEX` = 'grey'
  )) +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text()
        )


### DHW HEX

plt.dhw_hex <- ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Main_system), alpha = 0.5, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'black', angle = 0, nudge_x = 0.1, hjust = 0, size = size_labels) +
  theme_void() +
  scale_fill_manual(values = c(`Radiator` = 'grey', 
                               `UFH` = 'grey', 
                               `Towel dryer` = 'grey',
                               `DHW Tank` = 'grey',
                               `DHW HEX` = '#fde725'
  )) +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text()
        )
 

# plt.legend <- ggplot(data, aes(x = id, y = value, color = Main_system))+
#   geom_point() +
#   lims(x = c(0,0), y = c(0,0))+
#   theme_void()+
#   theme(legend.position = c(0.5,0.5),
#         legend.key.size = unit(1, "cm"),
#         legend.text = element_text(size =  12),
#         legend.title = element_text(size = 15, face = "bold"))+
#   scale_color_viridis(discrete = T, name = "Main system:") +
#   guides(color = guide_legend(override.aes = list(size=8)))

ggarrange(
  plt.radiator,
  plt.ufh,
  plt.td,
  plt.dhw_tank,
  plt.dhw_hex,
  # plt.legend,
  labels = c("a)", "b)", "c)", "d)", "e)"),
  font.label = list(size = 16),
  ncol = 3,
  nrow = 2) %>%
  ggexport(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/sankey_fault_patterns.png",
           width = 900,
           height = 450,
           units = "px")





# -------------------------------------------------------------------------
### REPETITIVE PATTERNS
# -------------------------------------------------------------------------

# PATTERNS PER FAULT LABEL

Rep_patterns <- FDD_groups %>%
  group_by(FDD_category, Fault_Repetition) %>% 
  dplyr::summarise(n_count = n()) %>%
  ungroup()

Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "DHW HEX - Defective"] <- "Defective"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "DHW Tank - Defective"] <- "Defective"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "Radiator - Defective"] <- "Defective"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "UFH - Defective"] <- "Defective"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "Towel dryer - Defective"] <- "Defective"


Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "DHW HEX - High settings"] <- "High settings"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "DHW Tank - High settings"] <- "High settings"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "Radiator - High settings"] <- "High settings"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "UFH - High settings"] <- "High settings"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "Towel dryer - High settings"] <- "High settings"

Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "DHW HEX - No details"] <- "No details"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "DHW Tank - No details"] <- "No details"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "Radiator - No details"] <- "No details"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "UFH - No details"] <- "No details"
Rep_patterns$Fault_nature[Rep_patterns$FDD_category == "Towel dryer - No details"] <- "No details"


plt.pattern_repetition <- ggplot(Rep_patterns, aes(x = Fault_Repetition, y = n_count, fill = Fault_Repetition)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~Fault_nature) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
    axis.text.y = element_text(size = 14),  # Increase size of y-axis labels
    axis.title = element_text(size = 16),   # Increase size of axis titles (xlab and ylab)
    text = element_text(size = 16)          # Increase size of other text (annotations, etc.)
  ) +
  xlab("") +
  ylab("Occurrences") + 
  scale_fill_discrete(name = "Repetitive fault:") +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = 0:10)  # Set y-axis to display integer values from 0 to 10


plt.pattern_repetition

ggsave(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/plt.pattern_repetition.png",
       plot = plt.pattern_repetition,
       width = 3000,
       height = 1500,
       units = "px")

  
       



# -------------------------------------------------------------------------
### MIN & MAX PER FAULT LABEL
# -------------------------------------------------------------------------

TR_MinMax <- FDD_groups %>% 
  ungroup() %>% 
  transmute(meter_id, FDD_category, Pattern_type = Treturn_Pattern, Difference = Last_TR - First_TR, Failure_days, Max_TR, Var_type = "Return temperature")

Vol_MinMax <- FDD_groups %>% 
  ungroup() %>% 
  transmute(meter_id, FDD_category, Pattern_type = Volume_Pattern, Difference = Last_Vol - First_Vol, Failure_days, Max_TR, Var_type = "Volume")

Ener_MinMax <- FDD_groups %>% 
  ungroup() %>% 
  transmute(meter_id, FDD_category, Pattern_type = Energy_Pattern, Difference = Last_Ener - First_Ener, Failure_days, Max_TR, Var_type = "Energy")

Combined_MinMax <- rbind(TR_MinMax, Vol_MinMax) %>% 
  rbind(Ener_MinMax)

Combined_MinMax$Var_type <- factor(Combined_MinMax$Var_type, levels = c("Return temperature", "Volume", "Energy"))

# Difference of values 
plt.diff <- ggplot(Combined_MinMax, aes(fill = FDD_category, y = FDD_category, x = Difference)) + 
  geom_beeswarm(priority = "random", size = 2.5, shape = 21, cex = 2, show.legend = F) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
    axis.text.y = element_text(size = 14),  # Increase size of y-axis labels
    axis.title = element_text(size = 16),   # Increase size of axis titles (xlab and ylab)
    text = element_text(size = 16)          # Increase size of other text (annotations, etc.)
  ) +
  ylab("Fault labels") +
  # xlab("Difference of values") +
  xlab("") +
  # labs(fill = "Pattern type:") +
  facet_wrap(.~Var_type, scales = "free_x")

plt.diff

ggsave(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/plt.diff.png",
       plot = plt.diff,
       width = 3000,
       height = 1500,
       units = "px")

# Difference of values per number of days
plt.diff_days <- ggplot(Combined_MinMax, aes(fill = FDD_category, y = FDD_category, x = Difference/Failure_days)) + 
  geom_beeswarm(priority = "random", size = 2.5, shape = 21, cex = 2, show.legend = F) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
    axis.title = element_text(size = 16),   # Increase size of axis titles (xlab and ylab)
    text = element_text(size = 16)          # Increase size of other text (annotations, etc.)
  ) +
  ylab("") +
  # xlab("Difference of values per number of days") +
  xlab("") +
  # labs(fill = "Pattern type:") +
  facet_wrap(.~Var_type, scales = "free_x")

plt.diff_days

# plt.diff_overall <- ggarrange(
#   plt.diff,
#   plt.diff_days,
#   common.legend = TRUE,
#   legend = "none",
#   labels = c("a)", "b)"),
#   widths = c(1.2,1),
#   ncol = 2,
#   nrow = 1)
# 
# plt.diff_overall

ggsave(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/plt.diff_days.png",
       plot = plt.diff_days,
       width = 3000,
       height = 1500,
       units = "px")

# -------------------------------------------------------------------------
### VOLATILITY PER FAULT LABEL
# -------------------------------------------------------------------------

df_Volatility <- dt_SHM_segmented


df_Volatility <- df_Volatility %>% 
  mutate(Failure_start_date = ifelse(Treturn_Pattern == "Degradation", as.Date(FDD_start),
                                     ifelse(Treturn_Pattern == "Level shift with degradation", as.Date(FDD_start) + days(5),
                                            ifelse(Treturn_Pattern == "Stroke", as.Date(Start),
                                            as.Date(FDD_end)))),
         Failure_end_date = as.Date(End))

df_Volatility <- df_Volatility %>% 
  filter(Date >= Failure_start_date & Date <= Failure_end_date)

# 
# for (i in 1:50){
#   
#   df <- df_Volatility %>% filter(meter_id == i)
#   
#   volatility_x = unique(df$Treturn_Volatility)
#   pattern_x = unique(df$Treturn_Pattern)
#   
#   
#   # if (nrow(df) != 0){
#   #   df <- ts(xts(df$Treturn, order.by = df$Date))
#   #   par(mfrow = c(2,1))
#   #   plot(df, main = paste(i,"(", pattern_x, ")", " with volatility =", volatility_x))
#   #   plot(diff(log(df)), main = paste("Overall volatility =", round(sd(diff(log(df))), digits = 2)))
#   # } else {
#   #   ""
#   # }
#   
#   if (all(is.na(df$Treturn)) != TRUE){
#     df <- ts(xts(df$Treturn, order.by = df$Date))
#     par(mfrow = c(2,1))
#     plot(df, main = paste(i,"(", pattern_x, ")", " with volatility =", volatility_x))
#     plot(diff(log(df)), main = paste("Overall volatility =", round(sd(diff(log(df))), digits = 2)))
#   } else {
#     ""
#   }
#   
# }


TR_Volatil <- df_Volatility %>% 
  group_by(meter_id, FDD_category, Treturn_Pattern) %>% 
  dplyr::summarise(TR_sd = sd(diff(log(Treturn)))) %>% 
  ungroup() %>% 
  transmute(meter_id, FDD_category, Pattern = Treturn_Pattern, Volatility = TR_sd, Var_type = "Return temperature")

TR_Volatil$Volatility[TR_Volatil$Pattern == "Stroke"] <- 0


Vol_Volatil <- df_Volatility %>% 
  group_by(meter_id, FDD_category, Volume_Pattern) %>% 
  dplyr::summarise(Vol_sd = sd(diff(log(Volume)))) %>% 
  ungroup() %>% 
  transmute(meter_id, FDD_category, Pattern = Volume_Pattern, Volatility = Vol_sd, Var_type = "Volume")

Vol_Volatil$Volatility[Vol_Volatil$Pattern == "Stroke"] <- 0


Ener_Volatil <- df_Volatility %>% 
  group_by(meter_id, FDD_category, Energy_Pattern) %>% 
  dplyr::summarise(Ener_sd = sd(diff(log(Energy)))) %>% 
  ungroup() %>% 
  transmute(meter_id, FDD_category, Pattern = Energy_Pattern, Volatility = Ener_sd, Var_type = "Energy")

Ener_Volatil$Volatility[Ener_Volatil$Pattern == "Stroke"] <- 0


Combined_Volatil <- rbind(TR_Volatil, Vol_Volatil) %>% 
  rbind(Ener_Volatil)

Combined_Volatil$Var_type <- factor(Combined_Volatil$Var_type, levels = c("Return temperature", "Volume", "Energy"))


plt.volatility <- ggplot(Combined_Volatil, aes(fill = FDD_category, y = FDD_category, x = Volatility*100)) + 
  geom_beeswarm(priority = "random", size = 2.5, shape = 21, cex = 2, show.legend = F) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
    axis.text.y = element_text(size = 14),  # Increase size of y-axis labels
    axis.title = element_text(size = 16),   # Increase size of axis titles (xlab and ylab)
    text = element_text(size = 16)          # Increase size of other text (annotations, etc.)
  ) +
  ylab("Fault labels") +
  xlab("Volatility [%]") +
  labs(fill = "Pattern type:") +
  facet_wrap(.~Var_type)



plt.volatility

ggsave(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/plt.volatility.png",
       plot = plt.volatility,
       width = 3000,
       height = 1500,
       units = "px")


# -------------------------------------------------------------------------
### NUMBER OF DAYS PER FAULT LABEL
# -------------------------------------------------------------------------

### Number of days per return temperature pattern
# 
# library(ggridges)
# 
# ggplot(FDD_groups, aes(fill = Treturn_Pattern, y = Treturn_Pattern, x = Failure_days)) + 
#   geom_boxplot() +
#   theme_bw() +
#   theme(
#     legend.position = "bottom"
#   ) +
#   ylab("Return temperature patterns") +
#   xlab("Number of days") +
#   labs(fill = "Pattern type:")
#   # facet_wrap(.~Var_type)
# 
# 
# 
# 
# ### Pattern types per season
# 
# TR_season <- FDD_groups %>% 
#   # mutate(Season = ifelse(Failure_period >= 4 & Failure_period <= 9, "Warmer months", "Colder months")) %>% 
#   group_by(meter_id, Treturn_Pattern, Failure_period) %>% 
#   dplyr::summarise(n_count = n()) %>% 
#   ungroup() %>% 
#   transmute(meter_id, Failure_period, Pattern_type = Treturn_Pattern, n_count, Var_type = "Return temperature")
# 
# Vol_season <- FDD_groups %>% 
#   # mutate(Season = ifelse(Failure_period >= 4 & Failure_period <= 9, "Warmer months", "Colder months")) %>% 
#   group_by(meter_id, Volume_Pattern, Failure_period) %>% 
#   dplyr::summarise(n_count = n()) %>% 
#   ungroup() %>% 
#   transmute(meter_id, Failure_period, Pattern_type = Volume_Pattern, n_count, Var_type = "Volume")
# 
# Ener_season <- FDD_groups %>% 
#   # mutate(Season = ifelse(Failure_period >= 4 & Failure_period <= 9, "Warmer months", "Colder months")) %>% 
#   group_by(meter_id, Energy_Pattern, Failure_period) %>% 
#   dplyr::summarise(n_count = n()) %>% 
#   ungroup() %>% 
#   transmute(meter_id, Failure_period, Pattern_type = Energy_Pattern, n_count, Var_type = "Energy")
# 
# Combined_season <- rbind(TR_season, Vol_season) %>% 
#   rbind(Ener_season)
# 
# # Combined_season$Var_type <- factor(Combined_season$Var_type, levels = c("Return temperature", "Volume", "Energy"))
# # Combined_season$Period_intensity[Combined_season$Failure_period == 1] <- 1.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 2] <- 2.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 3] <- 3.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 4] <- 4.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 5] <- 5.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 6] <- 6.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 7] <- 6.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 8] <- 5.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 9] <- 4.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 10] <- 3.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 11] <- 2.0
# # Combined_season$Period_intensity[Combined_season$Failure_period == 12] <- 1.0
# # 
# # 
# # ggplot(Combined_season, aes(fill = (Period_intensity), y = n_count, x = Pattern_type)) +
# #   geom_bar(position="fill", stat="identity") +
# #   coord_flip() +
# #   theme_bw() +
# #   theme(
# #     legend.position = "bottom"
# #   ) +
# #   scale_fill_gradient2(midpoint=3.5, low="red", mid="lightblue",
# #                         high="blue", space ="Lab" ) +
# #   ylab("Number of cases") +
# #   xlab("Fault pattern") +
# #   labs(fill = "Month:") +
# #   facet_wrap(.~Var_type)
# 
# 
# library(beeswarm)
# 
# 
# ggplot(Combined_season, aes(x = Failure_period, y = Pattern_type, fill = Pattern_type)) +
#   geom_beeswarm(priority = "random", size = 3, shape = 21, cex = 1.5, show.legend = F) +
#   theme(
#     legend.position = "none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   theme_bw() +
#   xlab("Months") +
#   ylab("Fault Pattern") +
#   geom_vline(xintercept = 4.7, linetype = "dashed") +
#   geom_vline(xintercept = 9.3, linetype = "dashed") +
#   scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
#   annotate("rect", xmin = 4.7, xmax = 9.3, ymin = -Inf, ymax = Inf, alpha = .3, fill = "grey") +
#   facet_wrap(.~Var_type)




# -------------------------------------------------------------------------
### SELF ORGANIZING MAPS (SOM)
# -------------------------------------------------------------------------

TR_volatil_x <- TR_Volatil %>% select(meter_id, Volatility)
TR_volatil_x <- dplyr::rename(TR_volatil_x, Treturn_volatil = Volatility)
# TR_volatil_x$Treturn_volatil[is.na(TR_volatil_x$Treturn_volatil)] <- 0

Vol_volatil_x <- Vol_Volatil %>% select(meter_id, Volatility)
Vol_volatil_x <- dplyr::rename(Vol_volatil_x, Volume_volatil = Volatility)
# Vol_volatil_x$Volume_volatil[is.na(Vol_volatil_x$Volume_volatil)] <- 0

Ener_volatil_x <- Ener_Volatil %>% select(meter_id, Volatility)
Ener_volatil_x <- dplyr::rename(Ener_volatil_x, Energy_volatil = Volatility)
# Ener_volatil_x$Energy_volatil[is.na(Ener_volatil_x$Energy_volatil)] <- 0



x_features <- FDD_groups %>%
  ungroup() %>% 
  mutate(Diff_TR = Last_TR - First_TR,
         Diff_Vol = Last_Vol - First_Vol,
         Diff_Ener = Last_Ener - First_Ener,
         Season = Failure_period
         # Season = ifelse(between(Failure_period,5,9), 1, 0)
         ) %>% 
  left_join(TR_volatil_x) %>% 
  left_join(Vol_volatil_x) %>% 
  left_join(Ener_volatil_x)


# x_features$Fault_Repetition[is.na(x_features$Fault_Repetition)] <- NA
# x_features$Fault_Repetition[x_features$Fault_Repetition == "FALSE"] <- 0
# x_features$Fault_Repetition[x_features$Fault_Repetition == "TRUE"] <- 1


# Season
x_features$Season[x_features$Failure_period == 1] <- 0
x_features$Season[x_features$Failure_period == 2] <- 1
x_features$Season[x_features$Failure_period == 3] <- 2
x_features$Season[x_features$Failure_period == 4] <- 3
x_features$Season[x_features$Failure_period == 5] <- 4
x_features$Season[x_features$Failure_period == 6] <- 5
x_features$Season[x_features$Failure_period == 7] <- 5
x_features$Season[x_features$Failure_period == 8] <- 4
x_features$Season[x_features$Failure_period == 9] <- 3
x_features$Season[x_features$Failure_period == 10] <- 2
x_features$Season[x_features$Failure_period == 11] <- 1
x_features$Season[x_features$Failure_period == 12] <- 0


x_train <- x_features %>% dplyr::select(Treturn_Pattern,
                                        Volume_Pattern,
                                        Energy_Pattern,
                                        # Failure_days,
                                        Season,
                                        # Fault_Repetition,
                                        Treturn_volatil,
                                        Volume_volatil,
                                        Energy_volatil
                                        # Diff_TR,
                                        # Diff_Vol,
                                        # Diff_Ener
                                        ) %>% 
  mutate(TR_S = ifelse(Treturn_Pattern == "Stroke", 1, 0),
         TR_LS = ifelse(Treturn_Pattern == "Level shift", 1, 0),
         TR_LS_D = ifelse(Treturn_Pattern == "Level shift with degradation", 1, 0),
         TR_D = ifelse(Treturn_Pattern == "Degradation", 1, 0),
         Vol_NO = ifelse(Volume_Pattern == "Not observed", 1, 0),
         Vol_S = ifelse(Volume_Pattern == "Stroke", 1, 0),
         Vol_LS = ifelse(Volume_Pattern == "Level shift", 1, 0),
         Vol_LS_D = ifelse(Volume_Pattern == "Level shift with degradation", 1, 0),
         Vol_D = ifelse(Volume_Pattern == "Degradation", 1, 0),
         Ener_NO = ifelse(Energy_Pattern == "Not observed", 1, 0),
         Ener_S = ifelse(Energy_Pattern == "Stroke", 1, 0),
         Ener_LS = ifelse(Energy_Pattern == "Level shift", 1, 0),
         # Ener_LS_D = ifelse(Energy_Pattern == "Level shift with degradation", 1, 0),
         Ener_D = ifelse(Energy_Pattern == "Degradation", 1, 0)
         # Season_cold = ifelse(Season < 0, -Season, 0),
         # Season_warm = ifelse(Season > 0, Season, 0)
         ) %>% 
  dplyr::select(-Treturn_Pattern, -Volume_Pattern, -Energy_Pattern)



ads.train <- scale(as.matrix(x_train))

# make a SOM grid
set.seed(100)

sample.size <- nrow(x_train)
grid.size <- ceiling(sample.size^(1/2.5))


ads.grid <- kohonen::somgrid(xdim = 4,
                             ydim = 4,
                             topo = "hexagonal",
                             toroidal = FALSE)

# Model SOM


ads.model <- som(ads.train,
                 ads.grid,
                 rlen = 20000,
                 # radius = 2.5,
                 keep.data = TRUE,
                 dist.fcts = "sumofsquares")

ads.model$unit.classif
somQuality(ads.model, ads.train)

# Training progress
plot(ads.model, type = "changes")


plot(ads.model, type = "mapping", pchs = 19, shape = "round")
plot(ads.model, type = "codes", codeRendering = "segments", main = "Codes Plot", palette.name = rainbow, shape = "round")

plot(ads.model, type="quality")


# Node count
plot(ads.model, type = "counts")

# Neighbour distance plot
plot(ads.model, type = "dist.neighbours")

# Properties plot


png(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/plt.som_one_hot.png",
    width = 1200, height = 600)

color_x <- "black"
par(mfrow = c(3, 5))
par(cex.main = 1.6)  # Adjust cex.main as needed
cex_size = 1.6

### RETURN TEMPERATURE  --------------------------

plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"TR_S"],
     main="a) Return temp.\nStroke",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)


box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"TR_LS"],
     main="b) Return temp.\nLevel shift",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"TR_LS_D"],
     main="c) Return temp.\nLevel shift with degradation",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"TR_D"],
     main="d) Return temp.\nDegradation",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


# plot(ads.model,
#      type = "property",
#      property = ads.model$codes[[1]][,"TR_NO"],
#      main="i) Return temp.\nNot observed",
#      shape = "round",
#      palette.name=topo.colors)


plot(1,
     type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "",
     main = "e) Return temp.\nNot observed")


box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


### VOLUME  --------------------------

plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Vol_S"],
     main="f) Volume\nStroke",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Vol_LS"],
     main="g) Volume\nLevel shift",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Vol_LS_D"],
     main="h) Volume\nLevel shift with degradation",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Vol_D"],
     main="i) Volume\nDegradation",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width

plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Vol_NO"],
     main="j) Volume\nNot observed",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


### ENERGY  --------------------------

plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Ener_S"],
     main="k) Energy\nStroke",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Ener_LS"],
     main="l) Energy\nLevel shift",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(1,
     type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "",
     main = "m) Energy\nLevel shift with degradation")

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Ener_D"],
     main="n) Energy\nDegradation",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Ener_NO"],
     main="o) Energy\nNot observed",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


# Close the device
dev.off()



### OTHER VARIABLES ----------------------------- 

png(filename = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Figures/plt.som_numeric.png",
    width = 800, height = 200)

par(mfrow = c(1, 4))
par(cex.main = 1.6)  # Adjust cex.main as needed
cex_size = 1.6


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Treturn_volatil"],
     main="a) Return temperature\nvolatility",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)


box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Volume_volatil"],
     main="b) Volume volatility",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width

plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Energy_volatil"],
     main="c) Energy volatility",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width




plot(ads.model,
     type = "property",
     property = ads.model$codes[[1]][,"Season"],
     main="d) Season",
     shape = "straight",
     palette.name=topo.colors,
     cex = cex_size)

box("figure", col= color_x, lwd = 1) # lwd - line tickness/width


# Close the device
dev.off()





#### CLUSTERING IN SOM --------------------------


set.seed(100)

# K-means
fviz_nbclust(ads.model$codes[[1]], kmeans, method = "silhouette", k.max = 8)
clust.km <- kmeans(ads.model$codes[[1]], 5)
clust <- clust.km$cluster

bg_colors <- viridis(5)

plot(ads.model,
     type = "mapping",
     pch = 21, col = "black", bg = "#D3D3D3",
     cex = 1.3,
     #col = "#D3D3D3",
     shape = "straight",
     bgcol = bg_colors[clust],
     main = "SOM with k-means (5 clusters)")
add.cluster.boundaries(ads.model, clust, col = "#A57439", lwd = 9)





# plot(ads.model, type = "codes",
#      codeRendering = "segments",
#      bgcol = rainbow(9)[clust],
#      shape = "round",
#      main = "b) Cluster map")
# add.cluster.boundaries(ads.model, clust)



aweSOMplot(som = ads.model, type = "Cloud", data = x_features,
           variables = c("Main_system", "meter_id", "FDD_category"),
           superclass = clust)

df_final <- x_features %>% 
  transmute(meter_id, Main_system, FDD_category, SOM_grid = ads.model[["unit.classif"]])



y_clust <- seq(1,5)

# Convert clust to a long format where the column name becomes a value in a new variable
clust_long <- reshape2::melt(clust, variable.name = "V_col", value.name = "y_clust") %>% 
  mutate(SOM_grid = seq(1,nrow(clust_long)))


df_final <- df_final %>% 
  left_join(clust_long) %>% 
  mutate(n_count = 1)






ggplot(data = df_final,
                       aes(fill = Main_system,
                           y = n_count,
                           x = y_clust)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  xlab("Cluster") +
  ylab("Number of cases") +
  labs(fill = "Main System:") +  
  theme(legend.position = "bottom")



# ggplot(data = df_final,
#                        aes(fill = FDD_category,
#                            y = n_count,
#                            x = y_clust)) + 
#   geom_bar(position="stack", stat="identity") +
#   theme_bw() +
#   xlab("Cluster") +
#   ylab("Number of cases") +
#   labs(fill = "Fault labels:") +  
#   theme(legend.position = "bottom")






# ## use hierarchical clustering to cluster the codebook vectors
# som_cluster <- cutree(hclust(dist(ads.model$codes)), 3)
# # plot these results:
# plot(ads.model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
# add.cluster.boundaries(ads.model, som_cluster)
# 
# 
# 
# 
# 
# 
# 
# ### Clustering
# 
# superclust_pam <- cluster::pam(ads.model$codes[[1]], 3)
# superclasses_pam <- superclust_pam$clustering
# 
# # superclust_hclust <- hclust(dist(ads.model$codes[[1]]), "complete")
# # superclasses_hclust <- cutree(superclust_hclust, 3)
# 
# 
# aweSOMplot(som = ads.model, type = "Cloud", data = x_features, 
#            variables = c("Main_system", "meter_id", "Main_system"),
#            superclass = superclasses_hclust)



