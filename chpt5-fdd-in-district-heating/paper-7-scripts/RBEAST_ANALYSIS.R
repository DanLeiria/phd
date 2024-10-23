# -------------------------------------------------------------------------
### LIBRARY OF PACKAGES
# -------------------------------------------------------------------------

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
library(ggdendro) # dendrograms
library(gplots) # heatmap
library(tseries) # bootstrap
library(TSclust) # cluster time series
library(dtwclust) # cluster time series with dynamic time warping
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
library(strucchange)
library(segmentr)
library(changepoint)
library(Rbeast)



# -------------------------------------------------------------------------
### SET WORKING DIRECTORY
# -------------------------------------------------------------------------

# Set working directory
setwd("C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis")  # Set working (main) directory
Sys.setenv(LANG = "en")                                                                                                   # Set system's language to English
Sys.setlocale("LC_ALL", "English")


# -------------------------------------------------------------------------
### LOAD DATASET
# -------------------------------------------------------------------------

# Load the dataset from the specified file path. The dataset is assumed to be an R data file.
load(file = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis/SHM_processed_BED4.rda")
load(file = "C:/Users/FV06XU/OneDrive - Aalborg Universitet/PhD/3. Papers/Journal papers/4. Faults in DH/Manuscript/Analysis/SHM_processed_BED4_full.rda")


dt_SHM_segmented <- dt_SHM_segmented %>% 
  arrange(meter_id, Date)

dt_SHM_preprocessed <- dt_SHM_preprocessed %>% 
  arrange(meter_id, Date)



df_id_segmented <- split(dt_SHM_segmented, dt_SHM_segmented$meter_id)
df_id_full <- split(dt_SHM_preprocessed, dt_SHM_preprocessed$meter_id)


ids_patterns <- c(10, 4, 6, 30)

dt_SHM <- as.data.table(dt_SHM_preprocessed)

dt_pattern_types <- dt_SHM[meter_id %in% ids_patterns]


  
# -------------------------------------------------------------------------
### RBEAST PACKAGE
# ------------------------------------------------------------------------- 

citation("Rbeast")


df_x <- dt_SHM_preprocessed %>% filter(meter_id == 20)
x_TR <- xts(df_x$Treturn, order.by = df_x$Date)
x_Vol <- xts(df_x$Volume, order.by = df_x$Date)
x_Ener <- xts(df_x$Energy, order.by = df_x$Date)

x_title = "Level shift pattern"

# par(mrfow = c(1,3))
# plot.xts(x_TR)
# plot.xts(x_Vol)
# plot.xts(x_Ener)

TR_plot <- ggplot(df_x,
                  aes(
                    x = Date, 
                    y = Treturn
                    )
                  ) +
  geom_line(color = "black", size = 0.6) +
  # geom_smooth(color = "red",
  #             size = 1.5,
  #             se = F,
  #             span = 0.1) +
  theme_bw() +
  ylab("Return temperature") +
  xlab("Time") +
  labs(title = paste(x_title, "- Return temperature")) +
  geom_vline(xintercept = as.Date(mean(df_x$FDD_start)), linetype = "dashed") +  # Add a dashed vertical line as a significant marker.
  geom_vline(xintercept = as.Date(mean(df_x$End)), linetype = "dashed") +  # Add another dashed vertical line as a significant marker.
  # scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +  # Set the x-axis to display months 1 through 12.
  annotate("rect", xmin = as.Date(mean(df_x$FDD_start)), xmax = as.Date(mean(df_x$End)), ymin = -Inf, ymax = Inf, alpha = .3, fill = "darkorange") +  # Highlight the area between the two vertical lines to emphasize a specific period.
  # geom_vline(xintercept = as.Date(mean(df_x$Visit_date)), linetype = "dashed", color = "darkgreen", size = 1.5) +  # Add a dashed vertical line as a significant marker.
  # annotate("text", x = as.Date(mean(df_x$Visit_date)), y = 30, label = "Intervention date", color = "darkgreen", angle = 90, vjust = -0.5) +
  annotate("rect", xmin = as.Date(mean(df_x$Visit_date)), xmax = tail(df_x$Date, 1), ymin = -Inf, ymax = Inf, alpha = .3, fill = "lightgreen")  # Highlight the area between the two vertical lines to emphasize a specific period.


Vol_plot <- ggplot(df_x,
                  aes(
                    x = Date, 
                    y = Volume
                  )
) +
  geom_line(color = "black", size = 0.6) +
  # geom_smooth(color = "red",
  #             size = 1.5,
  #             se = F,
  #             span = 0.1) +
  theme_bw() +
  ylab("Volume") +
  xlab("Time") +
  labs(title = paste(x_title, "- Volume")) +
  geom_vline(xintercept = as.Date(mean(df_x$FDD_start)), linetype = "dashed") +  # Add a dashed vertical line as a significant marker.
  geom_vline(xintercept = as.Date(mean(df_x$End)), linetype = "dashed") +  # Add another dashed vertical line as a significant marker.
  # scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +  # Set the x-axis to display months 1 through 12.
  annotate("rect", xmin = as.Date(mean(df_x$FDD_start)), xmax = as.Date(mean(df_x$End)), ymin = -Inf, ymax = Inf, alpha = .3, fill = "darkorange") +  # Highlight the area between the two vertical lines to emphasize a specific period.
  # geom_vline(xintercept = as.Date(mean(df_x$Visit_date)), linetype = "dashed", color = "darkgreen", size = 1.5) +  # Add a dashed vertical line as a significant marker.
  # annotate("text", x = as.Date(mean(df_x$Visit_date)), y = 30, label = "Intervention date", color = "darkgreen", angle = 90, vjust = -0.5) +
  annotate("rect", xmin = as.Date(mean(df_x$Visit_date)), xmax = tail(df_x$Date, 1), ymin = -Inf, ymax = Inf, alpha = .3, fill = "lightgreen")  # Highlight the area between the two vertical lines to emphasize a specific period.


Ener_plot <- ggplot(df_x,
                   aes(
                     x = Date, 
                     y = Energy
                   )
) +
  geom_line(color = "black", size = 0.6) +
  # geom_smooth(color = "red",
  #             size = 1.5,
  #             se = F,
  #             span = 0.1) +
  theme_bw() +
  ylab("Energy") +
  xlab("Time") +
  labs(title = paste(x_title, "- Energy")) +
  geom_vline(xintercept = as.Date(mean(df_x$FDD_start)), linetype = "dashed") +  # Add a dashed vertical line as a significant marker.
  geom_vline(xintercept = as.Date(mean(df_x$End)), linetype = "dashed") +  # Add another dashed vertical line as a significant marker.
  # scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +  # Set the x-axis to display months 1 through 12.
  annotate("rect", xmin = as.Date(mean(df_x$FDD_start)), xmax = as.Date(mean(df_x$End)), ymin = -Inf, ymax = Inf, alpha = .3, fill = "darkorange") +  # Highlight the area between the two vertical lines to emphasize a specific period.
  # geom_vline(xintercept = as.Date(mean(df_x$Visit_date)), linetype = "dashed", color = "darkgreen", size = 1.5) +  # Add a dashed vertical line as a significant marker.
  # annotate("text", x = as.Date(mean(df_x$Visit_date)), y = 30, label = "Intervention date", color = "darkgreen", angle = 90, vjust = -0.5) +
  annotate("rect", xmin = as.Date(mean(df_x$Visit_date)), xmax = tail(df_x$Date, 1), ymin = -Inf, ymax = Inf, alpha = .3, fill = "lightgreen")  # Highlight the area between the two vertical lines to emphasize a specific period.



ggarrange(
  TR_plot,
  Vol_plot,
  Ener_plot,
  labels = c("a)", "b)", "c)"),
  ncol = 1,
  nrow = 3)






ts_beast <- beast(x_TR, 
                  season = "none",
                  ocp=Inf, 
                  mcmc.seed = 123)

plot_list <- c("y",
               "t",
               "tcp",
               # "tslp",
               "slpsgn",
               "o",
               "ocp",
               "error")

plot(ts_beast,
     main = paste0(x_title, ": Return temperature"),
     vars = plot_list
     )


v <- tsextract(ts_beast)
remainder <- v$data - v$trend$Y - v$outlier$Y

hist(remainder)

library("car")
qqPlot(remainder)

for (i in 1:50){

  df_x <- dt_SHM_preprocessed %>% filter(meter_id == i)
  x_TR <- xts(df_x$Treturn, order.by = df_x$Date)
  pattern_i <- unique(df_x$Treturn_Pattern)

  # decomp_change <- beast(x_TR, start = as.Date(index(x_TR[1])), deltat = 1, season = "none", ocp = Inf)
  decomp_change <- beast(x_TR,
                         season = "none",
                         ocp = Inf, 
                         mcmc.seed = 123)

  plot(decomp_change, main = paste(i, pattern_i))


}






