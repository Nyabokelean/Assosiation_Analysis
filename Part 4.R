## Anomaly detection

# load in the dataset

supermarket2 <- read.csv("C:/Users/leonb/Downloads/Supermarket_Sales_Forecasting - Sales.csv", sep = ",", header = TRUE)
head(supermarket2)


# Installing anomalize package
# ---
# 
install.packages("anomalize")

# Load tidyverse and anomalize
# ---
# 
library(tidyverse)
library(anomalize)
library(AnomalyDetection)
tidyverse_cran_downloads

# Detecting our anomalies
# ---

data <- supermarket2 %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date)"%m/%d/%Y") %>% 
head(data)

 
data %>%
  group_by(Date) %>%
  summarise(Sales = sum(Sales)) %>%
  time_decompose(Sales, frequency = "auto", method = "stl") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose() %>%
# Anomaly Visualization
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25)

# There were no anomalies thatwere detected

