# Consider a library as the location where packages are stored.
# Execute the following command to list all the libraries available in your
# computer:
.libPaths()

# One of the libraries should be a folder inside the project if you are using
# renv

# Then execute the following command to see which packages are available in
# each library:
lapply(.libPaths(), list.files)



if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# Introduction ----
# Clustering is a type of unsupervised machine learning technique that aims to
# group similar data points together into clusters or segments based on certain
# characteristics or similarities, without the need for predefined labels or
# target outcomes. In clustering, the goal is to discover hidden patterns or
# structures in data and to create natural groupings of data points.

# STEP 1. Install and Load the Required Packages ----
## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naniar ----
if (require("naniar")) {
  require("naniar")
} else {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## ggplot2 ----
if (require("ggplot2")) {
  require("ggplot2")
} else {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## corrplot ----
if (require("corrplot")) {
  require("corrplot")
} else {
  install.packages("corrplot", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## ggcorrplot ----
if (require("ggcorrplot")) {
  require("ggcorrplot")
} else {
  install.packages("ggcorrplot", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## stringi ----
if (require("stringi")) {
  require("stringi")
} else {
  install.packages("stringi", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## dplyr ----
if (require("dplyr")) {
  require("dplyr")
} else {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# STEP 2. Load the Dataset ----


# Load the necessary library for reading CSV files (if not already loaded)
library(readr)

# Load the Air Traffic Passenger dataset
Air_Traffic_Passenger_Statistics <- read_csv("data/Air_Traffic_Passenger_Statistics.csv",
                                             col_types = cols(
                                               Activity_Period = col_integer(),
                                               Year = col_integer(),
                                               Passenger_Count = col_integer(),
                                               Adjusted_Passenger_Count = col_integer(),
                                               Month = col_character(),
                                               Boarding_Area = col_character(),
                                               Terminal = col_character(),
                                               Activity_Type_Code = col_character(),
                                               Price_Category_Code = col_character(),
                                               Published_Airline_IATA_Code = col_character(),
                                               GEO_Summary = col_character(),
                                               GEO_Region = col_character(),
                                               Operating_Airline = col_character(),
                                               Operating_Airline_IATA_Code = col_character(),
                                               Published_Airline = col_character(),
                                               Adjusted_Activity_Type_Code = col_character()
                                             )
)



str(Air_Traffic_Passenger_Statistics)
dim(Air_Traffic_Passenger_Statistics)
head(Air_Traffic_Passenger_Statistics)
summary(Air_Traffic_Passenger_Statistics)

# STEP 3. Check for Missing Data and Address it ----
# Are there missing values in the dataset?
any_na(Air_Traffic_Passenger_Statistics)

# How many?
n_miss(Air_Traffic_Passenger_Statistics)

# What is the proportion of missing data in the entire dataset?
prop_miss(Air_Traffic_Passenger_Statistics)

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(Air_Traffic_Passenger_Statistics)

# Which variables contain the most missing values?
gg_miss_var(Air_Traffic_Passenger_Statistics)

# Which combinations of variables are missing together?
gg_miss_upset(Air_Traffic_Passenger_Statistics)

# Where are missing values located (the shaded regions in the plot)?
vis_miss(Air_Traffic_Passenger_Statistics) +
  theme(axis.text.x = element_text(angle = 80))

## OPTION 1: Remove the observations with missing values ----
# We can decide to remove all the observations that have missing values
# as follows:
AirTraffic_removed_obs <- na.omit(Air_Traffic_Passenger_Statistics)

# The initial dataset had 15007 observations and 16 variables
dim(Air_Traffic_Passenger_Statistics)

# The filtered dataset has 14953 observations and 16 variables
dim(AirTraffic_removed_obs)

## OPTION 2: Remove the variables with missing values ----
# Alternatively, we can decide to remove the 2 variables that have missing data
AirTraffic_removed_vars <-
  Air_Traffic_Passenger_Statistics %>%
  dplyr::select(- Operating_Airline_IATA_Code, - Published_Airline_IATA_Code)

# The initial dataset had 15007 observations and 16 variables
dim(Air_Traffic_Passenger_Statistics)

# The filtered dataset has 15007 observations and 14 variables
dim(AirTraffic_removed_vars)

# Are there missing values in the dataset?
any_na(AirTraffic_removed_vars)


# STEP 4. Perform EDA and Feature Selection ----
## Compute the correlations between variables ----
# We identify the correlated variables because it is these correlated variables
# that can then be used to identify the clusters.

# Create a correlation matrix
# Option 1: Basic Table
cor(AirTraffic_removed_obs[, c(1,12,14,15)]) %>%
  View()

# Option 2: Basic Plot
cor(AirTraffic_removed_obs[, c(1,12,14,15)]) %>%
  corrplot(method = "square")

# Option 3: Fancy Plot using ggplot2
corr_matrix <- cor(AirTraffic_removed_obs[, c(1,12,14,15)])

p <- ggplot2::ggplot(data = reshape2::melt(corr_matrix),
                     ggplot2::aes(Var1, Var2, fill = value)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = label_wrap(label, width = 10)),
                     size = 4) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

#The correlation plot shows a 0.06 correlation between the Year and the
# Passenger Count.

# Other non-numeric variables and categorical variables are 
# not included in the correlation, but they can be used as an 
# additional dimension when plotting the scatter plot during EDA.


## Plot the scatter plots ----
# A scatter plot to show the activity period against year
# per terminal
ggplot(AirTraffic_removed_obs,
       aes(Passenger_Count, Year,
           color = Terminal)) +
  geom_point(alpha = 0.5) +
  xlab("Number of Passengers") +
  ylab("Number of Passengers/Terminal")

# A scatter plot to show the number of passenger against year
# per boarding area
ggplot(AirTraffic_removed_obs,
       aes(Passenger_Count, Year,
           color = Boarding_Area)) +
  geom_point(alpha = 0.5) +
  xlab("Number of Passengers") +
  ylab("Number of Passenger/Boarding Area")

# A scatter plot to show the number of passenger against year
# per geographic area
ggplot(AirTraffic_removed_obs,
       aes(Passenger_Count, Year,
           color = GEO_Region)) +
  geom_point(alpha = 0.5) +
  xlab("Number of Passengers") +
  ylab("Number of Passengers/GEO Region")


## Transform the data ----
# The K Means Clustering algorithm performs better when data transformation has
# been applied. This helps to standardize the data making it easier to compare
# multiple variables.

summary(AirTraffic_removed_obs)
model_of_the_transform <- preProcess(AirTraffic_removed_obs,
                                     method = c("scale", "center"))
print(model_of_the_transform)
AirTraffic_removed_obs_std <- predict(model_of_the_transform, # nolint
                                      AirTraffic_removed_obs)
summary(AirTraffic_removed_obs_std)
sapply(AirTraffic_removed_obs_std[, c(1,12,14,15)], sd)

## Select the features to use to create the clusters ----
# OPTION 1: Use all the numeric variables to create the clusters
AirTraffic_vars <-
  AirTraffic_removed_obs_std[, c(1,12,14,15)]
# OPTION 2: Use only the most significant variables to create the clusters
#AirTraffic_vars <-
  #AirTraffic_removed_obs_std[, c("Month",
                                       #"GEO_Region")]

# STEP 5. Create the clusters using the K-Means Clustering Algorithm ----
# We start with a random guess of the number of clusters we need
set.seed(7)
kmeans_cluster <- kmeans(AirTraffic_vars, centers = 4, nstart = 20)

# We then decide the maximum number of clusters to investigate
n_clusters <- 8


# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(7)

# Investigate 1 to n possible clusters (where n is the maximum number of 
# clusters that we want to investigate)
for (i in 1:n_clusters) {
  # Use the K Means cluster algorithm to create each cluster
  kmeans_cluster <- kmeans(AirTraffic_vars, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- kmeans_cluster$tot.withinss
}

## Plot a scree plot ----
# The scree plot should help you to note when additional clusters do not make
# any significant difference (the plateau).
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8)) +
  xlab("Number of Clusters")

scree_plot

# We can add guides to make it easier to identify the plateau (or "elbow").
scree_plot +
  geom_hline(
    yintercept = wss,
    linetype = "dashed",
    col = c(rep("#000000", 5), "#FF0000", rep("#000000", 2))
  )

# The plateau is reached at 5 clusters.
# We therefore create the final cluster with 5 clusters
# (not the initial 4 used at the beginning of this STEP.)
k <- 5
set.seed(7)
# Build model with k clusters: kmeans_cluster
kmeans_cluster <- kmeans(AirTraffic_vars, centers = k, nstart = 20)

# STEP 6. Add the cluster number as a label for each observation ----
AirTraffic_removed_obs$cluster_id <- factor(kmeans_cluster$cluster)

## View the results by plotting scatter plots with the labelled cluster ----
ggplot(AirTraffic_removed_obs, aes(Passenger_Count, Month,
                                         color = cluster_id)) +
  geom_point(alpha = 0.5) +
  xlab("Number of Passengers") +
  ylab("Month")


