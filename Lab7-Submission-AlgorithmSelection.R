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

# STEP 2. Load the Dataset ----
data(BreastCancer)

str(BreastCancer)
dim(BreastCancer)
head(BreastCancer)
summary(BreastCancer)

# STEP 3. Check for Missing Data and Address it ----
# Are there missing values in the dataset?
any_na(BreastCancer)

# How many?
n_miss(BreastCancer)

# What is the proportion of missing data in the entire dataset?
prop_miss(BreastCancer)

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(BreastCancer)

# Which variables contain the most missing values?
gg_miss_var(BreastCancer)

# Where are missing values located (the shaded regions in the plot)?
vis_miss(BreastCancer) +
  theme(axis.text.x = element_text(angle = 80))

## OPTION 1: Remove the observations with missing values ----
# We can decide to remove all the observations that have missing values
# as follows:
BreastCancer_removed_obs <- na.omit(BreastCancer)

# The initial dataset had 699 observations and 11 variables
dim(BreastCancer)

# The filtered dataset has 683 observations and 11 variables
dim(BreastCancer_removed_obs)

## OPTION 2: Remove the variables with missing values ----
# Alternatively, we can decide to remove the 2 variables that have missing data
BreastCancer_removed_vars <-
  BreastCancer %>%
  dplyr::select(- Bare.nuclei)

# The initial dataset had 699 observations and 11 variables
dim(BreastCancer)

# The filtered dataset has 699 observations and 10 variables
dim(BreastCancer_removed_vars)

# Are there missing values in the dataset?
any_na(BreastCancer_removed_vars)


## OPTION 3: Perform Data Imputation ----
