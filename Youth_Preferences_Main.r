# Youth Preferences: Main
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 10/05/2021
# Accessible via: https://github.com/MQCyrusKwan/BUSA3020-Assessment-3---Clustering

# Install package manager
if(find.package("pacman") == FALSE){
    install.packages("pacman")
}

# load packages
pacman::p_load(dplyr, VIM)

# Source functions from other R scripts
source(file = "Youth_Preferences_Analysis.r")

# Data from: https://ilearn.mq.edu.au/mod/folder/view.php?id=6351280
survey_raw <- read.csv(file="responses.csv")

# ------------------------------------------------------------------------------------------|
# DESCRIPTIVE ANALYSIS:
head(survey_data)
str(survey_data)
summary(survey_data)

# ------------------------------------------------------------------------------------------|
# DATA TREATMENT:

# Creates new dataframe that uses KNN imputation
# Extracts music, movies, and demographics data
survey_data <- kNN(data = survey_raw) %>%
    select("Music":"Opera", "Movies":"Action", "Age":"House...block.of.flats")