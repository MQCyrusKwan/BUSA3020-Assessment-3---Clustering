# Youth Preferences: Main
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 09/05/2021
# Accessible via: 

# Install package manager
if(find.package("pacman") == FALSE){
    install.packages("pacman")
}

# load packages
pacman::p_load(dplyr)

# Data from: https://ilearn.mq.edu.au/mod/folder/view.php?id=6351280
survey_responses <- read.csv(file="responses.csv")

# Extract music, movies, and demographics data
music <- survey_responses %>%
    select("Music":"Opera")
movies <- survey_responses %>%
    select("Movies":"Action")
demographics <- survey_responses %>%
    select("Age":"House - block of flats")