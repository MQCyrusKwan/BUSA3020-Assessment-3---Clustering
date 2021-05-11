# Youth Preferences: Main
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 12/05/2021
# Accessible via: https://github.com/MQCyrusKwan/BUSA3020-Assessment-3---Clustering

# Install package manager
if(find.package("pacman") == FALSE){
    install.packages("pacman")
}

# load packages
pacman::p_load(dplyr, reshape, ggplot2, VIM)

# Source functions from other R scripts
source(file = "Youth_Preferences_Analysis.r")

# Data from: https://ilearn.mq.edu.au/mod/folder/view.php?id=6351280
survey_raw <- read.csv(file="responses.csv", na.strings='')

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

# Removed redundant "Music"
music <- survey_data %>%
    select("Slow.songs.or.fast.songs":"Opera")

# Removed redudnant "Movies"
movies <- survey_data %>%
    select("Horror":"Action")

# Create demographic dummy variables and removed character categories
# Dummy variables required so as to not interfere with algorithms
demographics <- survey_data %>%
    select("Age":"House...block.of.flats")
    for(column in demographics){
        if(is.character(column)){
            for(category in unique(column)){
                demographics[category] <- ifelse(column==toString(category), 1, 0)
            }
        }
    }
    demographics <- select(demographics, -c("Gender": "House...block.of.flats"))

# Merge preference dataframes
music_movies <- survey_data %>%
    select("Slow.songs.or.fast.songs":"Opera", "Horror":"Action")

preferences_cormat <- round(cor(music_movies), 2)
    
# ------------------------------------------------------------------------------------------|
# PRINCIPAL COMPONENTS ANALYSIS:

# Principal components analysis of music and movie preferences
preferences_PCA <- prcomp(music_movies)

summary(preferences_PCA)

# Screeplot of principal components by squared variance
preferences_eigen <- preferences_PCA$sdev^2
plot(preferences_eigen,
     type = 'b',
     xlab = "Principal Component #",
     ylab = "Eigenvalues",
     main = "Screeplot of Music & Movies")
abline(v = 12, col = "blue", lty = 5)
abline(h = preferences_eigen[12], col = "red", lty = 5)

# Plot of principal components by cumulative proportion
preferences_cumpro <- cumsum(preferences_PCA$sdev^2/sum(preferences_PCA$sdev^2))
plot(preferences_cumpro,
     type = 'b',
     xlab = "Principal Component #",
     ylab = "Explained Variance",
     main = "Cumulative Variance of Music & Movies")
abline(v = 12, col = "blue", lty = 5)
abline(h = preferences_cumpro[12], col = "red", lty = 5)

# Correlation matrix between principal components and preferences
# Graphically produced in excel: "Preferences_PCA.xlsx"
write.csv(preferences_PCA$rotation, "Preferences_PCA.csv")

# ------------------------------------------------------------------------------------------|
# CLUSTERING ALGORITHMS: