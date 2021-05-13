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
pacman::p_load(dplyr, reshape, ggplot2, ggfortify, VIM, NbClust, dbscan, factoextra)

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
survey_data <- VIM::kNN(data=survey_raw) %>%
    select("Slow.songs.or.fast.songs":"Opera", "Horror":"Action", "Age":"House...block.of.flats")

# Separate continuous music and movies data from mostly categorical demographics data
music_movies <- survey_data %>%
    select("Slow.songs.or.fast.songs":"Opera", "Horror":"Action")

# Correlation matrix between music and moves
# Graphically produced in excel: "Music_Movies_Corr.xlsx"
preferences_cormat <- round(cor(music_movies), 2)
ggcorrplot(preferences_cormat, hc.order=TRUE, type="lower", lab=TRUE)
write.csv(preferences_cormat, "Music_Movies_Corr.csv")
    
# ------------------------------------------------------------------------------------------|
# PRINCIPAL COMPONENTS ANALYSIS:

# Principal components analysis of music and movie preferences
MM_PCA <- prcomp(music_movies)

summary(MM_PCA)

RMM_PCA <- varimax(MM_PCA$rotation[, 1:14])$loadings

# plot the rotated loadings
plot(RMM_PCA, 
     col = "lightblue", 
     pch = 19, 
     cex = 2,
     main = "Rotated Factors")
text(RMM_PCA, 
     labels = rownames(RMM_PCA), 
     cex = 0.75, 
     font = 1)


# Screeplot of principal components by squared variance
MM_eigen <- MM_PCA$sdev^2
plot(MM_eigen,
     type = 'b',
     xlab = "Principal Component #",
     ylab = "Eigenvalues",
     main = "Screeplot of Music & Movies")
abline(h = 1, col = "red", lty = 5)

# Plot of principal components by cumulative proportion
MM_cumprob <- cumsum(MM_PCA$sdev^2/sum(MM_PCA$sdev^2))
plot(MM_cumprob,
     type = 'b',
     xlab = "Principal Component #",
     ylab = "Explained Variance",
     main = "Cumulative Variance of Music & Movies")
abline(v = 14, col = "blue", lty = 5)
abline(h = MM_cumprob[14], col = "blue", lty = 5)

# Optimal principal components PC1 to PC12
opt_MM_PC <- data.frame(MM_PCA$x[,1:14])

# Correlation matrix between principal components and preferences
# Graphically produced in excel: "Preferences_PCA.xlsx"
write.csv(MM_PCA$rotation, "MM_PCA.csv")

# ------------------------------------------------------------------------------------------|
# CLUSTERING ALGORITHMS:

# WSS Plot to choose maximum number of clusters
wssplot(music_movies, title="Music & Movies k-means Elbow Plot")
wssplot(opt_MM_PC, title="Number of Clusters k-means Elbow Plot")

# Determines optimal number of clusters
# > Optimal clusters for music & movies
MM_nClust = NbClust(music_movies, method = "kmeans")

# > Optimal clusters for principal components
PC_nClust = NbClust(opt_MM_PC, method = "kmeans")

# K-means Clustering
# > Apply k-means with k=2
# > pair plot of principal components
PC_KMClust <- kmeans(opt_MM_PC, 2)
plot(opt_MM_PC, col = PC_KMClust$clust)

# > Detailed plots of principal components
MM_KMClust <- kmeans(music_movies, 2)
for(component in 2:length(opt_MM_PC)){
    KMP <- autoplot(MM_KMClust, 
           music_movies, 
           frame=TRUE, 
           main=paste("PCA plot for PC1 by PC", component),
           x=1, 
           y=component)

    ggsave(KMP, file=paste("Graphs - Clustering/", 
                       "k-means_2_PC1-", 
                       component, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# DBSCAN
# > Identifying the optimal eps(epsilon) value
kNNdistplot(music_movies, k=5)
abline(h=6.5, col="red", lty=5)
title("kNN Epsilon Plot")

# > Perform DBSCAN
set.seed(1234)
MM_db <- dbscan(MM_PCA$x, eps=6.5, minPts=2)

# > Pair plot of DBSCAN clusters
plot(opt_MM_PC, col=MM_db$cluster)

# > Closer look at PC1 clusters
for(component in 2:length(opt_MM_PC)){
    DBP<- ggplot(MM_PCA$x, aes(x=PC1, 
                               y=eval(parse(text=paste("PC", component, sep=''))), 
                               main="DBSCAN"))+
    geom_point(aes(color=factor(MM_db$cluster)))+
    ggtitle(paste("Music & Movies DBSCAN PC1 to PC", component, sep=''))+
    ylab(paste("PC", component, sep=''))

    ggsave(DBP, file=paste("Graphs - Clustering/", 
                       "DBSCAN_2_PC1-", 
                       component, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# ------------------------------------------------------------------------------------------|
# PROFILING:

# Merge dataframes such that clusters can be used to separate observations
KM_Merge <- data.frame(survey_data, cluster=MM_KMClust$cluster)

# Create demographics dataframe for profiling
demographics <- survey_data %>%
    select("Age":"House...block.of.flats")

# Histograms of Ages for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    hist_age <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(Age))+
        geom_histogram(stat="count")+
        ggtitle(paste("Histogram of Age in Cluster", clust_num))+
        ylim(0,150)

    ggsave(hist_age, file=paste("Graphs - Profiling/", 
                       "Histogram of Age in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# Histogram of Height for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    hist_height <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(Height))+
        geom_histogram(stat="bin")+
        ggtitle(paste("Histogram of Height in Cluster", clust_num))

    ggsave(hist_height, file=paste("Graphs - Profiling/", 
                       "Histogram of Height in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# Histogram of Weight for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    hist_weight <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(Weight))+
        geom_histogram(stat="bin")+
        ggtitle(paste("Histogram of Weight in Cluster", clust_num))

    ggsave(hist_weight, file=paste("Graphs - Profiling/", 
                       "Histogram of Weight in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# Histogram of Number of Siblings for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    hist_nsib <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(Number.of.siblings))+
        geom_bar()+
        ggtitle(paste("Barplot of Number of Siblings in Cluster", clust_num))

    ggsave(hist_nsib, file=paste("Graphs - Profiling/", 
                       "Barplot of Number of Siblings in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# Bar plot of Number of Siblings for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    gender <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(Gender))+
        geom_bar()+
        ggtitle(paste("Barplot of Gender in Cluster", clust_num))

    ggsave(gender, file=paste("Graphs - Profiling/", 
                       "Barplot of Gender in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# Bar plot of Handedness for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    handed <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(Left...right.handed))+
        geom_bar()+
        ggtitle(paste("Barplot of Handedness in Cluster", clust_num))

    ggsave(handed, file=paste("Graphs - Profiling/", 
                       "Barplot of Handedness in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# Bar plot of Education for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    education <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(Education))+
        geom_bar()+
        ggtitle(paste("Barplot of Education in Cluster", clust_num))

    ggsave(education, file=paste("Graphs - Profiling/", 
                       "Barplot of Education in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# Bar plot of Village or Town for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    vil_town <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(Village...town))+
        geom_bar()+
        ggtitle(paste("Barplot of Village - Town in Cluster", clust_num))

    ggsave(vil_town, file=paste("Graphs - Profiling/", 
                       "Barplot of Village - Town in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}

# Bar plot of House or Block of Flats for each cluster
for(clust_num in unique(KM_Merge$cluster)){
    house_flats <- KM_Merge %>%
    filter(cluster==clust_num) %>%
    ggplot(aes(House...block.of.flats))+
        geom_bar()+
        ggtitle(paste("Barplot of House - Block of Flats in Cluster", clust_num))

    ggsave(house_flats, file=paste("Graphs - Profiling/", 
                       "Barplot of House - Block of Flats in Cluster ", 
                       clust_num, 
                       ".png",
                       sep=''),
            width=8, 
            height=8)
}