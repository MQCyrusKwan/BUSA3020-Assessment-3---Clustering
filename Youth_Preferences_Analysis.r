# Youth Preferences: Analysis
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 10/05/2021
# Accessible via: https://github.com/MQCyrusKwan/BUSA3020-Assessment-3---Clustering

list_NA <- function(dataframe){
    # Returns a list column names that contain NA values from a parsed data frame
    new_list_NA <- colnames(dataframe)[apply(dataframe, 2, anyNA)]
    return(new_list_NA)
}

wssplot <- function(data, nc=15, seed=1234, title="k-means Elbow Plot"){
    # WSS plot function
    wss <- (nrow(data)-1)*sum(apply(data, 2, var))
    for(i in 2:nc){
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
    plot(1:nc, 
         wss, 
         type='b', 
         main=title, 
         xlab="Number of Clusters", 
         ylab="Within groups sum of squares")
}