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