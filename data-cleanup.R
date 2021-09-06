#sources:
#https://swcarpentry.github.io/r-novice-inflammation/03-loops-R/

#importing and cleaning data init
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rlist)

#import datasets - starting with first one as a test
#make lists for the different data conditions
Hospital_Data <- list.files(path = "C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/hospitals_current_data/EVALUATED", pattern = "*.csv", full.names = TRUE)
Transposed_Data <- NULL
Not_Transposed <- NULL
Duplicates_Exist <- NULL

for(j in 1:length(Hospital_Data)) {
  data = read.csv(Hospital_Data[j])
  
  #group the data by facility.ID to check for duplicate rows
  data_summary <-
    data %>%
    group_by(Facility.ID) %>%
    dplyr::summarize(count = n())
  
  #check how many duplicate facility ID's there are in each dataframe
  var_count <- 0
  for(i in 1:nrow(data_summary)) {
    if( data_summary$count[i] > 1 ) {
      var_count = var_count + 1;
    }
  }
  print(paste0("duplicates: ", var_count, " in dataset: ", Hospital_Data[j]))
  
  #separate dataframes with duplicates and without duplicates into separate lists
  if( var_count == 0 ) {
    Transposed_Data <- list.append(Transposed_Data, Hospital_Data[j])
  }
  
  if( var_count > 0 ) {
    Not_Transposed <- list.append(Not_Transposed, Hospital_Data[j])
  }
}

#Not_Transposed[1] is transposed but has duplicates - Move element 1 to Transposed_Data list
Transposed_Data <- list.append(Transposed_Data, Not_Transposed[1])
Not_Transposed <- Not_Transposed[-1]

#Create lists of dataframes for transposed and not transposed data
NT_DF <- list()
T_DF <- list()

for(j in 1:length(Transposed_Data)) {
  T_DF <- list.append(T_DF, read.csv(Transposed_Data[j]))
}

for(j in 1:length(Not_Transposed)) {
  NT_DF <- list.append(NT_DF, read.csv(Not_Transposed[j]))
}

#view datasets to determine which columns are needed
for(j in 1:length(NT_DF)) {
  glimpse(NT_DF[j])
  print("Next Dataframe...")
}

#Pull columns needed from 
