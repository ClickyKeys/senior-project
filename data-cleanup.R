# The purpose of this program is used to clean the hospital quality measure data in preparation for clustering aaalysis
# Current data can be downloaded from the following location: https://olemiss.app.box.com/s/ketgimkx4y26w2pusdy448l33dkl27cs
# The data in the folder titled "MEASURES" should be used

# Uncomment and run to install/update the following packages (whichever haven't already been installed) if running this code for the first time
# install.packages("tidyverse")
# install.packages("rlist")

# Load the necessary packages
library(tidyverse)
library(rlist)

# Import the datasets selected from CMS's hospital compare data - Change the paths on lines 14 & 15 to the location of the 'MEASURES' folder on your machine
# and change '\' to '/' in the filepath designation (otherwise it will not work)
# WARNING: Using updated, different, or additional dataframes apart from the data downloaded from GitHub may require significant changes to the code
Hospital_Data <- list.files(path = "C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/hospitals_current_data/MEASURES", pattern = "*.csv", full.names = TRUE)
names(Hospital_Data) <- list.files(path = "C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/hospitals_current_data/MEASURES", pattern = "*.csv", full.names = FALSE)

# Create lists to hold data depending on the needed transformations
# 'Transposed_Data' for dataframes that do not need to be transposed
# 'Not_Transposed' for dataframes that do need to be transposed
Transposed_Data <- list()
Not_Transposed <- list()
Duplicates_Exist <- NULL
T_Names <- NULL
NT_Names <- NULL

for(j in 1:length(Hospital_Data)) {
  
  data = read.csv(Hospital_Data[j])
  
  # Group the data by facility.ID to check for duplicate rows
  data_summary <-
    data %>%
    group_by(Facility.ID) %>%
    dplyr::summarize(count = n())

  # Check how many duplicate facility ID's there are in each dataframe
  var_count <- 0
  for(i in 1:nrow(data_summary)) {
    if( data_summary$count[i] > 1 ) {
      var_count = var_count + 1;
    }
  }

  print(paste0("duplicates: ", var_count, " in dataset: ", Hospital_Data[j]))
  
  # Separate dataframes with duplicates and without duplicates into separate lists
  if( var_count == 0 ) {
    Transposed_Data <- list.append(Transposed_Data, data.frame(data))
    T_Names <- list.append(T_Names, names(Hospital_Data[j]))
  }
  
  if( var_count > 0 ) {
    Not_Transposed <- list.append(Not_Transposed, data.frame(data))
    NT_Names <- list.append(NT_Names, names(Hospital_Data[j]))
  }
}

# Not_Transposed[1] is transposed but has duplicates - Move element 1 to Transposed_Data list
T_Names <- list.append(T_Names, NT_Names[1])
NT_Names <- NT_Names[-1]
Transposed_Data <- list.append(Transposed_Data, data.frame(Not_Transposed[1]))
Not_Transposed <- Not_Transposed[-1]

# Uncomment and run to glimpse dataframes in the 'Not Transposed' list
# Use this to check data and determine which columns are needed
# j <- 0
# length(Not_Transposed)
# for(j in 1:length(Not_Transposed)) {
#   glimpse(Not_Transposed[[j]])
#   cat("\n")
#   print("Next Dataframe...")
# }

# Uncomment and run to glimpse dataframes in the 'Not Transposed' list
# Use this to check data and determine which columns are needed
# j <- 0
# length(Transposed_Data)
# for(j in 1:length(Transposed_Data)) {
#   glimpse(Transposed_Data[[j]])
#   cat("\n")
#   print("\nNext Dataframe...\n")
# }

# Create new lists to hold desired columns from original dataframes
Transposed_Cols <- vector("list", length(Transposed_Data))
NT_Cols <- vector("list", length(Not_Transposed))

# Pick needed columns from the dataframes stored in 'Transposed_Data'
Transposed_Cols[[1]] <- Transposed_Data[[1]] %>%
  dplyr::select(c(1, 12, 16, 20, 24))
glimpse(Transposed_Cols[[1]])

Transposed_Cols[[2]] <- Transposed_Data[[2]] %>%
  dplyr::select(c(1, 6, 11))
glimpse(Transposed_Cols[[2]])

# Edit str multiple columns: https://stackoverflow.com/questions/60550222/r-str-remove-from-multiple-columns-within-pipe
# Col 3&4 are recorded as "#%" - remove "%" and change to decimal value
Transposed_Cols[[2]] <- Transposed_Cols[[2]] %>%
  mutate_at(2:3, ~ str_replace(., "%", "")) %>%
  mutate_at(2:3, as.numeric) %>%
  dplyr::mutate(HCAHPS.HLMR.Percentile = HCAHPS.HLMR.Percentile/100) %>%
  dplyr::mutate(COMP.HIP.KNEE.Percentile = COMP.HIP.KNEE.Percentile/100)

Transposed_Cols[[3]] <- Transposed_Data[[3]] %>%
  dplyr::select(c(2, 7, 9, 11, 13, 15, 17, 21))

Transposed_Cols[[3]] <- Transposed_Cols[[3]] %>%
  mutate_at(2:length(Transposed_Cols[[3]]), as.numeric)

Transposed_Cols[[4]] <- Transposed_Data[[4]] %>%
  dplyr::select(c(1, 9, 13))

Transposed_Cols[[4]] <- Transposed_Cols[[4]] %>%
  mutate_at(3, as.numeric)

Transposed_Cols[[5]] <- Transposed_Data[[5]] %>%
  dplyr::select(c(1, 3))

Transposed_Cols[[5]] <- Transposed_Cols[[5]] %>%
  mutate_at(2, as.numeric) %>%
  rename(`MSPB-1` = Value)

Transposed_Cols[[6]] <- Transposed_Data[[6]] %>%
  dplyr::select(c(2, 15, 22, 29, 36, 43))

# Scores are written as "# out of 10" - remove " out of 10" and change to numeric
Transposed_Cols[[6]] <- Transposed_Cols[[6]] %>%
  mutate_at(2:6, ~ str_replace(., " out of 10", "")) %>%
  mutate_at(2:6, as.numeric)

Transposed_Cols[[7]] <- Transposed_Data[[7]] %>%
  dplyr::select(c(2, 15))

# Scores are written as "# out of 10" - remove " out of 10" and change to numeric
Transposed_Cols[[7]] <- Transposed_Cols[[7]] %>%
  mutate_at(2, ~ str_replace(., " out of 10", "")) %>%
  mutate_at(2, as.numeric)

Transposed_Cols[[8]] <- Transposed_Data[[8]] %>%
  dplyr::select(c(2, 16, 24, 32, 40, 48, 56, 64, 72, 73, 74))

# Scores are written as "# out of 10" - remove " out of 10" and change to numeric
Transposed_Cols[[8]] <- Transposed_Cols[[8]] %>%
  mutate_at(2:9, ~ str_replace(., " out of 10", "")) %>%
  mutate_at(2:length(Transposed_Cols[[8]]), as.numeric)

Transposed_Cols[[9]] <- Transposed_Data[[9]] %>%
  dplyr::select(c(2, 15, 22, 30, 37, 44, 51))

# Scores are written as "# out of 10" - remove " out of 10" and change to numeric
Transposed_Cols[[9]] <- Transposed_Cols[[9]] %>%
  mutate_at(2:length(Transposed_Cols[[9]]), ~ str_replace(., " out of 10", "")) %>%
  mutate_at(2:length(Transposed_Cols[[9]]), as.numeric)

Transposed_Cols[[10]] <- Transposed_Data[[10]] %>%
  dplyr::select(c(2, 10:17))

Transposed_Cols[[10]] <- Transposed_Cols[[10]] %>%
  mutate_at(2:length(Transposed_Cols[[10]]), as.numeric)

Transposed_Cols[[11]] <- Transposed_Data[[11]] %>%
  dplyr::select(c(1, 9, 14, 19, 23, 27, 30, 34, 37, 41, 44, 48, 51, 55, 59, 65, 68, 74, 82, 89))

Transposed_Cols[[11]] <- Transposed_Cols[[11]] %>%
  mutate_at(2:length(Transposed_Cols[[11]]), as.numeric)

Transposed_Cols[[12]] <- Transposed_Data[[12]] %>%
  dplyr::select(c(1, 11))

Transposed_Cols[[12]] <- Transposed_Cols[[12]] %>%
  mutate_at(2, as.numeric) %>%
  rename(`MSPB-1` = Score)

Transposed_Cols[[13]] <- Transposed_Data[[13]] %>%
  dplyr::select(c(1, 12, 16, 20, 24, 28))

Transposed_Cols[[13]] <- Transposed_Cols[[13]] %>%
  mutate_at(2:length(Transposed_Cols[[13]]), as.numeric)

###SMALL DATA SET, MAY ELIMINATE###
Transposed_Cols[[14]] <- Transposed_Data[[14]] %>%
  dplyr::select(c(1, 11))

Transposed_Cols[[15]] <- Transposed_Data[[15]] %>%
  dplyr::select(c(1, 11))

###STRANGE FACILITY NAMES, INVESTIGATE###
Transposed_Cols[[16]] <- Transposed_Data[[16]] %>%
  dplyr::select(c(2, 8, 10, 14, 18, 20))

Transposed_Cols[[16]] <- Transposed_Cols[[16]] %>%
  mutate_at(2:length(Transposed_Cols[[16]]), as.numeric)

# Pick out needed columns from the dataframes stored in 'Not_Transposed'
NT_Cols[[1]] <- Not_Transposed[[1]] %>%
  dplyr::select(c(1, 8, 9, 10))

NT_Cols[[2]] <- Not_Transposed[[2]] %>%
  dplyr::select(c(1, 9, 10, 13))

NT_Cols[[3]] <- Not_Transposed[[3]] %>%
  dplyr::select(c(2, 4, 7))

NT_Cols[[4]] <- Not_Transposed[[4]] %>%
  dplyr::select(c(1, 9, 12))

NT_Cols[[5]] <- Not_Transposed[[5]] %>%
  dplyr::select(c(1, 9, 10, 12))

NT_Cols[[6]] <- Not_Transposed[[6]] %>%
  dplyr::select(c(1, 9:11))

NT_Cols[[7]] <- Not_Transposed[[7]] %>%
  dplyr::select(c(1, 9, 12))

NT_Cols[[8]] <- Not_Transposed[[8]] %>%
  dplyr::select(c(1, 9:11))

NT_Cols[[9]] <- Not_Transposed[[9]] %>%
  dplyr::select(c(1, 9, 10, 13))

NT_Cols[[10]] <- Not_Transposed[[10]] %>%
  dplyr::select(c(1, 10:12))

NT_Cols[[11]] <- Not_Transposed[[11]] %>%
  dplyr::select(c(1, 9, 10, 13))

NT_Cols[[12]] <- Not_Transposed[[12]] %>%
  dplyr::select(c(1, 10:12))

NT_Cols[[13]] <- Not_Transposed[[13]] %>%
  dplyr::select(c(1, 10, 11, 13))

# Inspecting NT_Cols score variables to determine data types
# glimpse(NT_Cols[[12]])
# sample <- head(NT_Cols[[12]], 500)
# 
# sample %>%
#   ggplot(aes(x = Score)) +
#   theme(axis.text.x = element_text(angle = 90, hjust=0)) +
#   geom_bar(fill = 'red')

# Transpose the data contained in NT_Cols and store to a new list
NT_Done <- vector("list", length(NT_Cols))

NT_Done[[1]] <- NT_Cols[[1]] %>%
  transform(Rate = as.numeric(Rate)) %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Rate)

NT_Done[[2]] <- NT_Cols[[2]] %>%
  transform(Score = as.numeric(Score)) %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

NT_Done[[3]] <- NT_Cols[[3]] %>%
  transform(Excess.Readmission.Ratio = as.numeric(Excess.Readmission.Ratio)) %>%
  pivot_wider(names_from = Measure.Name, values_from = Excess.Readmission.Ratio)

NT_Done[[4]] <- NT_Cols[[4]] %>%
  transform(Patient.Survey.Star.Rating = as.numeric(Patient.Survey.Star.Rating)) %>%
  pivot_wider(names_from = HCAHPS.Measure.ID, values_from = Patient.Survey.Star.Rating)

NT_Done[[4]] <- NT_Done[[4]] %>%
  dplyr::select(c(1, contains("STAR")))

NT_Done[[5]] <- NT_Cols[[5]] %>%
  transform(Score = as.numeric(Score)) %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

NT_Done[[5]] <- NT_Done[[5]] %>%
  dplyr::select(c(1, 2, contains("SIR")))

NT_Done[[6]] <- NT_Cols[[6]] %>%
  transform(Score = as.numeric(Score)) %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

NT_Done[[7]] <- NT_Cols[[7]] %>%
  transform(Patient.Survey.Star.Rating = as.numeric(Patient.Survey.Star.Rating)) %>%
  pivot_wider(names_from = HCAHPS.Measure.ID, values_from = Patient.Survey.Star.Rating)

NT_Done[[7]] <- NT_Done[[7]] %>%
  dplyr::select(c(1, contains("STAR")))

NT_Done[[8]] <- NT_Cols[[8]] %>%
  transform(Score = as.numeric(Score)) %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

NT_Done[[8]] <- NT_Done[[8]] %>%
  dplyr::select(c(1, 2, contains("SIR")))

NT_Done[[9]] <- NT_Cols[[9]] %>%
  dplyr::select(-Measure.Description) %>%
  pivot_wider(names_from = Measure.ID, values_from = Rate)

NT_Done[[10]] <- NT_Cols[[10]] %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

######Convert char score values to a scale of 1 - 4 where low = 1, med = 2, high = 3, very high = 4#######
NT_Done[[10]] <- NT_Done[[10]] %>%
  dplyr::mutate(EDV = case_when(EDV == "low" ~ "1", EDV == "medium" ~ "2", EDV == "high" ~ "3", EDV == "very high" ~ "4")) %>%
  mutate_at(2:length(NT_Done[[10]]), as.numeric)

NT_Done[[11]] <- NT_Cols[[11]] %>%
  transform(Score = as.numeric(Score)) %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

NT_Done[[12]] <- NT_Cols[[12]] %>%
  transform(Score = as.numeric(Score)) %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

NT_Done[[13]] <- NT_Cols[[13]] %>%
  dplyr::select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

######Convert char score values to a scale of 1 - 4 where low = 1, med = 2, high = 3, very high = 4#######
NT_Done[[13]] <- NT_Done[[13]] %>%
  dplyr::mutate(EDV = case_when(EDV == "low" ~ "1", EDV == "medium" ~ "2", EDV == "high" ~ "3", EDV == "very high" ~ "4")) %>%
  mutate_at(3:length(NT_Done[[13]]), as.numeric)

# Convert 'Facility ID' to character for dataframes stored in 'NT_Done'
for(i in 1:length(NT_Done)) {
  NT_Done[[i]] <- NT_Done[[i]] %>%
    dplyr::mutate(Facility.ID = as.character(Facility.ID))
}

# Convert 'Facility ID' to character for dataframes stored in 'Transposed_Cols'
for(i in 1:length(Transposed_Cols)) {
  Transposed_Cols[[i]] <- Transposed_Cols[[i]] %>%
    dplyr::mutate(Facility.ID = as.character(Facility.ID))
}

#Remove leading 0 from 'Facility ID' for some dataframes
Transposed_Cols[[4]]$Facility.ID <- Transposed_Cols[[4]]$Facility.ID %>%
  trimws("left", "0")

NT_Done[[2]]$Facility.ID <- NT_Done[[2]]$Facility.ID %>%
  trimws("left", "0")

NT_Done[[4]]$Facility.ID <- NT_Done[[4]]$Facility.ID %>%
  trimws("left", "0")

NT_Done[[5]]$Facility.ID <- NT_Done[[5]]$Facility.ID %>%
  trimws("left", "0")

NT_Done[[10]]$Facility.ID <- NT_Done[[10]]$Facility.ID %>%
  trimws("left", "0")

NT_Done[[11]]$Facility.ID <- NT_Done[[11]]$Facility.ID %>%
  trimws("left", "0")

# Join dataframes - This is the initial join
joined = full_join(x = NT_Done[[1]], y = NT_Done[[2]], by = c("Facility.ID" = "Facility.ID"))

# Function to join the remaining dataframes consecutively
join_tables <- function(t1, t2) {
  joined = full_join(x = t1, y = t2, by = c("Facility.ID" = "Facility.ID"))
}

# Join the dataframes stored in NT_Done
for(i in 3:length(NT_Done)) {
  joined <- join_tables(joined, NT_Done[[i]])
  print(paste0("successfully joined data set ", i))
}

# Join the dataframes stored in Transposed_Cols
for(i in 1:length(Transposed_Cols)) {
  joined <- join_tables(joined, Transposed_Cols[[i]])
  print(paste0("successfully joined data set ", i))
}

# Select Acute Care Hospitals only 
joined_ACH <- joined %>%
  filter(Hospital.Type == "Acute Care Hospitals")

# Check percentage of missing data for each column (amount of missing data per measure)
# for(i in 1:length(joined_ACH)) {
#   print((sum(is.na(joined_ACH[i]))/prod(dim(joined_ACH[i])))*100)
# }

# Checks the total amount of missing data in the dataframe
# (sum(is.na(joined_ACH_final))/prod(dim(joined_ACH_final)))*100

# Find which columns have 20% or less missing data and append those to a list
measures <- NULL
for(i in 1:length(joined_ACH)) {
  if((sum(is.na(joined_ACH[i]))/prod(dim(joined_ACH[i])))*100 < 21) {
    measures <- list.append(measures, i)
  }
}

# Create a dataframe of all the measures with 20% or less missing data
joined_ACH_final <- joined_ACH %>%
  dplyr::select(c(all_of(measures))) %>%
  dplyr::select(- c("PSI_03.y", "PSI_06.y", "PSI_08.y", "PSI_09.y", "PSI_10.y", "PSI_11.y", "PSI_12.y", "PSI_13.y", "PSI_15.y", "PSI_90.y", "MSPB-1.y", "Hospital.overall.rating", "Hospital.Type"))
glimpse(joined_ACH_final)

# Write the final dataframe to a file to be used in the clustering analysis
write_csv(joined_ACH_final, "data/Measures_by_Hospital_Acute_Care_120521.csv")
