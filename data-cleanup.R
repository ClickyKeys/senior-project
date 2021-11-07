#sources:
#https://swcarpentry.github.io/r-novice-inflammation/03-loops-R/
#https://stackoverflow.com/questions/19791760/adding-data-frames-as-list-elements-using-for-loop

#importing and cleaning data
pacman::p_load(tidyverse, rlist, skimr, gmodels)

#import datasets - make lists for the different data conditions
Hospital_Data <- list.files(path = "C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/hospitals_current_data/EVALUATED", pattern = "*.csv", full.names = TRUE)
names(Hospital_Data) <- list.files(path = "C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/hospitals_current_data/EVALUATED", pattern = "*.csv", full.names = FALSE)

Transposed_Data <- list()
Not_Transposed <- list()
Duplicates_Exist <- NULL
T_Names <- NULL
NT_Names <- NULL

for(j in 1:length(Hospital_Data)) {
  #changed to read_csv
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
    #Transposed_Data <- list.append(Transposed_Data, list(data))
    Transposed_Data <- list.append(Transposed_Data, data.frame(data))
    T_Names <- list.append(T_Names, names(Hospital_Data[j]))
  }
  
  if( var_count > 0 ) {
    #Not_Transposed <- list.append(Not_Transposed, list(data))
    Not_Transposed <- list.append(Not_Transposed, data.frame(data))
    NT_Names <- list.append(NT_Names, names(Hospital_Data[j]))
  }
}

#Not_Transposed[1] is transposed but has duplicates - Move element 1 to Transposed_Data list
T_Names <- list.append(T_Names, NT_Names[1])
NT_Names <- NT_Names[-1]

Transposed_Data <- list.append(Transposed_Data, data.frame(Not_Transposed[1]))
Not_Transposed <- Not_Transposed[-1]

#Not Transposed data frames
j <- 0
for(j in 1:length(Not_Transposed)) {
  glimpse(Not_Transposed[[j]])
  cat("\n")
  print("Next Dataframe...")
}

#Transposed data frame
j <- 0
for(j in 1:length(Transposed_Data)) {
  glimpse(Transposed_Data[[j]])
  cat("\n")
  print("\nNext Dataframe...\n")
}

Transposed_Cols <- vector("list", length(Transposed_Data))
NT_Cols <- vector("list", length(Not_Transposed))

#Pick columns from the data frames that don't need to be transposed
Transposed_Cols[[1]] <- Transposed_Data[[1]] %>%
  select(c(1, 12, 16, 20, 24))
glimpse(Transposed_Cols[[1]])

Transposed_Cols[[2]] <- Transposed_Data[[2]] %>%
  select(c(1, 6, 11))
glimpse(Transposed_Cols[[2]])

#edit str multiple columns: https://stackoverflow.com/questions/60550222/r-str-remove-from-multiple-columns-within-pipe
#col 3&4 are recorded as "#%" - remove "%" and change to decimal value
Transposed_Cols[[2]] <- Transposed_Cols[[2]] %>%
  mutate_at(2:3, ~ str_replace(., "%", "")) %>%
  mutate_at(2:3, as.numeric) %>%
  mutate(HCAHPS.HLMR.Percentile = HCAHPS.HLMR.Percentile/100) %>%
  mutate(COMP.HIP.KNEE.Percentile = COMP.HIP.KNEE.Percentile/100)
glimpse(Transposed_Cols[[2]])

Transposed_Cols[[3]] <- Transposed_Data[[3]] %>%
  select(c(2, 7, 9, 11, 13, 15, 17, 21))
glimpse(Transposed_Cols[[3]])

Transposed_Cols[[3]] <- Transposed_Cols[[3]] %>%
  mutate_at(2:length(Transposed_Cols[[3]]), as.numeric)
glimpse(Transposed_Cols[[3]])

Transposed_Cols[[4]] <- Transposed_Data[[4]] %>%
  select(c(1, 9, 13))
glimpse(Transposed_Cols[[4]])

Transposed_Cols[[4]] <- Transposed_Cols[[4]] %>%
  mutate_at(3, as.numeric)

Transposed_Cols[[5]] <- Transposed_Data[[5]] %>%
  select(c(1, 3))
glimpse(Transposed_Cols[[5]])

Transposed_Cols[[5]] <- Transposed_Cols[[5]] %>%
  mutate_at(2, as.numeric) %>%
  rename(`MSPB-1` = Value)

Transposed_Cols[[6]] <- Transposed_Data[[6]] %>%
  select(c(2, 15, 22, 29, 36, 43))

#scores are written as "# out of 10" - remove " out of 10" and change to numeric
Transposed_Cols[[6]] <- Transposed_Cols[[6]] %>%
  mutate_at(2:6, ~ str_replace(., " out of 10", "")) %>%
  mutate_at(2:6, as.numeric)
glimpse(Transposed_Cols[[6]])

Transposed_Cols[[7]] <- Transposed_Data[[7]] %>%
  select(c(2, 15))
glimpse(Transposed_Cols[[7]])

#scores are written as "# out of 10" - remove " out of 10" and change to numeric
Transposed_Cols[[7]] <- Transposed_Cols[[7]] %>%
  mutate_at(2, ~ str_replace(., " out of 10", "")) %>%
  mutate_at(2, as.numeric)
glimpse(Transposed_Cols[[7]])

Transposed_Cols[[8]] <- Transposed_Data[[8]] %>%
  select(c(2, 16, 24, 32, 40, 48, 56, 64))

#scores are written as "# out of 10" - remove " out of 10" and change to numeric
Transposed_Cols[[8]] <- Transposed_Cols[[8]] %>%
  mutate_at(2:8, ~ str_replace(., " out of 10", "")) %>%
  mutate_at(2:length(Transposed_Cols[[8]]), as.numeric)
glimpse(Transposed_Cols[[8]])

Transposed_Cols[[9]] <- Transposed_Data[[9]] %>%
  select(c(2, 15, 22, 30, 37, 44, 51))

#scores are written as "# out of 10" - remove " out of 10" and change to numeric
Transposed_Cols[[9]] <- Transposed_Cols[[9]] %>%
  mutate_at(2:length(Transposed_Cols[[9]]), ~ str_replace(., " out of 10", "")) %>%
  mutate_at(2:length(Transposed_Cols[[9]]), as.numeric)
glimpse(Transposed_Cols[[9]])

Transposed_Cols[[10]] <- Transposed_Data[[10]] %>%
  select(c(2, 10:17))

Transposed_Cols[[10]] <- Transposed_Cols[[10]] %>%
  mutate_at(2:length(Transposed_Cols[[10]]), as.numeric)
glimpse(Transposed_Cols[[10]])

Transposed_Cols[[11]] <- Transposed_Data[[11]] %>%
  select(c(1, 9, 14, 19, 23, 27, 30, 34, 37, 41, 44, 48, 51, 55, 59, 65, 68, 74, 82, 89))

Transposed_Cols[[11]] <- Transposed_Cols[[11]] %>%
  mutate_at(2:length(Transposed_Cols[[11]]), as.numeric)
glimpse(Transposed_Cols[[11]])

Transposed_Cols[[12]] <- Transposed_Data[[12]] %>%
  select(c(1, 11))

Transposed_Cols[[12]] <- Transposed_Cols[[12]] %>%
  mutate_at(2, as.numeric) %>%
  rename(`MSPB-1` = Score)
glimpse(Transposed_Cols[[12]])

Transposed_Cols[[13]] <- Transposed_Data[[13]] %>%
  select(c(1, 12, 16, 20, 24, 28))

Transposed_Cols[[13]] <- Transposed_Cols[[13]] %>%
  mutate_at(2:length(Transposed_Cols[[13]]), as.numeric)
glimpse(Transposed_Cols[[13]])

###SMALL DATA SET, MAY ELIMINATE###
Transposed_Cols[[14]] <- Transposed_Data[[14]] %>%
  select(c(1, 11))
glimpse(Transposed_Cols[[14]])

###SMALL DATA SET, MAY ELIMINATE###
Transposed_Cols[[15]] <- Transposed_Data[[15]] %>%
  select(c(1, 11))
glimpse(Transposed_Cols[[15]])

###STRANGE FACILITY NAMES, INVESTIGATE###
Transposed_Cols[[16]] <- Transposed_Data[[16]] %>%
  select(c(2, 8, 10, 14, 18, 20))

Transposed_Cols[[16]] <- Transposed_Cols[[16]] %>%
  mutate_at(2:length(Transposed_Cols[[16]]), as.numeric)
glimpse(Transposed_Cols[[16]])

#Pick columns from the data frames that do need to be transposed
NT_Cols[[1]] <- Not_Transposed[[1]] %>%
  select(c(1, 8, 9, 10))
glimpse(NT_Cols[[1]])

NT_Cols[[2]] <- Not_Transposed[[2]] %>%
  select(c(1, 9, 10, 13))
glimpse(NT_Cols[[2]])

NT_Cols[[3]] <- Not_Transposed[[3]] %>%
  select(c(2, 4, 7))
glimpse(NT_Cols[[3]])

NT_Cols[[4]] <- Not_Transposed[[4]] %>%
  select(c(1, 9, 12))
glimpse(NT_Cols[[4]])

NT_Cols[[5]] <- Not_Transposed[[5]] %>%
  select(c(1, 9, 10, 12))
glimpse(NT_Cols[[5]])

NT_Cols[[6]] <- Not_Transposed[[6]] %>%
  select(c(1, 9:11))
glimpse(NT_Cols[[6]])

NT_Cols[[7]] <- Not_Transposed[[7]] %>%
  select(c(1, 9, 12))
glimpse(NT_Cols[[7]])

NT_Cols[[8]] <- Not_Transposed[[8]] %>%
  select(c(1, 9:11))
glimpse(NT_Cols[[8]])

NT_Cols[[9]] <- Not_Transposed[[9]] %>%
  select(c(1, 9, 10, 13))
glimpse(NT_Cols[[9]])

NT_Cols[[10]] <- Not_Transposed[[10]] %>%
  select(c(1, 10:12))
glimpse(NT_Cols[[10]])

NT_Cols[[11]] <- Not_Transposed[[11]] %>%
  select(c(1, 9, 10, 13))
glimpse(NT_Cols[[11]])

NT_Cols[[12]] <- Not_Transposed[[12]] %>%
  select(c(1, 10:12))
glimpse(NT_Cols[[12]])

NT_Cols[[13]] <- Not_Transposed[[13]] %>%
  select(c(1, 10, 11, 13))
glimpse(NT_Cols[[13]])

#Inspecting NT_Cols score variables to determine data types
# glimpse(NT_Cols[[12]])
# sample <- head(NT_Cols[[12]], 500)
# 
# sample %>%
#   ggplot(aes(x = Score)) +
#   theme(axis.text.x = element_text(angle = 90, hjust=0)) +
#   geom_bar(fill = 'red')

#Transpose the data contained in NT_Cols
NT_Done <- vector("list", length(NT_Cols))

#######MAY NOT NEED THIS TALBE - PSI SCORES ARE IN NT_Names[[1]]########
NT_Done[[1]] <- NT_Cols[[1]] %>%
  transform(Rate = as.numeric(Rate)) %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Rate)
glimpse(NT_Done[[1]])

NT_Done[[2]] <- NT_Cols[[2]] %>%
  transform(Score = as.numeric(Score)) %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)
glimpse(NT_Done[[2]])

NT_Done[[3]] <- NT_Cols[[3]] %>%
  transform(Excess.Readmission.Ratio = as.numeric(Excess.Readmission.Ratio)) %>%
  pivot_wider(names_from = Measure.Name, values_from = Excess.Readmission.Ratio)
glimpse(NT_Done[[3]])

glimpse(NT_Cols[[4]])
NT_Done[[4]] <- NT_Cols[[4]] %>%
  transform(Patient.Survey.Star.Rating = as.numeric(Patient.Survey.Star.Rating)) %>%
  pivot_wider(names_from = HCAHPS.Measure.ID, values_from = Patient.Survey.Star.Rating)

NT_Done[[4]] <- NT_Done[[4]] %>%
  select(c(1, contains("STAR")))
glimpse(NT_Done[[4]])

NT_Done[[5]] <- NT_Cols[[5]] %>%
  transform(Score = as.numeric(Score)) %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

NT_Done[[5]] <- NT_Done[[5]] %>%
  select(c(1, 2, contains("SIR")))
glimpse(NT_Done[[5]])

NT_Done[[6]] <- NT_Cols[[6]] %>%
  transform(Score = as.numeric(Score)) %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)
glimpse(NT_Done[[6]])

#######MAY ONLY NEED THE LAST STAR RATING COLUMN#######
NT_Done[[7]] <- NT_Cols[[7]] %>%
  transform(Patient.Survey.Star.Rating = as.numeric(Patient.Survey.Star.Rating)) %>%
  pivot_wider(names_from = HCAHPS.Measure.ID, values_from = Patient.Survey.Star.Rating)

NT_Done[[7]] <- NT_Done[[7]] %>%
  select(c(1, contains("STAR")))
glimpse(NT_Done[[7]])

#######SMALL DATA TABLE MAY NEED TO OMIT#######
NT_Done[[8]] <- NT_Cols[[8]] %>%
  transform(Score = as.numeric(Score)) %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

NT_Done[[8]] <- NT_Done[[8]] %>%
  select(c(1, 2, contains("SIR")))
glimpse(NT_Done[[8]])

########SMALL DATA TABLE MAY NEED TO OMIT#######
NT_Done[[9]] <- NT_Cols[[9]] %>%
  select(-Measure.Description) %>%
  pivot_wider(names_from = Measure.ID, values_from = Rate)
glimpse(NT_Done[[9]])

NT_Done[[10]] <- NT_Cols[[10]] %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

######Convert char score values to a scale of 1 - 4 where low = 1, med = 2, high = 3, very high = 4#######
NT_Done[[10]] <- NT_Done[[10]] %>%
  mutate(EDV = case_when(EDV == "low" ~ "1", EDV == "medium" ~ "2", EDV == "high" ~ "3", EDV == "very high" ~ "4")) %>%
  mutate_at(2:length(NT_Done[[10]]), as.numeric)
glimpse(NT_Done[[10]])

NT_Done[[11]] <- NT_Cols[[11]] %>%
  transform(Score = as.numeric(Score)) %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)
glimpse(NT_Done[[11]])

#####ELIMINATING FOR NOW DUE TO SMALL DATA#####
NT_Done[[12]] <- NT_Cols[[12]] %>%
  transform(Score = as.numeric(Score)) %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)
glimpse(NT_Done[[12]])

NT_Done[[13]] <- NT_Cols[[13]] %>%
  select(-Measure.Name) %>%
  pivot_wider(names_from = Measure.ID, values_from = Score)

######Convert char score values to a scale of 1 - 4 where low = 1, med = 2, high = 3, very high = 4#######
NT_Done[[13]] <- NT_Done[[13]] %>%
  mutate(EDV = case_when(EDV == "low" ~ "1", EDV == "medium" ~ "2", EDV == "high" ~ "3", EDV == "very high" ~ "4")) %>%
  mutate_at(3:length(NT_Done[[13]]), as.numeric)
glimpse(NT_Done[[13]])

#evaluate facility ID's
for(i in 1:length(NT_Done)) {
  NT_Done[[i]] <- NT_Done[[i]] %>%
    mutate(Facility.ID = as.character(Facility.ID))
  print(i)
  glimpse(NT_Done[[i]])
}

for(i in 1:length(Transposed_Cols)) {
  Transposed_Cols[[i]] <- Transposed_Cols[[i]] %>%
    mutate(Facility.ID = as.character(Facility.ID))
  glimpse(Transposed_Cols[[i]])
}

#Remove leading 0 from some facility ID's
#make a function?
for(i in 1:length(NT_Done)) {
  glimpse(NT_Done[[i]])
}

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

#Join tables 
joined = full_join(x = NT_Done[[1]], y = NT_Done[[2]], by = c("Facility.ID" = "Facility.ID"))

join_tables <- function(t1, t2) {
  joined = full_join(x = t1, y = t2, by = c("Facility.ID" = "Facility.ID"))
}

for(i in 3:length(NT_Done)) {
  joined <- join_tables(joined, NT_Done[[i]])
  print(paste0("successfully joined data set ", i))
}

for(i in 1:length(Transposed_Cols)) {
  joined <- join_tables(joined, Transposed_Cols[[i]])
  print(paste0("successfully joined data set ", i))
}

glimpse(joined)

#78% total missing data - This is concerning
(sum(is.na(joined))/prod(dim(joined)))*100

#Checking General Hospital Information dataset to determine which measures have the most values by hospital type
CrossTable(Transposed_Data[[4]]$Hospital.Type, Transposed_Data[[4]]$Count.of.Facility.MORT.Measures)

#Select measures for Acute Care Hospitals only 
joined_ACH <- joined %>%
  filter(Hospital.Type == "Acute Care Hospitals")

#Check percentage of missing data for each column (amount of missing data per measure)
for(i in 1:length(joined_ACH)) {
  print((sum(is.na(joined_ACH[i]))/prod(dim(joined_ACH[i])))*100)
}
glimpse(joined_ACH)

#find which columns have < 25% missing data and append those to a list
measures <- NULL
for(i in 1:length(joined_ACH)) {
  if((sum(is.na(joined_ACH[i]))/prod(dim(joined_ACH[i])))*100 < 20) {
    measures <- list.append(measures, i)
  }
}
print(measures)

glimpse(joined_ACH)

joined_ACH_final <- joined_ACH %>%
  select(c(1,2,3,4,5,6,7,8,11,12,16,17,18,20,22,23,24,25,26,27,30,31,35,36,37,38,39,40,41,42,43,44,45,46,47,48,55,56,79,89,90,92,95,96,97,100,
          103,104,106,107,129,134,135,136,137,138,141,142,144,145,146,147,148,149,150,151,158,159,160,163,164,165,185)) %>%
  select(- c("PSI_03.y", "PSI_06.y", "PSI_08.y", "PSI_09.y", "PSI_10.y", "PSI_11.y", "PSI_12.y", "PSI_15.y", "PSI_90.y", "Hospital.overall.rating", "Hospital.Type", "MSPB-1.y"))
glimpse(joined_ACH_final)

#13% total missing data from the finished dataset
(sum(is.na(joined_ACH_final))/prod(dim(joined_ACH_final)))*100

#nearly all columns have <20% missing data 
for(i in 1:length(joined_ACH_final)) {
  print((sum(is.na(joined_ACH_final[i]))/prod(dim(joined_ACH_final[i])))*100)
}

write_csv(joined_ACH_final, "data/Measures_by_Hospital_Acute_Care_New.csv")

