library(dbplyr) 
library(tidyverse)
library(arrow)

########################################
# Set Directory for Unzipped CSVs here #
########################################

csv_path <- "C:/Users/zades/Desktop/Nursing Data/"


###############
# Parts 1 - 3 #
###############

### Get list of CSVs
list_of_files <- list.files(path = csv_path, pattern = "*.csv")

### Load CSVs as one Parquet
csv_parquet <- open_dataset(
  sources = paste0(csv_path, list_of_files),
  col_types = schema(facility_id = string()),
  format = "csv"
)

### Glimpse to check to see if all files loaded. 

glimpse(csv_parquet)

### Partition By State
csv_partitioned_state <- csv_parquet |> group_by(state)

### Write Partitioned Parquet
write_dataset(csv_partitioned_state, path = paste0(csv_path), format = "parquet")

### Get Folder List
state_file_list <- list.files(
  path = csv_path,
  recursive = TRUE,
  pattern = "part-0.parquet",
  full.names = TRUE)

### Load Partitioned Parquet
partitioned_parquet <- open_dataset(
  sources = state_file_list,
  format = "parquet"
)

#######################
# Function for Part 4 #
#######################

### Make Sure to input state name as two letter code for state. Ex: Alaska = AK
filter_timer <- function(two_letter_state){
  
  ### Non-Partitioned Data
  start_time_raw <- Sys.time()
  
  non_partitioned <- open_dataset(
    sources = paste0(csv_path, list_of_files),
    col_types = schema(facility_id = string()),
    format = "csv"
  )
  
  severity_breakdown_csv <- non_partitioned |> 
    filter(state == as.character({{two_letter_state}})) |>
    group_by(scope_severity) |>
    collect() |>
    summarise(n = n())
  
  n_rows_csv <- sum(severity_breakdown_csv$n)
  
  end_time_raw <- Sys.time()
  
  filtering_time_csv <- end_time_raw - start_time_raw
  
  print(paste0(filtering_time_csv, " seconds from start to finish: Filtering the non-partitioned data."))
  print(n_rows_csv)
  
  ### Partioned Data
  start_time_partitioned <- Sys.time()
  
  ### I couldn't figure out how to filter based on state after reading in every
  ### datasets as partitioned_parquet. However, I would argue that the wording of the 
  ### question makes this solution a valid one also.
  ### To make this an apples to apples comparison, I'll have the previous step
  ### also read in the dataset for the first time. 
  
  state_data <- open_dataset(
    sources = paste0(csv_path, "state=", as.character({{two_letter_state}}), "/part-0.parquet"),
    format = "parquet"
  )
  
  severity_breakdown_partitioned <- state_data |>
    group_by(scope_severity)|>
    collect() |>
    summarise(n = n())
  
  n_rows_partitioned <- sum(severity_breakdown_partitioned$n)
  
  end_time_partitioned <- Sys.time()
  
  filtering_time_partitioned <- end_time_partitioned - start_time_partitioned
  
  print(paste0(filtering_time_partitioned, " seconds from start to finish: Filtering the partitioned data."))
  print(n_rows_partitioned)
  
  time_difference <- filtering_time_csv - filtering_time_partitioned
  
  print(time_difference)
  
  computation_time <- data.frame(state = {{two_letter_state}}, 
                                 time_partitioned = filtering_time_partitioned,
                                 time_non_partitioned = filtering_time_csv,
                                 time_difference = time_difference,
                                 n = n_rows_partitioned)
}

### Test the Function
test <- filter_timer("NY")

### It works!

#####################################
# Set Up Dataframe for Part 5 and 6 #
#####################################

##################################################################################
### WARNING: THIS CODE CREATES THE CSVS IN /QUESTION1.5GRAPHS MAKE SURE 
### YOUR PATH DOESN'T OVERRIDE THE DATA USED TO GENERATE QUESTION1_5.PNG
### ALSO IT'LL TAKE SOME TIME FOR YOUR PC TO RUN THIS CODE. FEEL FREE TO CHANGE
### THE HIGH END OF H IF YOU DON'T THINK YOU NEED 100 ENTRIES. 
### YOU WILL NEED TO CHANGE GRAPH PATH HERE FOR FUTURE LINES OF CODE BUT 
### MAKE A NOTE THAT RUNNING THE FOR LOOP WILL OVERRIDE THE DATA CURRENTLY IN THE
### FOLDER.
##################################################################################

graph_path <- "C:/Users/zades/Documents/GitHub/problem-set-4-ScavengerDLC/Question1.5Graphs/"

for (h in 1:100) {
  
### Make Empty Dataframe for Time Information
### Number of columns is defined by function (see lines 112-116)
filtering_times <- data.frame(matrix(nrow = 0 , ncol = 5))

### Names of columns are defined by function
names(filtering_times) <- c("state", "time_partitioned", "time_non_partitioned", "time_difference", "n")

### For Loop to Get Data Filtering For All States

for (i in 1:length(state_file_list)) {
  ### Get State Name
  ### Remove First Part of File Path
  name_state <- str_remove(state_file_list[i], "C:/Users/zades/Desktop/Nursing Data/state=")
  
  ### Remove second Part of File Path
  name_state <- str_remove(name_state, "/part-0.parquet")
  
  ### Filter By State
  state_time <- filter_timer(name_state)
  
  filtering_times <- rbind(filtering_times, state_time)
  
}

### Create Magnitude of Time difference variable (ex. 5 seconds for non-partitioned
### and 1 second for partitioned is 5 times faster or 5 for this variable)
filtering_times <- filtering_times |>
  mutate(
    magnitude_time_difference = as.numeric(time_non_partitioned) / as.numeric(time_partitioned)
  )

### Write CSVs and Delete Data in R

write.csv(filtering_times, paste0(graph_path, "filtering_times_", as.character(h), ".csv"))
rm(filtering_times)
print(paste0("CSV ", h, " generated!"))
}


### Read in Generated CSVs and combine them
filtering_times <- data.frame(matrix(nrow = 0 , ncol = 6))

names(filtering_times) <- c("state", "time_partitioned", "time_non_partitioned", "time_difference", "n", "magnitude_time_difference")

### Don't change this upper bound, this reads the files 
for(g in 1:100){
  df <- read.csv(paste0(graph_path, "filtering_times_", as.character(g), ".csv"))
  filtering_times <- rbind(filtering_times, df)
}

filtering_times <- filtering_times |> select(!X)

filtering_times <- filtering_times |>
  mutate(
    n_transformed = log(n)
  )

#######################
# Plot for Question 5 #
#######################

### If you run the previous steps on your own computer and then this graph, you
### WILL get a different graph than are shown in the folder that is uploaded
### to github
### If you want to make sure that the graph is reproducible, run the code on 
### line XXX. I saved the CSVs for all the graphs that I made here to make the 
### argument that I'm making in the readme. 
filtering_times |> 
  ggplot(aes(x = n , y = magnitude_time_difference)) +
  geom_point() +
  stat_smooth(method = "loess", formula=y~log(x)) +
  ylim(0,80) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90")) + 
  labs(x = "Number of Observations",
       y = "Magnitude of Time Difference",
       title = "Magnitude of Time Differences when Filtering Partitioned Data vs Non Partioned Data",
       subtitle = "By Number of Observations",
       caption = "Magnitude of 2 means Partitioned Data filted twice as fast as Non-Partitioned")

ggsave(paste0(graph_path, "question1_5_normal_scale.png"))

### Graph with Log Scale for X Axis
filtering_times |> 
  ggplot(aes(x = n_transformed , y = magnitude_time_difference)) +
  geom_point() +
  stat_smooth(method = "loess", formula=y~x) +
  ylim(0,80) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90")) + 
  labs(x = "Log Number of Observations",
       y = "Magnitude of Time Difference",
       title = "Magnitude of Time Differences when Filtering Partitioned Data vs Non Partioned Data",
       subtitle = "By Number of Observations",
       caption = "Magnitude of 2 means Partitioned Data filted twice as fast as Non-Partitioned")

ggsave(paste0(graph_path, "question1_5_log_scale.png"))

cali_filter <- filtering_times |> filter(state == "CA")

#######################
# Plot for Question 6 #
#######################
filtering_times |>
  ggplot(aes(x = n)) +
  geom_point(aes(y = time_difference, color = "Time Difference")) +
  geom_point(aes(y = time_non_partitioned, color = "Non Partitioned")) +
  geom_point(aes(y = time_partitioned, color = "Partitioned")) +
  geom_smooth(aes(y = time_difference, color = "Time Difference"), method = loess) +
  labs(color = "Type") +
  scale_color_manual(values = c("red", "orange", "blue")) +
  labs(x = "Number of Observations",
       y = "Seconds Spend Processing",
       title = "Seconds Spent Processing Data by Number of Observations") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))

ggsave("C:/Users/zades/Documents/GitHub/problem-set-4-ScavengerDLC/question1_6.png")
