## -----------------------------------------------------------------------------------------------------------------------
#Colour palet: ["#d7e1ee", "#cbd6e4", "#bfcbdb", "#b3bfd1", "#a4a2a8", "#df8879", "#c86558", "#b04238", "#991f17"]
library(readr)
data <- read_csv("C:/Users/akobe/Downloads/Dataset 1 — Gun violence.csv")
attach(data)
library(dplyr)
library(leaflet)
library(tidyverse)
library(dplyr)
library(ggplot2) #plots
library(moments) #skewness calculation 
library(gridExtra)
library(corrplot)
require(lmtest)
require(plm)
require(psych)


## -----------------------------------------------------------------------------------------------------------------------
str(data)
summary(data)


## -----------------------------------------------------------------------------------------------------------------------
#Drop 4 useless url and source featues
data = subset(data, select = -c(incident_url, source_url, incident_url_fields_missing, sources))
data


## -----------------------------------------------------------------------------------------------------------------------
missing_counts = colSums(is.na(data))

percent_missing = missing_counts / nrow(data) * 100

missing_summary <- data.frame(
  Column = names(data),
  Missing_Count = missing_counts,
  Percent_Missing = percent_missing
)

missing_plot = ggplot(missing_summary, aes(x = reorder(Column, -Percent_Missing), y = Percent_Missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Missing Data by Column",
       x = "Column Names",
       y = "Percentage Missing") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(missing_plot)



## -----------------------------------------------------------------------------------------------------------------------
missing_summary <- missing_summary %>% arrange(desc(Percent_Missing))
missing_summary


## -----------------------------------------------------------------------------------------------------------------------
data = subset(data, select = -c(participant_relationship, location_description, participant_name))


## -----------------------------------------------------------------------------------------------------------------------
sapply(data, class)
all_var_names <- names(data)

quant_vars = sapply(data, is.numeric)
quant_var_names <- all_var_names[quant_vars]

char_vars <- sapply(data, is.character)
char_var_names <- all_var_names[char_vars]

quant_indices <- which(sapply(data, is.numeric))

char_indices <- which(sapply(data, is.character))

qual_vars = data.frame(char_indices)
qual_vars = data.frame(qual_vars[,1])

quant_vars = data.frame(quant_indices)
quant_vars = data.frame(quant_vars[,1])
data
filtered_data <- data %>% filter(total_vic < 10)

plot_release_year <- ggplot(filtered_data, aes(x = n_injured)) +
  geom_density(fill = "#29A98B", color = "#104236") +
  labs(title = "Density Plot of Release Year", x = "Release Year", y = "Density") +
  geom_vline(xintercept = median(filtered_data$n_injured), color = "#0077B6", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(filtered_data$n_injured), color = "#FFD100", linetype = "dashed", size = 1) +
  annotate("text", x = max(filtered_data$n_injured), y = max(density(filtered_data$n_injured)$y), 
           label = paste("Skewness:", round(skewness(filtered_data$n_injured), 1)), hjust = 2, color = "#FFD100") +
  annotate("text", x = max(filtered_data$n_injured), y = max(density(filtered_data$n_injured)$y), 
           label = paste("Median:", round(median(filtered_data$n_injured), 1)), hjust = 4, color = "#0077B6")
"#d7e1ee", "#cbd6e4", "#bfcbdb", "#b3bfd1", "#a4a2a8", "#df8879", "#c86558", "#b04238", "#991f17"]
data$total_vic = data$n_killed+data$n_injured

plot_vicitms <- ggplot(filtered_data, aes(x = filtered_data$total_vic)) +
  geom_density(fill = "#b04238", color = "#991f17") +
  labs(title = "Density Plot of Total Victims", x = "Total Victims", y = "Density") +
  geom_vline(xintercept = median(filtered_data$total_vic), color = "#b3bfd1", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = mean(filtered_data$total_vic), color = "#df8879", linetype = "dashed", size = 0.5) +
  annotate("text", x = max(filtered_data$total_vic), y = max(density(filtered_data$total_vic)$y), 
           label = paste("Skewness:", round(skewness(filtered_data$total_vic), 1)), hjust = 2, color = "#991f17") +
  annotate("text", x = max(filtered_data$total_vic), y = max(density(filtered_data$total_vic)$y), 
           label = paste("Median:", round(median(filtered_data$total_vic), 1)), hjust = 4, color = "#991f17")
plot_vicitms


## -----------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(scales)

data$date <- as.Date(data$date, format = "%Y/%m/%d")

data$year <- lubridate::year(data$date)
data$month <- lubridate::month(data$date)
data$day <- lubridate::day(data$date)
data$wday <- lubridate::wday(data$date)
data$quarter <- lubridate::quarter(data$date)

average_quarterly <- data %>%
  filter(year != 2013) %>% 
  group_by(year, quarter) %>%
  summarise(average_incidents = mean(n(), na.rm = TRUE))

total_per_year <- data %>%
  group_by(year) %>%
  summarise(total_incidents = n())


average_quarterly <- left_join(average_quarterly, total_per_year, by = "year") %>%
  mutate(percentage = (average_incidents / total_incidents) * 100)
custom_palette <- c("#d7e1ee", "#b3bfd1", "#df8879", "#991f17")

ggplot(average_quarterly, aes(x = as.factor(year), y = average_incidents, fill = as.factor(quarter))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = custom_palette) +
  #geom_text(aes(label = as.factor(quarter)), position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Gun Incidents per Year by Quarter",
       x = "Year",
       y = "Number of Incidents",
       fill = "Quarters") +
  theme_minimal() +
  theme(legend.position = "right") 



## -----------------------------------------------------------------------------------------------------------------------
library(dplyr)
state_pop <- read.csv("C:/Users/akobe/Downloads/nst-est2019-alldata.csv")
state_pop <- subset(state_pop, select = c("NAME","POPESTIMATE2013", "POPESTIMATE2014","POPESTIMATE2015","POPESTIMATE2016","POPESTIMATE2017","POPESTIMATE2018"))
state_pop %>% filter(!NAME %in% c('United States', 'Puerto Rico'))
state_pop = state_pop[6:56,]
state_pop = state_pop %>% rename(state= NAME)

state_pop_2014 = data.frame(population = state_pop$POPESTIMATE2014, state = state_pop$state)
state_pop_2015 = data.frame(population=state_pop$POPESTIMATE2015,state = state_pop$state)
state_pop_2016 = data.frame(population=state_pop$POPESTIMATE2016,state = state_pop$state)
state_pop_2017 = data.frame(population=state_pop$POPESTIMATE2017,state = state_pop$state)
state_pop_2018 = data.frame(population=state_pop$POPESTIMATE2018,state = state_pop$state)

violence_ratio_df = data.frame(NA)

#2014
violent_incidents_by_state_2014 <- data %>% 
  filter(year == 2014) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_violent_incidents = sum(n_killed + n_injured))

total_incidents_by_state_2014 <- data %>%  
  filter(year == 2014) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_incidents = n())

violent_incidents_by_state_2014 <- left_join(violent_incidents_by_state_2014, state_pop_2014, by = "state")
total_incidents_by_state_2014 <- left_join(total_incidents_by_state_2014, state_pop_2014, by = "state")

violent_incidents_by_state_2014 <- violent_incidents_by_state_2014 %>%
  mutate(violent_incidents_per_100k = round(total_violent_incidents / population * 100000))
  
total_incidents_by_state_2014 <- total_incidents_by_state_2014 %>%
  mutate(total_incidents_per_100k = round(total_incidents / population * 100000))

total_incidents_by_state_2014$violent_incidents = violent_incidents_by_state_2014$total_violent_incidents
total_incidents_by_state_2014$violent_incidents_per_100k = violent_incidents_by_state_2014$violent_incidents_per_100k

total_incidents_by_state_2014 <- total_incidents_by_state_2014 %>%
  mutate(total_to_violent_ratio = violent_incidents_per_100k / total_incidents_per_100k)

#total_incidents_by_state_2014 <- total_incidents_by_state_2014 %>% arrange(desc(total_to_violent_ratio))

#2015
violent_incidents_by_state_2015 <- data %>% 
  filter(year == 2015) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_violent_incidents = sum(n_killed + n_injured))

total_incidents_by_state_2015 <- data %>%  
  filter(year == 2015) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_incidents = n())

violent_incidents_by_state_2015 <- left_join(violent_incidents_by_state_2015, state_pop_2015, by = "state")
total_incidents_by_state_2015 <- left_join(total_incidents_by_state_2015, state_pop_2015, by = "state")

violent_incidents_by_state_2015 <- violent_incidents_by_state_2015 %>%
  mutate(violent_incidents_per_100k = round(total_violent_incidents / population * 100000))
  
total_incidents_by_state_2015 <- total_incidents_by_state_2015 %>%
  mutate(total_incidents_per_100k = round(total_incidents / population * 100000))

total_incidents_by_state_2015$violent_incidents = violent_incidents_by_state_2015$total_violent_incidents
total_incidents_by_state_2015$violent_incidents_per_100k = violent_incidents_by_state_2015$violent_incidents_per_100k

total_incidents_by_state_2015 <- total_incidents_by_state_2015 %>%
  mutate(total_to_violent_ratio = violent_incidents_per_100k / total_incidents_per_100k)

#total_incidents_by_state_2015 <- total_incidents_by_state_2015 %>% arrange(desc(total_to_violent_ratio))

#2016
violent_incidents_by_state_2016 <- data %>% 
  filter(year == 2016) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_violent_incidents = sum(n_killed + n_injured))

total_incidents_by_state_2016 <- data %>%  
  filter(year == 2016) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_incidents = n())

violent_incidents_by_state_2016 <- left_join(violent_incidents_by_state_2016, state_pop_2016, by = "state")
total_incidents_by_state_2016 <- left_join(total_incidents_by_state_2016, state_pop_2016, by = "state")

violent_incidents_by_state_2016 <- violent_incidents_by_state_2016 %>%
  mutate(violent_incidents_per_100k = round(total_violent_incidents / population * 100000))
  
total_incidents_by_state_2016 <- total_incidents_by_state_2016 %>%
  mutate(total_incidents_per_100k = round(total_incidents / population * 100000))

total_incidents_by_state_2016$violent_incidents = violent_incidents_by_state_2016$total_violent_incidents
total_incidents_by_state_2016$violent_incidents_per_100k = violent_incidents_by_state_2016$violent_incidents_per_100k

total_incidents_by_state_2016 <- total_incidents_by_state_2016 %>%
  mutate(total_to_violent_ratio = violent_incidents_per_100k / total_incidents_per_100k)

#total_incidents_by_state_2016 <- total_incidents_by_state_2016 %>% arrange(desc(total_to_violent_ratio))

#2017
violent_incidents_by_state_2017 <- data %>% 
  filter(year == 2017) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_violent_incidents = sum(n_killed + n_injured))

total_incidents_by_state_2017 <- data %>%  
  filter(year == 2017) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_incidents = n())

violent_incidents_by_state_2017 <- left_join(violent_incidents_by_state_2017, state_pop_2017, by = "state")
total_incidents_by_state_2017 <- left_join(total_incidents_by_state_2017, state_pop_2017, by = "state")

violent_incidents_by_state_2017 <- violent_incidents_by_state_2017 %>%
  mutate(violent_incidents_per_100k = round(total_violent_incidents / population * 100000))
  
total_incidents_by_state_2017 <- total_incidents_by_state_2017 %>%
  mutate(total_incidents_per_100k = round(total_incidents / population * 100000))

total_incidents_by_state_2017$violent_incidents = violent_incidents_by_state_2017$total_violent_incidents
total_incidents_by_state_2017$violent_incidents_per_100k = violent_incidents_by_state_2017$violent_incidents_per_100k

total_incidents_by_state_2017 <- total_incidents_by_state_2017 %>%
  mutate(total_to_violent_ratio = violent_incidents_per_100k / total_incidents_per_100k)

#2017
violent_incidents_by_state_2018 <- data %>% 
  filter(year == 2018) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_violent_incidents = sum(n_killed + n_injured))

total_incidents_by_state_2018 <- data %>%  
  filter(year == 2018) %>% 
  group_by(state, year) %>%
  dplyr::summarize(total_incidents = n())

violent_incidents_by_state_2018 <- left_join(violent_incidents_by_state_2018, state_pop_2018, by = "state")
total_incidents_by_state_2018 <- left_join(total_incidents_by_state_2018, state_pop_2018, by = "state")

violent_incidents_by_state_2018 <- violent_incidents_by_state_2018 %>%
  mutate(violent_incidents_per_100k = round(total_violent_incidents / population * 100000))
  
total_incidents_by_state_2018 <- total_incidents_by_state_2018 %>%
  mutate(total_incidents_per_100k = round(total_incidents / population * 100000))

total_incidents_by_state_2018$violent_incidents = violent_incidents_by_state_2018$total_violent_incidents
total_incidents_by_state_2018$violent_incidents_per_100k = violent_incidents_by_state_2018$violent_incidents_per_100k

total_incidents_by_state_2018 <- total_incidents_by_state_2018 %>%
  mutate(total_to_violent_ratio = violent_incidents_per_100k / total_incidents_per_100k)



## -----------------------------------------------------------------------------------------------------------------------
data <- data %>% filter(year != 2013)

incident_data <- rbind(total_incidents_by_state_2014,total_incidents_by_state_2015,total_incidents_by_state_2016,total_incidents_by_state_2017,total_incidents_by_state_2018)

data <- left_join(data, incident_data, by = c("state", "year"))


## -----------------------------------------------------------------------------------------------------------------------
library(dplyr)
state_pop_2017 <- read.csv("C:/Users/akobe/Downloads/scprc-est2017-18+pop-res.csv")
state_pop_2017 %>% filter(!NAME %in% c('United States', 'Puerto Rico'))

violent_incidents_by_state <- filtered_data %>%
  group_by(state) %>%
  dplyr::summarize(total_violent_incidents = sum(n_killed + n_injured))

total_incidents_by_state <- filtered_data %>%  
  group_by(state) %>%
  dplyr::summarize(total_incidents = n())

state_pop_2017 = state_pop_2017 %>% rename(state= NAME)

violent_incidents_by_state <- left_join(violent_incidents_by_state, state_pop_2017, by = "state")
total_incidents_by_state <- left_join(total_incidents_by_state, state_pop_2017, by = "state")

  # Creating df for the total violent incidents by state per person
violent_incidents_by_state <- violent_incidents_by_state %>%
  mutate(violent_incidents_per_100k_2017 = round(total_violent_incidents / POPESTIMATE2017 * 100000))

total_incidents_by_state <- total_incidents_by_state %>%
  mutate(total_incidents_per_100k_2017 = round(total_incidents / POPESTIMATE2017 * 100000))

total_incidents_by_state$violent_incidents = violent_incidents_by_state$total_violent_incidents
total_incidents_by_state$violent_incidents_per_100k_2017 = violent_incidents_by_state$violent_incidents_per_100k_2017
total_incidents_by_state
total_incidents_by_state <- total_incidents_by_state %>%
  mutate(total_to_violent_ratio = violent_incidents_per_100k_2017 / total_incidents_per_100k_2017)

total_incidents_by_state <- total_incidents_by_state %>% arrange(desc(total_to_violent_ratio))
total_incidents_by_state




## -----------------------------------------------------------------------------------------------------------------------
ggplot(incident_data, aes(x = state, y = total_to_violent_ratio)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total to Violent Incident Ratio by State",
       x = "State",
       y = "Total to Violent Incident Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

total_incidents_by_state
library(usmap)
library(ggplot2)
plot_usmap(data = total_incidents_by_state, values = 'total_to_violent_ratio', color = "red") +
  scale_fill_continuous(low = "white", high = "red", name = "Total Incidents", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Total Gun Related Incidents by U.S. State")


## -----------------------------------------------------------------------------------------------------------------------
# Create a bar plot for total_to_violent_ratio
length(city_or_county)
data$city_freq <- table(data$city_or_county)[data$city_or_county]
selected_data <- data[data$city_freq > mean(data$city_freq), ]
attach(selected_data)


ggplot(selected_data, aes(x = reorder(city_or_county, -city_freq), y = city_freq/10000)) +
  geom_bar(stat = "identity", fill = "#df8879") +
  labs(title = "City Frequency in Gun Related Incidents Over 5 Year Interval",
       x = "City",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+scale_y_continuous(labels = scales::comma)
#"#d7e1ee", "#b3bfd1", "#df8879", "#991f17"
#TAKE LESS DATA FILTER TOP AND BOTTOM !_


## -----------------------------------------------------------------------------------------------------------------------
violent_incidents_by_city <- data %>%  
  group_by(city_or_county) %>%
  summarize(total_violent_incidents = sum(n_killed + n_injured))

total_incidents_by_city <- data %>%  
  group_by(city_or_county) %>%
  summarize(total_incidents = n())




## -----------------------------------------------------------------------------------------------------------------------
library(usmap)
data$total_victims = data$n_killed + data$n_injured
custom_palette <- c("#d7e1ee", "#b3bfd1", "#df8879", "#991f17")

ggplot(average_quarterly, aes(x = as.factor(year), y = average_incidents, fill = as.factor(quarter))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = custom_palette)

base_map <- plot_usmap(data = total_incidents_by_state, values = 'total_incidents', color = "#991f17") +
  scale_fill_continuous(low = "#d7e1ee", high = "#991f17", name = "Total Incidents", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Total Gun Related Incidents by U.S. State")
top_10_incidents <- data %>%
  arrange(desc(total_victims)) %>%
  head(10)
ggplot_data <- top_10_incidents %>%
  select(longitude, latitude, total_victims, city_or_county, state)


#base_map + ggplot(TopMap, aes(x = longitude, y = latitude)) +
#  geom_point(aes(size = sqrt(total_victims), color = "red"), alpha = 0.7) +
#  scale_size_continuous(name = "Total Victims", labels = scales::comma) +
#  labs(title = "Top 10 Gun Incidents with Highest Number of Victims",
#       subtitle = "Circle size represents the number of victims",
#       x = "Longitude", y = "Latitude") +
#  theme_minimal() +
#  theme(legend.position = "none")


base_map#+geom_path(data = data, aes(x = longitude, y = latitude),
         #   color = "grey") +
#geom_point(data = data, aes(x = longitude, y = latitude))


#combined_map <- base_map +
#  geom_point(data = ggplot_data, aes(x = longitude[7], y = latitude[7], size = sqrt(total_victims[7])), color = "blue", alpha = 0.7) + geom_point(data = ggplot_data, aes(x = longitude[2], y = latitude[2], size = sqrt(total_victims[2])), color = "blue", alpha = 0.7) #  scale_size_continuous(name = "Total Victims", labels = scales::comma) 

#+ geom_point(data=data. aes(x = longitude[1], y = latitude[1]))+ geom_point(data=data, aes(x = longitude[2], #y = latitude[2]))+ geom_point(data=data, aes(x = longitude[3], y = latitude[3]))
#  geom_text(data = ggplot_data, aes(x = longitude, y = latitude, label = labels), vjust = 1.5, hjust = 0, #size = 2)
#combined_map
# Overlay circles for severity
#severity_map <- geom_point(data = data, aes(x = longitude, y = latitude, size = total_victims), color = "blue", alpha = 0.5) +
#  scale_size_continuous(name = "Total Victims", labels = scales::comma)

# Show the map
#severity_map


## -----------------------------------------------------------------------------------------------------------------------
#data$total_victims = n_killed + n_injured

base_map <- plot_usmap(data = total_incidents_by_state, values = 'total_incidents', color = "red") +
  scale_fill_continuous(low = "white", high = "red", name = "Total Incidents", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Total Gun Related Incidents by U.S. State")

ggplot_data <- top_10_incidents %>%
  select(longitude, latitude, total_victims, city_or_county, state)
mean(data$avg_victim_age)

library(usmap)
library(ggplot2)
library(dplyr)

base_map <- plot_usmap(data = total_incidents_by_state, values = 'total_incidents', color = "red") +
  scale_fill_continuous(low = "white", high = "red", name = "Total Incidents", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Total Gun Related Incidents by U.S. State")

# Calculate total victims for the top 10 incidents
top_10_incidents <- data %>%
  arrange(desc(total_victims)) %>%
  head(10)
#View(top_10_incidents)
# Extract relevant columns for the ggplot2 map
ggplot_data <- top_10_incidents %>%
  select(longitude, latitude, total_victims, city_or_county, state)

#combined_map <- base_map +
#  geom_point(data = ggplot_data, aes(x = longitude, y = latitude, size = sqrt(total_victims)), color = "blue", alpha = 0.7) +
#  scale_size_continuous(name = "Total Victims", labels = scales::comma) +
#  labs(subtitle = "Circle size represents the severity of incidents") +
#  theme_minimal()

# Calculate total victims for each incident
#data$total_victims = data$n_killed + data$n_injured

# Create a base map using ggplot2 and usmap
#base_map <- plot_usmap(data = total_incidents_by_state, values = 'total_incidents', color = "red") +
#  scale_fill_continuous(low = "white", high = "red", name = "Total Incidents", label = scales::comma) +
#  theme(legend.position = "right") +
#  labs(title = "Total Gun Related Incidents by U.S. State")

# Extract relevant columns for the ggplot2 map
ggplot_data <- data %>%
  arrange(desc(total_victims)) %>%
  head(10) %>%
  select(longitude, latitude, total_victims, city_or_county, state)



## -----------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(splitstackshape)  

data$avg_victim_age <- rep(NA, nrow(data))
data$avg_suspect_age <- rep(NA, nrow(data))

age_list = list()
type_list = list()

for (i in 1:nrow(data)) {
  age_string <- data$participant_age[i]
  type_string <- data$participant_type[i]
  
  age_elements <- as.numeric(strsplit(gsub("::", " ", gsub("\\|\\|", " ", age_string)), " ")[[1]])
  type_elements <- strsplit(gsub("::", " ", gsub("\\|\\|", " ", type_string)), " ")[[1]]
  
  age_elements <- as.numeric(age_elements[c(FALSE, TRUE)])
  type_elements <- type_elements[c(FALSE, TRUE)]
  
  age_list[[i]] <- age_elements
  type_list[[i]] <- type_elements
  
}

calculate_average_ages <- function(type_list, age_list) {
  victim_ages <- numeric(length(type_list))
  suspect_ages <- numeric(length(type_list))
  
  for (i in seq_along(type_list)) {
    victim_indexes <- which(type_list[[i]] == "Victim")
    suspect_indexes <- which(type_list[[i]] == "Subject-Suspect")
    
    victim_ages[i] <- mean(unlist(age_list[[i]][victim_indexes], use.names = FALSE), na.rm = TRUE)
    suspect_ages[i] <- mean(unlist(age_list[[i]][suspect_indexes], use.names = FALSE), na.rm = TRUE)
  }
  
  return(data.frame(victim_age = victim_ages, suspect_age = suspect_ages))
}

avg_age = list()
for (i in 1:nrow(data)){
  x = mean(unlist(age_list[[i]], use.names = FALSE), na.rm = TRUE)
  avg_age[[i]] <- x
}

avg_age = lapply(avg_age, as.numeric)
data$avg_age = unlist(avg_age,use.names = FALSE)

calculate_avg_suspect_age <- function(ages, participants) {
  suspect_ages <- ages[participants == "Subject-Suspect"]
  if (length(suspect_ages) > 0) {
    return(mean(suspect_ages, na.rm = TRUE))
  } else {
    return(NA)
  }
}

data$avg2_suspects = mapply(calculate_avg_suspect_age, age_list, type_list)

result_df <- calculate_average_ages(type_list, age_list)
result_df$victim_age[is.na(result_df$victim_age)] <- mean(result_df$victim_age, na.rm = TRUE)
result_df$suspect_age[is.na(result_df$suspect_age)] <- mean(result_df$suspect_age, na.rm = TRUE)
data$avg_age[is.na(data$avg_age)] <- mean(data$avg_age, na.rm = TRUE)
data$avg_victim_age = result_df$victim_age
data$avg_suspect_age = result_df$suspect_age


## -----------------------------------------------------------------------------------------------------------------------
data <- data %>%
  mutate(Males_count = str_count(data$participant_gender, "Male"),
         Females_count = str_count(data$participant_gender, "Female"))

data$n_ppl_involved = data$Males_count+ data$Females_count
data$Males_count[is.na(data$Males_count)] <- 0
data$Females_count[is.na(data$Females_count)] <- 0
data$n_ppl_involved[is.na(data$n_ppl_involved)] <- 0




## -----------------------------------------------------------------------------------------------------------------------
distric_df <- data %>%
  group_by(state_senate_district) %>%
  summarise(violent_incidents = sum(total_victims, na.rm = TRUE),
            total_incidents = n())
distric_df

district_counts <- data %>%
  count(state_senate_district
, sort = TRUE)
x = district_counts[-1,]
top_20 = head(x, 20)
data$top_senate_district <- as.integer(data$state_senate_district %in% top_20$state_senate_district)

state_house_counts <- data %>%
  count(state_house_district
, sort = TRUE)

state_house_counts = state_house_counts[-1,]
top_20 = head(state_house_counts, 20)
data$top_house_district <- as.integer(data$state_house_district %in% top_20$state_house_district)

congressional_counts <- data %>%
  count(congressional_district
, sort = TRUE)

congressional_counts = congressional_counts
top_20 = head(congressional_counts, 20)
data$top_congressional_district <- as.integer(data$congressional_district %in% top_20$congressional_district)

# Plotting with ggplot
ggplot(x, aes(x = reorder(x$state_senate_district, x$n), y = n, fill = x$state_house_district)) +
  geom_bar(stat = 'identity') +
  labs(title = "Frequency Count of Incidents by District",
       x = "District",
       y = "Number of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## -----------------------------------------------------------------------------------------------------------------------
data$city_freq <- table(data$city_or_county)[data$city_or_county]
data$top_city = ifelse(data$city_freq >= 2000, data$city_or_county, 'Other')
data$top_city = as.factor(data$top_city)
relevel(data$top_city, ref = "Other")
attach(data)

data$top_city_binary = as.integer(data$top_city %in% data$city_or_county)
table(data$top_city)

data$state_freq <- table(data$state)[data$state]
mean(data$state_freq)

data$top_state = ifelse(data$state_freq >= mean(data$state_freq), data$state, 'Other')
data$top_state = as.factor(data$top_state)
#relevel(data$top_city, ref = "Other")
attach(data)

data$top_state_binary = as.integer(data$top_state %in% data$state)
#table(data$top_state)



## -----------------------------------------------------------------------------------------------------------------------
data$incident_characteristics[1:4]
data
top_10_incidents <- data %>%
  arrange(desc(total_victims)) %>%
  head(10)   
TopMap <- top_10_incidents %>% select(latitude, longitude, total_victims, city_or_county, state)
labels <- paste0("<strong>City: </strong>", TopMap$city_or_county, 
                 "<br><strong>State: </strong>", TopMap$state,
                 "<br><strong>Victims </strong>", TopMap$total_victims) %>% lapply(htmltools::HTML)

leaflet(TopMap) %>%
        setView(lng=-96, lat=37.8, zoom=4) %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(~longitude, ~latitude, color = "red", radius=~sqrt(total_victims), label = labels)

ggplot(TopMap, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = sqrt(total_victims), color = "red"), alpha = 0.7) +
  scale_size_continuous(name = "Total Victims", labels = scales::comma) +
  labs(title = "Top 10 Gun Incidents with Highest Number of Victims",
       subtitle = "Circle size represents the number of victims",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "none") #+
 # geom_text(aes(label = labels), vjust = 1.5, hjust = 0, size = 3)

leaflet_data <- top_10_incidents %>%
  select(latitude, longitude, total_victims, city_or_county, state)

# Create labels for the leaflet map markers
labels <- paste0("<strong>City: </strong>", leaflet_data$city_or_county, 
                 "<br><strong>State: </strong>", leaflet_data$state,
                 "<br><strong>Victims </strong>", leaflet_data$total_victims) %>% lapply(htmltools::HTML)

# Overlay circles for the top 10 incidents on the leaflet map
#combined_map <- base_map +
#  leaflet(leaflet_data) %>%
#  addTiles() %>%
#  addProviderTiles("CartoDB.Positron") %>%
#  addCircleMarkers(~longitude, ~latitude, color = "blue", radius = ~sqrt(total_victims), label = labels)


## -----------------------------------------------------------------------------------------------------------------------
library(usmap)
library(ggplot2)
library(leaflet)
library(dplyr)
library(plotly)


# Create a base map using ggplot2 and usmap
base_map <- plot_usmap(data = total_incidents_by_state, values = 'total_incidents', color = "red") +
  scale_fill_continuous(low = "white", high = "red", name = "Total Incidents", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Total Gun Related Incidents by U.S. State")

# Extract relevant columns for the leaflet map
leaflet_data <- top_10_incidents %>%
  select(latitude, longitude, total_victims, city_or_county, state)

# Create labels for the leaflet map markers
labels <- paste0("<strong>City: </strong>", leaflet_data$city_or_county, 
                 "<br><strong>State: </strong>", leaflet_data$state,
                 "<br><strong>Victims </strong>", leaflet_data$total_victims) %>% lapply(htmltools::HTML)

# Create a leaflet map for the top 10 incidents
leaflet_map <- leaflet(leaflet_data) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~longitude, ~latitude, color = "blue", radius = ~sqrt(total_victims), label = labels)

# Convert leaflet map to a ggplot object
#sf_leaflet <- leaflet_data %>%
#  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# Combine the two plots
#combined_plot <- subplot(base_map, leaflet_ggplot, nrows = 2)

# Show the combined plot
#combined_plot




## -----------------------------------------------------------------------------------------------------------------------
library(splitstackshape)  
data$incident_characteristics <- gsub("\\|\\|", "|", data$incident_characteristics)
categories <- cSplit(data %>% select(incident_id, state, city_or_county, incident_characteristics), 'incident_characteristics', sep =  "|", direction="long")

all_categories <- unlist(categories$incident_characteristics)
category_df <- data.frame(category = all_categories)
category_counts <- count(category_df, category)
category_counts <- category_counts %>% arrange(desc(n))

N <- 30
top_categories <- head(category_counts, N)
key_words = top_categories[5:30,]
key_words = key_words$category
key_words

data$top20_incident <- ifelse(data$incident_characteristics %in% key_words, 1, 0)

data = data %>%
  mutate(n_guns_involved = if_else(is.na(n_guns_involved) & grepl("shot|gun|gunpoint|shooting|shootings|bullets", notes, ignore.case = TRUE), 1, n_guns_involved))


ggplot(top_categories, aes(x = reorder(category, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") + coord_flip()+
  labs(title = "Top Incident Categories",
       x = "Incident Category",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust =1))



## -----------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(stringr)

# Sample data
df = data$incident_characteristics
df = data.frame(incident_characteristics = df)

df <- df %>%
  mutate(
    Shot_Related = as.numeric(str_detect(incident_characteristics, "Shot")),
    Drug_Related = as.numeric(str_detect(incident_characteristics, "Drug")),
    Drive_by_related = as.numeric(str_detect(incident_characteristics, "Drive-by")),
    gang_related = as.numeric(str_detect(incident_characteristics, "Gang")),
    School_Related = as.numeric(str_detect(incident_characteristics, "School")),
    Defensive_Use = as.numeric(str_detect(incident_characteristics, "Defensive Use")),
    Officer = as.numeric(str_detect(incident_characteristics, "Officer")),
    Possession = as.numeric(str_detect(incident_characteristics, "Possession")),
    Home_House = as.numeric(str_detect(incident_characteristics, "Home")|str_detect(incident_characteristics, "House")),
    Stolen = as.numeric(str_detect(incident_characteristics, "Stolen")),
    Mass_Shooting = as.numeric(str_detect(incident_characteristics, "Mass Shooting")),
    Child = as.numeric(str_detect(incident_characteristics, "Child")),
    
  )

data$shooting_related = df$Shot_Related
data$drug_related = df$Drug_Related
data$gang_related = df$gang_related
data$drive_by_related = df$Drive_by_related
data$school_related = df$School_Related
data$defensive = df$Defensive_Use
data$officer_involved = df$Officer
data$possession_related = df$Possession
data$home_related = df$Home_House
data$stolen_related = df$Stolen
data$mass_shooting = df$Mass_Shooting
data$child_related = df$Child

data$shooting_related[is.na(data$shooting_related)] <- 0
data$drug_related[is.na(data$drug_related)] <- 0
data$school_related[is.na(data$school_related)] <- 0
data$defensive[is.na(data$defensive)] <- 0
data$officer_involved[is.na(data$officer_involved)] <- 0
data$possession_related[is.na(data$possession_related)] <- 0
data$home_related[is.na(data$home_related)] <- 0
data$stolen_related[is.na(data$stolen_related)] <- 0
data$mass_shooting[is.na(data$mass_shooting)] <- 0
data$child_related[is.na(data$child_related)] <- 0


## -----------------------------------------------------------------------------------------------------------------------
data$state = as.factor(data$state)
data$president_indicator <- ifelse(data$date < as.Date("2017-01-20"), 0, 1) #Trump 1, 0 Obama
data


## -----------------------------------------------------------------------------------------------------------------------
#Filling NA values for n_guns_involved
data$gun_stolen[is.na(data$gun_stolen)] <- "Unknown"

data <-data %>%
  mutate(n_guns_involved = ifelse(is.na(n_guns_involved) & grepl("shot|gun|gunpoint|shooting|shootings|bullets", tolower(notes)), 1, n_guns_involved))

data = data %>%  mutate(n_guns_involved = ifelse(is.na(n_guns_involved) & grepl("Shot - Dead (murder, accidental, suicide)", data$incident_characteristics), 1, n_guns_involved))

data$n_guns_involved[is.na(data$n_guns_involved)] <- 0

#Gun Types Processing
gun_types_list <- strsplit(data$gun_type, "\\|\\|")
all_gun_types <- unlist(gun_types_list)
all_gun_types <- gsub("\\d+::", "", all_gun_types)
all_gun_types <- trimws(all_gun_types)

gun_type_counts <- table(all_gun_types)

top_five <- names(sort(gun_type_counts, decreasing = TRUE)[2:6])

df1 = data$gun_type
df1 = data.frame(gun_types = df1)

df1 <- df1 %>%
  mutate(
    handgun_binary = as.numeric(str_detect(gun_types, "Handgun")),
    mm9_binary = as.numeric(str_detect(gun_types, "9mm")),
    rifle_binary = as.numeric(str_detect(gun_types, "Rifle")),
    shotgun_binary = as.numeric(str_detect(gun_types, "Shotgun")),
    LR22_binary = as.numeric(str_detect(gun_types, "22 LR")),
  )

data$handgun_binary <- df1$handgun_binary
data$mm9_binary <- df1$mm9_binary
data$rifle_binary <- df1$rifle_binary
data$shotgun_binary <- df1$shotgun_binary
data$LR22_binary <- df1$LR22_binary

data$handgun_binary[is.na(data$handgun_binary)] <- 0
data$mm9_binary[is.na(data$mm9_binary)] <- 0
data$rifle_binary[is.na(data$rifle_binary)] <- 0
data$shotgun_binary[is.na(data$shotgun_binary)] <- 0
data$LR22_binary[is.na(data$LR22_binary)] <- 0

gun_stolen_list <- strsplit(data$gun_stolen, "\\|\\|")
all_gun_stolen <- unlist(gun_stolen_list)
all_gun_stolen <- gsub("\\d+::", "", all_gun_stolen)
all_gun_stolen <- gsub("\\d+:", "", all_gun_stolen)
all_gun_stolen <- gsub("\\d+|", "", all_gun_stolen)
all_gun_stolen <- trimws(all_gun_stolen)

gun_stolen_counts <- table(all_gun_stolen) #all_gun_stolen Not-stolen Stolen 

df2 = data$gun_stolen
df2 = data.frame(gun_stolen = df2)

df2 <- df2 %>%
  mutate(
    stolen = as.numeric(str_detect(gun_stolen, "Stolen")|str_detect(gun_stolen, "stolen")|str_detect(gun_stolen, "all_gun_stolen")),
    not_stolen = as.numeric(str_detect(gun_stolen, "Not-stolen")),
  )

data$stolen <- df2$stolen
data$not_stolen <- df2$not_stolen

data$not_stolen[is.na(data$not_stolen)] <- 0
data$stolen[is.na(data$stolen)] <- 0


## -----------------------------------------------------------------------------------------------------------------------
attach(data)
data <-data[data$avg_age <= 100, ] #Drop observations over 100 years old age

plot_release_year <- ggplot(data, aes(x = data$avg_age)) +
  geom_density(fill = "#29A98B", color = "#104236") +
  labs(title = "Density Plot of Average Age", x = "Release Year", y = "Density") +
  geom_vline(xintercept = median(data$avg_age), color = "#0077B6", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(data$avg_age), color = "#FFD100", linetype = "dashed", size = 1) +
  annotate("text", x = max(data$avg_age), y = max(density(data$avg_age)$y), 
           label = paste("Skewness:", round(skewness(data$avg_age), 1)), hjust = 2, color = "#FFD100") +
  annotate("text", x = max(data$avg_age), y = max(density(data$avg_age)$y), 
           label = paste("Median:", round(median(data$avg_age), 1)), hjust = 4, color = "#0077B6")


plot_release_year
#max(data$avg_age)


## -----------------------------------------------------------------------------------------------------------------------
sapply(data, class)
all_var_names <- names(data)

quant_vars = sapply(data, is.numeric)
quant_var_names <- all_var_names[quant_vars]

char_vars <- sapply(data, is.character)
char_var_names <- all_var_names[char_vars]

quant_indices <- which(sapply(data, is.numeric))

char_indices <- which(sapply(data, is.character))

qual_vars = data.frame(char_indices)
qual_vars = data.frame(qual_vars[,1])

quant_vars = data.frame(quant_indices)
quant_vars = data.frame(quant_vars[,1])
sum(data$mass_shooting)


## -----------------------------------------------------------------------------------------------------------------------
#manually imputing values
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 577157, 29, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 980577, 26, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 456893, 28, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 1049217, 19, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 879953, 25, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 611479, 22.25, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 121031, 34, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 423223, 26, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 511436, 38, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 1033262, 15, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 197384, 16, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 676663, 25, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 597756, 25, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 108626, 18.67, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 138282, 22, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 380162, 59, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 390774, 27, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 454059, 59, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 501233, 24, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 924854, 20, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 361198, 23, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 743995, 26, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 421014, 22, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 761973, 20, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 848292, 18.5, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 356065, 24, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 966178, 20, avg_suspect_age))
data <- mutate(data, avg_suspect_age = ifelse(incident_id == 107519, 22, avg_suspect_age))

#data <- mutate(data, avg_suspect_age = ifelse(incident_id == 369014, mean_avgSus, avg_suspect_age))
#data <- mutate(data, avg_suspect_age = ifelse(incident_id == 375022, mean_avgSus, avg_suspect_age))
#data <- mutate(data, avg_suspect_age = ifelse(incident_id == 567614, mean_avgSus, avg_suspect_age))


## -----------------------------------------------------------------------------------------------------------------------
require(lmtest)
require(plm)
data_vars = subset(data, select = c(quant_vars[,1]))
data_vars



data_vars = subset(data_vars, select = -c(avg2_suspects))

data_vars1 = subset(data_vars, !is.na(congressional_district))
data_vars1 = subset(data_vars1, !is.na(state_house_district))
data_vars1 = subset(data_vars1, !is.na(state_senate_district))
data_vars1 = subset(data_vars1, !is.na(longitude))
data_vars1 = subset(data_vars1, !is.na(gang_related))

data_vars1 = data_vars1[-1]

missing_counts = colSums(is.na(data_vars1))

percent_missing = missing_counts / nrow(data_vars1) * 100

missing_summary <- data.frame(
  Column = names(data_vars1),
  Missing_Count = missing_counts,
  Percent_Missing = percent_missing
)
missing_summary
#data_vars1
#data_vars1 = data_vars1[-1]
#data_vars1.1 = data_vars1[-6]

summary(lm(total_to_violent_ratio~., data=data_vars1))

library(ggfortify)
library(ggpubr)
#since our model is predicting the total to violence ratio, we should be removing

pca=prcomp(data_vars1, scale=TRUE) 
autoplot(pca, data = data_vars1, loadings = TRUE, loadings.label = TRUE)


## -----------------------------------------------------------------------------------------------------------------------
#data_vars1$month = as.numeric(data_vars1$month)
#data_vars1$wday = as.numeric(data_vars1$wday)
data_vars2 = subset(data_vars1, select = -c(4, 5, avg_suspect_age, avg_victim_age, 26, 45, 46))

pca2.1=prcomp(data_vars2, scale=TRUE) 
autoplot(pca2.1, data = data_vars2, loadings = TRUE, loadings.label = TRUE)



## -----------------------------------------------------------------------------------------------------------------------
data_vars3 = subset(data_vars2, select = -c(1, 2, stolen_related, mass_shooting))

pca2.2=prcomp(data_vars3, scale=TRUE) 
autoplot(pca2.2, data = data_vars3, loadings = TRUE, loadings.label = TRUE)


## -----------------------------------------------------------------------------------------------------------------------
data_vars4 = subset(data_vars3, select = -c(3, violent_incidents, school_related, home_related))

pca2.3=prcomp(data_vars4, scale=TRUE) 
autoplot(pca2.3, data = data_vars4, loadings = TRUE, loadings.label = TRUE)


## -----------------------------------------------------------------------------------------------------------------------
data_vars5 = subset(data_vars4, select = -c(congressional_district, month, day, total_incidents, population))

pca2.4=prcomp(data_vars5, scale=TRUE) 
autoplot(pca2.4, data = data_vars5, loadings = TRUE, loadings.label = TRUE)



## -----------------------------------------------------------------------------------------------------------------------
data_vars6 = subset(data_vars5, select = c(total_to_violent_ratio, top_senate_district, top_house_district, top_congressional_district, top_city_binary, top_state_binary, shooting_related, drug_related, gang_related, drive_by_related, defensive, officer_involved, possession_related, handgun_binary, mm9_binary, rifle_binary, shotgun_binary, LR22_binary,stolen, not_stolen, top20_incident))

pca2.5=prcomp(data_vars6, scale=TRUE) 
autoplot(pca2.5, data = data_vars6, loadings = TRUE, loadings.label = TRUE)



## -----------------------------------------------------------------------------------------------------------------------
data_vars7 = subset(data_vars5, select = c(total_to_violent_ratio, top_senate_district, top_house_district, top_congressional_district, top_city_binary, top_state_binary, shooting_related, drug_related, gang_related, drive_by_related, officer_involved, possession_related, handgun_binary, mm9_binary, rifle_binary, shotgun_binary, LR22_binary,stolen, top20_incident))

pca2.6=prcomp(data_vars7, scale=TRUE) 
autoplot(pca2.6, data = data_vars7, loadings = TRUE, loadings.label = TRUE)



## -----------------------------------------------------------------------------------------------------------------------
data_vars8 = subset(data_vars5, select = c(total_to_violent_ratio, top_senate_district, top_house_district, top_congressional_district, top_city_binary, top_state_binary, shooting_related, drug_related, gang_related, drive_by_related, possession_related, handgun_binary, mm9_binary,stolen, top20_incident))

pca2.7=prcomp(data_vars8, scale=TRUE) 
autoplot(pca2.7, data = data_vars8, loadings = TRUE, loadings.label = TRUE, col=ifelse(data_vars8$drug_related==1, "blue", "transparent"))



## -----------------------------------------------------------------------------------------------------------------------
data_vars9 = subset(data_vars5, select = c(total_to_violent_ratio, top_senate_district, top_house_district, top_congressional_district, top_city_binary, top_state_binary, shooting_related, drug_related, gang_related, drive_by_related, possession_related, handgun_binary, mm9_binary,stolen, top20_incident))

pca2.8=prcomp(data_vars9, scale=TRUE) 
autoplot(pca2.8, data = data_vars9, loadings = TRUE, loadings.label = TRUE, col=ifelse(data_vars9$total_to_violent_ratio<= 0.50, "blue", "grey"))



## -----------------------------------------------------------------------------------------------------------------------
data_vars9 = subset(data_vars5, select = c(total_to_violent_ratio, top_house_district, top_senate_district, top_congressional_district, top_city_binary, top_state_binary, shooting_related, drug_related, gang_related, drive_by_related, possession_related, handgun_binary,stolen, top20_incident))

pca2.8=prcomp(data_vars9, scale=TRUE) 
autoplot(pca2.8, data = data_vars9, loadings = TRUE, loadings.label = TRUE)



## -----------------------------------------------------------------------------------------------------------------------
data_vars10 = subset(data_vars5, select = c(total_to_violent_ratio, top_house_district, top_senate_district, top_congressional_district, top_city_binary, shooting_related, drug_related, gang_related, drive_by_related, possession_related, handgun_binary,stolen, top20_incident))

pca2.9=prcomp(data_vars10, scale=TRUE) 
autoplot(pca2.9, data = data_vars10, loadings = TRUE, loadings.label = TRUE)



## -----------------------------------------------------------------------------------------------------------------------
data_vars11 = subset(data_vars5, select = c(total_to_violent_ratio, top_senate_district, top_city_binary, shooting_related, drug_related, gang_related, drive_by_related, possession_related,top_state_binary,stolen, top20_incident))

pca2.10=prcomp(data_vars11, scale=TRUE) 
autoplot(pca2.10, data = data_vars11, loadings = TRUE, loadings.label = TRUE, col=ifelse(data_vars11$total_to_violent_ratio<0.50, "blue", "grey"))



## -----------------------------------------------------------------------------------------------------------------------
data_vars12 = subset(data_vars5, select = -c(top_senate_district, top_house_district, top_congressional_district, top_city_binary, top_state_binary, shooting_related, drug_related, gang_related, drive_by_related, defensive, officer_involved, possession_related, handgun_binary, mm9_binary, rifle_binary, shotgun_binary, LR22_binary,stolen, not_stolen, top20_incident))

pca2.11=prcomp(data_vars12, scale=TRUE) 
autoplot(pca2.11, data = data_vars12, loadings = TRUE, loadings.label = TRUE)



## -----------------------------------------------------------------------------------------------------------------------
data_vars13 = subset(data_vars5, select = -c(wday,quarter,violent_incidents_per_100k,total_incidents_per_100k,top_senate_district, top_house_district, top_congressional_district, top_city_binary, top_state_binary, shooting_related, drug_related, gang_related, drive_by_related, defensive, officer_involved, possession_related, handgun_binary, mm9_binary, rifle_binary, shotgun_binary, LR22_binary,stolen, not_stolen, top20_incident))
data_vars12
pca2.12=prcomp(data_vars13, scale=TRUE) 
autoplot(pca2.12, data = data_vars13, loadings = TRUE, loadings.label = TRUE)



## -----------------------------------------------------------------------------------------------------------------------
data_vars14 = subset(data_vars1, select = c(total_to_violent_ratio,state_freq, city_freq, Males_count, year, avg_age, top_city_binary, top_senate_district, gang_related, possession_related, drive_by_related, shooting_related, stolen, drug_related))

pca2.13=prcomp(data_vars14, scale=TRUE) 
autoplot(pca2.13, data = data_vars14, loadings = TRUE,loadings.label = TRUE,loadings.color="#991f17",loadings.label.colour="black", col=ifelse(data_vars14$total_to_violent_ratio<0.50, "#df8879","#bfcbdb")) +labs(colour = "PCA Plot on Last Itteration")



## -----------------------------------------------------------------------------------------------------------------------
#scaled = scale(data_vars1.1)
#pca=prcomp(scaled, scale=TRUE) 
pve=(pca2.13$sdev^2)/sum(pca2.13$sdev^2)
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
#ggarrange(plot1, plot2, ncol = 2, nrow = 1)

#autoplot(pca, data = scaled, loadings = TRUE, loadings.label = TRUE)
#identify(qqplot(pca$x[,2],pch = 20, col = c(rep("red", 33), rep("blue", 99))))
#plot(pca$x[, 1], pca$x[, 2], col=rep(rainbow(2), each=7,6), xlab="PC1", ylab="PC2", cex=3)
#text(pca$x[, 1], pca$x[, 2])
#data_vars1


## -----------------------------------------------------------------------------------------------------------------------
#data_vars2 = subset(data_vars1.1, select = -c(13, 21, 22))

#pca2=prcomp(data_vars2, scale=TRUE) 
pve=(pca2.10$sdev^2)/sum(pca2.10$sdev^2)
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
pve
#ggarrange(plot1, plot2, ncol = 2, nrow = 1)

#data_vars1
#autoplot(pca2, data = data_vars2, loadings = TRUE, loadings.label = TRUE, col=ifelse(data_vars2$drug_related == 1, "blue", "transparent"))


## -----------------------------------------------------------------------------------------------------------------------
data_vars2 = subset(data_vars1.1, select = -c(12, 20, 21))

data_vars3 = subset(data_vars2, select = c(total_victims, top_city_binary, top_congressional_district, total_incidents_per_100k, violent_incidents_per_100k, total_to_violent_ratio, Males_count, shooting_related, state_senate_district, congressional_district, population, possession_related, state_house_district, stolen_related, mass_shooting, latitude, longitude, drug_related))
   
pca3=prcomp(data_vars3, scale=TRUE) 
pve=(pca2$sdev^2)/sum(pca2$sdev^2)
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
#ggarrange(plot1, plot2, ncol = 2, nrow = 1)

data_vars1  
autoplot(pca3, data = data_vars3, loadings = TRUE, loadings.label = TRUE, col=ifelse(data_vars3$Males_count >= 3, "blue", "transparent"))
autoplot(pca3, data = data_vars3, loadings = TRUE,
col=ifelse(data_vars3$drug_related==1,"blue","transparent"), loadings.label = TRUE )
data_vars3$drug_related


## -----------------------------------------------------------------------------------------------------------------------
attach(data)

data_c=data[,c("total_victims","avg_suspect_age")]
rownames(data_c)= data$incident_id

length(data_c)
library(ggplot2)
attach(data_c)
plot=ggplot(data_c,aes(y=total_victims, x=avg_suspect_age))
plot+geom_point()

km.2=kmeans(data_c, 2) #2 clusters
km.3=kmeans(data_c, 3) #3 clusters
km.4=kmeans(data_c, 4) #4 clusters
km.5=kmeans(data_c, 5) 
km.6=kmeans(data_c, 6) 

data_c$km4_cluster=as.factor(km.4$cluster)
attach(data_c)
plot+geom_point(aes(colour=km4_cluster)) #add centers. 


## -----------------------------------------------------------------------------------------------------------------------
attach(data)
data$avg_age1 = (data$avg_suspect_age + data$avg_victim_age)/2
data_c=data[,c("total_victims","avg_age1")]
rownames(data_c)= data$incident_id

length(data_c)
library(ggplot2)
attach(data_c)
plot=ggplot(data_c,aes(y=total_victims, x=avg_age1))
plot+geom_point()

km.2=kmeans(data_c, 2) #2 clusters
km.3=kmeans(data_c, 3) #3 clusters
km.4=kmeans(data_c, 4) #4 clusters
km.5=kmeans(data_c, 5) 
km.6=kmeans(data_c, 6) 

data_c$km4_cluster=as.factor(km.4$cluster)
attach(data_c)
plot+geom_point(aes(colour=km4_cluster)) #add centers. 



## -----------------------------------------------------------------------------------------------------------------------
km.4
data_c$km4_cluster <- as.factor(km.4$cluster)
# Create a ggplot scatter plot with cluster coloring
plot <- ggplot(data_c, aes(y = total_victims, x = avg_age1)) +
  geom_point(aes(color = km4_cluster), size = 3, alpha = 0.7) +
  geom_point(data = as.data.frame(km.4$centers), aes(y = total_victims, x = avg_age1),
             color = "black", size = 5, shape = 20) +  # Add cluster centers
  labs(title = "K-Means Clustering of Avgerage Suspect Age vs Total Victims",
       x = "Average Suspect Age",
       y = "Total Victims",
       color="Age Clusters") +
  theme_minimal() +
  theme(legend.position = "top") + 
  scale_color_manual(values= custom_palette)
print(plot)


## -----------------------------------------------------------------------------------------------------------------------
km.4


## -----------------------------------------------------------------------------------------------------------------------
data_qual = subset(data, select = c(qual_vars[,1], top_city, top_state))
data_qual = subset(data_qual, select = -c(address, gun_stolen, gun_type, incident_characteristics,notes,participant_age, participant_age_group, participant_gender, participant_status, participant_type))
data_qual


## -----------------------------------------------------------------------------------------------------------------------
library(randomForest)
library(tree)
library(rpart)
library(rpart.plot)

#data <- data %>%
#  mutate(is_violent = if_else(total_to_violent_ratio > 0.75, 1, 0))

data$age_cluster = data_c$km4_cluster
attach(data)

mytree=rpart(total_to_violent_ratio~ age_cluster+stolen+handgun_binary+ Males_count+ year+ data$avg_age+ top_congressional_district+top_senate_district+ gang_related+ possession_related+ drive_by_related+ shooting_related+ drug_related+top_city+possession_related+top_state+wday+year+avg_suspect_age+avg_victim_age+n_guns_involved ,control=rpart.control(cp=0.01))

testtree=rpart(total_victims~ age_cluster+stolen+handgun_binary+ Males_count+ year+ data$avg_age+ top_congressional_district+top_senate_district+ gang_related+ possession_related+ drive_by_related+ shooting_related+ drug_related+top_city+possession_related+top_state+wday+year+avg_suspect_age+avg_victim_age+n_guns_involved ,control=rpart.control(cp=0.01))

rpart.plot(testtree)
summary(testtree)
rpart.plot(mytree)
summary(mytree)


## -----------------------------------------------------------------------------------------------------------------------
overfittedtree=rpart(total_to_violent_ratio~ age_cluster+stolen+handgun_binary+ Males_count+ year+ data$avg_age+ top_congressional_district+top_senate_district+ gang_related+ possession_related+ drive_by_related+ shooting_related+drug_related+top_city+possession_related+top_state+wday+year+avg_suspect_age+avg_victim_age+n_guns_involved ,control=rpart.control(cp=0.000001))

overfittedtesttree=rpart(total_victims~ age_cluster+stolen+handgun_binary+ Males_count+ year+ data$avg_age+ top_congressional_district+top_senate_district+ gang_related+ possession_related+ drive_by_related+ shooting_related+drug_related+top_city+possession_related+top_state+wday+year+avg_suspect_age+avg_victim_age+n_guns_involved ,control=rpart.control(cp=0.000001))

#rpart.plot(overfittedtree)

printcp(overfittedtree)
plotcp(overfittedtree)
printcp(overfittedtesttree)
plotcp(overfittedtesttree)

opt_cp=overfittedtree$cptable[which.min(mytree$cptable[,"xerror"]),"CP"] 
opt_cptest=overfittedtesttree$cptable[which.min(testtree$cptable[,"xerror"]),"CP"] 



## -----------------------------------------------------------------------------------------------------------------------
besttree = rpart(total_to_violent_ratio~ age_cluster+stolen+handgun_binary+ Males_count+ year+ data$avg_age+ top_congressional_district+top_senate_district+ gang_related+ possession_related+ drive_by_related+ shooting_related+drug_related+top_city+possession_related+top_state+wday+year+avg_suspect_age+avg_victim_age+n_guns_involved ,control=rpart.control(cp=opt_cp))

#printcp(besttree)
summary(besttree)
rpart.plot(besttree)


## -----------------------------------------------------------------------------------------------------------------------
besttesttree = rpart(total_victims~ age_cluster+stolen+handgun_binary+ Males_count+ year+ data$avg_age+ top_congressional_district+top_senate_district+ gang_related+ possession_related+ drive_by_related+ shooting_related+drug_related+top_city+possession_related+top_state+wday+year+avg_suspect_age+avg_victim_age+n_guns_involved ,control=rpart.control(cp=opt_cptest))

summary(besttesttree)
rpart.plot(besttesttree)


## -----------------------------------------------------------------------------------------------------------------------
attach(data)
library(randomForest)
#p = 8 predictors, so our tree will use 2 to 4 predictors per tree
myforest=randomForest(total_to_violent_ratio~top_state+top_city+year+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster+stolen+handgun_binary, ntree=500, data=data, importance=TRUE, na.action = na.omit)

importance(myforest)
varImpPlot(myforest)



## -----------------------------------------------------------------------------------------------------------------------
myforest #44.28 R squared MSR = 0.01522


## -----------------------------------------------------------------------------------------------------------------------
attach(data)
data$age_cluster = as.factor(data$age_cluster)

reg1 = lm(total_to_violent_ratio~top_state+top_city+year+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster, data= data)
summary(reg1) #adj. R^2 of 29.78


## -----------------------------------------------------------------------------------------------------------------------
library(car)
outlierTest(reg1)
data_outliers = data[-c(227216),]
reg2 = lm(total_to_violent_ratio~top_state+top_city+year+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster+quarter+Males_count, data= data_outliers)
summary(reg2) #adj. R^2 of 29.75 - 29.81 - 0.2989


## -----------------------------------------------------------------------------------------------------------------------
require(psych)
corr_matrix = cor(data_vars2)
round(corr_matrix,2)
vif(reg2)
residualPlots(reg2)



## -----------------------------------------------------------------------------------------------------------------------
poly_reg1 = lm(total_to_violent_ratio~top_state+top_city+poly(year,2)+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster+quarter+poly(Males_count,2), data= data_outliers)
summary(poly_reg1) #Adj R, 0.3299

poly_reg2 = lm(total_to_violent_ratio~top_state+top_city+poly(year,2)+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster+quarter+poly(Males_count,3), data= data_outliers)
summary(poly_reg2) #Adj R, 0.3305

cat(summary(reg1)$r.squared, summary(reg2)$r.squared,summary(poly_reg1)$r.squared, summary(poly_reg2)$r.sqaured)

anova(reg2,poly_reg1,poly_reg2) #poly_reg2 is the model to use so far from ANOVA


## -----------------------------------------------------------------------------------------------------------------------
library(caret)

#mytree_fit1=rpart(total_to_violent_ratio~top_state+top_city+year+shooting_related+top_congressional_dist#rict+possession_related+gang_related+drug_related+age_cluster+quarter+Males_count,control=rpart.control(#cp=0.01))

#rpart.plot(mytree_fit1)

set.seed(123)
index <- createDataPartition(data$total_to_violent_ratio, p = 0.7, list = FALSE)

train_data <- data[index, ]
test_data <- data[-index, ]

mytree_fit2 <- rpart(total_to_violent_ratio~top_state+top_city+year+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster+stolen+handgun_binary+Males_count, 
                   data = train_data, control = rpart.control(cp = 0.01))

predictions <- predict(mytree_fit2, test_data, type = "vector")

mse <- mean((predictions - test_data$total_to_violent_ratio)^2)

print(paste("Mean Squared Error (MSE):", mse))


## -----------------------------------------------------------------------------------------------------------------------
mytree_fit3 <- rpart(total_to_violent_ratio~top_state+top_city+year+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster+stolen+handgun_binary+Males_count, 
                   data = train_data, control = rpart.control(cp = 0.000001))

predictions3 <- predict(mytree_fit3, test_data, type = "vector")

mse3 <- mean((predictions3 - test_data$total_to_violent_ratio)^2)

print(paste("Mean Squared Error (MSE):", mse3)) #overfitted tree has MSE of 0.01443

#Prune the tree:
opt_cp3=mytree_fit3$cptable[which.min(mytree_fit3$cptable[,"xerror"]),"CP"] 

mytree_fit_opt <- rpart(total_to_violent_ratio~top_state+top_city+year+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster+stolen+handgun_binary+Males_count, 
                   data = train_data, control = rpart.control(cp = opt_cp3))

predictions4 <- predict(mytree_fit_opt, test_data, type = "vector")

mse4 <- mean((predictions4 - test_data$total_to_violent_ratio)^2)

print(paste("Mean Squared Error (MSE):", mse4)) #0.0143764776615806"
rpart.plot(mytree_fit_opt)


## -----------------------------------------------------------------------------------------------------------------------
varImp(mytree_fit_opt)


## -----------------------------------------------------------------------------------------------------------------------
myforest_model=randomForest(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count, ntree=500 ,data=data, importance=TRUE, na.action = na.omit, do.trace=50, cp=opt_cp3) 




## -----------------------------------------------------------------------------------------------------------------------
library(gbm)
set.seed (1)
attach(data)
boosted=gbm(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count,data=data,distribution="gaussian",n.trees=1000, interaction.depth=4)

boosted1=gbm(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count,data=data,distribution="gaussian",n.trees=1000, interaction.depth=5)

summary(boosted)
summary(boosted1)

predicted_score=predict(boosted, newdata=data, n.trees=1000)
mse_gbm = mean((predicted_score - data$total_to_violent_ratio)^2) 

predicted_score1=predict(boosted1, newdata=data, n.trees=1000)
mse_gbm1 = mean((predicted_score1 - data$total_to_violent_ratio)^2) 

boosted2=gbm(total_victims~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count,data=data,distribution="gaussian",n.trees=1000, interaction.depth=4)

summary(boosted2)
predicted_score2=predict(boosted2, newdata=data, n.trees=1000)
mean((predicted_score2 -data$total_victims)^2) 


## -----------------------------------------------------------------------------------------------------------------------
model_lm = lm(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count, data = train_data)

lm_predictions = predict(model_lm, newdata=test_data)
lm_mse <- mean((lm_predictions - test_data$total_to_violent_ratio)^2)



## -----------------------------------------------------------------------------------------------------------------------
poly_reg2 = lm(total_to_violent_ratio~top_state+top_city+poly(year,2)+shooting_related+top_congressional_district+possession_related+gang_related+drug_related+age_cluster+quarter+poly(Males_count,3), data= train_data)

test_data1 = na.omit(test_data)

poly_preds = predict(poly_reg2, newdata=test_data1)
poly_mse = mean((poly_preds - test_data1$total_to_violent_ratio)^2)
poly_mse


## -----------------------------------------------------------------------------------------------------------------------
mytree_fit_opt <- rpart(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count, data = train_data, control = rpart.control(cp = opt_cp3))

predictions4 <- predict(mytree_fit_opt, test_data, type = "vector")

reg_tree_mse <- mean((predictions4 - test_data$total_to_violent_ratio)^2)

print(paste("Mean Squared Error (MSE):", reg_tree_mse)) #0.0143764776615806"
rpart.plot(mytree_fit_opt)
varImp(mytree_fit_opt)


## -----------------------------------------------------------------------------------------------------------------------
fit_myforest_model=randomForest(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count, ntree=500 ,data=train_data, importance=TRUE, na.action = na.omit, do.trace=50, cp=opt_cp3) 

## -----------------------------------------------------------------------------------------------------------------------
rf_preds = predict(fit_myforest_model, newdata=test_data)
rf_mse = mean((rf_preds - test_data$total_to_violent_ratio)^2) 
fit_myforest_model 


## -----------------------------------------------------------------------------------------------------------------------
boosted1=gbm(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count,data=train_data,distribution="gaussian",n.trees=1000, interaction.depth=3)

boosted2=gbm(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count,data=train_data,distribution="gaussian",n.trees=1000, interaction.depth=4)

boosted3=gbm(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count,data=train_data,distribution="gaussian",n.trees=1000, interaction.depth=5)

boosted4=gbm(total_to_violent_ratio~top_state+top_city+year+top_congressional_district++age_cluster+Males_count,data=train_data,distribution="gaussian",n.trees=1000, interaction.depth=3)

predicted_score=predict(boosted1, newdata=test_data, n.trees=1000)
mse_gbm1 = mean((predicted_score - test_data$total_to_violent_ratio)^2) 

predicted_score1=predict(boosted2, newdata=test_data, n.trees=1000)
mse_gbm2 = mean((predicted_score1 - test_data$total_to_violent_ratio)^2) 

predicted_score2=predict(boosted3, newdata=test_data, n.trees=1000)
mse_gbm3 = mean((predicted_score2 - test_data$total_to_violent_ratio)^2) 

predicted_score3=predict(boosted4, newdata=test_data, n.trees=1000)
mse_gbm4 = mean((predicted_score3 - test_data$total_to_violent_ratio)^2) 


## -----------------------------------------------------------------------------------------------------------------------
models <- data.frame(
Model = c("Linear Regression", "Polynomial Regression", "Regression Tree", "Random Forest", "GBM 1", "GBM 2", "GBM 3", "GBM 4"),
TestMSE = c(lm_mse, poly_mse, reg_tree_mse, rf_mse, mse_gbm1,mse_gbm2,mse_gbm3, mse_gbm4)
)
models
library(stargazer)
stargazer(models, summary=FALSE, rownames=FALSE, align=TRUE, type="latex")


## -----------------------------------------------------------------------------------------------------------------------
library(gbm)
boosted2_cv=gbm(total_to_violent_ratio~top_state+top_city+year+top_congressional_district+possession_related+age_cluster+Males_count,data=train_data,distribution="gaussian",n.trees=1000, interaction.depth=4, cv.folds = 20)
boosted2_cv
predicted_score_CV=predict(boosted2_cv, newdata=test_data, n.trees=1000)
mse_gbm_cv = mean((predicted_score_CV - test_data$total_to_violent_ratio)^2)
df = summary(boosted2_cv)
mse_gbm_cv
stargazer(df, summary=FALSE, rownames=FALSE, align=TRUE, type="latex")
predict(boosted2_cv, newdata = test_data, type = "link", n.trees = 1000)
best.iter <- gbm.perf(boosted2_cv, method = "OOB")
print(best.iter)
best.iter <- gbm.perf(boosted2_cv, method = "cv")
print(best.iter)

par(mfrow = c(1, 2))
summary(boosted2_cv, n.trees = 1) # using first tree
summary(boosted2_cv, n.trees = best.iter) # using estimated best number of trees
print(pretty.gbm.tree(boosted2_cv, i.tree = 1))
print(pretty.gbm.tree(boosted2_cv, i.tree = boosted2_cv$n.trees))


## -----------------------------------------------------------------------------------------------------------------------
library(pdp)

plot(boosted2_cv, i.var = 1, n.trees = best.iter)
plot(boosted2_cv, i.var = 2, n.trees = best.iter)
plot(boosted2_cv, i.var = "Males_count", n.trees = best.iter) # can use index or name


plot(boosted2_cv, i.var = 1:2, n.trees = best.iter)
plot(boosted2_cv, i.var = c("year", 'top_city'), n.trees = best.iter)
# Construct trivariate partial dependence plots
plot(boosted2_cv, i.var = c(1, 2, 6), n.trees = best.iter,
continuous.resolution = 20)

plot(boosted2_cv, i.var = 1:3, n.trees = best.iter)
plot(boosted2_cv, i.var = 2:4, n.trees = best.iter)
plot(boosted2_cv, i.var = 3:5, n.trees = best.iter)
summary(boosted2_cv)

par.Petal_W.Sepal_W <- partial(boosted2_cv, pred.var = c('top_city'), chull = TRUE,n.trees = best.iter)
plot.Petal_W.Sepal_W <- autoplot(par.Petal_W.Sepal_W, contour = TRUE, 
               legend.title = "Partial\ndependence")+
  labs(title = "Partial Dependence Plot for Top",
       x = "City", y = "Partial Dependence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot.Petal_W.Sepal_W
outputD = test_data[1:10,]
finalPreds = predict(boosted2_cv, newdata = outputD, n.trees = 1000)
true = test_data[1:10,]$total_to_violent_ratio
state1 = test_data[1:10,]$top_state
city1 = test_data[1:10,]$top_city
year = test_data[1:10,]$year
congress = test_data[1:10,]$top_congressional_district
poss = test_data[1:10,]$possession_related
age = test_data[1:10,]$age_cluster
males = test_data[1:10,]$Males_count
resid = abs(finalPreds-true)
df = data.frame(resid = resid, state = state1, city=city1, year=year, congress=congress, poss=poss, age=age, males=males)

stargazer(df,summary=FALSE, rownames=FALSE, align=TRUE, type="latex")



## -----------------------------------------------------------------------------------------------------------------------
new_data <- data.frame(
  data = c("New York","Albany",2019, 1,1,'4', 5),
  titles = c('top_state', 'top_city', 'year',  'top_congressional_district', 'possession_related', 'age_cluster', 'Males_count')
)
new_data <- data.frame(
  top_state= "New York",
  top_city = "Albany",
  year=2019,
  top_congressional_district = 1,
  possession_related=1,
  age_cluster='4',
  Males_count=5)
dfff = data[data$state == "New York" & data$city_or_county == "Albany", ]
mean(dfff$total_to_violent_ratio)
# Use the predict function to get predictions for the new data
prediction <- predict(boosted2_cv, newdata = new_data, type = "response")
prediction
stargazer(new_data,summary=FALSE, rownames=FALSE, align=TRUE, type="latex")


## -----------------------------------------------------------------------------------------------------------------------
pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))+plot(cumsum(pve), ylim=c(0,1))
names(data)


## -----------------------------------------------------------------------------------------------------------------------

tm <- DocumentTermMatrix(Corpus(VectorSource(data$incident_characteristics)))

# Convert DTM to a matrix
incident_matrix <- as.matrix(dtm)
tfidf <- weightTfIdf(dtm)
tfidf <- removeSparseTerms(tfidf, 0.999)
tfidf <- as.matrix(tfidf)
pc <- prcomp(tfidf, scale=TRUE)
tfidf <- as.matrix(tfidf)

points = pc$x[,1:2]
# Apply k-means clustering
k <- 5  # Choose the number of clusters
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(incident_matrix, centers = k)

# Add cluster assignments to the original data
data$cluster <- kmeans_result$cluster

# View the resulting dataframe
print(data)


## -----------------------------------------------------------------------------------------------------------------------
unique_categories <- unique(unlist(strsplit(data$incident_characteristics, '\\|\\|')))
category_counts <- table(unlist(strsplit(data$incident_characteristics, '\\|\\|')))

library(tm)
library(dplyr)
library(slam)

data$notes
# Tokenize and clean text
corpus <- Corpus(VectorSource(data$incident_characteristics))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(corpus, toSpace, "/")
docs <- tm_map(docs, toSpace, "\\(")
docs <- tm_map(docs, toSpace, "\\)")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("en"))
docs <- tm_map(docs, removeWords, c("Shot", "shot",'shots', "wounded","accidental")) 

doc <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(doc)
#dtm = removeSparseTerms(dtm, 0.99)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
library("wordcloud")
library("RColorBrewer")

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


tdm <- tm::DocumentTermMatrix(corpus.cleaned)
tdm.tfidf <- tm::weightTfIdf(tdm)
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999)

tfidf.matrix <- as.matrix(tdm.tfidf)
names(data)
# Create document-term matrix
dtm <- DocumentTermMatrix(corpus)
tfidf <- weightTfIdf(dtm)
tfidf <- removeSparseTerms(tfidf, 0.999)
tfidf <- as.matrix(tfidf)
pc <- prcomp(tfidf, scale=TRUE)
tfidf <- as.matrix(tfidf)

points = pc$x[,1:2] #Selecting the first 2 PC's

clustering.kmeans <- kmeans(tfidf.matrix, truth.K)

clustering.kmeans <- kmeans(tfidf, 5)
master.cluster <- clustering.kmeans$cluster
plot(points,
     main = 'K-Means clustering',
     col = as.factor(master.cluster),
     mai = c(0, 0, 0, 0),
     mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')

dtm_matrix <- as.matrix(dtm)
dtm_matrix


num_clusters <- 5  # Adjust as needed

# Perform k-means clustering on the TF-IDF matrix
kmeans_result <- kmeans(as.matrix(tfidf), centers = num_clusters)

# Extract cluster assignments
clusters <- kmeans_result$cluster

# Extract the first two principal components
pc <- prcomp(as.matrix(tfidf))$x[, 1:2]
pc
# Create a data frame for plotting
plot_data <- data.frame(PC1 = pc$x[, 1], PC2 = pc$x[, 2], Cluster = as.factor(clustering.kmeans))

# Plot the clusters
ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(title = "Clustering of Incidents based on TF-IDF",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

