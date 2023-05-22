#Load data and packages

install.packages('tidyverse')
install.packages('scales')
install.packages('plotly')
install.packages('gridExtra')
install.packages('dplyr')
install.packages('modelr')
install.packages('caret')
install.packages('ROSE')
install.packages('glmnet')
install.packages('ggrepel')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('fitdistrplus')
install.packages('plotrix')
install.packages('e1071')
install.packages('reshape2')


library(tidyverse)
library(scales)
library(lubridate)
library(plotly)
library(gridExtra)
library(tidytext)
library(modelr)
library(caret)
library(ROSE)
library(glmnet)
library(ggrepel)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(fitdistrplus)
library(plotrix)
library(e1071)
library(dplyr)
library(reshape2)


#Set Directory

getwd()
setwd("C:/Users/siddh/Desktop/HUSKY!/Probs & Stats/Project/Accidents")

#Load CSV

df_accidents <- read_csv("US_Accidents_Dec20_updated.csv", col_types = cols(.default = col_character())) %>% 
type_convert()

df_accidents %>% head(5)

# Data Pre-Processing

#1. Drop variables with high NA proportion

df_accidents %>% summarise_all(~ mean(is.na(.))) %>% 
  pivot_longer(1:47, names_to = "Variables to drop", values_to = "NA proportion") %>% 
  filter(`NA proportion` >= 0.5)

#Drop the variable "Number" since it has high proportion of NA values 
#and also it is not a significant variable

#2. Drop un-useful variables
#During initial analysis we thought tha wind direction would be an affecting factor but when it was
#plotted against the severity level, it was observed that it does not have a great impact on the severity levels.
#Hence, we decided to drop it.

ggplot(df_accidents, aes(Wind_Direction, ..prop.., group = Severity)) +
  geom_bar(aes(fill = Severity), position = "dodge") +
  scale_y_continuous(labels = percent) +
  labs(x = "Wind_Direction",
       y = "Proportion",
       title = "Wind direction does not have a great impact on severity") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

drops <- c("ID", "Timezone", "Airport_Code", "Weather_Timestamp","Wind_Direction", "Description","End_Lat", "End_Lng", "Number", "Wind_Chill(F)", "Precipitation(in)")
df_drop <- df_accidents[ , !(names(df_accidents) %in% drops)]


#3. Rename variables to avoid future error

df_drop <-  df_drop %>%
  rename("Distance" = `Distance(mi)`, "Temperature" = `Temperature(F)`, "Humidity" = `Humidity(%)`, 
         "Pressure" = `Pressure(in)`, "Visibility" = `Visibility(mi)`, "Wind_Speed" = `Wind_Speed(mph)`)


#4. Transform time related variables

#Date & Time as a whole would make us lose out on some of the key insights,
#hence it would be better to convert it into several variables.

df_drop %>% dplyr :: select(Start_Time, End_Time) %>% head(5)


df_time <- df_drop %>%
  mutate(Duration = as.numeric(End_Time - Start_Time)) %>%
  # accident duration should be positive
  filter(!(Duration < 0)) %>%
  separate(Start_Time, into = c("Date", "Time"), sep = " ") %>%
  mutate("Year" = str_sub(Date, 1, 4), "Month" = str_sub(Date, 6, 7), "Day" = str_sub(Date, 9, 10), 
         "Wday" = as.character(wday(Date)), "Hour" = str_sub(Time, 1, 2)) %>%
  dplyr::select(-c("Date", "Time", "End_Time")) %>%
  dplyr::select(Severity, Year, Month, Day, Hour, Wday, Duration, everything())

df_time %>%
  dplyr::select(Year, Month, Day, Hour, Wday, Duration) %>%
  head(5)


#5. Drop the NA values in the Weather_Condition variable

df_time %>% filter(is.na(Weather_Condition)) %>% dplyr::select(Temperature:Weather_Condition) %>%
  head(10)

df_weather <- df_time %>% filter(!is.na(Weather_Condition))


#6. Location related variables
address <- c("Country", "County", "Street", "Zipcode")
df_weather %>%
  dplyr::select(all_of(address)) %>%
  head(5)

df_add <- df_weather %>% dplyr::select(-all_of(address))


#7. Modify the variable type Severity

df_add <- df_add %>% 
  mutate(Severity = as.character(Severity)) %>% 
  mutate_if(is.logical, as.character)

#8. We need to handle NA values in continuous variables.

df_mean <- df_add %>%
  mutate_if(is.numeric, ~ replace_na(., mean(., na.rm = T)))

summary(df_mean %>% select_if(is.numeric))


#9. We need to handle NA values in categorical variables as well

df_mean %>% summarise_all(~sum(is.na(.))) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_count") %>% filter(NA_count > 0)

df_final <- df_mean %>%
  filter(!is.na(City)) %>%
  filter(!is.na(Sunrise_Sunset))%>%
  filter(!is.na(Civil_Twilight))%>%
  filter(!is.na(Nautical_Twilight))%>%
  filter(!is.na(Astronomical_Twilight))

#10. Write into a csv file for future use

write_csv(df_final, "US_Accidents_Cleaned.csv")


# Visualization

df_US_accidents <- read_csv("US_Accidents_Cleaned.csv", col_types = cols(.default = col_character())) %>% 
  type_convert() %>%
  mutate(Severity = factor(Severity), Year = factor(Year), Wday = factor(Wday)) %>%
  mutate_if(is.logical, factor) %>%
  mutate_if(is.character, factor)

#1.State-wise traffic accident count

states <- map_data("state") %>% as_tibble() %>% dplyr :: select(long, lat, group, region)
states_abb <- read_csv("USA_State_Name_&_Code.csv") %>%
  mutate(State = tolower(State)) %>%
  dplyr ::select(State, Code) %>%
  rename("State_full" = State)
accident_count <- df_US_accidents %>%
  count(State)  %>%
  left_join(states_abb, by = c("State" = "Code"))

accident_count_df <- accident_count
accident_count_df

accident_count_df <- rename(accident_count_df,"Accident_Count" = n)
accident_count_df

ggplot(accident_count_df, aes(State,Accident_Count)) +
  geom_bar(color = 'Black', fill = 'steelblue', stat='identity') +
  scale_y_continuous(breaks = seq(0, 700000, 100000), labels = unit_format(unit = "K", scale = 1e-03))+
theme(axis.text.x = element_text(angle = 60, vjust = 0.6))


#Top 10 states with highest number of accident count

states <- states %>%
  left_join(accident_count, by = c("region" = "State_full"))
top_10 <- accident_count %>%
  arrange(desc(n)) %>%
  head(10)
top_10_df <- top_10
top_10 <- top_10$State %>% unlist()

top_10_map <- states %>%
  filter(State %in% top_10)
top_10_label <- top_10_map %>%
  group_by(region, State) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(states, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = n), color = "#636363", size = 0.1) +
  geom_polygon(data = top_10_map, color = "red", fill = NA, size = 0.8) +
  scale_fill_gradient(low = "#fee5d9", high = "#de2d26",
                      name = "Accident Count", labels = unit_format(unit = "K", scale = 1e-03)) +
  ggrepel::geom_label_repel(mapping = aes(label = State, group = 1), data = top_10_label) +
  theme_minimal() +
  coord_quickmap() +
  labs(title = "Accident distribution in the U.S.",
       x = "Longitude",
       y = "Latitude")


# Bar Plot of top 10 states with highest number of accident count
df_US_accidents %>% 
  filter(State %in% top_10) %>%
  count(State) %>%
  ggplot(aes(reorder(State, n), n)) +
  geom_col(stat="identity", fill="coral") +
  geom_label(aes(label = n), nudge_y = -30000) +
  labs(x = NULL, y = "Number of accidents",
       title = "Top 10 States with the most accidents") +
  scale_x_discrete(labels = rev(c("California", "Florida", "Oregon", "Texas",
                                  "New York", "Minnesota","North Carolina",
                                  "Virginia", "Pennsylvania", "Illlinois"))) +
  scale_y_continuous(breaks = seq(0, 700000, 100000), labels = unit_format(unit = "K", scale = 1e-03)) +
  coord_flip()


#Pie chart of top 10 states with highest number of accident count

ggplot(top_10_df, aes(x="", y=n, fill=State)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void()


#Top 10 states with least number of accident count

states <- map_data("state") %>% as_tibble() %>% dplyr :: select(long, lat, group, region)
states_abb <- read_csv("USA_State_Name_&_Code.csv") %>%
  mutate(State = tolower(State)) %>%
  dplyr ::select(State, Code) %>%
  rename("State_full" = State)
accident_count <- df_US_accidents %>%
  count(State) %>%
  left_join(states_abb, by = c("State" = "Code"))
states <- states %>%
  left_join(accident_count, by = c("region" = "State_full"))

# least 10 states
least_10 <- accident_count %>%
  arrange(desc(n)) %>%
  tail(10)
least_10_df <- least_10
least_10_df
least_10 <- least_10$State %>% unlist()

least_10_map <- states %>%
  filter(State %in% least_10)
least_10_label <- least_10_map %>%
  group_by(region, State) %>%
  summarise(long = mean(long), lat = mean(lat))


ggplot(states, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = n), color = "#0d8cd6", size = 0.1) +  # #636363 is the 
  geom_polygon(data = least_10_map, color = "blue", fill = NA, size = 0.8) +
  scale_fill_gradient(low ="#116fd4" , high = "#93c5fa",
                      name = "Accident Count", labels = unit_format(unit = "k", scale = 0.4e0/10000)) +
  ggrepel::geom_label_repel(mapping = aes(label = State, group = 1), data = least_10_label) +
  theme_minimal() +
  coord_quickmap() +
  labs(title = "Accident distribution in the U.S.",
       x = "Longitude",
       y = "Latitude")

#Bar Plot of top 10 states with least number of accident count

df_US_accidents %>% 
  filter(State %in% least_10) %>%
  count(State) %>%
  ggplot(aes(reorder(State, n), n)) +
  geom_col(stat="identity", fill="#6ef0e3") +
  geom_label(aes(label = n), nudge_y = 0) +
  labs(x = NULL, y = "Number of accidents",
       title = "Top 10 States with the least accidents") +
  scale_x_discrete(labels = rev(c("Montana", "Mississippi", "Delaware", "Nebraska",
                                  "Maine", "New mexico", "North Dakota",
                                  "Vermont", "Wyoming", "South dakota"))) +
  
  coord_flip()


#Pie Chart of top 10 states with least number of accident count

ggplot(least_10_df, aes(x="", y=n, fill=State)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()


#2. Distance of road affected by accidents

df_US_accidents %>%
  group_by(Severity) %>%
  summarise(prop = mean(Distance)) %>%
  ggplot(aes(Severity, prop, fill = !Severity %in% c(3,4))) +
  geom_col() +
  labs(
    y = "Average affected distance (mi)",
    title = "More severe accidents tend to affect longer road distance") +
  scale_fill_discrete(name = "Severity", labels = c("More Severe: 3 or 4", "Less Severe: 1 or 2"))


#3. Accident count in each severity level

df_US_accidents %>%
  group_by(Year, Severity) %>%
  count() %>%
  group_by(Year) %>%
  mutate(sum = sum(n)) %>%
  
  
  mutate(Proportion = n / sum) %>%
  ggplot(aes(Severity, Proportion)) +
  geom_col(aes(fill = Year), position = "dodge") +
  labs(x = "Severity",
       y = "Proportion",
       title = "Severity proportion changes by year") +
  scale_y_continuous(labels = percent)


#4. Accident account in different time scales

  top <- df_US_accidents %>%
  count(Month) %>%
  ggplot(aes(Month, n)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  labs(y = "Accident Count",
       x = NULL,
       title = "Pattern between accident counts and month & day of the week") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep", "Oct",
                              "Nov", "Dec")) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))

  bottom <- df_US_accidents %>%
  ggplot(aes(Month, fill = Wday)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("deepskyblue1", "orange", "orange","orange","orange","orange", "deepskyblue1"),
                    name = "Day of the week",
                    labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep", "Oct",
                              "Nov", "Dec")) +
  labs(y = "Accident Count") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))

grid.arrange(top, bottom, heights = c(1/4, 3/4))

#Hourly & Weekly distribution of accidents

right <- df_US_accidents %>%
  ggplot(aes(Hour, color = Wday %in% c("1", "7"), group = Wday %in% c("1", "7"))) +
  geom_freqpoly(stat = "count") +
  scale_color_discrete(name = "Is weekdays?", labels = c("Yes", "No")) +
  labs(y = NULL,
       title = " ") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))

left <- df_US_accidents %>%
  ggplot(aes(Hour, fill = !Hour %in% c("07", "08", "16", "17"))) +
  geom_bar(show.legend = F) +
  labs(x = "Hour",
       y = "No of Accidents",
       title = "Hourly Distribution of Accidents") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))

grid.arrange(left, right, widths = c(1/2, 1/2))


#5.Impact of weather condition on accident severity

weather <- df_US_accidents %>% group_by(Severity) %>% count(Weather_Condition) %>% 
           mutate(n = n / sum(n)) %>% filter(n > 0.02)
weather <- weather$Weather_Condition

df_US_accidents %>%
  filter(Weather_Condition %in% weather) %>%
  group_by(Severity) %>%
  count(Weather_Condition) %>%
  mutate(n = n / sum(n)) %>%
  ggplot(aes(reorder_within(Weather_Condition, n, Severity), n)) +
  geom_col(aes(fill = !Weather_Condition == "Clear"), show.legend = F) +
  facet_wrap(~ Severity, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(breaks = seq(0, 0.4, 0.05), labels = percent) +
  geom_ref_line(h = 0.1, colour = "red", size = 1) +
  geom_ref_line(h = 0.3, colour = "red", size = 1) +
  labs(x = "Weather Condition",
       y = "Proportion",
       title = "Weather condition does not have a strong impact on accident severity")


#6. Fitting Distributions

fit_nb <- fitdist(accident_count_df$Accident_Count, 'nbinom')
summary(fit_nb)

fit_p <- fitdist(accident_count_df$Accident_Count, 'pois')
summary(fit_p)

gofstat(list(fit_nb, fit_p))


#7. PMF, CDF, Correlation Coefficient

#Accidents occurring at 8 AM

req_col_08 <- dplyr::select(df_US_accidents,Year, Month, Day, Hour)
head(req_col_08, 5)

req_rows_08 <- dplyr::filter(req_col_08,Year == "2019" & Hour == "08")
daily_08<- group_by(req_rows_08, Month, Day, Hour)

daily_08 <- summarise(daily_08, count = n())
head(daily_08)

days_08 <- group_by(daily_08,Month)
days_08 <- summarise(days_08, num_accidents = n())
head(days_08)

accident_08_pmf <- round(days_08$num_accidents/sum(days_08$num_accidents),3)
accident_08_pmf

accident_08_cdf <- round(cumsum(accident_08_pmf),3)
accident_08_cdf

accident_08_freq <- cbind(days_08, accident_08_pmf = accident_08_pmf, accident_08_cdf = accident_08_cdf)
accident_08_freq

#Frequenc Plot

ggplot(accident_08_freq, aes(accident_08_pmf, Month)) +
  geom_bar(stat="identity", fill="#faed32")+
  theme_bw() +
  labs(x = 'Accidents Occuring at 8 AM', y = 'Months') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous("Accident Probability", labels = as.character(accident_08_freq$accident_08_pmf),
                     breaks = accident_08_freq$accident_08_pmf) +
coord_flip()

#Accidents occurring at 17 PM

req_col_17 <- dplyr::select(df_US_accidents,Year, Month, Day, Hour)
head(req_col_17, 5)

req_rows_17 <- dplyr::filter(req_col_17,Year == "2019" & Hour == "17")
daily_17<- group_by(req_rows_17, Month, Day, Hour)

daily_17 <- summarise(daily_17, count = n())
head(daily_17)

days_17 <- group_by(daily_17,Month)
days_17 <- summarise(days_17, num_accidents = n())
head(days)

accident_17_pmf <- round(days_17$num_accidents/sum(days_17$num_accidents),3)
accident_17_pmf

accident_17_cdf <- round(cumsum(accident_17_pmf),3)
accident_17_cdf

accident_17_freq <- cbind(days_17, accident_17_pmf = accident_17_pmf, accident_17_cdf = accident_17_cdf)
accident_17_freq

#Frequency Plot

ggplot(accident_17_freq, aes(accident_17_pmf, Month)) +
  geom_bar(stat="identity", fill="#b960f0")+
  theme_bw() +
  labs(x = 'Accidents Occuring at 8 AM', y = 'Months') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous("Accident Probability", labels = as.character(accident_17_freq$accident_17_pmf),
                     breaks = accident_17_freq$accident_17_pmf) +
  coord_flip()

#Joint Frequency

joint_freq <- outer(accident_08_freq$num_accidents, accident_17_freq$num_accidents, FUN = "+")
rownames(joint_freq) <- accident_08_freq$Month
colnames(joint_freq) <- accident_17_freq$Month

joint_freq

#Joint Probability

joint_prob <- round(joint_freq/sum(joint_freq),3)
joint_prob

joint_df <- melt(joint_freq)
colnames(joint_df) <- c('accident_08', 'accident_17', 'frequency')
head(joint_df, 10)

ggplot(data = joint_df,aes(x=accident_08, y=accident_17)) +
  geom_point(aes(size = frequency, color = frequency)) +
  labs(x = 'Accident at 7 AM', y = 'Accident at 17 PM') +
  scale_x_continuous("accident_08", labels = as.character(joint_df$accident_08),
                     breaks = joint_df$accident_08) +
  scale_y_continuous("accident_17", labels = as.character(joint_df$accident_17),
                     breaks = joint_df$accident_17)

#Correlation Coefficient

acc_8 <- df_US_accidents %>%
  dplyr::select(Year, Month, Day, Hour) %>%
  dplyr::filter(Hour == '08') %>%
  group_by(Month = Month, Day) %>%
  summarise(count = n())
acc_8


acc_17 <- df_US_accidents %>%
  dplyr::select(Year, Month, Day, Hour) %>%
  dplyr::filter(Hour == '17') %>%
  group_by(Month = Month, Day) %>%
  summarise(count = n())
acc_17

cor(acc_8$count, acc_17$count)


#8. Hypothesis testing 

#Sampling  
set.seed(500000)

#Creating a sample of randomly selected 1,000 records using sample_n()  
accident_sample <- sample_n(df_US_accidents, 500000)

#More samples by sub-setting the accidents under severity-"4"
df_severity_4 <- df %>%
  dplyr::filter(Severity == '4' )

#a. One sample t-test - used to test the hypothesis about the mean duration of accident

# Sample Mean duration (Samp_Mean)
mean(as.numeric(accident_sample$Duration))

# Population Mean duration (Pop_Mean)
mean(as.numeric(df_US_accidents$Duration))

# X = R.V. of duration of the accident
# H0: Samp_Mean = 274.89(There is no significant difference between the mean duration of accidents with severity level 4 and the mean duration of population)
# H1: Samp_Mean!= 278.77(There is a significant difference between the mean duration of accidents with severity level 4 and the mean duration of population)

df_severity_4 <- df_US_accidents %>%
  dplyr :: filter(Severity == '4' )

sample   <- as.numeric(df_severity_4$Duration)
pop_mean <-mean(as.numeric(df_US_accidents$Duration))

# Performing the one sample t-test

t.test(x=sample , mu=pop_mean)

# Conclusion from the Test using the p-value approach

#As p_value < 0.05, we reject the null hypothesis and 
#conclude that there is significant difference between the mean duration of accidents 
#with severity level 4 and the mean accident duration of population


#b. Two sample t-test

#To test if the mean accident duration of two random samples generated from the accident data are equal or not.

#X1 = R.V. of duration of the accident from first sample
#X2 = R.V. of duration of the accident from second sample

# H0: Pop_mean1-Pop_mean2 = 0 (There is no significant difference between the mean accident duration of the two random samples)
# H1: Pop_mean1-Pop_mean2!= 0 (There is a significant difference between the mean accident duration of the two random samples)

#### Step 1: Create two samples

#to return the same data each time we sample it, we need to set the seed to a numerical value
set.seed(10000)

#First population - select all columns for rows 1 to 16000  
accident_sample_Hour_1 <- df_US_accidents[1:735990,]     

#Second population - select all columns for rows 16001 to 32561
accident_sample_Hour_2 <- df_US_accidents[735991:1471980,] 

#sample 1000 rows from the first population
acc_1_sample <- sample_n(accident_sample_Hour_1, 10000)

#sample 1000 rows from the second population
acc_2_sample <- sample_n(accident_sample_Hour_2, 10000) 

# selecting the age attribute from the first sample
sample1 <-as.numeric(acc_1_sample$Duration)
mean(sample1)
# selecting the age attribute from the second sample
sample2 <-as.numeric(acc_2_sample$Duration)
mean(sample2)
#### Step 2: Run the t-test
t.test(x=sample1,y=sample2)

# Conclusion from the Test using the p-value approach
#As p_value < 0.05, we reject the null hypothesis and conclude that there is a 
#significant difference between the mean accident duration of the two random samples.

#c. One sample proportion test:
#Based on accident data from 2016 to 2020, 83% of accident occurred on weekdays
#Is there any different percentage of accident occurred on weekdays in the year from 2016 to 2020?

# X = R.V. of proportion of accidents occurred on weekend in the year from 2016 to 2020.

# H0: Pop_prop = 83% (There is a significant difference of exactly 83% between Proportion of accident occurred on weekdays in the year 2016 to 2020 and Proportion of accidents occurred on a weekend)
# H1: Pop_prop!= 83% (There is no significant difference of exactly 83% between Proportion of accident occurred on weekdays in the year 2016 to 2020 and Proportion of accidents occurred on a weekend)

# Unique values in Weekday column (2,3,4,5,6,7,1 = Mon, Tues,.., Sunday)
unique(df_US_accidents$Wday)

# Filtering the data based on all different days in weekdays
Weekday <- df_US_accidents%>%
  dplyr::filter(Wday %in% (2:6))

# Gathering all the rows in weekends i.e., (Wday= Sunday or Saturday)
Weekend <- df_US_accidents%>%
  dplyr::filter(Wday==7 | Wday==1)

# Define x, a vector of counts of successes
Count_Weekday <- nrow(Weekday)
Count_Weekend <- nrow(Weekend)

# Total Sample size (n)
Count_df <- nrow(df_US_accidents)

# Run the one-sample proportion test
prop.test(x= Count_Weekday, n=Count_df, p=0.83, correct = TRUE, conf.level = 0.95,
          alternative = "two.sided")

# correct = a logical indicator for whether Yates' continuity correction should be applied to reduce error in approximation.

# Conclusion from the test using the p-value approach,
# As p_value > 0.05, we accept the null hypothesis and conclude that there is a significant difference of exactly 83%
# between proportion of accident occurred on weekdays and weekends.


#d. Two sample proportion test:
# Test to check whether the proportion of count of accident with severity 4 on an average weekday vs avg weekend 

#X1 = R.V. of proportion of count of accident with severity 4 on an average weekday.
#X2 = R.V. of proportion of count of accident with severity 4 on an average weekend. 

# H0: Pop_prop1-Pop_prop2 = 0 (There is no significant difference between Proportion of count of accidents on an avg weekday with severity 4 and on an avg weekend day with severity 4 )
# H1: Pop_prop1-Pop_prop2!= 0 (There is a significant difference between Proportion of count of accidents on an avg weekday with severity 4 and on an avg weekend day with severity 4 )


# Determine the number of accidents on an avg weekend and an avg weekday.
N1 <- Count_Weekday/5
N2 <- Count_Weekend/2
head(Weekday)

# Filter and calculate the number of rows based on a day and severity 4.
X1 <- Weekday%>%
  dplyr::filter(Severity==4)%>%
  nrow()
X2 <- Weekend%>%
  dplyr::filter(Severity==4)%>%
  nrow()

# Executing the test
prop.test(x=c(X1/5, X2/2),n=c(N1,N2))

# Conclusion from the test using the p-value approach,
# As p_value < 0.05, we reject the null hypothesis and conclude that there is a 
# significant difference between Proportion Proportion of count of accidents on
# an avg weekday with severity 4 and on an avg weekend day with severity 4.


#e. Ratio of Variances test (two-sample test)- F-test:
# Test to check whether the ratio of variance of distance affected by accident with
# severity 3 and severity 4?

# X1 = R.V. of variance of distance affected by the accident of severity 3.
# X2 = R.V. of variance of distance affected by the accident of severity 4. 

# H0: var_severity_3 - var_severity_4 == 0 (There is no significant difference between variance of distance affected by an accident of severity 3 and an accident of severity 4)
# H1: var_severity_3 - var_severity_4 != 0 (There is a significant difference between variance of distance affected by an accident of severity 3 and an accident of severity 4)

# Finding the unique values within severity
unique(df_US_accidents$Severity)

# Filtering the accidents of severity 3
Severity_3 <- df_US_accidents%>%
  dplyr::filter(Severity==3)
Distance_aff_S3 <- Severity_3$Distance
Distance_aff_S3

# Filtering the accidents of severity 4
Severity_4 <- df_US_accidents%>%
  dplyr::filter(Severity==4)
Distance_aff_S4 <- Severity_4$Distance
Distance_aff_S4

# Executing the test
var.test(x= Distance_aff_S3,y=Distance_aff_S4)

# Conclusion from the test using the p-value approach,
# As p_value < 0.05, we reject the null hypothesis and conclude that there is a significant difference between variance of 
# distance affected by an accident of severity 3 and an accident of severity 4.


