################### Data Importation


#setting working directory
setwd("C:/Users/User/Desktop/Global health R/data_ICU/Digital health assignment")
getwd()

#Importing "Life_Expectancy_Data" data as .CSV
ICU <- read.csv("ICU_Bed.csv", header = TRUE)




################## Data exploration (Primary)

#install.packages("dplyr")
library(dplyr)

#Checking data overview
head(ICU)
summary(ICU)

#Display a vertical preview of the dataset and dimension of the dataframe.
dim(ICU)
glimpse(ICU)

#Determine the data types of a data frame's columns
sapply(ICU, class)

#Count Distinct Values in All Columns
sapply(ICU, function(x) n_distinct(x))


################## Data exploration (Secondary)


#install.packages("skimr")
#install.packages("naniar")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
library(skimr)
library(naniar)
library(ggplot2)
library(dplyr)
library(tidyr)

#Displays most of the numerical attributes from summary, but it also displays missing values, more quantile information and an inline histogram for each variable!
skim(ICU)
gg_miss_fct(x=ICU, fct=State)
is.null(ICU)

#How many unique state and county names does the data have
length(unique(ICU$State))
length(unique(ICU$County))


################## Data Analysis (Correlation)


#install.packages("corrplot")
library(corrplot)

#Correlation matrix
ICU_bed <- select(ICU,ICU.Beds, Total.Population,Population.Aged.60.,Percent.of.Population.Aged.60.)
cor_matrix <- cor(ICU_bed)
corrplot(cor_matrix, method = 'number', order = 'alphabet', type = "upper")



################## Data Analysis (State level)

#install.packages("plotly")
#install.packages("forecats")
library(plotly)
library(forcats)
        
#Number of counties per state
no_of_counties <- ICU %>% count(State) %>% arrange(n)
no_of_counties
no_of_counties %>% 
  select(State, n) %>% 
  mutate(State = fct_reorder(State, n)) %>% 
  ggplot(aes(State, n, fill=State)) +
  geom_text(aes(label=n), vjust=0.2, hjust=-0.15) +
  geom_col()+
  coord_flip()


#Number of ICU beds per state
no_of_ICU_Bed <- ICU %>% group_by(State) %>% 
  summarise(ICU.Beds = sum(ICU.Beds))
no_of_ICU_Bed
no_of_ICU_Bed %>% 
  select(State, ICU.Beds) %>% 
  mutate(State = fct_reorder(State, ICU.Beds)) %>% 
  ggplot(aes(State, ICU.Beds, fill=State)) +
  geom_text(aes(label=ICU.Beds), vjust=0.2, hjust=-0.15) +
  geom_col()+
  coord_flip()


#Total population per state
Total_population <- ICU %>% group_by(State) %>% 
  summarise(Total.Population = sum(Total.Population))
Total_population
Total_population %>% 
  select(State, Total.Population) %>% 
  mutate(State = fct_reorder(State, Total.Population)) %>% 
  ggplot(aes(State, Total.Population, fill=State)) +
  geom_text(aes(label=Total.Population), vjust=0.2, hjust=-0.15) +
  geom_col()+
  coord_flip()
 

#Total population aged 60 and above per state
Total_population_60 <- ICU %>% group_by(State) %>% 
  summarise(Population.Aged.60. = sum(Population.Aged.60.))
Total_population_60
Total_population_60 %>% 
  select(State, Population.Aged.60.) %>% 
  mutate(State = fct_reorder(State, Population.Aged.60.)) %>% 
  ggplot(aes(State, Population.Aged.60., fill=State)) +
  geom_text(aes(label=Population.Aged.60.), vjust=0.2, hjust=-0.15) +
  geom_col()+
  coord_flip()



#Total population aged 60 per each ICU bed and above per state
Total_population_60_per_bed <- ICU %>% group_by(State) %>% 
  summarise(Residents.Aged.60..Per.Each.ICU.Bed = sum(Residents.Aged.60..Per.Each.ICU.Bed))
Total_population_60_per_bed
Total_population_60_per_bed %>% 
  select(State, Residents.Aged.60..Per.Each.ICU.Bed) %>% 
  mutate(State = fct_reorder(State, Residents.Aged.60..Per.Each.ICU.Bed)) %>% 
  ggplot(aes(State, Residents.Aged.60..Per.Each.ICU.Bed, fill=State)) +
  geom_text(aes(label=Residents.Aged.60..Per.Each.ICU.Bed), vjust=0.2, hjust=-0.15) +
  geom_col()+
  coord_flip()




################## Data Analysis (County level)


#Total number of counties
Total_no_of_counties <- length(unique(ICU$County))
Total_no_of_counties


#Counties with highest and lowest number of ICU beds
highest_ICU_beds <- ICU[order(ICU$ICU.Beds, decreasing = TRUE), ]
highest_ICU_beds_5 <- highest_ICU_beds[1:5,]
highest_ICU_beds_5

lowest_ICU_beds <- ICU[order(ICU$ICU.Beds, decreasing = FALSE), ]
lowest_ICU_beds_5 <- lowest_ICU_beds[1:5,]
lowest_ICU_beds_5


#Counties with highest and lowest population
highest_population <- ICU[order(ICU$Total.Population, decreasing = TRUE), ]
highest_population <- highest_population[1:5,]
highest_population

lowest_population <- ICU[order(ICU$Total.Population, decreasing = FALSE), ]
lowest_population <- lowest_population[1:5,]
lowest_population


#Counties with highest and lowest population aged 60 and above
highest_population_60 <- ICU[order(ICU$Population.Aged.60., decreasing = TRUE), ]
highest_population_60 <- highest_population_60[1:5,]
highest_population_60

lowest_population_60 <- ICU[order(ICU$Population.Aged.60., decreasing = FALSE), ]
lowest_population_60 <- lowest_population_60[1:5,]
lowest_population_60


#Counties with highest and lowest percent population aged 60 and above relative to total population
percentage_highest_population_above_60 <- ICU[order(ICU$Percent.of.Population.Aged.60., decreasing = TRUE), ]
percentage_highest_population_above_60 <- percentage_highest_population_above_60[1:5,]
percentage_highest_population_above_60

percentage_lowest_population_above_60 <- ICU[order(ICU$Percent.of.Population.Aged.60., decreasing = FALSE), ]
percentage_lowest_population_above_60 <- percentage_lowest_population_above_60[1:5,]
percentage_lowest_population_above_60


#Counties with highest and lowest ratio of population aged 60 compared to total population
highest_ratio_population_above_60 <- ICU[order(ICU$Residents.Aged.60..Per.Each.ICU.Bed, decreasing = TRUE), ]
highest_ratio_population_above_60 <- highest_ratio_population_above_60[1:5,]
highest_ratio_population_above_60

lowest_ratio_population_above_60 <- ICU[order(ICU$Residents.Aged.60..Per.Each.ICU.Bed, decreasing = FALSE), ]
lowest_ratio_population_above_60 <- lowest_ratio_population_above_60[1:5,]
lowest_ratio_population_above_60




################## Data Analysis (Maps)

#install.packages("usmap")
#install.packages("tidyverse")
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization
library(tidyverse)


# Loading county map data
#install.packages("maps")
library(maps)

counties <- map_data("county")
counties %>% sample_n(5) # see five rows of the county map
names(counties)[6]<-paste("County") # changing subregion to county
counties %>% sample_n(5) # see five rows of the county map

#changing region into state
names(counties)[5]<-paste("State") # changing subregion to county
counties %>% sample_n(5) # see five rows of the county map

#Showing five rows of ICU data
ICU %>% sample_n(5)
ICU$County <- tolower(ICU$County) #converting names of county to lower case
ICU %>% sample_n(5)
ICU$State <- tolower(ICU$State) #converting names of state to lower case
ICU %>% sample_n(5)

#combining the map counties data with ICU data 
combined_data <- merge(counties,ICU, by=c("County","State"))
combined_data %>% sample_n(5)

#Removing duplicates depending on County and State
combined_data_sort <- combined_data %>% distinct(County,State, .keep_all = TRUE)
combined_data_sort


#Map plots
#install.packages("mapproj")
#install.packages("ggthemes")
library(ggthemes)
library(mapproj)

#Map plot of number of ICU beds across US counties
p <- ggplot(data = combined_data,
            mapping = aes(x = long, y = lat, fill = ICU.Beds, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", linewidth = 0.05) + coord_equal()

p1 + labs(title ="ICU bed distribution across counties in the US",
  fill = "ICU bed per county") +
  theme_map() +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")

#Map plot of total population aged 60 and above








#create_report
#install.packages("DataExplorer")
#library(DataExplorer)
#DataExplorer::create_report(ICU)


#Subsetting data according to years
HDI_GI_2011 <- subset(HDI_GI, Year=="2012")
unique(HDI_GI$Country)
write.csv(combined_data2, "combined_data2.csv")







