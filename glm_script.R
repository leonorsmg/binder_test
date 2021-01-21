library(tidyverse)
library(Hmisc)
library(performance)

#read data
crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")

#pre-view data
head(crime)

# seperate city and state columns. 
crime <- separate(crime, col = "City, State", into = c("City", "State"))

#renaming variables
crime <- crime %>%
  rename(House_price = index_nsa) %>%
  rename(Violent_Crimes = "Violent Crimes")

head(crime)

# Ploting the data
crime %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population", 
       y = "Violent Crimes")

# Pearson's calculations
rcorr(crime$Population, crime$Violent_Crimes)

#Filtering crime by population.
crime_filtered <- filter(crime, Population < 2000000)

crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) + 
  geom_point(alpha = .25) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population", 
       y = "Violent Crimes")

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

crime_filtered <- filter(crime_filtered, Year == 2015)

crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes, label = City)) + 
  geom_point() + 
  geom_text(nudge_y = 500, check_overlap = TRUE) + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlim(0, 1800000) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population", 
       y = "Violent Crimes")

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered)

model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)

check_model(model1)

anova(model1, model2) #ANOVA

summary(model2) #Summary stats for model 2

##Challenge - Q3. 

crime_filtered <- filter(crime_filtered, Year == 2015)

crime_filtered %>%
  ggplot(aes(x = Population, y = House_price, label = City)) +
  geom_point() +
  geom_text (nudge_y = 500, check_overlap = TRUE) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "House_price")

rcorr(crime_filtered$Population, crime_filtered$House_price)

model3 <- lm(Population ~ 1, data = crime_filtered)
model4 <- lm(Population ~ House_price, data = crime_filtered)

anova(model3, model4)
