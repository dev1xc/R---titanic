library(stringr)
library(readxl)
library(dplyr)
library(tidyverse)
library(extrafont)
library(extrafontdb)
library(tibble) # Data manipulation
library(ggplot2) # Data visualization
library(ggthemes) # Data visualization
library(RColorBrewer)
library(randomForest)
library(caret)
library(psych)

train_data <- read_xls("E:/titanic.xls")
head(train_data)


unique(train_data$name)

train_data$age <- round(train_data$age)

#Làm sạch dữ liệu (Loại dữ liệu trống)
nrow(train_data)
sum(is.na(train_data))
sum(is.na(train_data$age))
table(train_data$age)


#Làm đầy cho age - những vị trị trống
sum(is.na(train_data$age))
median(train_data$age, na.rm=TRUE)
train_data$age[is.na(train_data$age)] <- median(train_data$age,na.rm = T)

round(train_data$age)
train_data[! train_data$age %in% boxplot.stats(train_data$age)$out,]

#Fare
sum(is.na(train_data$fare))
round(median(train_data$fare, na.rm=TRUE))
train_data$fare[is.na(train_data$fare)] <- median(train_data$fare,na.rm = T)

#Sex
sum(is.na(train_data$sex))

#boat
sum(is.na(train_data$boat))
train_data$boat[is.na(train_data$boat)] <- 0

#cabin
sum(is.na(train_data$cabin))
train_data$cabin[is.na(train_data$cabin)] <- 0

#body
sum(is.na(train_data$body))
train_data$body[is.na(train_data$body)] <- 0

sum(is.na(train_data))


#Embarked
table(train_data$embarked)
sum(is.na(train_data$embarked))
na.omit(train_data$embarked)
train_data$embarked[is.na(train_data$embarked)] <- "S"

sum(is.na(train_data))


#Tính toán

# Tổng người nam - nữ
table(train_data$sex)

# % nam - nu tren tàu
train_data %>% 
  group_by( sex ) %>% 
  summarise( percent = 100 * n() / nrow( train_data ) )


#Ti le song sot
Survived <- factor(train_data$survived)
table(train_data$survived )
train_data$survived <- factor(train_data$survived, levels = c(0, 1), labels = c("Died", "Survived"))
Survived <- factor(train_data$survived, levels = c(0, 1), labels = c("Died", "Survived"))
table(Survived )


train_data %>% 
ggplot(aes(x = survived)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()


#% song sot
percentage_survived<- train_data %>% 
  group_by( survived ) %>% 
  summarise( percent = round(100 * n() / nrow( train_data ) ))
percentage_survived

train_data %>% 
  ggplot(aes(x = survived,fill = survived)) +
  geom_bar(width=0.5) +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  geom_label(data = percentage_survived, 
             aes(x = survived, y = percent, label = paste0(percent, "%"), group = survived), 
             position = position_stack(vjust = 5)) +
  theme_classic()





# phan bo gioi tinh boi ti le song sot


gender <- train_data %>%
  group_by(sex) %>%
  summarise(count = n())

gender

gender_ratio <- train_data %>%
  group_by(sex, survived) %>%
  summarise( count = n(), .groups = "drop_last") %>%
  mutate(percentage = round(count/sum(count)*100))

gender_ratio

train_data %>% 
  ggplot(aes(x = sex,fill = survived)) +
  geom_bar(width = 0.4) +
  geom_label(data = gender_ratio, 
             aes(x = sex, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5)) +
  theme_classic()


train_data %>%
  ggplot() +
  geom_bar(aes(x = sex, fill = survived)) +
  geom_text(data = gender, 
            aes(x = sex, y = count, label = count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = gender_ratio, 
             aes(x = sex, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5)) +
  theme_few() 


#########################################################
# Tim max cua 1 embark
subset(train_data, is.na(train_data$fare))
fare_max_C <- subset(train_data, train_data$embarked=="C")
fare_max_C
m_fare <- max(fare_max_C$fare, na.rm=TRUE)
m_fare
view_fare_max_c <- subset(train_data,train_data$fare == m_fare)
head(view_fare_max_c)

# Tim min cua 1 embark
subset(train_data, is.na(train_data$fare))
fare_min_C <- subset(train_data, train_data$embarked=="C")
fare_min_C
min_fare <- min(fare_min_C$fare, na.rm=TRUE)
min_fare
view_fare_min_c <- subset(train_data,train_data$fare == min_fare)
head(view_fare_min_c)   
#########################################################

#########################################################
# Tim max cua 1 pclass
subset(train_data, is.na(train_data$pclass))
fare_max_C_pclass <- subset(train_data, train_data$pclass==1)
fare_max_C_pclass
m_fare_pclass <- max(fare_max_C_pclass$fare, na.rm=TRUE)
m_fare_pclass
view_fare_max_c_pclass <- subset(train_data,train_data$fare == m_fare_pclass)
head(view_fare_max_c_pclass)

# Tim min cua 1 pclass
subset(train_data, is.na(train_data$pclass))
fare_min_C_pclass <- subset(train_data, train_data$pclass==1)
fare_min_C_pclass
min_fare_pclass <- min(fare_min_C_pclass$fare, na.rm=TRUE)
min_fare_pclass
view_fare_min_c_pclass <- subset(train_data,train_data$fare == min_fare_pclass)
head(view_fare_min_c_pclass)   
#########################################################

# Tim max tuoi cua male
subset(train_data, is.na(train_data$pclass))
max_age_male <- subset(train_data, train_data$sex=="male")
max_age_male
max_age_male_calc <- max(max_age_male$age, na.rm=TRUE)
max_age_male_calc
view_max_age_male <- subset(train_data,train_data$age == max_age_male_calc)
head(view_max_age_male)

# Tim min cua male
subset(train_data, is.na(train_data$pclass))
min_age_male <- subset(train_data, train_data$sex=="female")
min_age_male
min_age_male_calc <- min(min_age_male$age, na.rm=TRUE)
min_age_male_calc
view_min_age_male <- subset(train_data,train_data$age == min_age_male_calc)
head(view_min_age_male)

#########################################################

# Ti le nguoi song sot embark

embark <- train_data %>%
  group_by(embarked) %>%
  summarise(count = n())

embark


embark_ratio <- train_data %>%
  group_by(embarked, survived) %>%
  summarise( count = n(), .groups = "drop_last") %>%
  mutate(percentage = round(count/sum(count)*100))

embark_ratio

train_data %>%
  ggplot() +
  geom_bar(aes(x = embarked, fill = survived)) +
  geom_text(data = embark, 
            aes(x = embarked, y = count, label = count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = embark_ratio, 
             aes(x = embarked, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5)) +
  theme_few() 

# ti le nguoi song sot pclass

pclass <- train_data %>%
  group_by(pclass) %>%
  summarise(count = n())

pclass

pclass_ratio <- train_data %>%
  group_by(pclass, survived) %>%
  summarise( count = n(), .groups = "drop_last" ) %>%
  mutate(percentage = round(count/sum(count)*100))

pclass_ratio


train_data %>%
  ggplot() +
  geom_bar(aes(x = pclass, fill = survived)) +
  geom_text(data = pclass, 
            aes(x = pclass, y = count, label = count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = pclass_ratio, 
             aes(x = pclass, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5)) +
  theme_few() 



#Phan bo ti le tuoi


age <- train_data %>%
  group_by(age) %>%
  summarise(count = n())

age







# TI le song sot cua tuoi cua nam

subset(train_data, is.na(train_data$survived))
age_men_survived_ratio <- subset(train_data, train_data$sex =="male" )
age_men_survived_ratio


age_men_survived_ratio_calc <- age_men_survived_ratio %>%
  group_by(age) %>%
  summarise(count = n())

age_men_survived_ratio_calc

age_men_ratio <- age_men_survived_ratio %>%
  group_by(age, survived) %>%
  summarise( count = n(), .groups = "drop" ) %>%
  mutate(percentage = round(count/sum(count)*100))

age_men_ratio

age_men_survived_ratio %>%
  ggplot() +
  geom_bar(aes(x = age, fill = survived)) +
  geom_text(data = age_men_survived_ratio_calc , 
            aes(x = age, y = count, label = count), 
            position = position_dodge(width=1), 
            vjust = -0.25, 
            fontface = "bold") +
  geom_label(data = age_men_ratio, 
             aes(x = age, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5),
             size = 2) +
  ggtitle("") +
  scale_x_continuous(name= "Passenger Age", breaks = 1*c(0:80)) +
  scale_y_continuous(name = "Passenger Count")


# TI le song sot cua tuoi cua nu

subset(train_data, is.na(train_data$survived))
age_female_survived_ratio <- subset(train_data, train_data$sex =="female" )
age_female_survived_ratio


age_female_survived_ratio_calc <- age_female_survived_ratio %>%
  group_by(age) %>%
  summarise(count = n())

age_female_survived_ratio_calc

age_female_ratio <- age_female_survived_ratio %>%
  group_by(age, survived) %>%
  summarise( count = n(), .groups = "drop" ) %>%
  mutate(percentage = round(count/sum(count)*100))

age_female_ratio

age_female_survived_ratio %>%
  ggplot() +
  geom_bar(aes(x = age, fill = survived)) +
  geom_text(data = age_female_survived_ratio_calc , 
            aes(x = age, y = count, label = count), 
            position = position_dodge(width=1), 
            vjust = -0.25, 
            fontface = "bold") +
  geom_label(data = age_female_ratio, 
             aes(x = age, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5),
             size = 2) +
  ggtitle("") +
  scale_x_continuous(name= "Passenger Age", breaks = 1*c(0:80)) +
  scale_y_continuous(name = "Passenger Count")



#Ti le nan nhan khong len tau nam - nu
boat_0 <- train_data %>%
  group_by(boat) %>%
  summarise(count = n())

boat_0

boat_0_ratio <- train_data %>%
  group_by(boat, survived) %>%
  summarise( count = n(), .groups = "drop_last" ) %>%
  mutate(percentage = round(count/sum(count)*100))

boat_0_ratio


train_data %>%
  ggplot() +
  geom_bar(aes(x = boat, fill = survived)) +
  geom_text(data = boat_0, 
            aes(x = boat, y = count, label = count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = boat_0_ratio, 
             aes(x = boat, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5),
             size = 2) +
  theme_few() 

