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
library(qdap)
library(pacman)
# # Tỉ lệ số người có anh cha mẹ / con cái - chết sống (done)
# Tỉ lệ số người có anh chị em/ vợ chồng - chết sống (done)
# Tỉ lệ người không lên tàu (done)
# Tỉ lệ sống sót / tuổi của nam (done)
# Tỉ lệ sống sót / tuổi của nữ (done)
# Tìm giá vé min - max của một embarked (done)
# Tìm giá vé min - max của một pclass (done)
# Tỉ lệ người sống sót embarked (done)
# Tỉ lệ người sống sót pclass (done)
# Tách MR, Miss (done)
# Có bao nhiêu nam chết, nam sống, nữ chết, nữ sống (done)
# % nam chết, nam sống, nữ chết, nữ sống (done)
# tuổi trung bình nam, nữ / tuổi min max nam nữ (done)
# Tỉ lệ sống sót theo cabin 
# có bao nhiêu xuồng cứu hộ (done)
# liệt kê người trong cabin (done)
# liệt kê người lên 1 thuyền cứu hộ
# % quê quán
# vẽ biểu đồ cho tất cả
# Ti le con song
# Co bao nhieu nguoi trong moi khoang
# co bao nhieu cap anh chi em / vo chong tren tau
# Co ba me tren tau hay khong
# sibsp mean the number of siblings or spouse of
# parch mean Number of Parents/Children Aboard
# fare : gia ve
# embarked - Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)
#body - Body number (if did not survive and body was recovered)


train_data <- read_xls("E:/school/R/titanic.xls")
head(train_data)


unique(train_data$name)

train_data$age <- round(train_data$age,2)

#Làm sạch dữ liệu (Loại dữ liệu trống)
nrow(train_data)
sum(is.na(train_data))
sum(is.na(train_data$age))
table(train_data$age)


#Làm đầy cho age - những vị trị trống
sum(is.na(train_data$age))
median(train_data$age, na.rm=TRUE)
train_data$age[is.na(train_data$age)] <- median(train_data$age,na.rm = T)

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
table(train_data$survived )
train_data$survived <- factor(train_data$survived, levels = c(0, 1), labels = c("Died", "Survived"))
table(train_data$survived )

########################################################################################
# name- get MR MS v..v
sum(is.na(train_data$name))
# Lấy MR mS v..v
none_na_name <- gsub(".*[,]([^.]+)[.].*", "\\1", train_data$name)
none_na_name

toString(none_na_name )
# Tương quan giữa name_after_get và số lương
train_data$name_after_get <- none_na_name
none_na_name_count <- train_data%>%
  group_by(name_after_get) %>%
  summarise(count = n())

none_na_name_count
# Biểu đồ tương quan giữa name_after_gaet và số lượng
train_data %>% 
  ggplot(aes(x = name_after_get)) +
  geom_bar(width=0.5) +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

# Tương quan giữa name_after_get và số lương - sống/ chết - %
name_after_get_ratio <- train_data %>%
  group_by(name_after_get, survived) %>%
  summarise( count = n(), .groups = "drop_last") %>%
  mutate(percentage = round(count/sum(count)*100))
name_after_get_ratio

# Biểu đồ Tương quan giữa name_after_get và số lương - sống/ chết - %
train_data %>%
  ggplot() +
  geom_bar(aes(x = name_after_get, fill = survived)) +
  geom_text(data = none_na_name_count, 
            aes(x = name_after_get, y = count, label = count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = name_after_get_ratio,
             aes(x = name_after_get, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5)) +
  theme_few() 

########################################################################################
# Có bao nhiêu nam chết, nam sống, nữ chết, nữ sống
########################################################################################

# Tương quan số lượng nam nữ
table(train_data$sex)

# % nam - nữ trên tàu
train_data %>% 
  group_by( sex ) %>% 
  summarise( percent = 100 * n() / nrow( train_data ) )


# Chuyển 0 , 1 thành Died và Survived
table(train_data$survived )
train_data$survived <- factor(train_data$survived, levels = c(0, 1), labels = c("Died", "Survived"))
table(train_data$survived )

# Biểu đồ tương quan chết - sống
train_data %>% 
ggplot(aes(x = survived, fill = survived)) +
  geom_bar(width=0.5) +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()


# % sống sót
percentage_survived<- train_data %>% 
  group_by( survived ) %>% 
  summarise( percent = round(100 * n() / nrow( train_data ) ))
percentage_survived

# Biểu đồ % chết sống
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

# Bảng tương quan giữa nam - nữ - chết - sống
gender_ratio <- train_data %>%
  group_by(sex, survived) %>%
  summarise( count = n(), .groups = "drop_last") %>%
  mutate(percentage = round(count/sum(count)*100))
gender_ratio

##################################################
# .drop nếu muốn %/ tổng số
train_data %>%
  group_by(sex, survived) %>%
  summarise( count = n()) %>%
  mutate(percentage = round(count/sum(count)*100))
##################################################


# Biểu đồ tương quan nam - nữ - chết sống
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

#######################################################################################
#######################################################################################
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




###################################################################################
###################################################################################


# Tỉ lệ sống sót trên tuổi của nam

subset(train_data, is.na(train_data$survived))
age_men_survived_ratio <- subset(train_data, train_data$sex =="male" )
age_men_survived_ratio

# Tương quan giữa tuổi - số lượng

age_men_survived_ratio_calc <- age_men_survived_ratio %>%
  group_by(age) %>%
  summarise(count = n())

age_men_survived_ratio_calc

# Tuổi min nam
min(age_men_survived_ratio$age , na.rm = TRUE) 
# Tuổi max nam
max(age_men_survived_ratio$age , na.rm = TRUE) 
# Tuổi trung bình nam
is.na(train_data$age)
median(age_men_survived_ratio$age , na.rm = TRUE)  



# Tương quan tuổi - tình trạng sống/ chết  - số lương - %
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


# Tuổi min nữ
min(age_female_survived_ratio$age , na.rm = TRUE) 
# Tuổi max nữ
max(age_female_survived_ratio$age , na.rm = TRUE) 
# Tuổi trung bình nữ
is.na(train_data$age)
median(age_female_survived_ratio$age , na.rm = TRUE)  





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

###################################################################################
###################################################################################

#Tỉ lệ người không lên tàu 
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
  geom_bar(aes(x = boat, fill = survived), width = 0.5) +
  geom_text(data = boat_0, 
            aes(x = boat, y = count, label = count), 
            position = position_dodge(width=0.9), 
            vjust=-1, 
            fontface = "bold") +
  geom_label(data = boat_0_ratio, 
             aes(x = boat, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5),
             size = 2) +
  theme_few() 


###################################################################################
###################################################################################

#Tỉ lệ sống sót theo cabin
cabin_count <- train_data %>%
  group_by(cabin) %>%
  summarise(count = n())

cabin_count

cabin_ratio <- train_data %>%
  group_by(cabin, survived) %>%
  summarise( count = n(), .groups = "drop_last" ) %>%
  mutate(percentage = round(count/sum(count)*100))

cabin_ratio
###################################################################################
###################################################################################

# Tỉ lệ số người đi với cha mẹ, con cái ... - chết /sống

parch_count <- train_data %>%
  group_by(parch) %>%
  summarise(count = n())

parch_count

parch_ratio <- train_data %>%
  group_by(parch, survived) %>%
  summarise( count = n(), .groups = "drop_last" ) %>%
  mutate(percentage = round(count/sum(count)*100))

parch_ratio

train_data %>%
  ggplot() +
  geom_bar(aes(x = parch, fill = survived), width = 0.5) +
  geom_text(data = parch_count, 
            aes(x = parch, y = count, label = count), 
            position = position_dodge(width=0.9), 
            vjust=-1, 
            fontface = "bold") +
  geom_label(data = parch_ratio, 
             aes(x = parch, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5),
             size = 2) +
  scale_x_continuous(name= "Parch", breaks = 1*c(0:9)) +
  theme_few() 
##########################################################################################
##########################################################################################

# Tỉ lệ số người có anh chị em/ vợ chồng - chết sống

sibsp_count <- train_data %>%
  group_by(sibsp) %>%
  summarise(count = n())

sibsp_count

sibsp_ratio <- train_data %>%
  group_by(sibsp, survived) %>%
  summarise( count = n(), .groups = "drop_last" ) %>%
  mutate(percentage = round(count/sum(count)*100))

sibsp_ratio

train_data %>%
  ggplot() +
  geom_bar(aes(x = sibsp, fill = survived), width = 0.5) +
  geom_text(data = sibsp_count, 
            aes(x = sibsp, y = count, label = count), 
            position = position_dodge(width=0.9), 
            vjust=-1, 
            fontface = "bold") +
  geom_label(data = sibsp_ratio, 
             aes(x = sibsp, y = count, label = paste0(percentage, "%"), group = survived), 
             position = position_stack(vjust = 0.5),
             size = 2) +
  scale_x_continuous(name= "Sibsp", breaks = 1*c(0:9)) +
  theme_few() 

########################################################################################
#######################################################################################
