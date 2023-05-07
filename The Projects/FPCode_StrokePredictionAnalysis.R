mystrokedata <- read.csv("healthcare.csv", header=TRUE, sep=",")
mystrokedata

#Install packages of library that involves in EDA
install.packages("tidyverse")
library(tidyverse)

install.packages("funModeling")
library(funModeling)

install.packages("Hmisc")
library(Hmisc)

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

####################################### DATA CLEANING #################################################

### TREAT N/A Values PROCESS ###
# 1.) Checking NA(missing value), zeros, data type and unique values
# Profiling the data input
df_status(mystrokedata)

#2.) Check the whole data frame for missing values
complete.cases(mystrokedata)

#3.) Sum all complete rows
sum(complete.cases(mystrokedata))

#4.) Create new data without missing values
strokedata2 <- mystrokedata[complete.cases(mystrokedata), ]
strokedata2

df_status(strokedata2)

### TREAT ZEROS (0) VALUES PROCESS ###
# Dataset for Data Cleaning Treat value Zeros(0) 
#1.)  Load dataset Kidney Disease
mykidney <- read.csv("kidneydisease.csv", header=TRUE, sep=",")
mykidney

#2.) Profiling data input
mykidney_status=df_status(mykidney)

#3.)Ordering data by percentage of zeros
arrange(mykidney_status, -p_zeros) %>% select(variable, q_zeros, p_zeros)

#4.) Removing variables with high number of NA/zeros
# Removing variables with 60% of zero values
mykidney_remove=filter(mykidney_status, p_zeros > 60) %>% .$variable
mykidney_remove

#5.) Remove the zeros value in the dataset
newkidney <- select(mykidney, -one_of(mykidney_remove))
newkidney

#6.) Profiling again
df_status(newkidney)


##################################### EXPLORATORY DATA ANALYSIS (EDA) ###################################
###################################### Univariate Analysis ##############################################

#Approach to data (using basic EDA)
basic_eda <- function(strokedata2){
  glimpse(strokedata2)
  freq(strokedata2)
  print(profiling_num(strokedata2))
  plot_num(strokedata2)
  describe(strokedata2)}
basic_eda(strokedata2)

### Categorical Data Analysis ###

#1.) Number of patients gender
mygender <- table(strokedata2$gender)
mygender
barplot(mygender,
        main = "Number of Patients's Gender",
        xlab = "Gender",
        ylab = "Number of Patients",
        col = c("pink","lightblue","grey"),
        border = "black")


#2.) Number of patients got stroke
mystroke <- table(strokedata2$stroke)
mystroke
barplot(mystroke,
        main = "Number of Patients Got Stroke",
        xlab = "Stroke Status",
        ylab = "Number of Patients",
        names.arg = c("No","Yes"),
        col = c("green","red"),
        border = "black")

#3.) Number of patients got heart disease
myheartDisease <- table(strokedata2$heart_disease)
myheartDisease
barplot(myheartDisease,
        main = "Number of Patients Got Heart Disease",
        xlab = "Heart Disease Status",
        ylab = "Number of Patients",
        names.arg = c("No","Yes"),
        col = c("yellow","red"),
        border = "black")

#4.) Number of smoking status of patients
mysmoke <- table(strokedata2$smoking_status)
mysmoke
barplot(mysmoke,
        main = "Smoking Status of Patients",
        xlab = "Smoking Status",
        ylab = "Number of Patients",
        col = c("yellow","green","red","grey"),
        border = "black")


### COntinuous Analysis ###

#1.) Number of Patients's age 
ggplot(strokedata2, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "red", color = "white") +
  labs(title = "Patients's Age Distribution", x = "Age", y = "Number of Patients")
myage <- table(strokedata2$age)
myage


#2.) Number of Patients's bmi
ggplot(strokedata2, aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = "green", color = "white") +
  labs(title = "Patients's BMI Distribution", x = "BMI", y = "Number of Patients")
mybmi <- table(strokedata2$bmi)
mybmi


#3) Number of Patients's on Average glucose level
ggplot(strokedata2, aes(x = avg_glucose_level)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Patients's Average Glucose Level", x = "Average Glucose Level", y = "Number of Patients")


########################################## BIVARIATE DATASET ###########################################
#################################### Continuous Variable VS Continuous Variable ########################

#1.) bmi vs Average Glucose Level

ggplot(strokedata2, aes(x = bmi, y = avg_glucose_level, color = as.factor(stroke))) +
  geom_point() +
  labs(x = "BMI", y = "Average Glucose Level", color = "Stroke") +
  theme_classic() + geom_smooth(method="lm")



#2.) Average glucose level vs Age
ggplot(strokedata2, aes(x = age, y = avg_glucose_level, color = as.factor(stroke))) +
  geom_point() +
  labs(title = "Patients's Age Distribution", x = "Age", y = "Average Glucose Level", color = "Stroke") +
  theme_classic() + geom_smooth(method="lm")

################################### Continuous Variable vs Categorical Variable ########################

#1.) Average Glucose Level vs stroke based on gender
ggplot(strokedata2, aes(x=avg_glucose_level, y=stroke, fill=gender),)+
  geom_boxplot()+
  theme_classic()+
  coord_flip()+
  scale_fill_brewer(palette = "Pastel2")+
  labs(title = "Glucose Level Distribution on Stroke",
       subtitle = "Will the stroke can be influenced by glucose levels? ",
       caption = "* ( 0= No , 1= Yes )" ,
       x="Average Glucose Level (mg/dL)\n",
       y="\nStroke Status" )

########################################## DATA VISUALIZATION ##########################################

########################################### VISUALIZATION ONE (1) #########################################################
##1.) Visualization 1 : Hypertension VS Stroke

# Print unique values and value counts of hypertension column
cat("Unique Values\n", unique(strokedata2$hypertension), "\n")
cat("Value Counts\n", table(strokedata2$hypertension), "\n")

# Create factor plot of hypertension and with respect to stroke
strokedata2$stroke <- as.factor(strokedata2$stroke)
strokedata2$hypertension <- as.factor(strokedata2$hypertension)
class(strokedata2$hypertension)
class(strokedata2$stroke)

#1.) Basic view on how Stroke and Hypertension Correlate
ggplot(strokedata2, aes(x = hypertension, fill = stroke)) +
  geom_bar(position = "dodge") +
  labs(title = "Hypertension Distribution on Stroke",
       x = "\nHypertension Status",
       caption = "*(0= No , 1= Yes)",
       subtitle = "Will the stroke can be influenced by hypertension ? ",
       y = "Number of Patients\n") + 
  theme(legend.title = element_text(color = "black", size = 12, face = "italic"),
        legend.text = element_text(color = "black", size = 10, face = "bold"),
        legend.position = "right")  # Custom legend appearance

#2.) Detailed view on how stroke can be affected with some category details
ggplot(strokedata2, aes(x = hypertension, fill = stroke)) +
  geom_bar(position = "dodge") +
  labs(title = "Hypertension Distribution on Stroke",
       x = "\nHypertension Status",
       caption = "*(0= No , 1= Yes)  /  *(Govt_job=  Government job)",
       subtitle = "Details on patients gender and work type that may be linked to a higher chances of stroke with respect to heart disease. ",
       y = "Number of Patients\n") + facet_grid(gender~work_type, scales="free") +
  theme(legend.title = element_text(color = "black", size = 12, face = "italic"),
        legend.text = element_text(color = "black", size = 10, face = "bold"),
        legend.position = "right")  # Custom legend appearance


############################################# vISUALIZATION TWO (2) ####################################################
## Visualization 2 : heart disease vs Stroke


# Print unique values and value counts of smoking_status column
cat("Unique Values\n", unique(strokedata2$heart_disease), "\n")
cat("Value Counts\n", table(strokedata2$heart_disease), "\n")

# Create a count plot of smoking_status with respect to stroke
strokedata2$stroke <- as.factor(strokedata2$stroke)
strokedata2$heart_disease <- as.factor(strokedata2$heart_disease)
class(strokedata2$stroke)
class(strokedata2$heart_disease)

#1.) Basic View
ggplot(strokedata2, aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Disease Distribution on Stroke",
       x = "\nHeart Disease Status",
       caption = "*(0= No , 1= Yes)",
       subtitle = "If the patient has heart disease, will it have an impact on the stroke?",
       y = "Number of Patients\n") + 
  theme(legend.title = element_text(color = "black", size = 12, face = "italic"),
        legend.text = element_text(color = "black", size = 10, face = "bold"),
        legend.position = "right")  # Custom legend appearance

#2.) details view
ggplot(strokedata2, aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Disease Distribution on Stroke",
       x = "\nHeart Disease Status",
       caption = "*(0= No , 1= Yes)  /  *(Govt_job=  Government job)",
       subtitle = "Details on patients gender and work type that may be linked to a higher chances of stroke with respect to heart disease. ",
       y = "Number of Patients\n") + facet_grid(work_type~gender, scales="free") +
  theme(legend.title = element_text(color = "black", size = 12, face = "italic"),
        legend.text = element_text(color = "black", size = 10, face = "bold"),
        legend.position = "right")  # Custom legend appearance


############################################## VISUALIZATION THREE (3) ##################################################
## Visualization 3 : Smoking Status Vs Stroke

# Print unique values and value counts of smoking_status column
cat("Unique Values\n", unique(strokedata2$smoking_status), "\n")
cat("Value Counts\n", table(strokedata2$smoking_status), "\n")

# Create a count plot of smoking_status with respect to stroke
strokedata2$stroke <- as.factor(strokedata2$stroke)
class(strokedata2$stroke)

#1.) Basic View
ggplot(strokedata2, aes(x = smoking_status, fill = stroke)) +
  geom_bar(position = "dodge") +
  labs(title = "Smoking Status Distribution on Stroke",
       x = "\nSmoking Status",
       caption = "*(0= No , 1= Yes)  /  *(Govt_job=  Government job)",
       subtitle = "Did the patient's smoking status have an impact on the stroke? ",
       y = "Number of Patients\n") + 
  theme(legend.title = element_text(color = "black", size = 12, face = "italic"),
        legend.text = element_text(color = "black", size = 10, face = "bold"),
        legend.position = "right")  # Custom legend appearance

#2.) details view
ggplot(strokedata2, aes(x = smoking_status, fill = stroke)) +
  geom_bar(position = "dodge") +
  labs(title = "Smoking Status Distribution on Stroke",
       x = "\nSmoking Status",
       caption = "*(0= No , 1= Yes)  /  *(Govt_job=  Government job)",
       subtitle = "Details on patients gender and work type that may be linked to a higher chances of stroke with respect to heart disease. ",
       y = "Number of Patients\n") + facet_grid(work_type~gender, scales="free") +
  theme(legend.title = element_text(color = "black", size = 12, face = "italic"),
        legend.text = element_text(color = "black", size = 10, face = "bold"),
        legend.position = "right")  # Custom legend appearance



########################################### VISUALIZATION FOUR (4) #####################################################

#4.) Visualization 4 : Average Glucose level vs Stroke

ggplot(strokedata2, aes(x=avg_glucose_level, y=stroke, fill=gender),)+
  geom_boxplot()+
  theme_classic()+
  coord_flip()+
  scale_fill_brewer(palette = "Pastel2")+
  labs(title = "Glucose Level Distribution on Stroke",
       subtitle = "Will the stroke can be influenced by glucose levels? ",
       caption = "* ( 0= No , 1= Yes )" ,
       x="Average Glucose Level (mg/dL)\n",
       y="\nStroke Status" )




