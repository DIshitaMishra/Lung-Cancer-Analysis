# Loading the data set

Cancer_df <- read.csv("C:\\Users\\ishita mishra\\Downloads\\survey lung cancer.csv")

# Structure of data set
str(Cancer_df)
summary(Cancer_df)
describe(Cancer_df)
# According to the structure of the dataset All the columns values are in numeric form except 
#gender and ling cancer column

# Top 5 Values
head(Cancer_df, 5)

# Bottom 5 values
tail(Cancer_df, 5)

# Creating a column of Lung Cancer for special task
Cancer_df$target =  Cancer_df$LUNG_CANCER

# Canging the type of target column
Cancer_df$target[Cancer_df$target== 'NO'] = "1"
Cancer_df$target[Cancer_df$target  == 'YES'] = "2"


# Creating a copy of this dataset for future analysis
library(corrplot)
cancer <- Cancer_df[-16]
cancer <- cancer[-1]

cancer$target<- as.integer(cancer$target)

names(Cancer_df)
names(cancer)

# Changing the values of gender column
Cancer_df$GENDER[Cancer_df$GENDER == 'M'] = 'Male'
Cancer_df$GENDER[Cancer_df$GENDER == 'F'] = 'Female'

# Changing the type of Smoking column
#Cancer_df$SMOKING[Cancer_df$SMOKING == '1'] = 'No'
#Cancer_df$SMOKING[Cancer_df$SMOKING == '2'] = 'Yes'

# Changing the type of YELLOW_FINGERS column
Cancer_df$YELLOW_FINGERS[Cancer_df$YELLOW_FINGERS == '1'] = 'No'
Cancer_df$YELLOW_FINGERS[Cancer_df$YELLOW_FINGERS== '2'] = 'Yes'

# Changing the type of ANXIETY column
Cancer_df$ANXIETY[Cancer_df$ANXIETY == '1'] = 'No'
Cancer_df$ANXIETY[Cancer_df$ANXIETY== '2'] = 'Yes'

# Changing the type of PEER_PRESSURE column
Cancer_df$PEER_PRESSURE[Cancer_df$PEER_PRESSURE == '1'] = 'No'
Cancer_df$PEER_PRESSURE[Cancer_df$PEER_PRESSURE== '2'] = 'Yes'


# Changing the type of CHRONIC.DISEASE column
Cancer_df$CHRONIC.DISEASE[Cancer_df$CHRONIC.DISEASE == '1'] = 'No'
Cancer_df$CHRONIC.DISEASE[Cancer_df$CHRONIC.DISEASE== '2'] = 'Yes'


# Changing the type of FATIGUE column
Cancer_df$FATIGUE[Cancer_df$FATIGUE == '1'] = 'No'
Cancer_df$FATIGUE[Cancer_df$FATIGUE== '2'] = 'Yes'

# Changing the type of ALLERGY  column
Cancer_df$ALLERGY [Cancer_df$ALLERGY == '1'] = 'No'
Cancer_df$ALLERGY [Cancer_df$ALLERGY == '2'] = 'Yes'

# Changing the type of ALLERGY  column
Cancer_df$ALLERGY [Cancer_df$ALLERGY == '1'] = 'No'
Cancer_df$ALLERGY [Cancer_df$ALLERGY == '2'] = 'Yes'

# Changing the type of ALCOHOL.CONSUMING  column
Cancer_df$ALCOHOL.CONSUMING[Cancer_df$ALCOHOL.CONSUMING == '1'] = 'No'
Cancer_df$ALCOHOL.CONSUMING[Cancer_df$ALCOHOL.CONSUMING == '2'] = 'Yes'

# Changing the type of COUGHING  column
Cancer_df$COUGHING[Cancer_df$COUGHING == '1'] = 'No'
Cancer_df$COUGHING[Cancer_df$COUGHING == '2'] = 'Yes'

# Changing the type of SHORTNESS.OF.BREATH  column
Cancer_df$SHORTNESS.OF.BREATH[Cancer_df$SHORTNESS.OF.BREATH == '1'] = 'No'
Cancer_df$SHORTNESS.OF.BREATH[Cancer_df$SHORTNESS.OF.BREATH == '2'] = 'Yes'

# Changing the type of SWALLOWING.DIFFICULTY   column
Cancer_df$SWALLOWING.DIFFICULTY[Cancer_df$SWALLOWING.DIFFICULTY == '1'] = 'No'
Cancer_df$SWALLOWING.DIFFICULTY[Cancer_df$SWALLOWING.DIFFICULTY  == '2'] = 'Yes'

# Changing the type of CHEST.PAIN column
Cancer_df$CHEST.PAIN[Cancer_df$CHEST.PAIN== '1'] = 'No'
Cancer_df$CHEST.PAIN[Cancer_df$CHEST.PAIN  == '2'] = 'Yes'




# EDA tASK:
#------------
# 1. What is the distribution of lung cancer cases in the dataset?
#proportions of Lung cancer
round(prop.table(table(Cancer_df$LUNG_CANCER)),2)

#Loading the Library 
library(ggplot2)

# Plotting a Bar Graph that displays the Proportions of Lung cancer patients
ggplot(Cancer_df,aes(x=LUNG_CANCER))+geom_bar(fill = "pink",color = "Black")+ggtitle("Proportions of Lung cancer patients")

# Creating another data.frame that contains the information related to positive Lung cancer patients 
has_cancer <- Cancer_df[Cancer_df$LUNG_CANCER == "YES",]

# Q How does age vary among individuals with and without lung cancer? 
#=====================================================================

# Create a box plot 
ggplot(Cancer_df, aes(x = LUNG_CANCER, y = AGE)) +
  geom_boxplot() +
  labs(title = "Age Distribution Among Individuals with and without Lung Cancer",
       x = "Lung Cancer Status",
       y = "Age")

# 2.What is the age distribution of patients with lung cancer?
#===========================================================

# Plotting a Histogram graph Which Find the age proportions of the patients
ggplot(has_cancer,aes(x = AGE))+ geom_histogram(fill = "green",color="black")+ ggtitle("Age of the Patients")

# Plotting a histogram to find the age group with lung cancer

library(dplyr)
has_cancer %>%
  ggplot(aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
  labs(title = "Age Distribution of Patients with Lung Cancer", x = "Age", y = "Count")
# By looking at the histogram, i can determine the peak age group is 55 to 80 where the prevalence of lung cancer is highest.

# How many males and females are having Lung cancer in the dataset?
# Plotting a Bar Graph that Check the patients according to their age
ggplot(has_cancer,aes(x = GENDER))+ geom_bar(fill = "yellow",color="black")+ ggtitle("Gender Count of the Patients")

#proportions of gender
round(prop.table(table(has_cancer$GENDER)),2)

# 3. What is the correlation between smoking and the presence of lung cancer?
#==============================================================================
cor(cancer$SMOKING, cancer$target, method = "pearson")

# 4.Are there any strong correlations between any of the predictor variables and the target variable?
#=======================================================================================================

corrplot(cor(cancer), type="upper")

# A robust and statistically significant association exists between Lung Cancer and both Allergy and Alcohol Consumption, indicating a higher risk of Lung Cancer among individuals with allergies and those who consume alcohol. Further research is needed to explore the causal mechanisms underlying these strong relationships.

# Create a stacked bar chart
ggplot(Cancer_df, aes(x = LUNG_CANCER, fill = ALLERGY)) +
  geom_bar(position = "dodge") +
  labs(title = "Relationship Between Lung Cancer and Allergy",
       x = "Lung Cancer Status",
       y = "Proportion") 
# Plotting a Bar Graph to analyze the relation between the Lung Cancer and chest pain 
ggplot(Cancer_df,aes(x = CHEST.PAIN, fill = target))+geom_bar(position = "dodge")+ggtitle("Analysis of Chest Pain")

# Age-wise distribution of males and females who has Lung Cancer and experiencing chest pain
ggplot(has_cancer,aes(x = AGE, fill = GENDER))+geom_bar(position = "dodge")+labs(x = "Age", y = "Cnout", fill= "Gender",title = "Age-wise Distribution of Chest Pain by Gender")+theme_minimal()
#The conclusion that can be drawn from this analysis is that there appears to be an increased incidence of chest pain in the age group of 57 to 65 in the dataset I examined.

# Plotting a Bar Graph to analyze the relation between the Lung Cancer and alcohol consumption 
ggplot(Cancer_df,aes(x = ALCOHOL.CONSUMING, fill =target))+geom_bar(position = "dodge")+ggtitle("Analysis of Alcohol Consumption ")

# Alcohol consumption shows a relatively weaker association with Lung Cancer, suggesting that its relationship to the disease is not strongly pronounced. 

#analyze the relationship between lung cancer, smoking, and anxiety 

ggplot(has_cancer, aes(x = SMOKING, fill = ANXIETY)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Smoking Status",
    y = "Count",
    fill = "Anxiety",
    title = "Relationship Between Positive Lung Cancer, Smoking, and Anxiety "
  ) +
  theme_minimal()

#Lung cancer can occur in individuals with both smoking and anxiety.
#Over 60 cases of lung cancer are found in individuals who neither smoke nor have anxiety (genetics).
#This highlights the complex nature of lung cancer, involving multiple risk factors and the influence of factors beyond smoking and anxiety.


# Allergy exhibits a robust and statistically significant association with Lung Cancer, indicating a heightened risk for Lung Cancer in individuals with allergies. In contrast, Anxiety and Smoking display a weaker, albeit noteworthy, association with Lung Cancer, implying a partial connection to the disease, necessitating further investigation.




