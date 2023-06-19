## Predictive Analysis of churn potential for customers
# Aslihan Oztunc #

######################
#Reading the data
customers <- read.csv('BankChurners.csv')

######################
#CLEANING

#Installing and loading the package
#install.packages("dplyr") 
library(dplyr)

#Getting quick info before cleaning
summary(customers)
#The original dataset has 10127 observations of 23 variables.

#Filtering out the unknown data
customers <- subset(customers, Income_Category != 'Unknown')
customers <- subset(customers, Marital_Status != 'Unknown')
customers <- subset(customers, Education_Level != 'Unknown')

#Removing the columns that are unused
customers <- customers[,-22]  #Removing the variable called "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1"
customers <- customers[,-22]  #Removing the variable called "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2"

#Testing the info after cleaning
glimpse(customers)
#The clean dataset has 7081 observations of 21 variables.

str(customers)
names(customers)
head(customers)

##------------------------------------------

# BOX PLOTS #

#Loading the libraries
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)

#Card Category & Total Revolving Balance on the Credit Card
ggplot(data = customers, mapping = aes(x = Card_Category, y = Total_Revolving_Bal)) +
  ggtitle("Card Category & Total Revolving Balance on the Credit Card") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Card Category", y = "Total Revolving Balance on the Credit Card") +
  geom_boxplot()
#Insight: Gold and platinum card categories are normal distribution. However, blue and silver are positive skew.

#Education Level & Total Transaction Count (Last 12 months))
ggplot(data = customers, mapping = aes(x = Education_Level, y = Total_Trans_Ct)) +
  ggtitle("Education Level & Total Transaction Count (Last 12 months)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Education Level", y = "Total Transaction Count (Last 12 months)") +
  geom_boxplot()
#Insight: The highest of total transaction count is at uneducated education level, lowest is doctorate education level. 

#Dependent Count & Change in Transaction Count (Q4 over Q1)
ggplot(data = customers, mapping = aes(x = Marital_Status, y = Total_Amt_Chng_Q4_Q1)) +
  ggtitle("Marital Status & Change in Transaction Count (Q4 over Q1)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Marital Status", y = "Change in Transaction Count (Q4 over Q1)") +
  geom_boxplot(fill="darkturquoise", alpha=0.4)
#Insights: The most transaction count goes to single customers, lowest divorced customers.
#The median is the same value for all of them.

#Income Category & Credit limit
customers$Income_Category <- factor(customers$Income_Category,
                                    levels = c("Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +"))
ggplot(data = customers, mapping = aes(x = Income_Category, y = Credit_Limit)) +
  ggtitle("Credit Limit & Income Category") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Income Category", y = "Credit Limit") +
  geom_boxplot(fill="sandybrown", alpha=0.4)
#Insights: Credit limit and income are linear. The lowest income has more outliers than the others.

#Marital Status & Customer Age
ggplot(data = customers, mapping = aes(x = Marital_Status, y = Customer_Age, main = "vv")) +
  ggtitle("Customer Age & Marital Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Marital Status", y = "Customer Age") +
  geom_boxplot()
#Insights: Divorced and single are normal distribution, married customers are positive skew.

#Education Level & Average Card Utilization Ratio
ggplot(data = customers, mapping = aes(x = Education_Level, y = Avg_Utilization_Ratio)) +
  ggtitle("Education Level & Avg_Utilization_Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Education Level", y = "Average Card Utilization Ratio") +
  geom_boxplot()
#Insights: The lowest card utilization ratio goes to doctorate. The highest is uneducated.
#The highest median is highschool.

#########################################################

### Beginning of Classification

### Understanding Attrited Customer's main characteristics
# Select Attrition Flag and Customer Age
selected_cols <- subset(customers, select = c("Attrition_Flag", "Customer_Age"))
# Use the subset() function to select only the rows where Attrition_Flag is "Attrited Customer"
attrited_customers <- subset(customers, Attrition_Flag == "Attrited Customer")
# Print the count of attrited customers
cat("Number of attrited customers: ", nrow(attrited_customers))

### Attried and Age
# Create a histogram of Age for the Attrited Customers
hist(attrited_customers$Customer_Age, breaks = 10, main = "Histogram of Age for Attrited Customers", xlab = "Age")
# Customers who are in age range 40-55 seem to churn the most
# Calculate the percentage of attrited customers in age range 40-55
age_range_count <- sum(attrited_customers$Customer_Age >= 40 & attrited_customers$Customer_Age <= 55)
total_attrited_customers <- nrow(attrited_customers)
percentage_in_range <- (age_range_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers who are in age range 40-55: ", round(percentage_in_range, 2), "%\n")

### Attried and Gender
# Count the number of customers for each gender
gender_count <- table(attrited_customers$Gender)
# Create a bar plot of Gender for the Attrited Customers
barplot(gender_count, main = "Bar Plot of Gender for Attrited Customers", xlab = "Gender", ylab = "Count")
# No significant Findings

### Attried and Dependent count
# Create a histogram of Dependent count for the Attrited Customers
hist(attrited_customers$Dependent_count, breaks = 10, main = "Histogram of Dependent Count for Attrited Customers", xlab = "Dependent Count")
# Calculate the percentage of attrited customers with 2 or 3 Dependents
dep_count_2_3 <- sum(attrited_customers$Dependent_count %in% c(2, 3))
total_attrited_customers <- nrow(attrited_customers)
percentage_dep_2_3 <- (dep_count_2_3 / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers who have 2 or 3 Dependents: ", round(percentage_dep_2_3, 2), "%\n")

### Attried and Education level
# Count the number of customers for each education level
education_count <- table(attrited_customers$Education_Level)
# Create a bar plot of Education Level for the Attrited Customers
barplot(education_count, main = "Bar Plot of Education Level for Attrited Customers", xlab = "Education Level", ylab = "Count", las=2)
# Calculate the percentage of attrited customers with education levels of Graduate, High School, or Uneducated
edu_count <- sum(attrited_customers$Education_Level %in% c("Graduate", "High School", "Uneducated"))
total_attrited_customers <- nrow(attrited_customers)
percentage_edu <- (edu_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with education levels of Graduate, High School, or Uneducated: ", round(percentage_edu, 2), "%\n")

### Attried and Marital status
# Count the number of customers for each marital status
marital_count <- table(attrited_customers$Marital_Status)
# Create a bar plot of Marital Status for the Attrited Customers
barplot(marital_count, main = "Bar Plot of Marital Status for Attrited Customers", xlab = "Marital Status", ylab = "Count")

### Attried and Income category
# Count the number of customers for each income category
income_count <- table(attrited_customers$Income_Category)
# Create a bar plot of Income Category for the Attrited Customers
barplot(income_count, main = "Bar Plot of Income Category for Attrited Customers", xlab = "Income Category", ylab = "Count")
# Calculate the percentage of attrited customers with an Income_Category less than 40k
income_count <- sum(attrited_customers$Income_Category == "Less than $40K")
total_attrited_customers <- nrow(attrited_customers)
percentage_income <- (income_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with an Income_Category less than 40k: ", round(percentage_income, 2), "%\n")

### Attried and Card category
# Count the number of customers for each card category
card_count <- table(attrited_customers$Card_Category)
# Create a bar plot of Card Category for the Attrited Customers
barplot(card_count, main = "Bar Plot of Card Category for Attrited Customers", xlab = "Card Category", ylab = "Count")
# Calculate the percentage of attrited customers with a Card_Category of Blue
card_count <- sum(attrited_customers$Card_Category == "Blue")
total_attrited_customers <- nrow(attrited_customers)
percentage_card <- (card_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Card_Category of Blue: ", round(percentage_card, 2), "%\n")

### Attried and Months on book
# Create a histogram of Months_on_book for the Attrited Customers
hist(attrited_customers$Months_on_book, main = "Histogram of Months on Book for Attrited Customers", xlab = "Months on Book")
# Calculate the percentage of attrited customers with a Months_on_book value between 35 and 40
months_count <- sum(attrited_customers$Months_on_book >= 35 & attrited_customers$Months_on_book <= 40)
total_attrited_customers <- nrow(attrited_customers)
percentage_months <- (months_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Months_on_book value between 35 and 40: ", round(percentage_months, 2), "%\n")

### Attried and Total Relationship Count
# Create a histogram of Total_Relationship_Count for the Attrited Customers
hist(attrited_customers$Total_Relationship_Count, main = "Histogram of Total Relationship Count for Attrited Customers", xlab = "Total Relationship Count")
# Calculate the percentage of attrited customers with a Total_Relationship_Count of 2 or 3
rel_count <- sum(attrited_customers$Total_Relationship_Count %in% c(2,3))
total_attrited_customers <- nrow(attrited_customers)
percentage_rel <- (rel_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Total_Relationship_Count of 2 or 3: ", round(percentage_rel, 2), "%\n")

### Attried and Months Inactive 12 month
# Create a histogram of Months_Inactive_12_mon for the Attrited Customers
hist(attrited_customers$Months_Inactive_12_mon, main = "Histogram of Months Inactive in Last 12 Months for Attrited Customers", xlab = "Months Inactive in Last 12 Months")
# Calculate the percentage of attrited customers with a Months_Inactive_12_mon value of 2 or 3
inactive_count <- sum(attrited_customers$Months_Inactive_12_mon %in% c(2,3))
total_attrited_customers <- nrow(attrited_customers)
percentage_inactive <- (inactive_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Months_Inactive_12_mon value of 2 or 3: ", round(percentage_inactive, 2), "%\n")

### Attried and Contacts Count 12 months
# Create a histogram of Contacts_Count_12_mon for the Attrited Customers
hist(attrited_customers$Contacts_Count_12_mon, main = "Histogram of Contacts Count in Last 12 Months for Attrited Customers", xlab = "Contacts Count in Last 12 Months")
# Calculate the percentage of attrited customers with a Contacts_Count_12_mon value of 2, 3, or 4
contacts_count <- sum(attrited_customers$Contacts_Count_12_mon %in% c(2,3,4))
total_attrited_customers <- nrow(attrited_customers)
percentage_contacts <- (contacts_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Contacts_Count_12_mon value of 2, 3, or 4: ", round(percentage_contacts, 2), "%\n")

### Attried and Credit limit
# Create a histogram of Credit_Limit for the Attrited Customers
hist(attrited_customers$Credit_Limit, main = "Histogram of Credit Limit for Attrited Customers", xlab = "Credit Limit")
# Calculate the percentage of attrited customers with a Credit_Limit value less than or equal to 5000
credit_limit_count <- sum(attrited_customers$Credit_Limit <= 5000)
total_attrited_customers <- nrow(attrited_customers)
percentage_credit_limit <- (credit_limit_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Credit_Limit value less than or equal to 5000: ", round(percentage_credit_limit, 2), "%\n")
# Calculate the percentage of attrited customers with a Credit_Limit value more than or equal to 35000
credit_limit_count <- sum(attrited_customers$Credit_Limit >= 35000)
total_attrited_customers <- nrow(attrited_customers)
percentage_credit_limit <- (credit_limit_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Credit_Limit value more than or equal to 35000: ", round(percentage_credit_limit, 2), "%\n")

### Attried and Total Revolving Balance
# Create a histogram of Total_Revolving_Bal for the Attrited Customers
hist(attrited_customers$Total_Revolving_Bal, main = "Histogram of Total Revolving Balance for Attrited Customers", xlab = "Total Revolving Balance")
# Calculate the percentage of attrited customers with a Total_Revolving_Bal value less than 250
revolving_bal_count <- sum(attrited_customers$Total_Revolving_Bal < 250)
total_attrited_customers <- nrow(attrited_customers)
percentage_revolving_bal <- (revolving_bal_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Total_Revolving_Bal value less than 250: ", round(percentage_revolving_bal, 2), "%\n")

### Attried and Average Open To Buy
# Create a histogram of Avg_Open_To_Buy for the Attrited Customers
hist(attrited_customers$Avg_Open_To_Buy, main = "Histogram of Average Open to Buy for Attrited Customers", xlab = "Average Open to Buy")
# Calculate the percentage of attrited customers with an Avg_Open_To_Buy value less than 5000
open_to_buy_count <- sum(attrited_customers$Avg_Open_To_Buy < 5000)
total_attrited_customers <- nrow(attrited_customers)
percentage_open_to_buy <- (open_to_buy_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with an Avg_Open_To_Buy value less than 5000: ", round(percentage_open_to_buy, 2), "%\n")

### Attried and Total amount change from Q4 to Q1
# Create a histogram of Total_Amt_Chng_Q4_Q1 for the Attrited Customers
hist(attrited_customers$Total_Amt_Chng_Q4_Q1, main = "Histogram of Total Amount Change Q4 to Q1 for Attrited Customers", xlab = "Total Amount Change Q4 to Q1")

### Attried and Total transaction amount
# Create a histogram of Total_Trans_Amt for the Attrited Customers
hist(attrited_customers$Total_Trans_Amt, main = "Histogram of Total Transaction Amount for Attrited Customers", xlab = "Total Transaction Amount")
# Calculate the percentage of attrited customers with a Total_Trans_Amt value less than 3000
trans_amt_count <- sum(attrited_customers$Total_Trans_Amt < 3000)
total_attrited_customers <- nrow(attrited_customers)
percentage_trans_amt <- (trans_amt_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Total_Trans_Amt value less than 3000: ", round(percentage_trans_amt, 2), "%\n")

### Attried and Total transaction count
# Create a histogram of Total_Trans_Ct for the Attrited Customers
hist(attrited_customers$Total_Trans_Ct, main = "Histogram of Total Transaction Count for Attrited Customers", xlab = "Total Transaction Count")
# Calculate the percentage of attrited customers with a Total_Trans_Ct value between 35 and 50
trans_count_count <- sum(attrited_customers$Total_Trans_Ct >= 35 & attrited_customers$Total_Trans_Ct <= 50)
total_attrited_customers <- nrow(attrited_customers)
percentage_trans_count <- (trans_count_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with a Total_Trans_Ct value between 35 and 50: ", round(percentage_trans_count, 2), "%\n")

### Attried and Total count change from Q4 to Q1
# Create a histogram of Total_Ct_Chng_Q4_Q1 for the Attrited Customers
hist(attrited_customers$Total_Ct_Chng_Q4_Q1, main = "Histogram of Total Count Change Q4 to Q1 for Attrited Customers", xlab = "Total Count Change Q4 to Q1")

### Attried and Average utilization ratio
# Create a histogram of Avg_Utilization_Ratio for the Attrited Customers
hist(attrited_customers$Avg_Utilization_Ratio, main = "Histogram of Average Utilization Ratio for Attrited Customers", xlab = "Average Utilization Ratio")
# Calculate the percentage of attrited customers with an Avg_Utilization_Ratio value less than or equal to 0.1
util_ratio_count <- sum(attrited_customers$Avg_Utilization_Ratio <= 0.1)
total_attrited_customers <- nrow(attrited_customers)
percentage_util_ratio <- (util_ratio_count / total_attrited_customers) * 100
# Print the percentage
cat("Percentage of total attrited customers with an Avg_Utilization_Ratio value less than or equal to 0.1: ", round(percentage_util_ratio, 2), "%\n")

#############################################
# Attrited Customer Characteristic Findings:
#
# Age: Customers between the ages of 40 and 55 make up the majority of attrited customers at 69.54%.
# Dependents: Customers with 2 or 3 dependents account for 55.17% of the total attrited customers.
# Education: Customers with education levels of graduate, high school, or uneducated account for 74.48% of the total attrited customers.
# Income: Customers with an income less than $40K account for 42.5% of the total attrited customers.
# Card Category: Customers with a blue card category account for 92.9% of the total attrited customers.
# Months on Book: Customers who are 35-40 months on book account for 42.68% of the total attrited customers.
# Months Inactive 12 Mon: Customers who are inactive for 2-3 months in the last 12 months account for 82.48% of the total attrited customers.
# Contacts Count 12 Mon: Customers who did 2-4 contacts count in the last 12 months account for 85.8% of the total attrited customers.
# Credit Limit: Customers whose credit limit value is less than or equal to 5000 account for 57.77% of the total attrited customers.
# Total Revolving Balance: Customers whose total revolving balance is less than 250 account for 57.32% of the total attrited customers.
# Average Open To Buy: Customers whose average open to buy is less than 5000 account for 61.1% of the total attrited customers.
# Total Transaction Amount: Customers whose total transaction amount is less than 3000 account for 77.99% of the total attrited customers.
# Total Transaction Count: Customers whose total transaction count is between 35 and 50 account for 53.91% of the total attrited customers.
# Average Utilization Ratio: Customers whose average utilization ratio is less than or equal to 0.1 account for 66.04% of the total attrited customers.
#############################################

library(ggplot2)

### scatter plot of Total Transaction Amount vs Total Transaction Count for Customers between 40 and 55
# Subset the data to include only customers between the ages of 40 and 55
#subset_data <- customer_data[(customer_data$Age >= 40) & (customer_data$Age <= 55),]
# Create the scatter plot
#ggplot(subset_data, aes(x = Total_Trans_Ct, y = Total_Trans_Amt)) +
#  geom_point() +
#  labs(x = "Total Transaction Count", y = "Total Transaction Amount") +
#  ggtitle("Total Transaction Amount vs Total Transaction Count for Customers between 40 and 55")


### scatter plot of customers who's card category is "Blue" vs Months Inactive 12 Mon is 2-3 for attrited customers
# blue_customers <- attrited_customers[attrited_customers$Card_Category == "Blue" & attrited_customers$Months_Inactive_12_mon %in% c(2, 3), ]
# ggplot(blue_customers, aes(x = Months_Inactive_12_mon, y = Total_Trans_Amt)) +
#   geom_point() +
#   labs(x = "Months Inactive 12 Mon", y = "Total Transaction Amount") +
#   ggtitle("Scatter Plot of Blue Card Customers with 2-3 Months Inactive and Total Transaction Amount")




### calculate the percentage of attrited customers who's card category is "Blue" and also education level is graduate, highschool, or uneducated
# Subset attrited customers data to only include customers with a card category of "Blue"
blue_customers <- attrited_customers[attrited_customers$Card_Category == "Blue",]
# Subset resulting data to only include customers with an education level of graduate, highschool, or uneducated
edu_subset <- subset(blue_customers, Education_Level %in% c("Graduate", "High School", "Uneducated"))
# Calculate the percentage of customers in the resulting subset
percent <- nrow(edu_subset) / nrow(attrited_customers) * 100
cat(percent)




### Attrited Customer Subsets ###
### Potential indicators ###
################################################
# 1. Subset attrited customers data to only include customers from age 40-55
att_customers_age_40_55 <- attrited_customers[attrited_customers$Card_Category == "Blue",]
# cat(nrow(att_customers_age_40_55))
# 2. Subset attrited customers data to only include customers with 2 or 3 dependents
att_customers_dependent_2_3 <- attrited_customers[attrited_customers$Dependent_count %in% c(2, 3),]
# cat(nrow(att_customers_dependent_2_3))
# 3. Subset attrited customers data to only include customers with graduate, high school, or uneducated education level
education_levels <- c("Graduate", "High School", "Uneducated")
att_customers_edu_levels <- attrited_customers[attrited_customers$Education_Level %in% education_levels,]
# cat(nrow(att_customers_edu_levels))
# 4. Subset attrited customers data to only include customers with less than $40K income
att_customers_income_40K <- subset(attrited_customers, Income_Category == "Less than $40K")
# cat(nrow(att_customers_income_40K))
# 5. Subset attrited customers data to only include customers with a card category of "Blue"
att_customers_blue_card <- subset(attrited_customers, Customer_Age >= 40 & Customer_Age <= 55)
# cat(nrow(att_customers_blue_card))
# 6. Subset attrited customers data to only include customers who are 35-40 months on book
att_customers_book_35_40 <- attrited_customers[attrited_customers$Months_on_book >= 35 & attrited_customers$Months_on_book <= 40,]
# cat(nrow(att_customers_book_35_40))
# 7. Subset attrited customers data to only include customers who are 2-3 months inactive in the last 12 months
att_customers_inactive_2_3 <- subset(attrited_customers, Months_Inactive_12_mon >= 2 & Months_Inactive_12_mon <= 3)
# cat(nrow(att_customers_inactive_2_3))
# 8. Subset attrited customers data to only include customers who did 2-4 contacts count in the last 12 months
att_customers_contact_2_4 <- attrited_customers[attrited_customers$Contacts_Count_12_mon >= 2 & attrited_customers$Contacts_Count_12_mon <= 4, ]
# cat(nrow(att_customers_contact_2_4))
# 9. Subset attrited customers data to only include customers whose credit limit value is less than or equal to 5000
att_customers_creditlimit_5000 <- attrited_customers[attrited_customers$Credit_Limit <= 5000, ]
# cat(nrow(att_customers_creditlimit_5000))
# 10.Subset attrited customers data to only include customers whose total revolving balance is less than 250
att_customers_revolving_250 <- attrited_customers[attrited_customers$Total_Revolving_Bal < 250, ]
# cat(nrow(att_customers_revolving_250))
# 11.Subset attrited customers data to only include customers whose average open to buy is less than 5000
att_customers_avg_opentobuy_5000 <- attrited_customers[attrited_customers$Avg_Open_To_Buy < 5000, ]
# cat(nrow(att_customers_avg_opentobuy_5000))
# 12.Subset attrited customers data to only include customers whose total transaction amount is less than 3000
attrited_customers_total_trans_amt_3000 <- subset(attrited_customers, Total_Trans_Amt < 3000)
# cat(nrow(att_customers_total_trans_amt_3000))
# 13.Subset attrited customers data to only include customers whose total transaction count is between 35 and 50
att_customers_total_trans_ct_35_50 <- attrited_customers[attrited_customers$Total_Trans_Ct >= 35 & attrited_customers$Total_Trans_Ct <= 50,]
# cat(nrow(att_customers_total_trans_ct_35_50))
# 14.Subset attrited customers data to only include customers whose average utilization ratio is less or equal to 0.1
att_customers_uti_ratio_01 <- subset(attrited_customers, Avg_Utilization_Ratio <= 0.1)
# cat(nrow(att_customers_uti_ratio_01))
#####################################################

### All Customer Subsets ###
# Subset the customers dataset by columns
#####################################################
# 1. Customer Flag Subset
Attrition_Flag_subset <- subset(customers, select = Attrition_Flag)
# 2. Customer Age Subset
Age_subset <- subset(customers, select = Customer_Age)
# 3. Customer Gender Subset
Gender_subset <- subset(customers, select = Gender)
# 4. Customer Dependent Count Subset
Dependent_count_subset <- subset(customers, select = Dependent_count)
# 5. Customer Education Level Subset
Education_Level_subset <- subset(customers, select = Education_Level)
# 6. Customer Marital Status Subset
Marital_Status_subset <- subset(customers, select = Marital_Status)
# 7. Customer Income Category Subset
Income_Category_subset <- subset(customers, select = Income_Category)
# 8. Customer Card Category Subset
Card_Category_subset <- subset(customers, select = Card_Category)
# 9. Customer Months on Book Subset
Months_on_book_subset <- subset(customers, select = Months_on_book)
# 10.Total Relationship Count Subset
Total_Relationship_Count_subset <- subset(customers, select = Total_Relationship_Count)
# 11.Month Inactive in 12 Months Subset
Months_Inactive_12_mon_subset <- subset(customers, select = Months_Inactive_12_mon)
# 12.Contacts Count in 12 Months Subset
Contacts_Count_12_mon_subset <- subset(customers, select = Contacts_Count_12_mon)
# 13.Credit Limit Subset
Credit_Limit_subset <- subset(customers, select = Credit_Limit)
# 14.Total Revolving Balance Subset
Total_Revolving_Bal_subset <- subset(customers, select = Total_Revolving_Bal)
# 15.Average Open to Buy Subset
Avg_Open_To_Buy_subset <- subset(customers, select = Avg_Open_To_Buy)
# 16.Total Amount Change from Q4 to Q1
Total_Amt_Chng_Q4_Q1_subset <- subset(customers, select = Total_Amt_Chng_Q4_Q1)
# 17.Total Transaction Amount Subset
Total_Trans_Amt_subset <- subset(customers, select = Total_Trans_Amt)
# 18.Total Transaction Count Subset
Total_Trans_Ct_subset <- subset(customers, select = Total_Trans_Ct)
# 19.Total Count Change from Q4 to Q1
Total_Ct_Chng_Q4_Q1_subset <- subset(customers, select = Total_Ct_Chng_Q4_Q1)
# 20.Average Utilization Ratio Subset
Avg_Utilization_Ratio_subset <- subset(customers, select = Avg_Utilization_Ratio)
###################################################



### End of Classification
###################################################
###################################################



##########################
##########################
##########################
# Factorizing categorical fields for futher analaysis
#### DATA VIZ ###

#Setting customers to new data frame to not affect the source data
customers1 <- customers


# Update non-numeric variables to be numeric

# Attrition
customers1$Attrition_Flag <- as.numeric(as.factor(customers1$Attrition_Flag))
customers1$Attrition_Flag[customers1$Attrition_Flag == 1] <- 0
customers1$Attrition_Flag[customers1$Attrition_Flag == 2] <- 1
#0 Attrition Customer
#1 Existing customer


# Gender
customers1$Gender <- as.numeric(as.factor(customers1$Gender))
customers1$Gender[customers1$Gender == 2] <- 0
#0 male
#1 female


# Education level
customers1$Education_Level <- as.numeric(as.factor(customers1$Education_Level))

#Updating values of numeric to sort the order from least educated to most educated
#Update level for Uneducated to 0
customers1$Education_Level[customers1$Education_Level == 6] <-0
#Update level for highschool to 11
customers1$Education_Level[customers1$Education_Level == 4] <-11
#updating college to 12
customers1$Education_Level[customers1$Education_Level == 1] <-12
#Graduate already in desired level
#customers1$Education_Level[customers1$Education_Level == 3] <-3
#Update Post Graduate to 14
customers1$Education_Level[customers1$Education_Level == 5] <-14
#Update Doctorate to 15
customers1$Education_Level[customers1$Education_Level == 2] <-15

#Update Highschool down to 1
customers1$Education_Level[customers1$Education_Level == 11] <-1
#Update college down to 2
customers1$Education_Level[customers1$Education_Level == 12] <-2
#Update post grad down to 4
customers1$Education_Level[customers1$Education_Level == 14] <-4
#Update Doctorate to 5
customers1$Education_Level[customers1$Education_Level == 15] <-5

#0 Uneducated
#1 High School
#2 College
#3 Graduate
#4 Post Graduate
#5 Doctorate


# Marital status
customers1$Marital_Status <- as.numeric(as.factor(customers1$Marital_Status))
#1 Divorced
#2 Married
#3 Single


# Income category
customers1$Income_Category <- as.numeric(as.factor(customers1$Income_Category))

customers1$Income_Category[customers1$Income_Category == 5] <-0
customers1$Income_Category[customers1$Income_Category == 1] <-11
customers1$Income_Category[customers1$Income_Category == 2] <-1
customers1$Income_Category[customers1$Income_Category == 3] <-2
customers1$Income_Category[customers1$Income_Category == 4] <-3
customers1$Income_Category[customers1$Income_Category == 11] <-4

#0 Less than $40K
#1 $40K - $60K
#2 $60 - $80K
#3 $80K - $120K
#4 $120K +


# Card category
customers1$Card_Category <- as.numeric(as.factor(customers1$Card_Category))
# Updating Blue from 1 to 0
customers1$Card_Category[customers1$Card_Category == 1] <-0
# Updating Silver from 4 to 1
customers1$Card_Category[customers1$Card_Category == 4] <-1

#0 Blue
#1 Silver
#2 Gold
#3 Platinum


###########################
###########################

#Checking correlation between variables and attrition
rest <- cor(customers1)
round(rest, 1)

# Visualize better with correlation plot
install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot::ggcorrplot(rest, hc.order = TRUE, type = "lower",
                       outline.col = "white", title = "Customers Correllation Plot")



################################
################################
# Logistic regression  #

# Load libraries
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("caret")
library(caret)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Subset with variables that have correlation, as explored in our correlation matrix
customers_subset <- customers1 %>%
  dplyr::select(c("Attrition_Flag", "Total_Revolving_Bal", "Total_Trans_Ct", "Total_Ct_Chng_Q4_Q1"))

# Split the data into training and testing sets, 80 % for the train set, 20 % for our test data.
set.seed(123)
train_index <- sample(nrow(customers_subset), size = round(nrow(customers_subset) * 0.8), replace = FALSE)
train <- customers_subset[train_index, ]
test <- customers_subset[-train_index, ]

# Model the logistic regression with the 3 variables that have a correlation with the outcome variable. Make outcome variable binomial.
model <- glm(Attrition_Flag ~ Total_Revolving_Bal + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, data = train, family = binomial)

# Print the summary of the model
summary(model)


# Output interpretation
#   
# Total_Revolving_Bal: For a one-unit increase in the total revolving balance, the odds of churn increase by 0.00099,
# This means that customers with higher revolving balances are more likely to churn.
# Total_Trans_Ct: For a one-unit increase in the total number of transactions, the odds of churn increase by 0.05176,
# This means that customers with a higher number of transactions are more likely to churn.
# Total_Ct_Chng_Q4_Q1: For a one-unit increase in the change in the total number of transactions between Q4 and Q1,the odds of churn increase by 2.802,
# This means that customers who show a large change in their transaction behavior from Q4 to Q1 are more likely to churn.

# All the coefficients are statistically significant with p-values less than 0.001.

# Make predictions on the testing set
predictions <- predict(model, newdata = test, type = "response")

plot(predictions)

# Convert the predictions to binary values based on a threshold of 0.5. 
# In our Introduction to Data Science class we learned the following code. 
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Visualize the binary predictions
plot(binary_predictions)


### MODEL PERFORMANCE ###

# Calculate the accuracy of the model
accuracy <- sum(binary_predictions == test$Attrition_Flag) / length(test$Attrition_Flag)
accuracy

# [1] 0.8877119
# Accuracy is 89%

# Confusion matrix
# Factorize predictions and test data
binary_predictions1 <- as.factor(binary_predictions)
testfactor <- as.factor(test$Attrition_Flag)

# Confusion matrix
confusionMatrix(binary_predictions1, testfactor)

# Output
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0    1
#            0  100   31
#            1  128 1157
# 
# Accuracy : 0.8877          
# 95% CI : (0.8701, 0.9037)
# No Information Rate : 0.839           
# P-Value [Acc > NIR] : 1.126e-07       
# 
# Kappa : 0.4981          
# 
# Mcnemar's Test P-Value : 2.672e-14       
#                                           
#             Sensitivity : 0.43860         
#             Specificity : 0.97391         
#          Pos Pred Value : 0.76336         
#          Neg Pred Value : 0.90039         
#              Prevalence : 0.16102         
#          Detection Rate : 0.07062         
#    Detection Prevalence : 0.09251         
#       Balanced Accuracy : 0.70625         
#                                           
#        'Positive' Class : 0   

# For a churn analysis, Recall is a more important metric than accuracy because it's preferable to have a model that does not miss any churns 
# but sometimes classify a non-churns as churns, than a model that does not classify non-churns as churns but misses a lot of churns. 
# In other words, we prefer to be incorrect when classifying a non-churning customer than when classifying a churning customer.
# In order to calculate the Recall, we divide the True Positives by the True Positives plus the False Negatives:

1157 / (1157+100)

# The output is:
#   [1] 0.9204455

# So we have a recall of 92%

# Area under the curve
#install.packages("pROC")
library(pROC)
roc <- roc(binary_predictions, test$Attrition_Flag)
roc
plot(roc)

# Output
# Area under the curve: 0.8319

## TRY ANOTHER WAY TO SEE IF THIS MODEL PERFORMS BETTER ##

## RANDOM FOREST ##

#install.packages("randomForest")
library(randomForest)


n <- nrow(customers_subset)  # Number of observations

ntrain <- round(n*0.7)  # 70% for training set
set.seed(315)    # Set seed for reproducible results

tindex <- sample(n, ntrain)   # Create an index

train_attrition <- customers_subset[tindex,]   # Create training set
test_attrition <- customers_subset[-tindex,]   # Create test set

rf <- randomForest(formula=Attrition_Flag~., data=train_attrition, ntree=500, 
                   mtry=2, importance=TRUE)

prediction <- predict(rf, newdata=test_attrition, type="class")


binary_prediction <- ifelse(prediction > 0.5, 1, 0)

binary_prediction1 <- as.factor(binary_prediction)
testfactor1 <- as.factor(test_attrition$Attrition_Flag)

### MODEL PERFORMANCE ###

confusionMatrix(binary_prediction1, testfactor1)

# Confusion Matrix and Statistics
# 
#             Reference
# Prediction    0    1
#           0  169   88
#           1  151 1716
# 
# Accuracy : 0.8875          
# 95% CI : (0.8733, 0.9006)
# No Information Rate : 0.8493          
# P-Value [Acc > NIR] : 2.031e-07       
# 
# Kappa : 0.5216          
# 
# Mcnemar's Test P-Value : 6.060e-05       
#                                           
#             Sensitivity : 0.52812         
#             Specificity : 0.95122         
#          Pos Pred Value : 0.65759         
#          Neg Pred Value : 0.91912         
#              Prevalence : 0.15066         
#          Detection Rate : 0.07957         
#    Detection Prevalence : 0.12100         
#       Balanced Accuracy : 0.73967         
#                                           
#        'Positive' Class : 0               
                                        

roc1 <- roc(binary_prediction, test_attrition$Attrition_Flag)
str(binary_prediction)
str(test_attrition)
roc1

# Output
# Area under the curve: 0.8099

plot(roc1)

# Dot chart of variable importance as measured by the Random Forest. 
# We see that the variable Total_Ct_Chng_Q4_Q1 is not important as it is to the left.
varImpPlot(rf)


