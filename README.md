# MH3511 - Data Analysis with Computer Project
*A data analysis project done for the module MH3511 Data Analysis with Computer in Nanyang Technological University AY2020-21 Semester 2*  
*A detailed report can be found in [Report.pdf](https://github.com/nicholasneo78/data-analysis-with-computer/blob/main/Report.pdf) in the same repository*   
  
**Aim**   
Find the Relationship Between Attrition of Credit Card Users and Other Variables   
  
### Abstract
Over the last few years, consumer interest in credit cards appears to be declining, with the return on assets in the credit card business decreasing by around 40% between 2011 and 2018. (Wood, 2020) 
Due to this, the cost of acquiring new users has increased. Therefore, banks are now working on improving the retention rate of their current customers instead, 
as it costs less to maintain an existing customer than to seek out new ones. To accomplish this, banks first try to identify customers who have a high risk of being attrited. 
They then take appropriate measures to lower the risk, and hopefully by doing so, the customer chooses to continue utilizing their credit card services.     

### Introduction
In our project, a dataset containing the credit card attrition of a bank is analysed, with other variables such as average credit card utilisation ratio, 
total transaction amount, customer age, etc. Based on this dataset, we seek to answer the following popular questions around the attrition of credit card customers:   
- Does attrition depend on the customer’s average credit card utilization ratio?  
- Does attrition depend on the total revolving balance of the customer’s credit card?  
- Is the total credit card transaction amount and count dependent on the attrition status?  
- How does the age of the customer affect their attrition status?  
- Does the total number of products held by the customer influence attrition?  
  
This report will cover the data descriptions and analysis using R language. 
For each of our research objectives, we performed statistical analysis and drew conclusions in the most appropriate approach, together with explanations and elaborations.  

### Data Description
The dataset, titled “Credit Card Customers”, is obtained from the online data science community Kaggle. The original data consists of 1 csv data frame, titled “BankChurners.csv”. 
The dataset was originally posted on https://leaps.analyttica.com, an innovative experiential data science platform which is open to the public for study and research.   
   
Before proceeding to data analysis, we first performed a preliminary data cleaning to ensure that:    
- Irrelevant columns are eliminated, e.g. “CLIENTNUM”, etc;   
- Rows with null entries are eliminated;   
- The zero values in variables Total_Revolving_Balance and Avg_Utilisation_Ratio are excluded;   

After all the preparation, 6923 observations (customers) with 18 variables were retained for analysis:   
- Customer_Age : Customer's age in years   
- Gender : Gender of the customer  
- Dependent_count : Number of dependents   
- Education_Level : Educational qualification of the account holder  
- Marital_Status : Marital status of the customer   
- Income_Category : Annual income category of the account holder   
- Card_Category : Type of card   
- Months_on_book : Period of relationship with bank   
- Total_Relationship_Count : Total no. of products held by the customer   
- Months_Inactive_12_mon : No. of months inactive in the last 12 months   
- Contacts Count_12_mon : No. of contacts in the last 12 months   
- Credit_Limit : Credit limit on the credit card   
- Total_Revolving_Bal : Total revolving balance on the credit card  
- Avg_Open_To_Buy : Open to buy credit line (average of last 12 months)  
- Total_Trans_Amt : Total transaction amount (last 12 months)  
- Total_Trans_Ct : Total transaction count (last 12 months)  
- Avg_Utilization_Ratio : Average credit card utilization ratio  
- Attrition_Flag: Attrition status of the customer (attrited or existing)   
  
We then further perform Exploratory Data Analysis and Statistical Analysis on the cleaned data,
as shown in the [Report.pdf]((https://github.com/nicholasneo78/data-analysis-with-computer/blob/main/Report.pdf))
