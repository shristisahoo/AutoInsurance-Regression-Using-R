#------ Automobile Insurance Company -------

#Problem Statement: To predict Customer Life time Value (CLV)[Total Revenue] 
#for an auto insurance company

#Target Variable: Customer Lifetime Value

#Importing Raw Data
AutoInsurance_Data=read.csv("C:/Users/SHRISTI SAHOO/Desktop/analytics/R Prog/Shristi Sahoo - Final R Project IVY/Final R Project IVY/Fn-UseC_-Marketing-Customer-Value-Analysis.csv",
                            na.strings = c(""," ","NA","NULL"),stringsAsFactors = T)
View(AutoInsurance_Data)
#Target Variable is Customer Lifetime Value can be viewed
#Customer Lifetime Value is a continuous column

#Exploring the Dataset

dim(AutoInsurance_Data)
#In this dataset there are all total
#24-Columns and 9134-Rows

###Customer: 
#Qualitative Column consists of Id of customers  
#It is a Garbage Column as unique id doesn't play
#any role in estimating the business and can be rejected.

###State: 
#Categorical Column
#Shows the name of states belonging to the customers

#Automating to Check for the Unique values in the columns
FunctionUniqueData=function(col_input){
  return(length(unique(col_input)))
}

FunctionUniqueData(AutoInsurance_Data$State)

#State of the customer can help in analyzing the need or requirement of auto insurance 
#of that state and can help attracting more customers to the company based on need
#Has 5 unique values can be considered for further analysis

###Customer Lifetime Value: 
#Target Variable - Continuous Column
#Type of Problem : Linear Regression
#Provides the total revenue the company benefits in the relationship 
#or duration with the customer.

###Response: 
#Categorical Column
#Tracks response of Interaction of the customers with the company
#with respect to various company courtesies

FunctionUniqueData(AutoInsurance_Data$Response)
table(AutoInsurance_Data$Response)

#Response by the customers to the company plays an important role, inturn indicating 
#the customer satisfaction with the company, which makes them keep in touch or respond to
#messages or call or e-mails made by the company
#Has 2 unique values, but here Negative Response is higher than the positive response
#can pass the first level of filtration for further analysis to watch if plays any role in
#driving the revenue


###Coverage: 
#Categorical Column
#Gives Different Categories of Schemes provided 
#by the company to customers based on their requirement

FunctionUniqueData(AutoInsurance_Data$Coverage)

#With the help of this column we can draw analysis what the customers prefer majorly
#as a part of insurance schemes for their automobiles.
#Has 3 unique values can be considered for further analysis

###Education:
#Categorical Column
#Provides the Academic qualification of customers 

FunctionUniqueData(AutoInsurance_Data$Education)

#Has 5 unique values or levels and can be considered for further analysis

###Effective To Date:
#Qualitative Column
#Provides the dates when the policies of customers became effective with the company
#Garbage Column as this column won't drive the revenue with the company
#This column can be rejected

###EmploymentStatus:
#Categorical Column
#Tracks the status of employment of customers with the company

FunctionUniqueData(AutoInsurance_Data$EmploymentStatus)

#with the help of this column an analysis can be drawn based on
#active or inactive customers and can be further filtered out
#Has 5 unique values can be considered for further analysis

###Gender: 
#Categorical Column
#Provides the Gender of the Customers listed

FunctionUniqueData(AutoInsurance_Data$Gender)

#Can help in identifying the type of customers more interested in investing 
#in automobile insurance, which can be profitable in choosing more customers in future
#Has 2 unique that is F-Female,M-Male  can be considered for further analysis

###Income: 
#Continuous Column
#Shows the income of listed customers
#This column can help in analysing the customers who can put in more for investment or
#in growth of revenue

###Location Code: 
#Categorical Column
#Provides the Area where the customers are resided in

FunctionUniqueData(AutoInsurance_Data$Location.Code)

#Has 3 Levels:  Rural,Suburban,Urban
#this column in turn can help in identifying the area or location which 
#helps in keeping the company's growth in track and can help in investing more in those
#areas to drive more revenue

###Marital Status: 
#Categorical Column
#Shows the Marital Status of Customers

FunctionUniqueData(AutoInsurance_Data$Marital.Status)

#Has 3 unique values and can be considered for further analysis

###Monthly Premium Auto: 
#Continuous Column
#Provides Month wise Premium paid by the Customers
#This column can prove an important column in maximizing the revenue as 
#premium are the income of the companies also, it is the liability for the customers
#With the help of this companies can grow in their business and if analyzed in 
#accurate manner based on areas can be an added advantage for both company and customer
#For example the odds of claim being made against a teenager in an urban area may be higher
#against a teenage driver in a suburban area, in-turn the greater the risk associated
#the more expensive the insurance policy.


###Months Since Last Claim: 
#Continuous Column
#Gives the count of months from the month the customer has made their last claim
#This can be equally same as the monthly premium column
#as can help in extracting customer with high risk

###Months Since Policy Inception: 
#Continuous Column
#This column give the number of months the insurance policy coverage of the customer began 

###Type of Open Complaints:
#Categorical Column
#Types of Complaints raised by the Customers

FunctionUniqueData(AutoInsurance_Data$Type.of.Open.Complaints)

#Has 6 unique values based on types
#the goal should be in minimizing the customer complaints in-order to give good customer
#satisfaction which will help in them investing in the company more

###Type of Policies:
#Categorical Column
#Types of Policies offered by the company 

FunctionUniqueData(AutoInsurance_Data$Type.of.Policies)

#Has 9 unique values based on type

###Policy Type:
#Categorical Column
#Different types of auto insurance policies

FunctionUniqueData(AutoInsurance_Data$Policy.Type)

#Has 3 levels:
#Corporate = insurance coverage for cars, trucks, vans or other vehicles used in business 
#Personal = covers personal vehicle insurances
#Special = covers insurance policies extended to drivers who are likely to go 
#uninsured because of limited financial resources

###Policy: 
#Categorical Column
#Provides the different levels of various auto insurance policies

FunctionUniqueData(AutoInsurance_Data$Policy)

#Has 9 unique values, L1,L2,L3 for each policy type that is Corporate,Personal,Special

###Renew Offer Type: 
#Categorical Column
#Type of Renewal Offers provided to Customers

FunctionUniqueData(AutoInsurance_Data$Renew.Offer.Type)

#Has 4 unique values

###Sales Channel:
#Categorical Column
#Methods through which the customers puchased the policies

FunctionUniqueData(AutoInsurance_Data$Sales.Channel)

#Has 4 unique values Agent,Branch,Call Center,Web
#Can help in identifying the method through which customers are majorly or most 
#preferring to join the company

###Total Claim Amount:
#Continuous Column
#Gives the total amount of claims made by customers for respective policies for 
#auto coverage 
#Can help in identifying the customers are willing to pay expensive premiums and are high risk

###Vehicle Class: 
#Categorical Column
#Different Categories of vehicles for which insurance is being provided for the customers

FunctionUniqueData(AutoInsurance_Data$Vehicle.Class)

#Has 6 unique values
#can help in analyzing the category of automobile which the customer are most preferring for 
#insurance

###Vehicle Size: 
#Categorical Column
#Various Sizes of Vehicles for which insurance is being provided by the company

FunctionUniqueData(AutoInsurance_Data$Vehicle.Size)

#Has 3 unique values Large,Medsize,Small 

#Removing the Garbage Columns
AutoInsurance_Data$Customer=NULL
AutoInsurance_Data$Effective.To.Date=NULL

head(AutoInsurance_Data)

#Checking if all the categorical column are factor or not if not it needs to be converted 
#into factor

str(AutoInsurance_Data)

#Categorical Columns: State,Response,Coverage,Education,EmploymentStatus,Gender,
#Location Code,Marital Status,Type of Open Complaints,Type of Policies,Policy Type,
#Policy,Renew Offer Type,Sales Channel,Vehicle Class,Vehicle Size

AutoInsurance_Data$Type.of.Open.Complaints=as.factor(AutoInsurance_Data$Type.of.Open.Complaints)
AutoInsurance_Data$Type.of.Policies=as.factor(AutoInsurance_Data$Type.of.Policies)

str(AutoInsurance_Data)

#Identifying the Problem Type

#Target Variable: Customer Lifetime Value
#Type of Problem : Linear Regression

#Checking and Treating Missing Values

colSums(is.na(AutoInsurance_Data))

#No Missing Values 

#Winsorization
#Checking the presence of outliers by creating boxplots and treating the outliers

#Stating all the continuous columns in a vector
cont_cols=c("Customer.Lifetime.Value","Income","Monthly.Premium.Auto",
            "Months.Since.Last.Claim","Months.Since.Policy.Inception","Total.Claim.Amount")

library(RColorBrewer)

#Splitting the plot window into 3 parts
par(mfrow=c(2,3))

for(outlier_cols in cont_cols){
  boxplot(AutoInsurance_Data[,c(outlier_cols)],data = AutoInsurance_Data,main=paste("BoxPlot of :", outlier_cols),
          col=brewer.pal(8,"Paired"),horizontal = T)
}

#Customer.Lifetime.Value , Monthly.Premium.Auto , Total.Claim.Amount has the presence of outliers
#Whereas Income , Months.Since.Last.Claim , Months.Since.Policy.Inception doesn't have any outliers

#For Customer.Lifetime.Value 253 approx as the last value in data
quant_Customer.Lifetime.Value=quantile(AutoInsurance_Data$Customer.Lifetime.Value,c(0.97,0.98,0.99,0.995,0.996,0.997,0.998,0.999))
quant_Customer.Lifetime.Value

outlier_Customer.Lifetime.Value=quantile(AutoInsurance_Data$Customer.Lifetime.Value,0.998)
outlier_Customer.Lifetime.Value

AutoInsurance_Data$Customer.Lifetime.Value=ifelse(AutoInsurance_Data$Customer.Lifetime.Value>outlier_Customer.Lifetime.Value,
                                               outlier_Customer.Lifetime.Value,AutoInsurance_Data$Customer.Lifetime.Value)


#For Monthly.Premium.Auto considering 253 approx as the last value in data
quant_Monthly.Premium.Auto=quantile(AutoInsurance_Data$Monthly.Premium.Auto,c(0.97,0.98,0.99,0.995,0.999))
quant_Monthly.Premium.Auto

outlier_Monthly.Premium.Auto=quantile(AutoInsurance_Data$Monthly.Premium.Auto,0.995)
outlier_Monthly.Premium.Auto

AutoInsurance_Data$Monthly.Premium.Auto=ifelse(AutoInsurance_Data$Monthly.Premium.Auto>outlier_Monthly.Premium.Auto,
                                               outlier_Monthly.Premium.Auto,AutoInsurance_Data$Monthly.Premium.Auto)

#For Total.Claim.Amount considering 2261 approx as the last value in data
quant_Total.Claim.Amount=quantile(AutoInsurance_Data$Total.Claim.Amount,c(0.97,0.98,0.99,0.995,0.996,0.997,0.998,0.999))
quant_Total.Claim.Amount

outlier_Total.Claim.Amount=quantile(AutoInsurance_Data$Total.Claim.Amount,0.999)
outlier_Total.Claim.Amount

AutoInsurance_Data$Total.Claim.Amount=ifelse(AutoInsurance_Data$Total.Claim.Amount>outlier_Total.Claim.Amount,
                                               outlier_Total.Claim.Amount,AutoInsurance_Data$Total.Claim.Amount)
#Checking If the outliers are treated or not

#Splitting the plot window into 2 parts
par(mfrow=c(2,2))

for(outlier_cols in c("Customer.Lifetime.Value","Monthly.Premium.Auto","Total.Claim.Amount")){
  boxplot(AutoInsurance_Data[,c(outlier_cols)],data = AutoInsurance_Data,main=paste("BoxPlot of :", outlier_cols),
          col=brewer.pal(8,"Paired"),horizontal = T)
}

#STEP-9
# Explore each "Potential" predictor for distribution and Quality

colnames(AutoInsurance_Data)

##For Continuous Column

#Central Tendencies
sapply(AutoInsurance_Data[,cont_cols],mean)
sapply(AutoInsurance_Data[,cont_cols],median)

#Measures of location
sapply(AutoInsurance_Data[,cont_cols],quantile)

#Measure of Dispersion
sapply(AutoInsurance_Data[,cont_cols],sd)
sapply(AutoInsurance_Data[,cont_cols],var)
sapply(AutoInsurance_Data[,cont_cols],range)

#Plotting
#For Continuous Columns: Histogram 

#Splitting the plot window into 3 parts
par(mfrow=c(2,3))

for(hist_cols in cont_cols){
  hist(AutoInsurance_Data[,c(hist_cols)],main = paste("Histogram of :", hist_cols),
       col=brewer.pal(8,"Paired"))
}

##For Categorical Columns

#Stating all the categorical columns in a vector
cate_cols=c("State","Response","Coverage","Education","EmploymentStatus","Gender",
            "Location.Code","Marital.Status","Type.of.Open.Complaints","Type.of.Policies",
            "Policy.Type","Policy","Renew.Offer.Type",
            "Sales.Channel","Vehicle.Class","Vehicle.Size")

#Central Tendencies
FunctionMode=function(inpData){
  ModeValue=names(table(inpData)[table(inpData)==max(table(inpData))])
  return(ModeValue)
}

sapply(AutoInsurance_Data[,cate_cols],FunctionMode)

#Plotting
#For Categorical Columns: Barplot

#Splitting the plot window into 8 parts
par(mfrow=c(2,8))

for(bar_cols in cate_cols){
  barplot(table(AutoInsurance_Data[,c(bar_cols)]),main = paste("BarPlot of :", bar_cols),
          col=brewer.pal(8,"Paired"))
}

#Bivariate Analysis
#Visual Relationship between Predictors and Target Variable

#Continuous(Target Variable) vs Continuous(Predictors) -> Scatter Plot

par(mfrow=c(1,1))

plot(AutoInsurance_Data[,cont_cols],col='blue')

#Continuous(Target variable) vs Categorical(Predictors) -> Box Plot

par(mfrow=c(2,8))

for(box_cols in cate_cols){
  boxplot(Customer.Lifetime.Value ~ AutoInsurance_Data[,c(box_cols)],data = AutoInsurance_Data,main=paste("BoxPlot of :", box_cols),
          col=brewer.pal(8,"Paired"))
}

# Strength of Relationship between predictor and target variable

# Continuous Vs Continuous -> Correlation test

CorrCheck=cor(AutoInsurance_Data[,cont_cols],use = "complete.obs")
CorrCheck

CorrCheck[,"Customer.Lifetime.Value"]
# Income = 0.024365661 ; Monthly.Premium.Auto = 0.396261738 ; Months.Since.Last.Claim = 0.011516682 
#Months.Since.Policy.Inception = 0.009418381 ; Total.Claim.Amount = 0.226450915

#As the Correlation Coefficient is very low for all the columns
#So taking the Threshold value of 0.5 doesn't give correlation with a single column
#So Threshold value is being reduced to 0.2 hence will give columns correlated with target variable
 
names(CorrCheck[,'Customer.Lifetime.Value'][abs(CorrCheck[,'Customer.Lifetime.Value'])>0.2])

#Monthly.Premium.Auto,Total.Claim.Amount are considered as good variable for the Model

# Continuous Vs Categorical -> ANOVA test

# H0(Null Hypothesis): Variables are NOT correlated
# Small P-Value(p-value<5%)-> Variables are correlated(H0 is rejected)
# Large P-Value(p-value>5%)-> Variables are NOT correlated (H0 is accepted)

for (aov_cols in cate_cols){
  aov_summary=summary(aov(Customer.Lifetime.Value ~ AutoInsurance_Data[,c(aov_cols)], data = AutoInsurance_Data))
  print(paste("The Anova test with",aov_cols))
  print(aov_summary)
}

#If Probability values > 5% or 0.05 then H0 Accepted thus variable not correlated with target variable

#State,Response,Gender,Location.Code,Policy.Type,Policy,Sales.Channel,Vehicle.Size
#are accepted by the Null Hypothesis and hence are not correlated with the target variable

#If Probability value < 5% or 0.05 then H0 Rejected, variable are correlated with Target Variable

#Coverage,Education,EmploymentStatus,Marital.Status,Type.of.Open.Complaints,
#Type.of.Policies,Renew.Offer.Type,Vehicle.Class are rejected by Null Hypothesis
#Hence are correlated with the target variable  and considered for the model.

#Generating Data for ML
#Getting Data into Standardization Form

InpData=AutoInsurance_Data
TargetVariableName="Customer.Lifetime.Value"
BestPredictorName=c("Coverage","Education","EmploymentStatus","Marital.Status",
                    "Monthly.Premium.Auto","Type.of.Open.Complaints",
                    "Type.of.Policies","Renew.Offer.Type","Total.Claim.Amount","Vehicle.Class")

#Extracting target variable & Predictor variable from data respectively to create a generic DataSet

TargetVariable=InpData[,c(TargetVariableName)]
str(TargetVariable)

PredictorVariable=InpData[,BestPredictorName]
str(PredictorVariable)

#Creating Data for Machine Learning
Data_ML=data.frame(TargetVariable,PredictorVariable)
head(Data_ML)

#Performing Sampling

# Sampling | Splitting data into 70% for training 30% for testing

set.seed(123)
TrainingSample=sample(1:nrow(Data_ML), size=0.7 * nrow(Data_ML))
length(TrainSample)

Data_MLTrain=Data_ML[TrainingSample, ]
Data_MLTest=Data_ML[-TrainingSample, ]

dim(Data_MLTrain)
dim(Data_MLTest)

head(Data_MLTrain)
head(Data_MLTest)

#Creating Predictive models on training data to check the accuracy on test data

# Linear Regression

Model_Reg=lm(TargetVariable~.,data=Data_MLTrain)
summary(Model_Reg)

#Identifying and Remove the insignificant predictors one by one to arrive at the final model
#Variable ares eliminated in such a way, where the probability value is highest and less than 5%
#this process of elimination is done on variable one by one

#Corresponding to Vehicle.Class of Category Luxury SUV has highest p-value
#so eliminating this variable
Model_Reg_2=lm(TargetVariable~Coverage+Education+EmploymentStatus+Marital.Status+
                 Monthly.Premium.Auto+Type.of.Open.Complaints+Type.of.Policies+
                 Renew.Offer.Type+Total.Claim.Amount+I(Vehicle.Class=="Luxury Car")+
                 I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV")+
                 I(Vehicle.Class=="Two-Door Car"),data=Data_MLTrain)
summary(Model_Reg_2)

#Eliminating EmploymentStatus of Category Unemployed 

Model_Reg_3=lm(TargetVariable~Coverage+Education+Marital.Status+Monthly.Premium.Auto+Type.of.Open.Complaints+Type.of.Policies+
                 Renew.Offer.Type+Total.Claim.Amount+I(EmploymentStatus=="Employed")+
                 I(EmploymentStatus=="Medical Leave")+I(EmploymentStatus=="Retired")+
                 I(Vehicle.Class=="Luxury Car")+I(Vehicle.Class=="Sports Car")+
                 I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=Data_MLTrain)
summary(Model_Reg_3)

#Eliminating Vehicle.Class of Category Two-Door Car

Model_Reg_4=lm(TargetVariable~Coverage+Education+Marital.Status+Monthly.Premium.Auto+Type.of.Open.Complaints+Type.of.Policies+
                 Renew.Offer.Type+Total.Claim.Amount+I(EmploymentStatus=="Employed")+
                 I(EmploymentStatus=="Medical Leave")+I(EmploymentStatus=="Retired")+
                 I(Vehicle.Class=="Luxury Car")+I(Vehicle.Class=="Sports Car")+
                 I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_4)

#Eliminating Renew.Offer.Type of Category Offer3 

Model_Reg_5=lm(TargetVariable~Coverage+Education+Marital.Status+Monthly.Premium.Auto+
                 Type.of.Open.Complaints+Type.of.Policies+Total.Claim.Amount+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+
                 I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                 I(EmploymentStatus=="Retired")+I(Vehicle.Class=="Luxury Car")+
                 I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_5)

#Eliminating Education of category College

Model_Reg_6=lm(TargetVariable~Coverage+Marital.Status+Monthly.Premium.Auto+
                 Type.of.Open.Complaints+Type.of.Policies+Total.Claim.Amount+
                 I(Education=="Doctor")+I(Education=="High School or Below")+I(Education=="Master")+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+
                 I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                 I(EmploymentStatus=="Retired")+I(Vehicle.Class=="Luxury Car")+
                 I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_6)

#Eliminating EmploymentStatus of Category Retired

Model_Reg_7=lm(TargetVariable~Coverage+Marital.Status+Monthly.Premium.Auto+
                 Type.of.Open.Complaints+Type.of.Policies+Total.Claim.Amount+
                 I(Education=="Doctor")+I(Education=="High School or Below")+I(Education=="Master")+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+
                 I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                 +I(Vehicle.Class=="Luxury Car")+I(Vehicle.Class=="Sports Car")+
                 I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_7)

#Eliminating Type.of.Open.Complaints of Category 1 

Model_Reg_8=lm(TargetVariable~Coverage+Marital.Status+Monthly.Premium.Auto+
                 Type.of.Policies+Total.Claim.Amount+
                 I(Type.of.Open.Complaints==2)+I(Type.of.Open.Complaints==3)+
                 I(Type.of.Open.Complaints==4)+I(Type.of.Open.Complaints==5)+
                 I(Education=="Doctor")+I(Education=="High School or Below")+I(Education=="Master")+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+
                 I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                 +I(Vehicle.Class=="Luxury Car")+I(Vehicle.Class=="Sports Car")+
                 I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_8)

#Eliminating Marital.Status of Category Married 

Model_Reg_9=lm(TargetVariable~Coverage+Monthly.Premium.Auto+
                 Type.of.Policies+Total.Claim.Amount+
                 I(Marital.Status=="Single")+I(Type.of.Open.Complaints==2)+
                 I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+
                 I(Type.of.Open.Complaints==5)+I(Education=="Doctor")+
                 I(Education=="High School or Below")+I(Education=="Master")+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+
                 I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                 +I(Vehicle.Class=="Luxury Car")+I(Vehicle.Class=="Sports Car")+
                 I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_9)

#Eliminating Renew.Offer.Type of Category Offer4

Model_Reg_10=lm(TargetVariable~Coverage+Monthly.Premium.Auto+
                 Type.of.Policies+Total.Claim.Amount+
                 I(Marital.Status=="Single")+I(Type.of.Open.Complaints==2)+
                 I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+
                 I(Type.of.Open.Complaints==5)+I(Education=="Doctor")+
                 I(Education=="High School or Below")+I(Education=="Master")+
                 I(Renew.Offer.Type=="Offer2")+I(EmploymentStatus=="Employed")+
                  I(EmploymentStatus=="Medical Leave")+I(Vehicle.Class=="Luxury Car")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_10)

#Eliminating Renew.Offer.Type of Category Offer2

Model_Reg_11=lm(TargetVariable~Coverage+Monthly.Premium.Auto+
                  Type.of.Policies+Total.Claim.Amount+
                  I(Marital.Status=="Single")+I(Type.of.Open.Complaints==2)+
                  I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+
                  I(Type.of.Open.Complaints==5)+I(Education=="Doctor")+
                  I(Education=="High School or Below")+I(Education=="Master")+
                  I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                  I(Vehicle.Class=="Luxury Car")+I(Vehicle.Class=="Sports Car")+
                  I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_11)

#Eliminating Coverage of Category Extended 

Model_Reg_12=lm(TargetVariable~Monthly.Premium.Auto+
                  Type.of.Policies+Total.Claim.Amount+
                  I(Coverage=="Premium")+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==2)+I(Type.of.Open.Complaints==3)+
                  I(Type.of.Open.Complaints==4)+I(Type.of.Open.Complaints==5)+
                  I(Education=="Doctor")+I(Education=="High School or Below")+
                  I(Education=="Master")+I(EmploymentStatus=="Employed")+
                  I(EmploymentStatus=="Medical Leave")+I(Vehicle.Class=="Luxury Car")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_12)

#Eliminating Vehicle.Class of Category Luxury Car

Model_Reg_13=lm(TargetVariable~Monthly.Premium.Auto+
                  Type.of.Policies+Total.Claim.Amount+
                  I(Coverage=="Premium")+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==2)+I(Type.of.Open.Complaints==3)+
                  I(Type.of.Open.Complaints==4)+I(Type.of.Open.Complaints==5)+
                  I(Education=="Doctor")+I(Education=="High School or Below")+
                  I(Education=="Master")+I(EmploymentStatus=="Employed")+
                  I(EmploymentStatus=="Medical Leave")+I(Vehicle.Class=="Sports Car")+
                  I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_13)

#Eliminating Type.of.Open.Complaints of Category 2

Model_Reg_14=lm(TargetVariable~Monthly.Premium.Auto+
                  Type.of.Policies+Total.Claim.Amount+
                  I(Coverage=="Premium")+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+
                  I(Type.of.Open.Complaints==5)+I(Education=="Doctor")+
                  I(Education=="High School or Below")+ I(Education=="Master")+
                  I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_14)

#Eliminating Coverage of Category Premium

Model_Reg_15=lm(TargetVariable~Monthly.Premium.Auto+
                  Type.of.Policies+Total.Claim.Amount+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+
                  I(Type.of.Open.Complaints==5)+I(Education=="Doctor")+
                  I(Education=="High School or Below")+ I(Education=="Master")+
                  I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_15)

#Eliminating Total.Claim.Amount 

Model_Reg_16=lm(TargetVariable~Monthly.Premium.Auto+
                  Type.of.Policies+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+
                  I(Type.of.Open.Complaints==5)+I(Education=="Doctor")+
                  I(Education=="High School or Below")+ I(Education=="Master")+
                  I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_16)

#Eliminating Type.of.Open.Complaints of Category 5

Model_Reg_17=lm(TargetVariable~Monthly.Premium.Auto+
                  Type.of.Policies+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+
                  I(Education=="Doctor")+I(Education=="High School or Below")+
                  I(Education=="Master")+I(EmploymentStatus=="Employed")+
                  I(EmploymentStatus=="Medical Leave")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_17)

#Eliminating Type.of.Open.Complaints of Category 3

Model_Reg_18=lm(TargetVariable~Monthly.Premium.Auto+
                  Type.of.Policies+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==4)+
                  I(Education=="Doctor")+I(Education=="High School or Below")+
                  I(Education=="Master")+I(EmploymentStatus=="Employed")+
                  I(EmploymentStatus=="Medical Leave")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_18)

#Eliminating Education of category Doctor

Model_Reg_19=lm(TargetVariable~Monthly.Premium.Auto+
                  Type.of.Policies+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==4)+I(Education=="High School or Below")+
                  I(Education=="Master")+I(EmploymentStatus=="Employed")+
                  I(EmploymentStatus=="Medical Leave")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_19)

#Checking the presence of Multicollinearity
#And if exists corresponding removal of variables with high multicollinearity one by one

#Breaking down categorical variable to checking Multicollinearity

Model_Reg_19=lm(TargetVariable~Monthly.Premium.Auto+
                  I(Type.of.Policies==2)+I(Type.of.Policies==3)+I(Type.of.Policies==4)+
                  I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
                  I(Type.of.Policies==8)+I(Type.of.Policies==9)+I(Marital.Status=="Single")+
                  I(Type.of.Open.Complaints==4)+I(Education=="High School or Below")+
                  I(Education=="Master")+I(EmploymentStatus=="Employed")+
                  I(EmploymentStatus=="Medical Leave")+
                  I(Vehicle.Class=="Sports Car")+I(Vehicle.Class=="SUV"),data=Data_MLTrain)
summary(Model_Reg_19)

#library used for vif-variation inflation factor
library("car")

#If VIF = 1, then variable are not associated
# 1 < VIF < 5, then variables are moderately associated
#   VIF > 5, then highly associated
#So variable having VIF value greater than 5 are to be eliminated from the model

VIF=vif(Model_Reg_19)
data.frame(VIF)

#No variable is highly associated with each other, so no elimination will be done
#Model_Reg_19 will be the final model which will be used to generate predictions on the test data

#Considering the Multiple R-squared:0.6534 & Adjusted R-squared:  0.6525, it can be concluded that 
#Model_Reg_19 is possible and is a good fit

### Various Tests on the model for validation

##Homoskedasticity or Heteroskedasticity

#H0: There exists Homoskedasticity, error in variances are equal
#p-value<5% -> Reject H0
#p-value>5% -> Accept H0

library("lmtest")

bptest(Model_Reg_19)

#As p-value < 2.2e-16 which is very low so H0 is rejected and 
#There exists Heteroskedasticity

##Serial Correlation Test

#H0: There exists no Auto Correlation
#p-value<5% -> Reject H0
#p-value>5% -> Accept H0

#library("lmtest") is used 

dwtest(Model_Reg_19)

#As p-value = 0.0688, which is greater than 5% so H0 is accepted 
#And concluded the absence of auto correlation

##Normality Test

#H0: Error terms are normally distributed
#p-value<5% -> Reject H0
#p-value>5% -> Accept H0

library("nortest")

#Extracting Residual part of the model for testing
resid_Model_Reg_19=Model_Reg_19$residuals

ad.test(resid_Model_Reg_19)

#As p-value < 2.2e-16, which is very low so H0 is rejected 
#And concluded that errors are not normally distributed

### Checking Accuracy of model on Testing data
Data_MLTest$Pred_LM=predict(Model_Reg_19,Data_MLTest)
head(Data_MLTest)

## Calculating the Absolute Percentage Error for each prediction
Data_MLTest$LM_APE=100*abs((Data_MLTest$TargetVariable-Data_MLTest$Pred_LM)/
                             Data_MLTest$TargetVariable)
head(Data_MLTest)

MeanAPE=mean(Data_MLTest$LM_APE)
MedianAPE=median(Data_MLTest$LM_APE)

print(paste("Mean Error Percentage is: ",MeanAPE))
print(paste("Median Error Percentage is: ",MedianAPE))

#Mean Error Percentage is:  25.00%
#Median Error Percentage is:  17.37%

print(paste('Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

#Mean Accuracy of Linear Regression Model is:  74.99%
#Median Accuracy of Linear Regression Model is:  82.62%


