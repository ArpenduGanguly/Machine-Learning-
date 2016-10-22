data=read.csv("Fn-UseC_-Marketing-Customer-Value-Analysis.csv")
data1=data
str(data1)

summary(data1)

names(data1)[names(data1)=="Customer.Lifetime.Value"]="clv"

str(data1)
quantile(data1$clv,c(0,0.5,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

data3=data1[data1$clv <36000,]

nrow(data1)

nrow(data3)

nrow(data1)-nrow(data3)

quantile(data3$clv,c(0,0.5,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

data4=data3[data3$clv <14722, ]

nrow(data4)

nrow(data1)-nrow(data4)

#Missing value checking and treatment

sapply(data4,function(x)sum(is.na(x)))

sapply(data4$clv,function(x)sum(is.na(x)))

#Droping the rendundant variables

data5=data4[,-c(1,2,7)]

str(data5)



#Exporting the data into csv file

write.csv(data5,"data.csv")

#Creating the training and testing data

set.seed(1)

breakingbad=split(data5,sample(1:nrow(data5)>round(nrow(data5)*0.7)))

original.data=breakingbad$"FALSE"

test.data=breakingbad$"TRUE"

nrow(original.data)
nrow(test.data)


#Fitting the model

LinearModel=lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies+	Policy.Type+	Policy+	Renew.Offer.Type+	Sales.Channel+	Total.Claim.Amount+	Vehicle.Class+	Vehicle.Size, data=original.data)
summary(LinearModel)

#Removing the first two insignificant variables: policy and policy types


LinearModel1=lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Sales.Channel+	Total.Claim.Amount+	Vehicle.Class+	Vehicle.Size, data=original.data)

summary(LinearModel1)

#Removing the insignificant variables: Total Claim, Sales Channels, Policy Special, Months Since Policy Inception, Marital  Status, Location, Income

LinearModel2=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Vehicle.Class, data=original.data)

summary(LinearModel2)


#Removing the insignificant classes in dummy variables

LinearModel3=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV")+ I(Vehicle.Size == "Small"), data=original.data)

summary(LinearModel3)

LinearModel4=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)

summary(LinearModel4)

LinearModel5=lm(clv~	Coverage+	I(Education=="College")+I(Education =="High School or Below")+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)

summary(LinearModel5)

##Final Model

FinalModel=lm(clv~	Coverage+	I(Education=="College")+	I(EmploymentStatus=="Medical Leave")+I(EmploymentStatus =="Retired")+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)

summary(FinalModel)


#Checking Multicollinearity in the model

library(car)
vif(FinalModel)

.libPaths()
RInsideinstall.packages("car", type = "source")

fitted(FinalModel)
