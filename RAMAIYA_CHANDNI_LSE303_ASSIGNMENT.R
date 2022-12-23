# Install tidyverse.
install.packages('tidyverse')


# Import tidyverse library.
library(tidyverse) 

# Import the data set (turtle_sales.csv).
turtle_sales <- read.csv(file.choose(), header=TRUE) 

# View the data frame.
View(turtle_sales) 
as_tibble(turtle_sales)

# View a summary of the data frame.
summary(turtle_sales)
#Remove unnecessary columns
dfnew=turtle_sales[,-1]
dfnew1=dfnew[,-3:-5]
#View the dataframe
dfnew1
# Specify X as Product, y as Global_Sales, and turtle_sales as the data source 
# (the x-axis variable is passed first, followed by the y-axis,
#  and then the source of the data is specified).
qplot(Product, Global_Sales, data=dfnew1)
#Create a box plot
qplot(Product, Global_Sales, data=dfnew1,geom ='boxplot')
#Create a histogram
qplot(Global_Sales,bins=30,data=dfnew1)
#Create a plot for North America sales
qplot(Product,NA_Sales,data=dfnew1)
#Create a box plot for North America Sales
qplot(Product,NA_Sales,data=dfnew1,geom='boxplot')
#Create a histogram for North America Sales
qplot(NA_Sales,bins=30,data=dfnew1)
#Create a scatter plot for EU Sales
qplot(Product,EU_Sales,data=dfnew1)
#Create a box plot for EU Sales
qplot(Product,EU_Sales,data=dfnew1,geom='boxplot')
#Create a histogram for EU Sales
qplot(EU_Sales,bins=30,data=dfnew1)

#Calculate the minimum, maximum and mean sales- NA_Sales
min(dfnew1$NA_Sales)  
max(dfnew1$NA_Sales)
mean(dfnew1$NA_Sales)
#Calculate the minimum,maximum and mean sales-EU_Sales
min(dfnew1$EU_Sales)
max(dfnew1$EU_Sales)
mean(dfnew1$EU_Sales)
#Calculate the minimum, maximum and mean sales-Global_Sales
min(dfnew1$Global_Sales)
max(dfnew1$Global_Sales)
mean(dfnew1$Global_Sales)
#View the summary of data frame
summary(dfnew1)
# Use of group by function to identify the impact of sales
Product_Sales = dfnew1%>% group_by(Product) %>%
  summarise(Total_NA_Sales=sum(NA_Sales),Total_EU_Sales=sum(EU_Sales),Total_Global_Sales=sum(Global_Sales),.group='drop')
#View the data frame
view(Product_Sales)
#Explore the data frame
dim(Product_Sales)
Product_Sales_stats=summary(Product_Sales)
#Create a scatter plot for NA_Sales
# Build a plot: Start with data, mapping and geom.
# Set the data source, add mapping elements.
ggplot (data = Product_Sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = Product, y = Total_NA_Sales)) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
            alpha=0.75,size=2.5 )+
  labs(title="North American Sales",x='Product',y="Sales", subtitle = "Product wise sales")+
# Add the line-of-best-fit to the plot.
geom_smooth(method = 'lm')
#Create a histogram for North American Sales
ggplot(Product_Sales,aes(x=Total_NA_Sales))+
  # Add a geom layer to specify the plot type.
   geom_histogram(fill='blue',color='red',)+
   labs(x="Product",y="NA_Sales",title="North American Sales per product")
#Create a boxplot for North American Sales
ggplot(Product_Sales,aes(x=Total_NA_Sales))+
  # Add a geom layer to specify the plot type.
  geom_boxplot()+
  labs(x="Product",y="NA_Sales",title="North American Sales per product")
#Create a scatter plot for EU_Sales
# Build a plot: Start with data, mapping and geom.
# Set the data source, add mapping elements.
ggplot (data = Product_Sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = Product, y = Total_EU_Sales)) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             alpha=0.75,size=2.5 )+
  labs(title="Eu Sales",x='Product',y="Sales", subtitle = "Product wise sales")+
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')
#Create a histogram for European Union Sales
ggplot(Product_Sales,aes(x=Total_EU_Sales))+
  # Add a geom layer to specify the plot type.
  geom_histogram(fill='blue',color='red',)+
  labs(x="Product",y="EU_Sales",title="European Union Sales per product")
#Create a box plot for European Union Sales
ggplot(Product_Sales,aes(x=Total_EU_Sales))+
  # Add a geom layer to specify the plot type.
  geom_boxplot()+
  labs(x="Product",y="EU_Sales",title="European Union Sales per product")
#Create a scatter plot for Global_Sales
ggplot (data = Product_Sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = Product, y = Total_Global_Sales)) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             alpha=0.75,size=2.5 )+
  labs(title="Global Sales",x='Product',y='Global_Sales', subtitle = "Product wise sales")+
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')
#Create a histogram for Global_Sales
ggplot(Product_Sales,aes(x=Total_Global_Sales))+
  # Add a geom layer to specify the plot type.
  geom_histogram(fill='blue',color='red',)+
  labs(x="Product",y="Global_Sales",title="Global Sales per product")
#Create a boxplot for Global_Sales
ggplot(Product_Sales,aes(x=Total_Global_Sales))+
  # Add a geom layer to specify the plot type.
  geom_boxplot()+
  labs(x="Product",y="Global_Sales",title="Global Sales per product")
#Create a qq plot for all sales data (EU Sales)
qqnorm(Product_Sales$Total_EU_Sales)
qqline(Product_Sales$Total_EU_Sales,col="steelblue",lwd=2)
#Create a qq plot for all sales data(NA Sales)
qqnorm(Product_Sales$Total_NA_Sales)
qqline(Product_Sales$Total_NA_Sales,col="red",lwd=2)
#Create a qq plot for all sales data(Global Sales)
qqnorm(Product_Sales$Total_Global_Sales)
qqline(Product_Sales$Total_Global_Sales,col="red",lwd=2)
#Determine the normality of the data set
#Run a Shapiro-Wilk Test:
shapiro.test(Product_Sales$Total_NA_Sales)
shapiro.test(Product_Sales$Total_EU_Sales)
shapiro.test(Product_Sales$Total_Global_Sales)
#Determining the skewness and Kurtosis for each sales data
#Install the moments package and load library
install.packages("moments") 
library(moments)
#Determining the skewness and kurtosis for North American Sales
skewness(Product_Sales$Total_NA_Sales) 
kurtosis(Product_Sales$Total_NA_Sales)
#Determining the skewness and kurtosis for European Union Sales
skewness(Product_Sales$EU_Sales)
kurtosis(Product_Sales$EU_Sales)
#Determining the skewness and kurtosis for Global Sales
skewness(Product_Sales$Total_Global_Sales)
kurtosis(Product_Sales$Total_Global_Sales)
#Determining the correlation between the sales data
cor(Product_Sales$Total_EU_Sales,Product_Sales$Total_NA_Sales)
cor(Product_Sales$Total_EU_Sales,Product_Sales$Total_Global_Sales)
cor(Product_Sales$Total_NA_Sales,Product_Sales$Total_Global_Sales)
#Create a simple linear regression
#Create a model with only one x variable (EU Sales to North American Sales)
model1 <- lm(Total_EU_Sales~Total_NA_Sales,
             data=Product_Sales)
#Plot the model
plot(Product_Sales$Total_EU_Sales,Product_Sales$Total_NA_Sales)
# View more outputs for the model - the full regression table.
summary(model1)
plot(model1$residuals)
abline(coefficients(model1))
#Create a simple linear regression model for EU to Global Sales
model2 <- lm(Total_EU_Sales~Total_Global_Sales,
             data=Product_Sales)
#Plot the model

plot(Product_Sales$Total_EU_Sales,Product_Sales$Total_Global_Sales)
# View more outputs for the model - the full regression table.
summary(model2)
plot(model2$residuals)
abline(coefficients(model2))
#Create a simple linear regression model for North American to Global Sales
model3 <- lm(Total_NA_Sales~Total_Global_Sales,
             data=Product_Sales)
#Plot the model
plot(Product_Sales$Total_NA_Sales,Product_Sales$Total_Global_Sales)
# View more outputs for the model - the full regression table.
summary(model3)
plot(model3$residuals)
abline(coefficients(model3))
#Create a MLR model
#Select only numeric columns from the original data frame
Product_Sales2=subset(Product_Sales, Select=-c(Product))
str(Product_Sales2)
View(Product_Sales2)
summary(Product_Sales2)
#Multiple linear regression model
Sales_Model= lm(Total_Global_Sales~Total_NA_Sales+Total_EU_Sales,data=Product_Sales2)
Sales_Model
summary(Sales_Model)
#Predictions based on given values
#Create a new object with prediction function
Predict_Sales=predict(Sales_Model,newdata = Product_Sales,interval='confidence')
#View the object
View(Predict_Sales)
