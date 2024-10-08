#Building Product Recommendation Systems

#The dataset using is Online Retail Dataset.
#Understanding how customers engage and interact with the different products
#is crucial for various companies, especially among e-commerce.

#loading the necessary R libraries.
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(coop)
install.packages("lubridate")
install.packages("reshape2")
install.packages('coop')
data <- read.csv("C:/Users/MIIT/Desktop/OnlineRetail.csv")
View(data)

dim(data)

head(data)
tail(data)

unique(data$InvoiceNo)
unique(data$StockCode)
unique(data$Description)
unique(data$Quantity)
unique(data$InvoiceDate)
unique(data$UnitPrice)
unique(data$CustomerID)
unique(data$Country)


#filtering out negative quantities
data <- data %>%
  filter(Quantity > 0)

#changing to date format and extracting date
data$InvoiceDate=as.POSIXct(data$InvoiceDate,
                            format="%m/%d/%Y %H:%M",
                            tz=Sys.timezone())
data$InvoiceDate <-date(data$InvoiceDate)

#exploring overall time series trends in the revenue and 
#the number of orders to understand whether the business 
#is growing or shrinking over time!!!
#number of orders over time
NumInvoices <- data %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(NumOrders=n_distinct(InvoiceNo))

options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(NumInvoices, aes(x=InvoiceDate, y=NumOrders)) +
  geom_line(color="darkgreen") +
  labs(title="Number of Orders over Time") +
  ylab("Number of Orders")+
  xlab("Date")+
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size=18))

#sudden drop in the number of orders in December 2011?
#this is because we only have the data from December 1 to December 9.
#We are going to remove the data from December for our analysis.
#Otherwise, it would be a misrepresentation.
#removing this data from the data set
data <- data %>%
  filter(InvoiceDate < "2011-12-01")

NumInvoices <- data %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(NumOrders=n_distinct(InvoiceNo))

#plotting again
ggplot(NumInvoices, aes(x=InvoiceDate, y=NumOrders)) +
  geom_line(color="darkgreen") +
  labs(title="Number of Orders over Time") +
  ylab("Number of Orders")+
  xlab("Date")+
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size=18))

#number of orders started to increase significantly from
#September 2011 and had their peak in November 2011. 
#That could be because the business was growing or 
#we really need to look at the previous years to tell more.
#Let's look at the monthly sales!
#calculate the sales column
data <- data %>%
  mutate(Sales=Quantity*UnitPrice)

Revenue <- data %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(Sales=sum(Sales))

ggplot(Revenue, aes(x=InvoiceDate, y=Sales)) +
  geom_line(color="darkgreen") +
  labs(title="Revenue over Time") +
  ylab("Sales")+
  xlab("Date")+
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size=18))

#Repeating Customers
#Let's see how many sales are from existing customers 
#for this online retail business 
#aggregating data so that one row represents one purchase order
Invoice <- data %>%
  group_by(InvoiceNo, InvoiceDate) %>%
  summarize(CustomerID=max(CustomerID), Sales=sum(Sales))

#aggregating data into months
InvoiceCustomer <- Invoice %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month"), CustomerID) %>%
  summarise(Count=n_distinct(InvoiceNo), Sales=sum(Sales))

#filter out customer with 1 order and NA CustomerId
RepeatCustomers <-InvoiceCustomer%>%
  filter(!is.na(CustomerID)) %>%
  filter(Count>1)

RepeatCustomers <- RepeatCustomers %>%
  group_by(InvoiceDate) %>%
  summarize(Count=n_distinct(CustomerID), Sales=sum(Sales))

#total number of monthly customers
UniqueCustomers <- data %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(Count=n_distinct(CustomerID))

#find the percentage of monthly revenue that are attributed to
#the repeat customers
RepeatCustomers$Perc <- RepeatCustomers$Sales/Revenue$Sales*100.0

#append unique customers
RepeatCustomers$Total <- UniqueCustomers$Count

#visualize repeat customers data
ggplot(RepeatCustomers) +
  geom_line(aes(x=InvoiceDate, y=Total), stat="identity", color="blue") +
  geom_line(aes(x=InvoiceDate, y=Count), stat="identity", color="red") +
  geom_bar(aes(x=InvoiceDate, y=Perc*20), stat="identity", color="green",
           alpha=0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./20, name="Percentage (%)")) +
  labs(title="Number of Unique vs Repeat & Revenue from Repeat Customers") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size=18))

#number of items sold for each product for each period
PopularProducts <- data %>%
  group_by(InvoiceDate = floor_date(InvoiceDate, "month"), StockCode) %>%
  summarise(Quantity=sum(Quantity))

#let's find top 5 items sold in November 2011 (most recent trends)
Top5 <- PopularProducts %>%
  filter(InvoiceDate=="2011-11-01") %>%
  arrange(desc(Quantity)) %>%
  top_n(5)

#here we take the data from PopularProducts data frame with 
#the stock codes in Top5
Top5monthly <- PopularProducts[
  which(PopularProducts$StockCode %in% Top5$StockCode),]

ggplot(Top5monthly, aes(x=InvoiceDate, y=Quantity, color=StockCode)) +
  geom_line() +
  labs(title="Top 5 Popular Products over Time") +
  ylab("Number of purchases")+
  xlab("Date")+
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size=18))

#Here we see top-5 trending products with 
#the stock codes 22086, 22197, 23084, 84826, and 85099B.

#Collaborative filtering, which we are going to do in this analysis,
#is a method to recommend products based on previous user behaviors
#(viewed pages, purchased products, given ratings). 
#The idea is to find similarities between users and products/content,
#and recommend the most similar products or content. For example, 
#if one user purchased a white t-shirt, blue jeans, and a hat and
#the other user purchased a white t-shirt, blue jeans, and gloves,
#then the first user is more likely to buy gloves and 
#the second - a hat (because of their similarities in purchasing products).

#handling NA values in the CustomerID field
sum(is.na(data$CustomerID))
#133 361 NA in CustomerID

#let's omit NA values
data <- na.omit(data)

#building a customer-item matrix
#reshape our data frame and
#let's encode 0-1 where 1 means that the product was purchased (at least once)
#and 0 means that the product was never purchased by the given customer.

# Reshape the data using dcast with a specified aggregation function

#Running time of below code line takes about (2 mins)
CustomerItemMatrix <- dcast(data, CustomerID ~ StockCode,
                            value.var = "Quantity", fun.aggregate = sum)
# Define the encode function
encode <- function(x) {
  as.integer(x > 0)
}
# Update the code to mutate the columns using a list of lambdas
CustomerItemMatrix <- CustomerItemMatrix %>%
  mutate_at(vars(-CustomerID), list(~ encode(.)))

#User-based vs. Item-based collaborative filtering

#We will explore two approaches to building a product recommendation system:
  
#User-based approach - looking for similarities between users based on
                        #their product purchase history.

#Item-based approach - looking for similarities between products based on 
                      #which items are often bought together with other items.

#Let's start with a user-based approach
View(CustomerItemMatrix)

#compute cosine similarities between users
#==>take approximatley (3-4) min to run following code lines
UserToUser <- cosine(
  as.matrix(
    t(CustomerItemMatxix[, 2:dim(CustomerItemMatxix)[2]])
  )
)
colnames(UserToUser) <- CustomerItemMatxix$CustomerID

View(UserToUser)
#As you can guess, the closer the cosine similarity
#between two customers to 1,
#the more likely those customers buy similar products.
#rank the most similar customers to our customer with ID 12350
Top10Similar <- CustomerItemMatxix$CustomerID[
  order(UserToUser[, "12350"], decreasing = TRUE)[1:11]]

#let's find what the customer A(12350) bought
boughtbyA <- CustomerItemMatxix %>%
  filter(CustomerID == "12350")
boughtbyA <- colnames(CustomerItemMatxix)[which(boughtbyA !=0)]

#let's find what bought the B customer (we pick a customer with 
#ID=17935 from Top10Similar)
boughtbyB <- CustomerItemMatxix %>%
  filter(CustomerID == "17935")
boughtbyB <- colnames(CustomerItemMatxix)[which(boughtbyB !=0)]

#let's find the items that the customer B didn't buy
#so we can recommend these items to buy for B
RecommendToB <-setdiff(boughtbyA, boughtbyB)

#let's find the descriptions of these items
RecommendToBDescription <- unique(
  data[which(data$StockCode %in% RecommendToB),
       c("StockCode", "Description")])

RecommendToBDescription <- RecommendToBDescription[
  match(RecommendToB, RecommendToBDescription$StockCode),]
#here is the list of the items descriptions as a recommendation to B
RecommendToBDescription


#Here we have a list of products that we can recommend to the customer B 
#(with CustomerID=17935).

#There is a disadvantage of this method.
#We only can recommend products based on a customer purchase history.
#So we can not use this method for new customers. To manage this problem, 

#we can use item-based collaborative filtering.

#Now we will find similarities between items as we did before with customers.

#item-to-item similarity matrix
ItemToItemMatrix <-cosine(
  as.matrix(CustomerItemMatxix[, 2:dim(CustomerItemMatxix)[2]]))

View(ItemToItemMatrix)

#Here we see another matrix that shows
#cosine similarities between defferent items

#customer who  bought a product with StockCode 23166. 
#We want to include some products that this customer is 
#most likely to purchase

#find top10 most similar products to the product with StockCode 23166
Top10SimilarItems <- colnames(ItemToItemMatrix)[
  order(ItemToItemMatrix[, "23166"], decreasing = TRUE)[1:11]]

#get descriptions
Top10SimilarItemsDescriptions <- unique(
  data[which(data$StockCode %in% Top10SimilarItems), 
       c("StockCode", "Description")])

Top10SimilarItemsDescriptions <- Top10SimilarItemsDescriptions[
  match(Top10SimilarItems, Top10SimilarItemsDescriptions$StockCode),]

Top10SimilarItemsDescriptions
#The first product here is the product that the target customer just bought.

#The remaining 10 products are the items that are often bought by
#other customers who have bought the first product.

#So we can use these items to recommend to our new customer. 

#By using item-based collaborative filtering algorithm,company can do 
#product recommendations for new and existing customers.
