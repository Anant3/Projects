require(ggplot2)
require(dplyr)
library(reshape2)

sales_data <- read.csv("D:/anant/datasets/SalesData.csv")

#Practice-1 [Comparing sales by region for 2015 and 2016]
q1 <- subset(sales_data, , c(Region, Sales2015, Sales2016))
q1_data <- dplyr::group_by(q1,Region)
q1_data_f <- dplyr::summarise(q1_data,sales_2015 = sum(Sales2015), sales_2016=sum(Sales2016))
colnames(q1_data_f)[colnames(q1_data_f)=="sales_2015"] = "2015"
colnames(q1_data_f)[colnames(q1_data_f)=="sales_2016"] = "2016"

q1_data_f_long <- melt(q1_data_f)

colnames(q1_data_f_long)[colnames(q1_data_f_long)=="variable"] <- "year"
colnames(q1_data_f_long)[colnames(q1_data_f_long)=="value"] = "sale"

graph_q1 <- ggplot(data=q1_data_f_long)
graph_q1 <- graph_q1 + aes(y=sale , x=Region , fill = year)
graph_q1 <- graph_q1 + geom_bar(stat = "identity", color = "Blue" , position = "dodge")
graph_q1



q2 <- subset(sales_data, , c(Region, Sales2016))

#Practice-2:- [Finding contributing factors to the sales of each region in 2016 by pie chart] 
q2 <- subset(sales_data, , c(Region, Sales2016))
q2_data <- dplyr::group_by(q2,Region)
q2_data_f <- dplyr::summarise(q2_data,sales_2016=sum(Sales2016))

graph_q2 <- ggplot(data=q2_data_f)
graph_q2 <- graph_q2 + aes(x="", y=sales_2016, fill=Region)
graph_q2 <- graph_q2 + geom_bar(stat = "identity")+coord_polar("y",start=0)+ylab("")
#graph_q2 <- graph_q2 + labels=as.character(sales_2016)
graph_q2

x <- sum(q2_data_f$sales_2016)

#Practice-3 [Comparing total sales of 2015 and 2016 WRT regions and tiers]
q3 <- subset(sales_data, , c(Region,Tier, Sales2015, Sales2016))
q3_sales_Region2 <- group_by(q3,Region,Tier)
q3_sales2016_Region_Tier <- summarise(q3_sales_Region2,sales_2015 = sum(Sales2015), sales_2016=sum(Sales2016))

colnames(q3_sales2016_Region_Tier)[colnames(q3_sales2016_Region_Tier)=="sales_2015"] = "2015"
colnames(q3_sales2016_Region_Tier)[colnames(q3_sales2016_Region_Tier)=="sales_2016"] = "2016"

q3_long <- melt(q3_sales2016_Region_Tier)

colnames(q3_long)[colnames(q3_long)=="variable"] <- "year"
colnames(q3_long)[colnames(q3_long)=="value"] = "sale"

graph_q3 <- ggplot(data=q3_long)
graph_q3 <- graph_q3 + aes(y=sale , x=Region , fill = year)
graph_q3 <- graph_q3 + geom_bar(stat = "identity", color = "Blue" , position = "dodge")
graph_q3

#Practice-4 :- [Finding which states showed declins in east region]
q4 <- subset(sales_data, Region=="East", c(State,Sales2015, Sales2016))
q4_grouped <- group_by(q4,State)
q4_final <- summarise(q4_grouped, Sales_2015=sum(Sales2015), Sales_2016=sum(Sales2016))

colnames(q4_final)[colnames(q4_final)=="Sales_2015"] = "2015"
colnames(q4_final)[colnames(q4_final)=="Sales_2016"] = "2016"

q4_final_long <- melt(q4_final)

colnames(q4_final_long)[colnames(q4_final_long)=="variable"] <- "year"
colnames(q4_final_long)[colnames(q4_final_long)=="value"] = "sale"

#q4_test <- q4_final
#q4_test$diff <- q4_test$Sales_2016 - q4_test$Sales_2015
#q4_test_sorted <- q4_test[order(q4_test$diff),]
#state_reqd <- q4_test_sorted$State[1]
#View(state_reqd)

graph_q4 <- ggplot(data=q4_final_long)
graph_q4 <- graph_q4 + aes(y=sale , x=State , fill = year)
graph_q4 <- graph_q4 + geom_bar(stat = "identity", color = "Blue" , position = "dodge")
graph_q4

#Practice-5 :- [Finding which division saw decline in units sold]
q5 <- subset(sales_data, Tier =="High", c(Division, Sales2015, Sales2016))
q5_grouped <- group_by(q5,Division)
q5_final <- summarise(q5_grouped, Sales_2015=sum(Sales2015),Sales_2016=sum(Sales2016))

#q5_test <- q5_final
#q5_test$Difference <- q5_test$"2016" - q5_test$"2015"
#q5_test_sorted <- q5_test[order(q5_test$Difference),]
#division_reqd <- q5_test_sorted$Division[1]
#View(division_reqd)

colnames(q5_final)[colnames(q5_final)=="Sales_2015"] = "2015"
colnames(q5_final)[colnames(q5_final)=="Sales_2016"] = "2016"

q5_final_long <- melt(q5_final)

colnames(q5_final_long)[colnames(q5_final_long)=="variable"] <- "year"
colnames(q5_final_long)[colnames(q5_final_long)=="value"] = "sale"

graph_q5 <- ggplot(data=q5_final_long)
graph_q5 <- graph_q5 + aes(y=sale , x=Division , fill = year)
graph_q5 <- graph_q5 + geom_bar(stat = "identity", color = "Blue" , position = "dodge")
graph_q5

#Practice-6 :- [Creating new column for quarters]
q6 <- sales_data
q6$quarters <- ifelse(q6$Month %in% c("Jan", "Feb", "Mar"),"Qtr-1",
                      ifelse(q6$Month %in% c("Apr", "May","Jun"),"Qtr-2",
                             ifelse(q6$Month %in% c("Jul", "Aug", "Sep"), "Qtr-3", "Qtr-4")))

test2 <- subset(q6, , c(Month,quarters))


#Practice-7 :- [comparing quarter wise sales for 2015 and 2016]
q7 <- q6
q7_grouped <- group_by(q7,quarters)
q7_final <- summarise(q7_grouped,Sales_2015 = sum(Sales2015),Sales_2016 = sum(Sales2016))

colnames(q7_final)[colnames(q7_final)=="Sales_2015"] = "2015"
colnames(q7_final)[colnames(q7_final)=="Sales_2016"] = "2016"

q7_final_long <- melt(q7_final)

colnames(q7_final_long)[colnames(q7_final_long)=="variable"] <- "year"
colnames(q7_final_long)[colnames(q7_final_long)=="value"] = "sale"

graph_q7 <- ggplot(data=q7_final_long)
graph_q7 <- graph_q7 + aes(y=sale , x=quarters , fill = year)
graph_q7 <- graph_q7 + geom_bar(stat = "identity", color = "Blue" , position = "dodge")
graph_q7

#Practice-8 :- [Determining composition of Quarter wise sales in 2016 with regards to all the 
#                tiers in pie chart]

q8 <- dplyr::group_by(q6, quarters, Tier)
q8_final <- dplyr::summarise(q8 , Sales = sum(Sales2016))

q8_final_q1 <- subset(q8_final,quarters=="Qtr-1")
q8_final_q2 <- subset(q8_final,quarters=="Qtr-2")
q8_final_q3 <- subset(q8_final,quarters=="Qtr-3")
q8_final_q4 <- subset(q8_final,quarters=="Qtr-4")

graph_q8_q1 <- ggplot(data=q8_final_q1)
graph_q8_q1 <- graph_q8_q1 + aes(x="", y=Sales, fill=Tier)
graph_q8_q1 <- graph_q8_q1 + geom_bar(stat = "identity")+coord_polar("y",start=0)+ylab("")
#graph_q2 <- graph_q2 + labels=as.character(sales_2016)
graph_q8_q1

graph_q8_q2 <- ggplot(data=q8_final_q2)
graph_q8_q2 <- graph_q8_q2 + aes(x="", y=Sales, fill=Tier)
graph_q8_q2 <- graph_q8_q2 + geom_bar(stat = "identity")+coord_polar("y",start=0)+ylab("")
#graph_q2 <- graph_q2 + labels=as.character(sales_2016)
graph_q8_q2

graph_q8_q3 <- ggplot(data=q8_final_q3)
graph_q8_q3 <- graph_q8_q3 + aes(x="", y=Sales, fill=Tier)
graph_q8_q3 <- graph_q8_q3 + geom_bar(stat = "identity")+coord_polar("y",start=0)+ylab("")
#graph_q2 <- graph_q2 + labels=as.character(sales_2016)
graph_q8_q3

graph_q8_q4 <- ggplot(data=q8_final_q1)
graph_q8_q4 <- graph_q8_q4 + aes(x="", y=Sales, fill=Tier)
graph_q8_q4 <- graph_q8_q4 + geom_bar(stat = "identity")+coord_polar("y",start=0)+ylab("")
#graph_q2 <- graph_q2 + labels=as.character(sales_2016)
graph_q8_q4
