MBA=read.csv("D://Study//BA//March-April Classes//Par-Market-Basket-Analysis//data//online_retail.csv")
str(MBA)
summary(MBA)

MBA_sorted=MBA[order(MBA$CustomerID),]
View(MBA_sorted)
library(plyr)
library(dplyr)

itemlist=ddply(MBA_sorted,c("CustomerID"),function(df1)paste(df1$Description,collapse =","))

itemlist$CustomerID=NULL
colnames(itemlist)=c("items")


str(itemlist)

write.csv(itemlist, file ='c:/Users/Shaurya/MBAfinal.csv')

library(arules)
MBA_modified=read.transactions('C://Users/Shaurya/MBAfinal.csv',format = 'basket', sep=",")

rules=apriori(MBA_modified,parameter = list(supp=0.0001, conf=0.8,minlen=2,maxlen=4))
inspect(rules[1:6])
