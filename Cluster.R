rail=read.csv("D://Study//BA//March-April Classes//PBA-Par-Clustering//data//rail_data.csv")
str(rail)
#Exporting data as comma seprated values

scaled.dat=scale(rail[,-1])

#importing comma seprated so that we cna use read.transactions

scaled.dat=as.data.frame(cbind(rail[,1],scaled.dat))

#defining the data and the number of clusters

set.seed(20)
cluster_output=kmeans(scaled.dat[,-1],5)

#Calculating variance of all the columns in the data frame and summing to find out the minimum variance

wss=(nrow(scaled.dat[,-1])-1)*sum(apply(scaled.dat[,-1],2,var))

#plotting kmeans variance (at least 2 cluster), and max 15(random number)

for (i in 2:15) wss[i]=sum(kmeans(scaled.dat[,-1],centers=i)$withinss)
plot(1:15, wss, type="b")

#at point 4 the data start getting constant

str(cluster_output)
View(cluster_output)

#final cluster

set.seed(20)
cluster_output=kmeans(scaled.dat[,-1], 4)
clusters=cluster_output$cluster

#saving the cluster

final_data=as.data.frame(cbind(scaled.dat,clusters))
View(cluster_output$centers)
View(final_data)
names(final_data)
