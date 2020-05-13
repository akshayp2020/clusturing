crime = read.csv(choose.files())
attach(crime)
mydata<- crime
mydata1<-scale(mydata[,2:5])

#Hierarchical Clustering Using euclidean distance and centroid linkage 
d<-dist(mydata1,method="euclidean")
fit<- hclust(d,method="centroid")
plot(fit)

groups<-cutree(fit, k=4) # cut tree in 4 clusters
rect.hclust(fit,k=4,border="red")#draw denderogram with red colour
table(groups)
# attached cluster no to test 
clusters=data.frame("crime"=mydata[,1],"cluster"=groups)
View(clusters)
CrimeRateCategory2 <- as.matrix(groups) #Group or cluster numbers
g <- data.frame(crime,CrimeRateCategory2)
g1 <-g[,c(ncol(g),1:(ncol(g)-1))]
View(g1)
aggregate(crime[,-1],by=list(g$CrimeRateCategory),mean)

 #Hierarchical Clustering Using euclidean distance and average linkage 
d1<-dist(mydata1,method="euclidean")
fit<- hclust(d,method="average")
plot(fit)

groups1<-cutree(fit, k=6) # cut tree in 6 clusters
rect.hclust(fit,k=6,border="blue")#draw denderogram with blue colour
table(groups1)
# attached cluster no to test 
clusters=data.frame("crime"=mydata[,1],"cluster"=groups1)
View(clusters)
CrimeRateCategory2 <- as.matrix(groups1) #Group or cluster numbers
g <- data.frame(crime,CrimeRateCategory2)
g1 <-g[,c(ncol(g),1:(ncol(g)-1))]
View(g1)
aggregate(crime[,-1],by=list(g$CrimeRateCategory),mean)



#Hierarchical Clustering using manhattan distance and centroid linkage

n1<-dist(mydata,method = "manhattan")
fit<- hclust(n1,method = "centroid")
plot(fit)
Groups1 <- cutree(fit,k=5)  #cut tree into 5 clusters
rect.hclust(fit,k=5,border ="blue")
table(Groups1)

#attach the cluster numbers to State
clust1= data.frame('State'=mydata2[,1],'Cluster'=Groups1)
View(clust1)



CrimeRateCategory1 <- as.matrix(Groups1) #Group or cluster numbers
f1 <- data.frame(crime_data,CrimeRateCategory1)

f2 <-f1[,c(ncol(f1),1:(ncol(f1)-1))]
View(f2)
aggregate(crime_data[,-1],by=list(f1$CrimeRateCategory),mean)
