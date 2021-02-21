#Great Bareer Reef Data Question 1

the.data  <-as.matrix(read.csv("GBRData.csv",  header  =  TRUE,  sep  = ","))
my.data <- the.data [sample(1:556,250),c(1:4)]
write.table(my.data,"Ashutosh-217669865-GBRMyData.txt")
View(my.data)
#Q1.1 for drawing histograms
hist(my.data[,1],xlim = c(0,40),col="green",main="Histogram for HeronIsland-Salinity",xlab="Salinity Values")
hist(my.data[,2],xlim = c(0,30),col="green",main="Histogram for HeronIsland-Water Temperature",xlab = "Temperature")

boxplot(my.data[,2],my.data[,4],main="Boxplot of HeronIsland-Water
Temperature' and the 'LadyMusgrave-Water Temperature",xlab="Different Water Temperatures",names=c("HeronIsland-Water
Temperature","LadyMusgrave-Water Temperature"),col=c("green","#0099CC"))

summary(my.data[,2])
summary(my.data[,4])

e<-cbind(my.data[,2],my.data[,3])
plot(my.data[1:150,2],my.data[1:150,3],xlab = "HeronIsland
-Water Temperature", ylab ="LadyMusgrave-
Water Temperature",col="red")

lm(e[,1]~e[,2])#linear model fitting
abline(lm(e[,2]~e[,1]),col="blue")


correlation <- cor(e[,2],e[,1])
print(paste("Correlation Co-efficient = ",correlation))
coeffDet <- correlation^2
print(paste("Co-efficient of determination = ",coeffDet))



#Clustering question 6

zz<-read.table("SITEdata2019.txt") 
zz<-as.matrix(zz) 
zz
plot(zz[,1],zz[,2])

#K means
library(stats)
km <- kmeans(zz, centers=5, nstart=5)
plot(zz, col=km$cluster)


km <- kmeans(zz, centers=15, nstart=5)
plot(zz, col=km$cluster)


km <- kmeans(zz, centers=20, nstart=5)
plot(zz, col=km$cluster)


r <- 2:20
gph <- c()
for (i in r) {
  cluster <- kmeans(zz,i)
  gph <- rbind(gph,c(cluster$tot.withinss))
}
plot(gph,xlab = "Number of Clusters",xlim=c(1,length(r)),ylab = "TOTWSS (Total within sum
of squares)",main = "Plot of TOTWSS at different number of Clusters",type="o")

#Task 6.2

#Spectral CLustering
install.packages("kernlab")
library(kernlab)
specClust<-specc(zz, centers=4)
plot(zz, col=specClust,xlab='X',ylab="Y")

#Spectral Clustering
#maxMinScale <-
#  function(x){(x
#               -min(x))/(max(x)
#                         -min(x))}
#xx<-maxMinScale(zz) 
#plot(xx)

#Step2
#dXX<-as.matrix(dist(xx)) # compute Euclidean distance between data points
#cParam =1   # parameter of similarity function
#S<-exp(-dXX/cParam)  #compute similarity matrix
#S 

#Step 3
#AffMat<-function(S,k) #S-distance matrix and k-no of neighbours  

#{ 
#  AM <- matrix(0,nrow=nrow(S),ncol=ncol(S))
#  for(i in 1:nrow(S)){
#    d <- sort(S[i,],decreasing=TRUE)
#    for (t in 1:ncol(S)) 
#    { 
#      if (S[i,t] < d[k])
#      { 
#        AM[i,t]<-0 
#        AM[t,i]<-0 
#      } 
#      else 
#      { 
#        AM[i,t] <- S[i,t]
#        AM[t,i] <- AM[i,t]
#      } 
#    } 
#  } 
#  AM
#} 
#A<-AffMat(S,3) 
#A 


#Step 4
#library(shape)
#library(diagram)
#names <-("1", "2", "3", "4", "5", "6", "7", "8")
#B<-A 
#diag(B) <- 0  # to avoid self loop in the graph.
#B 
#pp <- plotmat(B, curve = 0, name = names, lwd= 1, box.lwd = 2, cex.txt = 0.8,box.type = "circle", box.prop = 0.1, arr.width=0,  
#            arr.pos = 0.5, shadow.size = 0,
#            main = "Graph: connented components")

#Step 5 Affinity  Matrix

#D <- diag(apply(A, 1, sum)) # sum rows
#D 

#Step 6 Laplasian Matrix
#L <- D -A 
#L 

#Step 7
#eigL<-eigen(L)
#eigL
#plot(eigL$values) 

#Step 8
#k<-2 
#Z<- eigL$vectors[,(ncol(eigL$vectors)-k+1):
#                   ncol(eigL$vectors)]
#plot data using the two eigenvectors
#plot(Z)

#Kmeans
#km <- kmeans(zz, centers=5, nstart=5)
#plot(zz, col=km$cluster)
#plot(zz, col=km$cluster)

#Question 5
colors <- c("black", "blue", "red")
labels <- c("prior (mean=10, var=1)", "liklihood (x1=13.9130, var=0.511)", "posterior") 
#prior 
mean=10; sd=sqrt(1) 
x <- seq(-10,10,length=200)*sd + mean 
hx <- dnorm(x,mean,sd) 
plot(x, hx, type="n", xlab="", ylab="", ylim=c(0, 0.4),main="Bayesian estimation",axes=TRUE) 
lines(x, hx, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors[1])  
#liklihood  
mean1=13.9130; sd1=0.715 
hx <- dnorm(x,mean1,sd1) 
lines(x, hx,lwd=2, col=colors[2])  
legend("topleft", inset=.005, 
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors) 
#posterior 
mean2=18/7; sd2=sqrt(12/7) 
hx <- dnorm(x,mean2,sd2) 
lines(x, hx,lwd=2, col=colors[3])  
legend("topleft", inset=.005, 
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)


#Question 7
the.data <-as.matrix(read.csv("GBRData.csv", header =TRUE, sep = ",")) 
#extract the 'LadyMusgrave-Water Temperature'  values 
LMWTdata <-the.data[,4] 

plot(LMWTdata,main="Time series plot of LMWTData")
hist(LMWTdata,main = "histogram for LMWT data")

library(MASS) 
fit1<-fitdistr(LMWTdata,"normal") 
fit1
mixmdl = normalmixEM(LMWTdata) 
mixmdl
summary(mixmdl)
plot(mixmdl,which=2)
lines(density(LMWTdata), lty=2, lwd=2)
mixmdl$lambda
mixmdl$mu
mixmdl$sigma
plot(mixmdl$all.loglik)