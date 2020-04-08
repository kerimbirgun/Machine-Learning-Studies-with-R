data = read.csv('C:/Users/Kerim Birgun/Desktop/R/jpm_cluster.csv',stringsAsFactors = F)

names(data) = c('id','performance','fees_commissions','depth_of_products','ability_resolve_problems','online_services','choice_of_providers','quality_of_advice','knowledge_of_reps','rep_knowing_your_needs','professional_services','provider_knows_me','quality_of_service',
                'age','marital_status','education')
data$age = factor(data$age,labels = c('27-57','58-68','69-75','75+'))
data$marital_status = factor(data$marital_status,labels=c('married','not married'))
data$education = factor(data$education,labels=c('no college','some college','college graduate','some graduate school','masters degree','doctorate'))
str(data)

data_cluster = data[,2:13]
head(data_cluster[,1:5])

#Missing imputation
library(mice)
set.seed(617)
data_cluster = mice::complete(mice(data_cluster))
head(data_cluster[,1:4])

#Scaling
data_cluster = scale(data_cluster)
head(data_cluster[,1:4])

####### HIERARCHICAL CLUSTERING
d = dist(x = data_cluster,method = 'euclidean')
d
clusters = hclust(d = d,method='ward.D2')
plot(clusters)
cor(cophenetic(clusters),d)

#Number of Clusters
plot(cut(as.dendrogram(clusters),h=5)$upper)

# Better Visualization
library(dendextend)
plot(color_branches(as.dendrogram(clusters),k = 2,groupLabels = F))

#Selecting Clusters
h_segments = cutree(tree = clusters,k=4)
table(h_segments)


library(psych)
temp = data.frame(cluster = factor(h_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])

library(ggplot2)
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

####### K MEANS CLUSTERING
set.seed(617)
km = kmeans(x = data_cluster,centers = 3,iter.max=10000,nstart=25)
table(km$cluster)

paste(km$totss,'=',km$betweenss,'+',km$tot.withinss,sep = ' ')

within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

library(psych)
temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()



