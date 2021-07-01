library(nortest)
library(MASS)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(caTools)
library(caret)
library(gridExtra)
library(ggpubr)
library(dplyr)
library(car)
library(gplots)
library(plyr)
library(leaps)
library("ggcorrplot")
library(MASS)
library(glmnet)
library(naniar)#NA plot
library(car)
library(lmtest)
library("readxl")
library(naniar)
library("factoextra")
library("FactoMineR")
library("corrplot")
library("formattable")
library(tidyr)
library(ggdendro)
library(sets)
library(dendextend)
library(magrittr)
library(tibble)
library(proxy)
options(scipen=9999)# no scientific notation

library("maps")



setwd("C:/Users/barla/OneDrive/Desktop/Academia/Data Science and Economics/Statistical Learning/individual project/PCA and Clustering")
data<-read_excel("fragile states index 2020.xlsx")
View(data)



countries<-as.matrix(data[,1])
data_copy<-data[,-c(1,2,3,4,17)] 

rownames(data_copy)<-countries[,1]
View(data_copy)






sum(is.na(data_copy)) #0 NA

vis_miss(data_copy)
clean_data<-data_copy

#Descriptive Stats

indexes.distr <- ggplot(gather(clean_data), aes(value)) + 
  geom_histogram(aes(y=..density..), bins=15,
                 colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~key, scales='free_y')

indexes.distr


stats <- data.frame("Index"=colnames(clean_data), 
                    "Mean"=apply(clean_data,2,mean),
                    "Variance"=apply(clean_data,2,var))

mean.graph <- ggplot(stats,aes(x=reorder(Index,Mean),y=Mean)) + 
  geom_bar(stat="identity", fill="steelblue",color="black") +
  geom_text(aes(label=round(Mean,digits=2)), 
            hjust=1, color="white", size=3) +
  theme_minimal() + xlab("Indexes") + coord_flip()

mean.graph

var.graph <- ggplot(stats,aes(x=reorder(Index,Variance),y=Variance)) + 
  geom_bar(stat="identity", fill="steelblue",color="black") +
  geom_text(aes(label=round(Variance,digits=2)), 
            hjust=1, color="white", size=3) +
  theme_minimal() + xlab("Indexes") + coord_flip()

var.graph

m<-cor(clean_data)
corrplot(m, method="number", number.cex = 0.5)


#PCA



res.pca <-PCA(clean_data,graph=FALSE)

eig.val<-get_eigenvalue(res.pca)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
row.names(eig.val)<-NULL
dims<-matrix(c("Dim.1","Dim.2","Dim.3","Dim.4","Dim.5","Dim.6","Dim.7","Dim.8","Dim.9","Dim.10","Dim.11","Dim.12"))
eig.val<-cbind(dims,eig.val)
colnames(eig.val)[1]<-"Dimensions"
row.names(eig.val)<-NULL
grid.table(eig.val)


plot(eig.val[,2],type="b",main="Scree Diagram",xlab="Number of Component",ylab="Eigenvalues")
abline(h=1,lwd=3,col="red")


plot(eig.val[,4],type="b",main="Scree Diagram",xlab="Number of Component",ylab="Cumulative Variance")
abline(h=80.281518869289,lwd=3,col="red")


fviz_eig(res.pca, addlabels = TRUE, ylim = c(0,75 ))


##how well the variables are represented

var <- get_pca_var(res.pca)
corr.comp <- data.frame("Comp 1" = round(var$cor[,1],3),
                        "Comp 2" = round(var$cor[,2],3),
                        "Communality"=round(var$cor[,1]^2+var$cor[,2]^2,3))

grid.table(corr.comp)

corrplot(var$cos2, is.corr=FALSE)

##loadings


fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)+ ggtitle(paste0("PCA - Loadings")) 


#Project the countries

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, max.overlaps=200) + ylim(c(-10,4)) 


###K-MEANS


seeds <- c(1,42,101,123,300)
cl <- 1:10
tot.withinss <- NULL

for (s in seeds) {
  set.seed(s)
  for (k in cl) {
    km.out <- kmeans(clean_data, k, nstart = 50)
    tot.withinss <- append(tot.withinss,km.out$tot.withinss)
  }
}

kmeans.data <- data.frame("Seed"= as.factor(rep(seeds,each=length(cl))),
                          "Nr of clusters"= as.factor(rep(cl,length(seeds))),
                          "Tot Within SS" = tot.withinss)

kmeans.plot <- ggplot(kmeans.data, aes(x=Nr.of.clusters, 
                                       y=Tot.Within.SS, 
                                       group=Seed)) +
  geom_point(aes(shape=Seed, color=Seed)) + geom_line(aes(color=Seed)) + theme_minimal()

kmeans.plot


#K-Means with 5 cluster

set.seed(123)
km.out <- kmeans(clean_data,5,nstart=50)
cluster_means<-t(as.data.frame(km.out$centers))
cluster_means<-rbind(cluster_means,colMeans(cluster_means))
rownames(cluster_means)[13]<-"Indicator Cluster Means"
grid.table(cluster_means)

simil(as.data.frame(km.out$centers))#similarity



view(clean_data)


##MAP
cluster.data <- data.frame("region"=row.names(clean_data), 
                           "Cluster"=as.factor(km.out$cluster),
                           row.names = 1:178)
view(cluster.data)
old.names <- c(5, 26, 32,69,87,149,150,151)
new.names <- c("Democratic Republic of the Congo","Republic of Congo","Ivory Coast","Israel","Kyrgyzstan",
               "USA","UK","Slovakia")
  

View(cluster.data) 


cluster.data$region<-replace(cluster.data$region,old.names,new.names)
View(cluster.data)

world.map <- map_data("world")


cluster.data.map <- full_join(cluster.data, world.map, by = "region")

cluster.map <- ggplot(cluster.data.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Cluster, color=Cluster), color="black")+ 
  scale_fill_discrete(na.value="darkgray") +
  theme_minimal() + theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank())
cluster.map


## MAP the states in PC space

cluster.plot <- fviz_cluster(km.out, data=clean_data, 
                             geom = c("point", "text"),show.clust.cent=TRUE,
                             repel = TRUE) +
  theme_minimal() + geom_vline(xintercept = 0, lty="dashed") +
  geom_hline(yintercept = 0, lty="dashed")
cluster.plot


##HIERARCHICAL CLUSTERING
set.seed(123)

par(mfrow=c(2,2))
distance <- dist(scale(clean_data))

hc.complete<- as.dendrogram(hclust(distance, "complete")) %>% 
  set("labels_cex", c(1, 0.3, .4,0.3, 0.3 , 0.6))
plot(hc.complete, ylab = "Height", leaflab = "none",  main="Complete linkage")

hc.single<- as.dendrogram(hclust(distance, "single"))%>% 
  set("labels_cex", c(1, 0.3, .4,0.3, 0.3 , 0.6))
plot(hc.single, ylab = "Height", leaflab = "none", main="Single linkage")

hc.average<- as.dendrogram(hclust(distance, "average"))%>% 
  set("labels_cex", c(1, 0.3, .4,0.3, 0.3 , 0.6))
plot(hc.complete, ylab = "Height",leaflab = "none",  main="Average linkage")

hc.centroid<- as.dendrogram(hclust(distance, "centroid"))%>% 
  set("labels_cex", c(1, 0.3, .4,0.3, 0.3 , 0.6))
plot(hc.centroid, ylab = "Height", leaflab = "none",  main="Centroid linkage")



#complete approach


hc.complete<- as.dendrogram(hclust(distance, "complete")) %>% 
  set("labels_cex", c(0.6))%>% 
  set("branches_k_color", k=3) 
plot(hc.complete, ylab = "Height", main="Complete linkage")
abline(h = 7, lty = 2, col="#82b74b")

##WARD

hc.ward<- as.dendrogram(hclust(distance, method="ward.D2")) %>% 
  set("labels_cex", c(0.6))%>% 
  set("branches_k_color", k=5) 
plot(hc.ward, ylab = "Height",  main="Ward linkage")
abline(h = 12, lty = 2, col="#82b74b")




segments.complete<- cutree(hc.complete, k=3)
segments.ward<-cutree(hc.ward, k=5)
seg.summ<-function(data, groups){
  aggregate(data, list(groups),function(x)mean(x))
}
segments<-recode(segments.ward, 1 = 4, 2 = 1, 4 = 2, 3 = 3 )



cluster.plot.2 <- fviz_cluster(object = list(data = clean_data, cluster = segments), 
                               geom = c("point", "text"),
                               repel = F)+
  theme_minimal() + geom_vline(xintercept = 0, lty="dashed") +
  geom_hline(yintercept = 0, lty="dashed")
cluster.plot.2



print("cluster size:");table(segments)
index.ward<-cbind(data, segments)
index.ward %>% 
  group_by(segments) %>% 
  summarise_all(.funs=mean) %>%
  rownames_to_column %>% 
  gather(Clusters, value, -rowname) %>% 
  spread(rowname, value) %>% 
  slice(-11)
table("ward.clusters"=segments, "kmeans"=km.out$cluster)

