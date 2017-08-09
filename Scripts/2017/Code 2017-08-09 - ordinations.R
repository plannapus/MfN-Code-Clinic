#Principal component analysis in base R
pca1 <- prcomp(USArrests)
pca2 <- princomp(USArrests)

pca1$rotation
unclass(pca2$loadings)

#These objects contains the actual coordinates of the data on the PC axes
pca1$x
pca2$scores

#Both are identical
abs(abs(pca1$x)-abs(pca2$scores)) < 1e-10

pca1$sdev #Only differences between both functions
pca2$sdev
#Beware that those are not eigenvalues but standard deviations
(pca1$sdev^2/sum(pca1$sdev^2)) # <- Proportion of variance explained

#In practice though you want to scale your data (i. e. scale each column to a variance of 1):

pca3 <- prcomp(USArrests, scale=TRUE)
pca4 <- princomp(USArrests, cor=TRUE)

abs(abs(pca3$x)-abs(pca4$scores)) < 1e-10 #Not TRUE anymore as variance computed differently in both functions

#Native plots for pca
plot(pca3)
biplot(pca3)

plot(pca4)
biplot(pca4)

#Plotting on your own
plot(pca4$scores[,1:2])
plot(pca4$scores[,c(1,3)])

#In vegan
library(vegan)

#Function cmdscale does Principal COORDINATE analysis, which, when used with an euclidean distance metric, is the same as PCA.
pca5 <- cmdscale(dist(USArrests, method="euclidean"), k=4) #Gives same as pca2$scores or pca1$x

#Other means of ordinations:
#CCA

ca1 <- cca(USArrests)

cca1 <- cca(USArrests[,-2], USArrests[,2])

plot(cca1)
plot(cca1,choices=c(1,3))

str(cca1)

#This contains the scores on the constrained axes
cca1$CCA$u

#NMDS (non-metric multidimensional scaling)
library(MASS)
nm1 <- isoMDS(dist(USArrests, method="euclidean")) #A bit pointless as whole point of nmds is to be non-euclidean
nm2 <- isoMDS(vegdist(USArrests, method="bray"))
nm3 <- isoMDS(vegdist(USArrests, method="bray"), k=4)

#Check quality of the ordination
stressplot(nm2, vegdist(USArrests, method="bray"))
stressplot(nm3, vegdist(USArrests, method="bray"))

plot(nm2$points)
plot(nm3$points[,c(1,3)])

library(vegan)
nm4 <- metaMDS(USArrests, "bray", k=4)

stressplot(nm4)
plot(nm4)
plot(nm4, choices=c(1,3))
str(nm4)

#Separating by clusters
data(iris)
a <- prcomp(iris[,1:4], scale=TRUE)
plot(a$x[,1:2]) # == to biplot(a)
plot(a$x[,1:2],col=iris$Species)

setosa <- a$x[iris$Species=="setosa",1:2]
S <- chull(setosa) #Convex Hull
polygon(setosa[S,])

versi <- a$x[iris$Species=="versicolor",1:2]
polygon(versi[chull(versi),], border="red")

virgi <- a$x[iris$Species=="virginica",1:2]
polygon(virgi[chull(virgi),], border="green")

k <- kmeans(a$x,2)
plot(a$x[,1:2],col=k$cluster)

k1 <- a$x[k$cluster==1,1:2]
k2 <- a$x[k$cluster==2,1:2]
polygon(k1[chull(k1),])
polygon(k2[chull(k2),], border="red")

#You can also fit a surface to an ordination space using "ordisurf",
#predict the ordination score of an additional sample using "predict", etc.

# In an NMDS remember to not use a standard anova or manova (implemented in R as "aov"), 
# but a permanova (as NMDS non-metric, centroids are meaningless): implemented in R as "adonis".


