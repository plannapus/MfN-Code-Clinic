# R CRAN Task View on Phylogenetics: https://cran.r-project.org/web/views/Phylogenetics.html
# Introduction to phylogenies in R: http://www.phytools.org/eqg/Exercise_3.2/
# "ape" basic package to manipulate trees. "phangorn" can do phylogenetic analyses. "rmesquite" can call Mesquite from R.
# Blog of the guy who created package phytools: http://blog.phytools.org/

library(ape)
newick_text <- "(A,((B,C),(D,(E,F))));"
tree <- read.tree(text=newick_text)

str(tree)

plot(tree)
plot(tree,type="cladogram", direction="upwards")
plot(tree,type="fan")

?plot.phylo

#
# Following script was written to compare morphological distance vs phylogenetic distance
# and test the significance of that relationship
#

nexus_file <- "tree_therapsids.txt"
tree <- read.nexus(nexus_file)
tree <- compute.brlen(tree, 1)
#tree <- compute.brlen(tree, method="Grafen") #If needs be ultrametric

name_list <- tree$tip.label
name_comb <- t(combn(name_list,2)) #Find all unique pairs of taxa on the tree
all <- as.data.frame(name_comb, stringsAsFactors=FALSE)

phylo_dist <- dist.nodes(tree)[1:length(name_list),1:length(name_list)]

PCA <- read.table("PCA.csv", sep="\t", header=TRUE, stringsAsFactors=FALSE)
PCA

name.color <- rep("black", length(name_list))
name.color[name_list%in%PCA$Name] <- "red"
plot(tree, tip.color= name.color, font=4)

all$MorphoDist <- apply(name_comb,1,function(x)ifelse(x[1]%in%PCA$Name & x[2]%in%PCA$Name, 
                                                abs(PCA$Coef[PCA$Name==x[1]]-PCA$Coef[PCA$Name==x[2]]), 
                                                NA))

all$TruePhyloDist <- apply(name_comb,1,function(x)phylo_dist[name_list==x[1],name_list==x[2]])

obslm <- lm(all$TruePhyloDist~all$MorphoDist)
obsCoef <- obslm$coef[2]
obsR <- summary(obslm)$r.squared
print(summary(obslm))
cor.test(all$TruePhyloDist,all$MorphoDist)

#####Permuting the tree##############
n <- 9999 #Number of randomizations
set.seed(1983)
randCoef <- rep(0, n)
for(i in 1:n){
	random_tree <- rtree(length(name_list), tip.label=name_list)
	rand_phylo_dist <- dist.nodes(random_tree)[1:length(name_list),1:length(name_list)]
	random_dist <- apply(name_comb,1,function(x)rand_phylo_dist[name_list==x[1],name_list==x[2]])
	rand_lm <- lm(random_dist~all$MorphoDist)
	randCoef[i]<-rand_lm$coef[2]
	cat(i,"/",n,"\r",sep="")
	}
obs_signif <- sum(abs(randCoef)>abs(obsCoef)) / n

par(mfcol=c(2,1))
par(mar=c(4,4,1,1))
plot(all$MorphoDist, all$TruePhyloDist, xlab="Morphological Distance", ylab="Phylogenetic Distance", pch=19, xaxs="i", yaxs="i")
abline(obslm, col="red")
text(25,5, sprintf("Coefficients = %.3f\nR-squared = %.3f",obsCoef,obsR), col="red", pos=3)

par(mar=c(4,4,4,1))
hist(abs(randCoef), 256, main="Linear regression coefficient\nof 9999 random trees")
abline(v=abs(obsCoef), col="red", lwd=2)
text(abs(obsCoef), 80, sprintf("Observed = %.3f",obsCoef), col="red", cex=0.5, pos=4)

#
# Another snippet of code from Christie: Mapping character visually on a phylogeny
#

nexus_christie <- "Christie/Adulttree.nex"
params_christie <- "Christie/Adult92spp2.txt"

tree <- read.nexus(nexus_christie) # read in tree
data <- read.table(params_christie, header=TRUE, row.names=1, sep="\t", stringsAsFactors=FALSE) # read in data file
data[1:10,1:10]
phylo.data <- data[tree$tip.label,] #Because row names of data are == to the tree's tip labels
Biome <- phylo.data$Biome

#Mapping the characters on the tree tips
color.scheme <- c("red","blue","green","yellow","orange","black")
plot(tree, tip.color=color.scheme[as.factor(Biome)], type="fan", edge.width=2)

#Mapping the characters on the tree edges
biome.edge <- ifelse(tree$edge[ ,2] %in% 1:nrow(phylo.data), Biome[tree$edge[ ,2]], "unknown")

biome.edge2 <- Biome[tree$edge[ ,2]]
biome.edge2[is.na(biome.edge2)] <- "unknown"
all(biome.edge==biome.edge2)

plot(tree, edge.color=color.scheme[as.factor(biome.edge)], type="fan", edge.width=2)