library(gdata)
library(reshape)
library(igraph)
library(rgdal)

setwd("Rojas et al 2017")
albian <- read.xls("~/Desktop/Rojas et al 2017/2017213_Table DR1.xlsx",check.names=FALSE,row.names=1,skip=1,sheet=1)

longitude <- albian[1,]
latitude <- albian[2,]
n_samples <- unlist(albian[4,])
n_formations <- albian[5,]

B <- albian[-(1:5),]
S <- colnames(B)
P <- rownames(B)

G <- graph_from_incidence_matrix(B)

plot(G, vertex.size=(1:2)[1+V(G)$type], vertex.label=NA, vertex.color=c("blue","red")[1+V(G)$type])

b <- B
b$taxon <- rownames(B)
adj <- melt(b,id.vars="taxon")
adj <- adj[adj$value!=0,]

species_per_sites <- sapply(S,function(x)adj$taxon[adj[,2]==x])
CS <- matrix(nr=length(S),nc=length(S))
for(i in 1:nrow(CS)){
	for(j in 1:ncol(CS)){
		if(i!=j){
			CS[i,j] <- length(intersect(species_per_sites[[i]],species_per_sites[[j]]))/(n_samples[i]+n_samples[j])
			}
		}
	}
# CS <- matrix(nr=length(S),nc=length(S))
# n_species <- sapply(S, function(x)sum(adj[,2]==x))
# for(i in 1:nrow(CS)){
	# for(j in 1:ncol(CS)){
		# if(i!=j){
			# CS[i,j] <- length(intersect(species_per_sites[[i]],species_per_sites[[j]]))/(n_species[i]+n_species[j])
			# }
		# }
	# }

# Gp <- graph_from_adjacency_matrix(CS,diag=FALSE,weighted=TRUE)
# cluster_infomap(Gp)

Gp2 <- graph_from_adjacency_matrix(CS[rowSums(CS,na.rm=TRUE)!=0,colSums(CS,na.rm=TRUE)!=0],diag=FALSE,weighted=TRUE, mode="undirected")
ci <- cluster_infomap(Gp2, nb.trials=100)

lonlat <- cbind(unlist(longitude),unlist(latitude))
albian_map <- readOGR(".","reconstructed_100.00Ma",)
par(mar=c(0,0,0,0))
plot(albian_map,col="grey80",border="grey80", xaxs="i",yaxs="i",xlim=c(-180,180),ylim=c(-90,90))
palette(c("red","green","cornflowerblue","yellow","darkorange","darkblue","grey50","white"))
points(lonlat[rowSums(CS,na.rm=TRUE)!=0,],bg=membership(ci),pch=22,cex=2)
points(lonlat[rowSums(CS,na.rm=TRUE)==0,],bg="black",pch=22,cex=1.5)


#plot(Gp2,layout=lonlat[rowSums(CS,na.rm=TRUE)!=0,], rescaled=FALSE, add=TRUE)
