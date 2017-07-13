max_likelihood <- function(res){
	library(ape)
	n <- nrow(res$Params)
	all_trials <- res$Trials
	result <- matrix(nrow=length(all_trials),ncol=n)
	for(i in seq_along(all_trials)){
		#Making a phylogenetic tree out of each simulation
		A <- all_trials[[i]]
		tips <- A$taxa[!A$taxa%in%A$ancestor]
		nodes <- unique(A$ancestor[A$ancestor!=0])
		edges <- cbind(A$ancestor,A$taxa)[-1,]
		brlen <- A$ext_time - A$orig_time
		eco_tips <- A$eco_type[tips]
		ext_tips <- A$ext_time[tips]
		orig_tips <- A$orig_time[tips]
		eco_living <- eco_tips
		eco_living[ext_tips!=max(ext_tips)] <- NA
		eco_living <- eco_living[!is.na(eco_living)]
		TIPS <- seq_along(tips)
		NODES <- seq_along(nodes) + length(tips)
		EDGES <- cbind(NODES[match(edges[,1],nodes)],ifelse(edges[,2]%in%tips,TIPS[match(edges[,2],tips)],NODES[match(edges[,2],nodes)]))
		tree <- structure(list(edge=EDGES, tip.label=paste0("taxa_",tips), Nnode=length(nodes)),class="phylo")
		tree <- compute.brlen(tree,brlen[-1])
		#Ancestral Character Trait reconstruction based on living taxa
		if(all(eco_living==eco_living[1])){ #If all living taxa shares their ecology, no need to compute
			result[i,eco_living[1]] <- 1
			result[i,-eco_living[1]] <- 0
		}else{if(length(eco_living)>2){
				living_tree <- drop.tip(tree,tree$tip.label[ext_tips!=max(ext_tips)]) #Chop dead branches
				a <- ace(factor(eco_living,1:n),living_tree,"discrete") #Maximum likelihood ancestral character reconstruction
				result[i,] <- a$lik.anc[1,] #Keep only the score attributed to ecology 2 for the ancestral taxon
			}else{result[i,unique(eco_living)]<-0.5}}
		cat(i,"\r")
	}
	cat("\n")
	result
}