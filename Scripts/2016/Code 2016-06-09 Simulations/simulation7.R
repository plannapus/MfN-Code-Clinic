simulation <- function(speciation_rate1, speciation_rate2, 
						extinction_rate1, extinction_rate2, 
						mutation_rate1, mutation_rate2,
						age_max, occupancy1, occupancy2, 
						N_TRIALS, RNG_SEED, starting_with=2){
							
	ecologies <- data.frame(type=1:2,
							speciation_rate=c(speciation_rate1,speciation_rate2),
							extinction_rate=c(extinction_rate1,extinction_rate2),
							mutation_rate=c(mutation_rate1,mutation_rate2),
							occupancy=c(occupancy1,occupancy2))
	all_trials <- list()
	set.seed(RNG_SEED)
	x <- 0
		
	#Simulated evolution
	while(x<N_TRIALS){
		x <- x+1
		t <- 0
		taxa <- 1
		orig_time <- 0
		ext_time <- NA
		eco_type <- starting_with
		ancestor <- 0
		while(t<age_max){
			t <- t+1
			i <- 1
			while(i<=length(taxa)){
				if(is.na(ext_time[i])){
					eco <- eco_type[i]
					mr <- ecologies$mutation_rate[eco]
					MUT <- rpois(1,mr)
					if(MUT & taxa[i]!=1){
						eco_type[i] <- sample(ecologies$type[-eco],1)
						eco <- eco_type[i]
						}
					if(orig_time[i]!=t){
						sr <- ecologies$speciation_rate[eco]
						er <- ecologies$extinction_rate[eco]
						N <- sum(eco_type[is.na(ext_time)]==eco)
						er <- er + (sr-er)*N/ecologies$occupancy[eco]
						EXT <- rpois(1,er)
						ORG <- rpois(1,sr)
						if(ORG){
							ext_time[i] <- t
							taxa <- c(taxa, max(taxa)+1, max(taxa)+2)
							orig_time <- c(orig_time, t, t)
							ext_time <- c(ext_time, ifelse(EXT,t,NA), NA)
							eco_type <- c(eco_type, eco, eco)
							ancestor <- c(ancestor,taxa[i], taxa[i])
							}
						if(EXT){ext_time[i] <- t}
					}
				}
				i <- i+1
			}
			if(sum(is.na(ext_time))==0){break}
			}
		if(sum(is.na(ext_time))==0){
			x <- x-1
		}else{
			ext_time[is.na(ext_time)] <- t+1
			all_trials[[x]] <- data.frame(taxa, orig_time, ext_time, eco_type, ancestor)
			cat("Trial",x,"done.\n")
		}
	}
	list(Params=ecologies, Trials=all_trials)
}


if(!interactive()){
	RNG_SEED <- 20060707 #Random seed
	N_TRIALS <- 1000 #Number of trials
	args <- as.numeric(commandArgs(TRUE))
	sp1 <- args[1]
	sp2 <- args[2]
	ex1 <- args[3]
	ex2 <- args[4]
	mu1 <- args[5]
	mu2 <- args[6]
	age <- args[7]
	occ1 <- args[8]
	occ2 <- args[9]
	res <- simulation(sp1,sp2,ex1,ex2,mu1,mu2,age,occ1,occ2,N_TRIALS,RNG_SEED)
	save(res,file=sprintf("sim7 - Sp%.2f-%.2f,Ext%.2f-%.2f,Mut%.2f-%.2f,Age%i,K%i-%i.Rdata",sp1,sp2,ex1,ex2,mu1,mu2,age,occ1,occ2))
	}