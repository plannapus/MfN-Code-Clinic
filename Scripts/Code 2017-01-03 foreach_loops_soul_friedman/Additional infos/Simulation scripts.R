source("http://www.graemetlloyd.com/pubdata/functions_5.r")

library(caper)
library(geiger)
library(phangorn)
library(OUwie)
library(paleotree)
library(foreach)

library(doParallel)
registerDoParallel(cores=detectCores()-1)

#######################################################################################

extincttable<-function(ttsltree, starttime, endtime){

    ext.tab <-matrix(ncol=4, nrow=Ntip(ttsltree))
    edgetotip <-match(c(1:Ntip(ttsltree)), ttsltree$edge[,2])
    tiplengths <-ttsltree$edge.length[edgetotip]
    endrange <-ttsltree$root.time-diag(vcv(ttsltree))
    startrange <-endrange+tiplengths
	
    ext.tab[,1] <-ttsltree$tip.label
    ext.tab[,2] <-startrange
    ext.tab[,3] <-endrange

    for (i in 1:Ntip(ttsltree)){
        keytime <-round(as.numeric(as.character(ext.tab[i,3])),4)
        if (keytime<=endtime){
       	    ext.tab[i,4] <-0
       	}
    	if (keytime>endtime & keytime<=starttime){
       	    ext.tab[i,4] <-1
      	}
    	if (keytime>starttime){
       	    ext.tab[i,4] <-2
       	}
	}
    namettd <-ext.tab[ext.tab[,4]==2,1]
    ttd <-match(namettd, ext.tab[,1])
    if (length(ttd)>0){
        ext.tab <-ext.tab[-ttd,]
    }
    colnames(ext.tab) <-c("species", "FAD", "LAD", "extinct")
    ext.tab <-as.data.frame(ext.tab)
    result <-list(ext.tab, ttsltree)
    names(result) <-c("ext.tab", "ttsltree")
    return(result)
}

############################################################################################
############################################################################################

sampling<- function(run, r) {
    
    cladogram<-taxa2cladogram(run)
    ranges<-sampleRanges(taxad=run, r=r, modern.samp.prob=r)

    sampledtree <- drop.tip(cladogram, cladogram$tip.label[is.na(match(cladogram$tip.label, names(which(!is.na(ranges[, 1])))))])
    sampledtree <- basemulti2di(sampledtree)
    sampledtree$root.edge<-0
    sampledtree$root.time<-max(ranges)

    sampledages<-ranges[complete.cases(ranges),]
    result<-list(sampledages, sampledtree)
    names(result)<-c("sampledages", "sampledtree")
    return(result)
}

####################################################################################################
####################################################################################################
#random extinction during the simulation under a budding model of evolution

count<-1
baseruns<-list()
DValues<-list()

while (count < howmany) {

    baserun <-simFossilTaxa(p=0.1, q=0.1, mintaxa=200, maxtime=50, min.cond=FALSE)

    for (i in 1:nrow(baserun)){
        rownames(baserun)[i]<-paste("b", i, sep="")
    }

    for (k in 1:4){

        basetree <-taxa2phylo(baserun)

        edgetotip <-match(c(1:Ntip(basetree)), basetree$edge[,2])
        tiplengths <-basetree$edge.length[edgetotip]
        endrange <-basetree$root.time-diag(vcv(basetree))
        startrange <-endrange+tiplengths
		
        realages <-matrix(ncol=2, nrow=Ntip(basetree))
        rownames(realages) <-basetree$tip.label
        realages[,1] <-startrange
        realages[,2] <-endrange

        data1 <-matrix(nrow=Ntip(basetree), ncol=2)
        data1[,2] <-1
        data1[,1] <-basetree$tip.label
        data1 <-as.data.frame(data1)
        basetree$node.label <-rep("1", Nnode(basetree))

        Trait1 <-OUwie.sim(basetree, data1, alpha=1e-10, sigma.sq=0.5, theta0=1, theta=0)
        threshold <-quantile(Trait1[,3], 0.8)
        extant <-which(baserun[,5]==1)
        extant_traits <-Trait1[extant,]

        risk <-extant_traits[which(extant_traits[,3]<=threshold,3),1]
        safe <-extant_traits[which(extant_traits[,3]>threshold,3),1]
        alive <-character()
        dead <-character()

        for (j in 1:length(risk)){
            if(runif(1,0,1)<=0.05){
                alive <-c(alive, risk[j])
            } else {
                dead <-c(dead, risk[j])
            }
        }

        for (j in 1:length(safe)){
            if(runif(1,0,1)>=0.1){
                alive <-c(alive, safe[j])
            } else {
                dead <-c(dead, safe[j])
            }
        }

        alive<-sort(alive)
        if(alive[1]=="b1"){
            alive<-alive[-1]
            dead<-c("b1", dead)
        }

        if(alive[1]=="b2"){
            alive<-alive[-1]
            dead<-c("b2", dead)
        }

        no_newtrees <-length(alive)

        baserun[,3:4]<-baserun[,3:4]+20
        baserun[,5]<-0

        for (m in 1:no_newtrees){

            newrun <-simFossilTaxaSilent(p=0.1, q=0.1, mintaxa=5, maxtime=20, min.cond=FALSE, nruns=1)
            attachment_tip <-which(rownames(baserun)==alive[m])
            first_table <-baserun[1:attachment_tip,]
            if(attachment_tip!=nrow(baserun) & attachment_tip!=(nrow(baserun)-1)){
                last_table <-baserun[(attachment_tip+1):nrow(baserun),]
                first_table[nrow(first_table),4]<-newrun[1,4]
                first_table[nrow(first_table),5]<-newrun[1,5]
                newrun<-newrun[-1,]
                newrun[,c(1,6)] <-(nrow(baserun)+1):(nrow(baserun)+nrow(newrun))
                newrun[newrun[,2]!=1,2] <-(newrun[newrun[,2]!=1,2])+nrow(baserun)-1
                newrun[newrun[,2]==1,2]<- baserun[attachment_tip,1]
                baserun <-rbind(first_table, newrun, last_table)
            }
        }

        for (l in 1:nrow(baserun)){
            rownames(baserun)[l]<-paste("b", l, sep="")
        }
        cat("level up ", k, "\n")
    }

    baserun<-baserun[order(baserun[,1]),]
    for (l in 1:nrow(baserun)){
        rownames(baserun)[l]<-paste("b", l, sep="")
    }

    realtree<-taxa2phylo(baserun, obs_time=baserun[,3])
    realtree<-di2multi(realtree)

    edgetotip<-match(c(1:Ntip(realtree)), realtree$edge[,2])
    extratiplengths<-baserun[,3]-baserun[,4]
    realtree$edge.length[edgetotip]<-(realtree$edge.length[edgetotip])+extratiplengths
    tiplengths<-realtree$edge.length[edgetotip]

    slicetimeend<- 19.99
    slicetimestart<- 30

    ttsltree<-timeSliceTree(realtree, sliceTime=slicetimeend, drop.extinct=F, plot=F)

    extincttab<-extincttable(ttsltree, slicetimestart, slicetimeend)
    inputdata<-comparative.data(extincttab$ext.tab, phy=ttsltree, species, vcv=T)
    res <- phylo.d(data=inputdata, binvar=extinct, permut=1000)

    if (res$Pval0>=0.05){
        baseruns[[count]]<-baserun
        DValues[[count]]<- res
        count<-count+1
    }
}



#########################################################################################
#########################################################################################
#No random extinction during the simulation under 
#a budding model of evolution

count<-1
baseruns<-list()
DValues<-list()

while (count < 6) {

    baserun <-simFossilTaxa(p=0.1, q=0, mintaxa=20, maxtime=50, min.cond=FALSE)

    for (i in 1:nrow(baserun)){
        rownames(baserun)[i]<-paste("b", i, sep="")
    }

    for (k in 1:4){

        basetree <-taxa2phylo(baserun)

        edgetotip <-match(c(1:Ntip(basetree)), basetree$edge[,2])
        tiplengths <-basetree$edge.length[edgetotip]
        endrange <-basetree$root.time-diag(vcv(basetree))
        startrange <-endrange+tiplength

        data1 <-matrix(nrow=Ntip(basetree), ncol=2)
        data1[,2] <-1
        data1[,1] <-basetree$tip.label
        data1 <-as.data.frame(data1)
        basetree$node.label <-rep("1", Nnode(basetree))

        Trait1 <-OUwie.sim(basetree, data1, alpha=1e-10, sigma.sq=0.3, theta0=1, theta=0)
        threshold <-quantile(Trait1[,3], 0.8)
        extant_traits <-Trait1

        risk <-extant_traits[which(extant_traits[,3]<=threshold,3),1]
        safe <-extant_traits[which(extant_traits[,3]>threshold,3),1]
        alive <-character()
        dead <-character()

        for (j in 1:length(risk)){
            if(runif(1,0,1)<=0.05){
                alive <-c(alive, risk[j])
            } else {
                dead <-c(dead, risk[j])
            }
        }

        for (j in 1:length(safe)){
            if(runif(1,0,1)>=0.1){
                alive <-c(alive, safe[j])
            } else {
                 dead <-c(dead, safe[j])
            }
        }

        alive<-sort(alive)
        if(alive[1]=="b1"){
            alive<-alive[-1]
            dead<-c("b1", dead)
        }

        if(alive[1]=="b2"){
            alive<-alive[-1]
            dead<-c("b2", dead)
        }

        no_newtrees <-length(alive)

        baserun[,3:4]<-baserun[,3:4]+20
        baserun[,5]<-0

        for (m in 1:no_newtrees){

            newrun <-simFossilTaxaSilent(p=0.1, q=0, mintaxa=5, maxtime=20, min.cond=FALSE, nruns=1)
            attachment_tip <-which(rownames(baserun)==alive[m])
            first_table <-baserun[1:attachment_tip,]
            if(attachment_tip!=nrow(baserun) & attachment_tip!=(nrow(baserun)-1)){
                last_table <-baserun[(attachment_tip+1):nrow(baserun),]
                first_table[nrow(first_table),4]<-newrun[1,4]
    	        first_table[nrow(first_table),5]<-newrun[1,5]
            	newrun<-newrun[-1,]
    	        newrun[,c(1,6)] <-(nrow(baserun)+1):(nrow(baserun)+nrow(newrun))
    	        newrun[newrun[,2]!=1,2] <-(newrun[newrun[,2]!=1,2])+nrow(baserun)-1
    	        newrun[newrun[,2]==1,2]<- baserun[attachment_tip,1]
    	        baserun <-rbind(first_table, newrun, last_table)
            }
        }
        
        for (l in 1:nrow(baserun)){
            rownames(baserun)[l]<-paste("b", l, sep="")
        }
        cat("level up ", k, "\n")
    }

    baserun<-baserun[order(baserun[,1]),]
    for (l in 1:nrow(baserun)){
        rownames(baserun)[l]<-paste("b", l, sep="")
    }

    realtree<-taxa2phylo(baserun,obs_time=baserun[,3])
    realtree<-di2multi(realtree)

    edgetotip<-match(c(1:Ntip(realtree)), realtree$edge[,2])
    extratiplengths<-baserun[,3]-baserun[,4]
    realtree$edge.length[edgetotip]<-(realtree$edge.length[edgetotip])+extratiplengths
    tiplengths<-realtree$edge.length[edgetotip]

    slicetimeend<- 19.99
    slicetimestart<- 30

    ttsltree<-timeSliceTree(realtree, sliceTime=slicetimeend, drop.extinct=F, plot=F)

    extincttab<-extincttable(ttsltree, slicetimestart, slicetimeend)
    inputdata<-comparative.data(extincttab$ext.tab, phy=ttsltree, species, vcv=T)
    res <- phylo.d(data=inputdata, binvar=extinct, permut=1000)

    if (res$DEstimate<0.1){
        baseruns[[count]]<-baserun
        DValues[[count]]<- res
        count<-count+1
    }
}



#########################################################################################
#########################################################################################
# a BIFURCATING model of evolution

count<-1
baseruns<-list()
DValues<-list()

while (count < 6) {

    baserun <-simFossilTaxa(p=0.1, q=0, mintaxa=20, maxtime=50, prop.bifurc=1, min.cond=FALSE)

    for (i in 1:nrow(baserun)){
        rownames(baserun)[i]<-paste("b", i, sep="")
        }

    for (k in 1:4){

        basetree <-taxa2phylo(baserun)

        edgetotip <-match(c(1:Ntip(basetree)), basetree$edge[,2])
        tiplengths <-basetree$edge.length[edgetotip]
        endrange <-basetree$root.time-diag(vcv(basetree))
        startrange <-endrange+tiplengths
		
        realages <-matrix(ncol=2, nrow=Ntip(basetree))
        rownames(realages) <-basetree$tip.label
        realages[,1] <-startrange
        realages[,2] <-endrange

        data1 <-matrix(nrow=Ntip(basetree), ncol=2)
        data1[,2] <-1
        data1[,1] <-basetree$tip.label
        data1 <-as.data.frame(data1)
        basetree$node.label <-rep("1", Nnode(basetree))

        Trait1 <-OUwie.sim(basetree, data1, alpha=1e-10, sigma.sq=0.3, theta0=1, theta=0)
        threshold <-quantile(Trait1[,3], 0.8)
        extant <-which(baserun[,5]==1)
        extant_traits <-Trait1[extant,]

        risk <-extant_traits[which(extant_traits[,3]<=threshold,3),1]
        safe <-extant_traits[which(extant_traits[,3]>threshold,3),1]
        alive <-character()
        dead <-character()

        for (j in 1:length(risk)){
            if(runif(1,0,1)<=0.05){
                alive <-c(alive, risk[j])
                } else {
                dead <-c(dead, risk[j])
                }
            }

        for (j in 1:length(safe)){
            if(runif(1,0,1)>=0.1){
                alive <-c(alive, safe[j])
                } else {
                 dead <-c(dead, safe[j])
                }
            }

        alive<-sort(alive)
        if(alive[1]=="b1"){
            alive<-alive[-1]
            dead<-c("b1", dead)
            }

        if(alive[1]=="b2"){
            alive<-alive[-1]
            dead<-c("b2", dead)
            }

        no_newtrees <-length(alive)

        baserun[,3:4]<-baserun[,3:4]+20
        baserun[,5]<-0

        for (m in 1:no_newtrees){

            newrun <-simFossilTaxaSilent(p=0.1, q=0, mintaxa=3, maxtime=20, prop.bifurc=1, min.cond=FALSE, nruns=1)
            attachment_tip <-which(rownames(baserun)==alive[m])
            first_table <-baserun[1:attachment_tip,]
            if(attachment_tip!=nrow(baserun) & attachment_tip!=(nrow(baserun)-1)){
                last_table <-baserun[(attachment_tip+1):nrow(baserun),]
                first_table[nrow(first_table),4]<-newrun[1,4]
    	        first_table[nrow(first_table),5]<-newrun[1,5]
            	newrun<-newrun[-1,]
    	        newrun[,c(1,6)] <-(nrow(baserun)+1):(nrow(baserun)+nrow(newrun))
    	        newrun[newrun[,2]!=1,2] <-(newrun[newrun[,2]!=1,2])+nrow(baserun)-1
    	        newrun[newrun[,2]==1,2]<- baserun[attachment_tip,1]
    	        baserun <-rbind(first_table, newrun, last_table)
                }
            }
        
        for (l in 1:nrow(baserun)){
            rownames(baserun)[l]<-paste("b", l, sep="")
            }
        cat("level up ", k, "\n")
    }

    baserun<-baserun[order(baserun[,1]),]
    for (l in 1:nrow(baserun)){
            rownames(baserun)[l]<-paste("b", l, sep="")
            }

    realtree<-taxa2phylo(baserun,obs_time=baserun[,3])
    realtree<-di2multi(realtree)
    
    ttd<-vector("numeric")
    for (i in 1:length(baserun[,1])) {
        if ((length(which(baserun[,2]==i))>=2) & (baserun[i,5]==0)) {
            ttd<-c(ttd,i)
            }
        }
    
    edgetotip<-match(c(1:Ntip(realtree)), realtree$edge[,2])
    extratiplengths<-baserun[,3]-baserun[,4]
    realtree$edge.length[edgetotip]<-(realtree$edge.length[edgetotip])+extratiplengths
    tiplengths<-realtree$edge.length[edgetotip]
    
    realtree<-drop.tip(realtree,ttd)

    realtree[which(realtree$edge.length==0)]<-0.00001    
  
    slicetimeend<- 19.99
    slicetimestart<- 30

    ttsltree<-timeSliceTree(realtree, sliceTime=slicetimeend, drop.extinct=F, plot=F)

    extincttab<-extincttable(ttsltree, slicetimestart, slicetimeend)
    inputdata<-comparative.data(extincttab$ext.tab, phy=ttsltree, species, vcv=T)
    res <- phylo.d(data=inputdata, binvar=extinct, permut=1000)

    if (res$DEstimate<0.1){
        baseruns[[count]]<-baserun
        DValues[[count]]<- res
        count<-count+1
        }
    }


##############################################################################################
##############################################################################################

sample.scale.mbl<- function(paltree){

    ages<-paltree$sampledages
    tree<-paltree$sampledtree

    ttree<-timePaleoPhy(tree, ages, type="mbl", vartime=2, add.term=TRUE ,ntrees=1)

    orderedages<-ages[match(ttree$tip.label, rownames(ages)),1:2]
    orderedages<-orderedages[complete.cases(orderedages),]
    edgetotip<-match(c(1:Ntip(ttree)), ttree$edge[,2])
    tiplengths<-ttree$edge.length[edgetotip]
    endrange<-ttree$root.time-diag(vcv(ttree))
    startrange<-endrange+tiplengths
    orderedages[,1]<-startrange

    result<-list(ttree,orderedages)
    names(result)<-c("STtree", "SAges")
    return(result)

    }

sample.scale.hedman<- function(paltree){
    outgroup.ages<-c(153.8,145,139.8,139.8,132.9,132.9)
    t<-163.5
    ages<-paltree$sampledages
    tree<-paltree$sampledtree
    
    orderedages<-ages[match(tree$tip.label, rownames(ages)),1:2]
    tree <- compute.brlen(tree,runif)
    ttreeall<-Hedman.tree.dates(tree, tip.ages=orderedages[,1], outgroup.ages, resolution=1000, conservative=TRUE, t0=t)
    ttree<-ttreeall$tree
    ext.time<-orderedages[,2]
    
    obs_ranges <- orderedages[, 1] - orderedages[, 2]
    term_id <- ttree$tip.label[ttree$edge[ttree$edge[,2] <= Ntip(ttree), 2]]
    term_add <- sapply(term_id, function(x) obs_ranges[x])
    ttree$edge.length[ttree$edge[, 2] <= Ntip(ttree)] <- ttree$edge.length[ttree$edge[,2] <= Ntip(ttree)] + term_add
    ttree$root.time <- max(orderedages[ttree$tip.label,2]) + min(dist.nodes(ttree)[1:Ntip(ttree), Ntip(ttree) + 1])

    edgetotip<-match(c(1:Ntip(ttree)), ttree$edge[,2])
    tiplengths<-ttree$edge.length[edgetotip]
    endrange<-ttree$root.time-diag(vcv(ttree))
    startrange<-endrange+tiplengths
    orderedages[,1]<-startrange

    result<-list(ttree, orderedages)
    names(result)<-c("STtree", "SAges")
    return(result)
    }

sample.scale.cal3<- function(paltree, anc.wt){
    ages<-paltree$sampledages
    tree<-paltree$sampledtree
    SRres<-getSampRateCont(ages)
    sampRate <- SRres[[2]][2]
    brRate <- extRate <- SRres[[2]][1]
    ttree<-cal3TimePaleoPhy(tree, ages, brRate, extRate, sampRate, anc.wt, ntrees=1)
    
    edgetotip<-match(c(1:Ntip(ttree)), ttree$edge[,2])
    tiplengths<-ttree$edge.length[edgetotip]
    endrange<-ttree$root.time-diag(vcv(ttree))
    startrange<-endrange+tiplengths
    orderedages<-cbind(startrange, endrange)

    result<-list(ttree, orderedages)
    names(result)<-c("STtree", "SAges")
    return(result)
    }

##############################################################################################
#Some example sampling runs
##############################################################################################

Bud.Ext.hed001.Clust<- foreach (p=1:length(baseruns), .packages=c('foreach','doParallel','paleotree','geiger','caper','phangorn')) %do% {
    
    baserun<-baseruns[[p]]

    slicetimeend<- 19.99
    slicetimestart<- 30

    r<-0.01


    foreach (i=1:50, .combine='rbind', .packages=c('foreach','doParallel','paleotree','geiger','caper','phangorn')) %dopar% {
    
        sampled.res<-sampled.sim<-sampled.sim.scaled<-sampled.tree<-sampled.ages<-sampled.ttsltree<-sampled.extincttab<-sampled.inputdata<-NA
        sampled.sim<-sampling(baserun, r)
        try(sampled.sim.scaled<-sample.scale.hedman(sampled.sim))
        try(sampled.tree<-sampled.sim.scaled$STtree)
        try(sampled.ages<-sampled.sim.scaled$SAges)
        
        try(sampled.ttsltree<-timeSliceTree(sampled.tree, sliceTime=slicetimeend, drop.extinct=F, plot=F))

        try(sampled.extincttab<-extincttable(sampled.ttsltree, slicetimestart, slicetimeend),silent=T)
        try(sampled.inputdata<-comparative.data(sampled.extincttab$ext.tab, phy=sampled.ttsltree, species, vcv=T),silent=T)
        try(sampled.res <- phylo.d(data=sampled.inputdata, binvar=extinct, permut=1000),silent=T)

        if (is.na(sampled.res)) {
            ans<-c(NA,NA,NA,NA,NA)
            } else {
            ans<-as.numeric(c(sampled.res$DEstimate, sampled.res$StatesTable[1], sampled.res$StatesTable[2], sampled.res$Pval1, sampled.res$Pval0))
            }
        }
    }

#########################################################################################
#########################################################################################
Bud.Ext.hed1.Clust<- foreach (p=1:length(baseruns), .packages=c('foreach','doParallel','paleotree','geiger','caper','phangorn')) %do% {
    
    baserun<-baseruns[[p]]

    slicetimeend<- 19.99
    slicetimestart<- 30

    r<-0.1

    foreach (i=1:50, .combine='rbind', .packages=c('foreach','doParallel','paleotree','geiger','caper','phangorn')) %dopar% {
    
        sampled.res<-sampled.sim<-sampled.sim.scaled<-sampled.tree<-sampled.ages<-sampled.ttsltree<-sampled.extincttab<-sampled.inputdata<-NA
        sampled.sim<-sampling(baserun, r)
        try(sampled.sim.scaled<-sample.scale.hedman(sampled.sim))
        try(sampled.tree<-sampled.sim.scaled$STtree)
        try(sampled.ages<-sampled.sim.scaled$SAges)
        
        try(sampled.ttsltree<-timeSliceTree(sampled.tree, sliceTime=slicetimeend, drop.extinct=F, plot=F))

        try(sampled.extincttab<-extincttable(sampled.ttsltree, slicetimestart, slicetimeend),silent=T)
        try(sampled.inputdata<-comparative.data(sampled.extincttab$ext.tab, phy=sampled.ttsltree, species, vcv=T),silent=T)
        try(sampled.res <- phylo.d(data=sampled.inputdata, binvar=extinct, permut=1000),silent=T)

        if (is.na(sampled.res)) {
            ans<-c(NA,NA,NA,NA,NA)
            } else {
            ans<-as.numeric(c(sampled.res$DEstimate, sampled.res$StatesTable[1], sampled.res$StatesTable[2], sampled.res$Pval1, sampled.res$Pval0))
            }
        }
    }

#########################################################################################
#########################################################################################
Bud.Ext.hed5.Clust<- foreach (p=1:length(baseruns), .packages=c('foreach','doParallel','paleotree','geiger','caper','phangorn')) %do% {
    
    baserun<-baseruns[[p]]

    slicetimeend<- 19.99
    slicetimestart<- 30

    r<-0.5

    foreach (i=1:50, .combine='rbind', .packages=c('foreach','doParallel','paleotree','geiger','caper','phangorn')) %dopar% {
    
        sampled.res<-sampled.sim<-sampled.sim.scaled<-sampled.tree<-sampled.ages<-sampled.ttsltree<-sampled.extincttab<-sampled.inputdata<-NA
        sampled.sim<-sampling(baserun, r)
        try(sampled.sim.scaled<-sample.scale.hedman(sampled.sim))
        try(sampled.tree<-sampled.sim.scaled$STtree)
        try(sampled.ages<-sampled.sim.scaled$SAges)
        
        try(sampled.ttsltree<-timeSliceTree(sampled.tree, sliceTime=slicetimeend, drop.extinct=F, plot=F))

        try(sampled.extincttab<-extincttable(sampled.ttsltree, slicetimestart, slicetimeend),silent=T)
        try(sampled.inputdata<-comparative.data(sampled.extincttab$ext.tab, phy=sampled.ttsltree, species, vcv=T),silent=T)
        try(sampled.res <- phylo.d(data=sampled.inputdata, binvar=extinct, permut=1000),silent=T)

        if (is.na(sampled.res)) {
            ans<-c(NA,NA,NA,NA,NA)
            } else {
            ans<-as.numeric(c(sampled.res$DEstimate, sampled.res$StatesTable[1], sampled.res$StatesTable[2], sampled.res$Pval1, sampled.res$Pval0))
            }
        }
    }

######################################################################################