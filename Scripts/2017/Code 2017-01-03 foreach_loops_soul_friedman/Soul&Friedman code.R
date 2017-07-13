library(phytools)
library(caper)
library(foreach)

######Function to perform the method
FandP<-function(timetree, starttime, endtime){
    
    ttsltree<-NA
    try(ttsltree<-timeSliceTree(timetree, sliceTime=endtime, drop.extinct=F, plot=FALSE),silent=TRUE)
    if (is.na(ttsltree[1])){
        ans<-c(NA,NA,NA,NA,NA)
        } else {
        ttsltree$node.label<-NA
        which_extinct<-extincttable(tree=ttsltree, starttime, endtime)
        data<-as.data.frame(cbind(which_extinct$ext.tab,rownames(which_extinct$ext.tab)))
        inputdata<-comparative.data(data, phy=ttsltree, V4, vcv=T)
        res<-NA
        try(res <- phylo.d(data=inputdata, binvar=extinct, permut=1000), silent=FALSE)
        if (is.na(res[2])==FALSE){
           #stores D, #extinct, #survivors, Brownian p value and random p value
           ans<-as.numeric(c(res$DEstimate, res$StatesTable[1], res$StatesTable[2], res$Pval1, res$Pval0))
           } else {
           ans<-c(NA,NA,NA,NA,NA)
           }
        }
}


######Function to put results in a table

extincttable<-function(tree, starttime, endtime){
    
    ext.tab<-matrix(ncol=3, nrow=Ntip(tree))
    edgetotip<-match(c(1:Ntip(tree)), tree$edge[,2])
    tiplengths<-tree$edge.length[edgetotip]
    endrange<-tree$root.time-diag(vcv(tree))
    startrange<-endrange+tiplengths
	
    rownames(ext.tab)<-tree$tip.label
    ext.tab[,1]<-startrange
    ext.tab[,2]<-endrange

    for (i in 1:Ntip(tree)){
        keytime<-round(ext.tab[i,2],4)
        if (keytime<=endtime){
            ext.tab[i,3]<-0
            }
        if (keytime>endtime & keytime<=starttime){
            ext.tab[i,3]<-1
            }
        if (keytime>starttime){
            ext.tab[i,3]<-2
            }
        }
    namettd<-names(which(ext.tab[,3]==2))
    ttd<-match(namettd, rownames(ext.tab))
    if (length(ttd)>0){
        ext.tab<-ext.tab[-ttd,]
        }
    colnames(ext.tab)<- c("FAD", "LAD", "extinct")
    result<-list(ext.tab, tree)
    names(result)<-c("ext.tab", "tree")
    return(result)
}



######Script to carry out function on multiple trees and time bins
timeslicebins<-  #####matrix of begining and end times of time bins
ttrees<-   #####object containing all the trees you want


results <- foreach (m = 1:length(ttrees), .packages=c('foreach','paleotree','caper','geiger','phangorn')) %do% {
    timetree<-ttrees[[m]]
    foreach(f=1:(nrow(timeslicebins)), .combine='rbind') %do% FandP(timetree, starttime=timeslicebins[f,1], endtime=timeslicebins[f,2])
}

