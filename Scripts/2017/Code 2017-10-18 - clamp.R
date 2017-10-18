clamp <- function(physio, meteo, fossils, method="new",smooth_method="gp"){
  require(vegan)
  require(mgcv)
  if(any(colnames(fossils)!=colnames(physio))) stop("colnames not identical in physio and fossils datasets.")
  if(!method%in%c("new","classic")) stop("method not implemented.")
  A <- cca(physio ~ as.matrix(meteo))
  n <- A$CCA$rank
  if(length(A$CCA$alias)){
    alias <- gsub("as.matrix\\(meteo\\)","",A$CCA$alias)
    meteo <- meteo[,!colnames(meteo)%in%alias]
    cat(sprintf("Some meteorological parameters were ignored:\n%s\n",paste(alias,collapse=", ")))
  }
  f_cca <- predict(A, fossils, "wa")
  
  clamp <- matrix(0, ncol=n, nrow=nrow(fossils), dimnames=list(rownames(fossils),colnames(meteo)))
  stdev <- double(n)
  names(stdev) <- colnames(meteo)
  fitted <- list()
  
  if(method=="classic"){
    env <- A$CCA$biplot[,1:4]
    site <- A$CCA$wa[,1:4]
    scores <- t(apply(site,1,function(x)apply(env,1,function(y)sum(x*y)/sqrt(sum(y^2)))))
    score_fossil <- t(apply(f_cca[,1:4,drop=FALSE],1,function(x)apply(env,1,function(y)sum(x*y)/sqrt(sum(y^2)))))
    for(i in 1:nrow(env)){
      s <- scores[,i]
      s2 <- s^2
      fitted[[i]] <- lm(meteo[,i]~s+s2)
      X <- fitted[[i]]$coef
      predicted <- X[1]+X[2]*s+X[3]*s2
      stdev[i] <- sd(sqrt((predicted - meteo[,i])^2))
      clamp[,i] <- X[1]+X[2]*score_fossil[,i]+X[3]*score_fossil[,i]^2
    }
  }else if(method=="new"){
    for(i in 1:n){
      w <- as.data.frame(A$CCA$wa)
      f <- paste(sprintf("meteo[,%i] ~ ",i),paste(sprintf("s(%s, bs='%s')",colnames(w),smooth_method),collapse=" + "))
      fitted[[i]] <- gam(as.formula(f),data=w)
      clamp[,i] <- predict.gam(fitted[[i]],as.data.frame(f_cca))
      stdev[i] <- sd(sqrt((fitted[[i]]$fitted.values - meteo[,i])^2))
    }
  }
  
  names(fitted) <- colnames(meteo)
  list(estimates=clamp, stdev=stdev, cca=A, fit=fitted)
}
