40th Meeting - 13th of February 2018
----
Attendance: 5

First we tried to save a ggplot as an object inside a loop (Don't do that), then we tried to do missing data imputation based on a PCA (can't do that - no vcov possible for prcomp) and finally we tried to see what was wrong with a CSL file read in RMarkdown (No idea). All in all, a productive meeting...


-----
**Update 16-02-2018:**

An explanation at what went wrong in Antoine's code with the ggplot for-loop.

At the last code clinic, Antoine had the following piece of code which was intending to save all his ggplots into a list but when trying to plot them afterwards, the data were wrong:

	logplot <- c()
	for(i in 1:30){
	  d <- cbind(log(meso_sub[,c(3, i+3)]), species=meso_sub$species)
	  mod <- lm(d[,2]~ d[,1])
	  logplot$lm[[i]] <- mod
	  logplot$plot[[i]] <- ggplot(data=d, aes(d[,1], d[,2]), na.rm=T) +
	    geom_point(aes(colour=species), size=2) +
	    labs(x="log(femur length)",
	         y=paste0("log(", names(d[,2]), ")")) +
	    scale_colour_manual(name="species", values=colSp) +
	    theme_bw() +
	    geom_abline(intercept=mod[[1]][1], slope=mod[[2]][1], col="firebrick") +
	    geom_abline(intercept=mod[[1]][1], slope=1, col="grey35")
	}

Antoine just wrote a piece of code that ended up working correctly:

	logplot <- mapply(function(x, n){
	  mod <- lm(log(x) ~ log(df$femur_lgth))
	  p <- qplot(log(df$femur_lgth),log(x), colour=df$species, size=I(2), na.rm=T) +
	    labs(x="log(femur length)", y=paste0("log(", n, ")")) +
	    #+ geom_text(aes(label=rownames(df), hjust=-.05))
	    scale_colour_manual(name="species", values=colSp) +
	    theme_bw() +
	    geom_abline(intercept=mod$coef[1], slope=mod$coef[2], col="red")
	  message(n)
	  return(p)
	}, x=df[,-c(1:3)], n=names(df)[-c(1:3)], SIMPLIFY = F)


Why does the last one works and the first didn't.
qplot is just a wrapper around ggplot, so it's not that.
The difference lies in three key aspects:
1) the argument `data` is not explicitely called in qplot.
2) the aesthetic called in the first attempt refers to the columns as d[,1] and d[,2] while their actual names are femur_length and something else similar for the second one.
3) the second attempt used mapply instead of a for-loop.

So here's what happened:

The saved object ggplot has three elements of interest here:
1) One called `data` that save the object passed to argument `data`.
2) One called `plot_env` that save the name of the environment in which the call was made.
3) One called `mapping` that save the aesthetic mapping "as it was called in the function call"

So in the first attempt, the mapping refers to columns "d[,1]"" and "d[,2]" while data is a nameless data frame which has columns called femur_length etc. So the functions search for a column called "d[,1]" and doesn't find anything, so what it does is look-up "plot_env" which in this case, because it was made in a for-loop, just says "R_Global_Env", i. e. the environement in which we're working, look for a variable called d and do the plot using it... except we changed d at each iteration so it uses the last version of d which is wrong.
In the second attempt however, the aesthetic mapping says that we re plotting df$femur_lgth vs x. It looks at `data` which contains nothing since there was no explicit calls to `data`, so it then looks at plo_env. And here because the call was in `mapply`, it finds a unique environment that only existed for that particular iteration of the loop (true for every `*apply` functions), goes into that environment, finds `x` which is the correct one as it only existed in that small environment and thus plots the correct plot.
