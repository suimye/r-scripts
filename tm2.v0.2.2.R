tm2 = function(data,k,alpha,perm=10000){
	cat("Tietjen-Moore test \n (original code from http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm) \n\n")
	cat("###This is a wrapper of original T-M test from above.\n###")
	cat("###Modified by Suimye, 26 March 2013.###\n\n")
	cat("Please refer to the original website described above!\n")
	cat("Default number of randomization is 10000.\n")

	# Computing critical value.
	cat("Preparating the simulation data.....")
	SimData <- matrix(rnorm(length(data)*perm),perm,length(data))
	cat(".")
	cat("Done\n")


	#Original tm.test code
	tm = function(y,k2){
			n = length(y)
			r = abs(y - mean(y))
			df = data.frame(y,r)
			dfs = df[order(df$r),]
			klarge = c((n-k2+1):n)
			subx = dfs$y[-klarge]
			ksub = (subx - mean(subx))**2
			all = (df$y - mean(df$y))**2
			ek = sum(ksub)/sum(all)	
			return(ek)	
	}
 	result <- NULL
	cname <- NULL

	
	#the number of putative outliners
 	for(i in 1:k){
		cname <- cbind(cname,paste("k",i,sep=""))
		sim <- apply(SimData,1,tm,k2=i)
		sample <- tm(data,i)
		result <- cbind(result,rbind(quantile(sim,alpha),sample))
	}

		colnames(result) <- cname
		rownames(result) <- c("simulation","sample")
  	cat("Result of the T-W test statistic (criteria",alpha,")\n")
  	return(result)
}
