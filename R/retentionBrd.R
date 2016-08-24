
retentionBrd <- function(Plist, Gamlist, alpha = 0.05, method = c("Fisher", "TruncatedP"), talpha = .2){
	
	stopifnot(is.list(Plist))
	stopifnot(is.list(Gamlist))
	stopifnot(all.equal(sapply(Gamlist, length), sapply(Plist, length)))	
	stopifnot(Reduce('&', sapply(Plist, is.numeric), TRUE))	
	stopifnot(Reduce('&', sapply(Gamlist, is.numeric), TRUE))
	stopifnot(!prod(sapply(Plist, is.unsorted)))	
	stopifnot(!prod(sapply(Gamlist, is.unsorted)))

	if(missing(method)) method = "Fisher"

	B = as.list(1:length(Plist[[1]]))
	d = length(Plist)
	countD = 1
	
	while(countD < d){
		for( gamIdx in 1:length(B)){
			projGam = B[[gamIdx]]
			if(length(projGam) == countD)
				B[[gamIdx]] = c(B[[gamIdx]], findBorder(Plist, projGam, countD+1, alpha, method, talpha))
		}
		countD = countD + 1
	}
	
	if(sum(sapply(B, length) == d) == 0 ) return(matrix(0,0,d))
	
	if(missing(Gamlist)){
		return(matrix(unlist(B[sapply(B, length) == d]), ncol = d, byrow=T))
	} else
		return(matrix(unlist(lapply(B[sapply(B, length) == d],
						function(x){
							val = c()
							for(i in 1:length(x))
								val = c(val, Gamlist[[i]][x[i]])
							val
						})
					), ncol = d, byrow=T))
}
