findBorder <- function(Plist, projGam, dIdx, alpha = 0.05, method = c("Fisher", "TruncatedP"), talpha = .2){
	if(missing(method)) method = "Fisher"
	if(method == "Fisher") talpha = 1

	.dim = length(Plist)
	
	Ptemp = 1
	for( i in 1:length(projGam))
		Ptemp = multtrunc(Ptemp, Plist[[i]][projGam[i]], talpha)
	Ptemp = multtrunc(Ptemp, Plist[[dIdx]], talpha)
	Ptemp = prodtrunc(Ptemp, (as.numeric(sapply(tail(Plist, (.dim-dIdx)),max))), talpha)

	i1 = 1
	i2 = length(Plist[[dIdx]])	
	
	if(rej(P = Ptemp, idx = i2, d = .dim, alpha, method, talpha = talpha)) return(i2)
	if(!rej(P = Ptemp, idx = i1, d = .dim, alpha, method, talpha = talpha)) return(integer(0))
	
	while(1){
		if(abs(i2-i1) <=1){
			index = ifelse(!rej(Ptemp, i2, .dim, alpha, method, talpha = talpha),  i1, integer(0))
			break
		}
		itemp = (i1+i2)/2
		if(!rej(Ptemp, ceiling(itemp), .dim, alpha, method, talpha = talpha)) i2 = ceiling(itemp)
		if(rej(Ptemp, floor(itemp), .dim, alpha, method, talpha = talpha)) i1 = floor(itemp)
	}
	return(index)
}
