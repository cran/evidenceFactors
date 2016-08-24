rej <- function(P, idx, d, alpha = 0.05, method = c("Fisher", "Truncated"), talpha = .2){
	if(method == "Fisher")
		return( ifelse(pchisq(-2*log(P[idx]), 2*d, lower.tail = FALSE) < alpha, TRUE, FALSE) )
	if(method == "Truncated"){
		#print(c(P[idx], d, talpha))
		return( ifelse( truncP(P[idx], d, talpha) < alpha, TRUE, FALSE) )
	}
}