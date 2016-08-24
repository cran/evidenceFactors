multtrunc <- function(inti, x, talpha){
	inti * x^(x <= talpha)
}