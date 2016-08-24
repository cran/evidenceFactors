prodtrunc <- function(inti, x, talpha){
	inti * prod(x^(x <= talpha))
}