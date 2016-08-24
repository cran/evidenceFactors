truncP <- function(w, L, talpha){

    if (w > talpha) {
        1
    }
    else {
        pr <- 0
        for (k in 1:L) {
            s <- 0:(k - 1)
            term1 <- sum(w * (w <= (talpha^k)) * (((k * log(talpha)) - 
                log(w))^s)/factorial(s))
            term2 <- (talpha^k) * (w > (talpha^k))
            pr <- pr + (choose(L, k) * ((1 - talpha)^(L - k)) * 
                (term1 + term2))
        }
        pr
    }
}
