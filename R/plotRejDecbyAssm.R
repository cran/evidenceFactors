plotRejDecbyAssm <-
function(retentionBrd, Gamlist, Plist, alpha = 0.05, ...){
stopifnot(ncol(retentionBrd) == 2)
stopifnot(length(Gamlist) >= 2)
stopifnot(is.list(Plist))
stopifnot(is.list(Gamlist))
stopifnot(all.equal(sapply(Gamlist, length), sapply(Plist, length)))
stopifnot(Reduce('&', sapply(Plist, is.numeric), TRUE))
stopifnot(Reduce('&', sapply(Gamlist, is.numeric), TRUE))
stopifnot(!prod(sapply(Plist, is.unsorted)))
stopifnot(!prod(sapply(Gamlist, is.unsorted)))

x = Gamlist[[1]]
y = Gamlist[[2]]
z = matrix(0, length(x), length(y))

colnames(z) = y
rownames(z) = x
brd2 = retentionBrd[,2]
names(brd2) = retentionBrd[,1]

for(gam in as.character(x)){
if(length(brd2[gam]) > 0)
z[gam, y > as.numeric(brd2[gam])] = 1
}

z = 1-z
z1 = z
z1 = z*(z1 + 1*(Plist[[1]] < alpha))
z1 = z*(t(t(z1) + 2*(Plist[[2]] < alpha)))

image(x, y, z1, col = gray(c(30, 25, 16, 20, 8)/32), xlim = c(x[1], tail(x, 1)), ylim = c(y[1], tail(y, 1)),
xlab = 'Gamma_1', ylab = 'Gamma_2', axes = F, ...)
axis(1, at = x)
axis(2, at = y)
par(xpd=TRUE)
legend(x[1], tail(y, 1)+(tail(y,1)-y[1])/10, 
c("Rej: Assm. 1, 2", "Rej: Assm. 1", "Rej: Assm. 2", "Rej: w/o Attr"), 
col = gray(c(8, 16, 20, 25)/32),  pch = rep(15, 5),
horiz = T, ...)
par(xpd=FALSE)
}
