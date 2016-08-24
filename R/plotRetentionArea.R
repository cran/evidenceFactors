plotRetentionArea <-
function(retentionBrd, Gamlist, ...){
stopifnot(ncol(retentionBrd) == 2)
stopifnot(length(Gamlist) >= 2)

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

image(x, y, (1-z), col = gray((30:10)/32), xlim = c(x[1], tail(x, 1)), ylim = c(y[1], tail(y, 1)),
xlab = 'Gamma_1', ylab = 'Gamma_2', axes = F, ...)
axis(1, at = x)
axis(2, at = y)
par(xpd=TRUE)
legend(x[1] + (tail(x,1)-x[1])/5, tail(y, 1)+(tail(y,1)-y[1])/10, c("Retention region", "Rejection area"), 
col = gray(c(30,10)/32),  pch = c(15, 15),
horiz = T, ...)
par(xpd=FALSE)
}
