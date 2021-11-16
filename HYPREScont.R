# HYPRES continuous PTF
# 
# Author: M. Weynants based on Woesten et al. Geoderma. 1999. 90. 169-185
###############################################################################

HYPREScont <- function(silt, clay, D, OM, topsoil) {

# check input arguments
# silt, clay, OM, D cannot be equal to zero
ind <- silt==0; silt[ind] <- NA
ind <- clay==0; clay[ind] <- NA
ind <- D==0; D[ind] <- NA
ind <- OM==0; OM[ind] <- NA

par = list()

par$thr <- rep(0,length(silt))

par$ths <- 0.7919 + 0.001691 * clay - 0.29619 * D - 0.000001491 * silt^2 +
		0.0000821 * OM^2 + 0.02427 * clay^(-1) + 0.01113 * silt^(-1) +
		0.01472 * log(silt) - 0.0000733 * OM*clay - 0.000619 * D*clay -
		0.001183 * D*OM - 0.0001664 * topsoil*silt

alp <- -14.96 + 0.03135 * clay + 0.0351 * silt + 0.646 * OM +
		15.29 * D - 0.192 * topsoil - 4.671 * D^2 - 0.000781 * clay^2 - 
		0.00687 * OM^2 + 0.449 * OM^(-1) + 0.0663 * log(silt) + 
		0.1482 * log(OM) - 0.04546 * D * silt - 0.4852 * D * OM + 
		0.00673 * topsoil * clay
par$alp <- exp(alp)

n <- -25.23 - 0.02195 * clay + 0.0074 * silt - 0.1940 * OM + 
		45.5 * D - 7.24 * D^2 + 0.0003658 * clay^2 + 0.002885 * OM^2 - 
		12.81 * D^(-1) - 0.1524 * silt^(-1) - 0.01958 * OM^(-1) -
		0.2876 * log(silt) - 0.0709 * log(OM) - 44.6 * log(D) - 
		0.02264 * D*clay + 0.0896 * D*OM + 0.00718 * topsoil*clay
par$n <- exp(n)+1
par$m <- 1-1/par$n

ks <- 7.755 + 0.0352 * silt + 0.93 * topsoil - 0.967 * D^2 - 
		0.000484 * clay^2 - 0.000322 * silt^2 + 0.001 * silt^(-1) - 
		0.0748 * OM^(-1) - 0.643 * log(silt) - 0.01398*D*clay - 
		0.1673 * D*OM + 0.02986 * topsoil*clay - 0.03305*topsoil*silt
par$Ks <- exp(ks)

L <- 0.0202 + 0.0006193 * clay^2 - 0.001136 * OM^2 - 0.2316 * log(OM) - 
		0.03544 * D*clay + 0.00283 * D*silt + 0.0488 * D*OM
par$L <-(exp(L)-1)/(1+exp(L))*10	

return(as.data.frame(par))
}

