# Mualem-van Genuchten model describing the soil moisture retention and hydraulic 
# conductivity curves
#
# Usage
# MV(par,h)
#
# Arguments
# h		a vector of suction head values (cm water column)
# par	a list of parameters: thr, ths, alp, n, m, Ks, L
#
# Value
# A data frame with columns h, theta, K and Se
# 
# Author: M.Weynants
###############################################################################
MV <- function(par,h=NULL,theta=NULL){
	if (is.null(theta)){
		Se <- (1+(par$alp * h)^par$n)^(-par$m)
		theta <- par$thr + (par$ths-par$thr)*Se
	} else {
		Se <- (theta - par$thr)/(par$ths-par$thr)
		h <- 1/par$alp * (1 + Se^(-par$n/(par$n-1)))^(1/par$n)
	}
	K <- par$Ks * Se^par$L * (1-(1-Se^(1/par$m))^par$m)^2
	return(data.frame(h,theta,K,Se))
}


