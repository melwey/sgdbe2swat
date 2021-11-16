# Estimation of the soil albedo from the soil organic carbon content
# based on SWAT usersoildefault values
# 
# Author: M. Weynants
###############################################################################

albedo <- function(soc){
	a <- rep(NA,length(soc))
	ind <- soc >= 1.45
	a[ind & !is.na(soc)] <- 0.01
	a[!ind & !is.na(soc)] <- 0.23 - 0.24 * soc[!ind & !is.na(soc)]
	return(a)
}
