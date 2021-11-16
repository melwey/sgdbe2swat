# Subsoil organic carbon content as a function of topsoil OC and land cover.
# Based on Hiederer, 2009. Distribution of Organic Carbon in Soil Profile Data.
# EUR 23980 EN
# 
# Author: M. Weynants, 2011
###############################################################################


OCsub <- function(OCtop,LC,TEXT1,TEXT2){
	value <- rep(NA,length(OCtop))
	# organic soils
	ind <- TEXT1==9 & TEXT2 %in% c(0,9)
	value[ind]<-OCtop[ind]*1.1
	# mineral soils
	ind <- (TEXT1!=9 | (TEXT1==9 & TEXT2!=9)) & LC == "C"
	value[ind] <- OCtop[ind]*0.7
	ind <- (TEXT1!=9 | (TEXT1==9 & TEXT2!=9)) & LC!="C"
	value[ind] <- OCtop[ind]*0.3
	return(value)
}
