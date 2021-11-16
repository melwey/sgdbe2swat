# Convert depth class to a textural change into depth (cm)
# 
# Author: M. Weynants
###############################################################################
textdepchg<-function(TDC){
	output <- rep(0,length(TDC))
	ind <- TDC %in% c(0,5) # No info or no textural change between 20 and 120
	output[ind] <- 350 # maximum depth
	ind <- TDC == 1 # 20-40
	output[ind] <- 30
	ind <- TDC == 2 # 40-60
	output[ind] <- 50
	ind <- TDC == 3 # 60-80
	output[ind] <- 70
	ind <- TDC == 4 # 80-120
	output[ind] <- 100
	ind <- TDC == 6 # 20-60
	output[ind] <- 40
	ind <- TDC == 7 # 60-120
	output[ind] <- 90
	return(output)
}


