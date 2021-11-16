# USLE soil erodibility factor (K)
# 
# Author: M.Weynants, based on SWAT Input chap 22 (.SOL)
###############################################################################

usleK <- function(msilt,mvfs=NA,mc,om,struct,Ks,msand=NA){
	# inputs:
	# msilt: percent silt content (0.002-0.05 mm diameter)
	# mvfs: percent very fine sand (0.05-0.1 mm diameter)
	# mc: percent clay content (<0.002 mm diameter)
	# om: organic matter content (1.72 orgC)
	# struct => cstr: soil structure code
	# Ks (values for all layers or minimum value) => cperm: profile permeability class based on the lowest Ks in the profile
	# msand: percent sand (0.05-2 mm)
	
	if (sum(is.na(c(msilt,mvfs,mc,om,struct,Ks)))==0) {
	# Wischmeier et al. 1971
	cstr <- csoilstr(struct)
	cperm <- profperm(Ks)
	M <- (msilt+mvfs)*(100-mc)
	K <- (0.00021 * M^(1.14) * (12-om)+3.25*(cstr-2)+2.5*(cperm-3))/100
	} else {
	# Williams 1995
	orgC <- om/1.74
	fcsand <- (0.2 + 0.3 * exp(-0.256 * msand * (1-msilt/100)))
	fclsi <- (msilt/(mc + msilt))^(0.3)
	forgc <-(1-(0.25*orgC)/(orgC+exp(3.72-2.95*orgC)))
	fhisand <-(1-(0.7*(1-msand/100)/((1-msand/100)+exp(-5.51+22.9*(1-msand/100)))))
	K <- fcsand * fclsi * forgc * fhisand
	}
}

csoilstr <- function(struct){
	if (!is.character(struct)){
	# convert structure codes from Spadbe to size/shape of structure for swat
	# limitation: in Spadbe, no info on SIZE of structure
	cstr <- rep(NA,length(struct))
	ind <- struct %in% c(1:5,8)
	cstr[ind] <- 4
	# let's say that crumb is medium or coarse granular 
	ind <- struct==7
	cstr[ind] <- 3
	# granular to fine granular
	ind <- struct==6
	cstr[ind] <- 2	
	# and single grain very fine granular
	ind <- struct==9
	cstr[ind] <- 1
	return(cstr)
	} else {
	# convert PTR structure (King et al. 1995) to swat
	cstr <- rep(NA,length(struct))
	ind <- struct == "G" # good
	cstr[ind]<-2 # I can't dinstinguish between very fine granular (1) and fine granular (2)
	ind <- struct == "N" # normal
	cstr[ind] <- 3 # medium or coarse granular
	ind <- struct == "P" # poor; massive, coarse or medium, angular blocky or prismatic
	cstr[ind] <- 4 # blocky, platy, prismlike or massive
	}
}

profperm <- function(Ks){
	# determine profile permeability from layers Ks (mm/hr)
	Kmin <- min(Ks)
	if (Kmin>150) {cperm <- 1}
	if (Kmin<=150 & Kmin>50) cperm<-2
	if (Kmin<=50 & Kmin>15) cperm<-3
	if (Kmin<=15 & Kmin>5) cperm<-4
	if (Kmin<=5 & Kmin>1) cperm<-5
	if (Kmin<=1) cperm<-6
	return(cperm)
}