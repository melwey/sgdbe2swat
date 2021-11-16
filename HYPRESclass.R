# HYPRES class PTF
# units are in cm and days
# 
# Author: M. Weynants based on Woesten et al. Geoderma. 1999. 90. 169-185
###############################################################################

HYPRESclass <- function(sand=0,clay=0,topsoil,OM=0,texture=NULL){
if (is.null(texture)){
# Check input arguments
N <- length(sand)
if (length(clay)!=N | length(topsoil)!=N | length(OM)!= N) {
	simpleError("all input arguments must be vectors of the same length")
}
# Class defintion:
# Coarse
indC <- clay < 18 & sand >= 65
# Medium
indM <- (clay >= 18 & clay < 35  & sand >= 15) | (clay < 18 & sand >= 15 & sand < 65) 
# Medium Fine
indMF <- clay < 35 & sand < 15
# Fine
indF <- clay >= 35 & clay < 60
# Very Fine
indVF <- clay >= 60
# Organic
indO <- (OM >= 18*1.74 & clay>=60) | (OM>= 12*1.74 & clay==0) | 
		(OM >= (12+clay*0.1)*1.74 & clay < 60) 

} else {
if (is.character(texture)){
if (sum(tolower(texture) %in% tolower(c("Coarse","Medium","Medium fine","Fine","Very Fine",
		"C","M","MF","F","VF","Organic","O"))!=0)){simpleError("At least one texture class is not valid")
}
N <- length(texture)
# Class defintion:
# Coarse
indC <- tolower(texture) %in% c("coarse","c")
# Medium
indM <- tolower(texture) %in% c("medium","m")
# Medium Fine
indMF <- tolower(texture) %in% c("medium fine","mf")
# Fine
indF <- tolower(texture) %in% c("fine","f")
# Very Fine
indVF <- tolower(texture) %in% c("veru fine","vf")
# Organic
indO <- tolower(texture) %in% c("organic","o")
} else {
N <- length(texture)
indC <- texture==1;indM<-texture==2;indMF<-texture==3;indF<-texture==4;indVF<-texture==5
indO <- texture==9
}}

if (!is.logical(topsoil)) {topsoil <- as.logical(topsoil)}

# output: list
par <- list()
par$thr <- (indC) * rep(0.025,N) +
		(indM | indMF | indF | indVF | indO) * rep(0.010,N)

par$ths <- (indC & topsoil)*rep(0.403,N) + (indC & !topsoil)* rep(0.366,N) +
		(indM & topsoil)*rep(0.439,N) + (indM & !topsoil)*rep(0.392,N) +
		(indMF & topsoil)*rep(0.430,N) + (indMF & !topsoil)*rep(0.412,N) + 
		(indF & topsoil)*rep(0.520,N) + (indF & !topsoil)* rep(0.481,N) +
		(indVF & topsoil)*rep(0.614,N) + (indVF & !topsoil)*rep(0.538,N)
par$ths[indO] <- 0.766

par$alp <- (indC & topsoil)*rep(0.0383,N) + (indC & !topsoil)* rep(0.0430,N) +
		(indM & topsoil)*rep(0.0314,N) + (indM & !topsoil)*rep(0.0249,N) +
		(indMF & topsoil)*rep(0.0083,N) + (indMF & !topsoil)*rep(0.0082,N) + 
		(indF & topsoil)*rep(0.0367,N) + (indF & !topsoil)* rep(0.0198,N) +
		(indVF & topsoil)*rep(0.0265,N) + (indVF & !topsoil)*rep(0.0168,N)
par$alp[indO] <- 0.0130


par$n <- (indC & topsoil)*rep(1.3774,N) + (indC & !topsoil)* rep(1.5206,N) +
		(indM & topsoil)*rep(1.1804,N) + (indM & !topsoil)*rep(1.1686,N) +
		(indMF & topsoil)*rep(1.2539,N) + (indMF & !topsoil)*rep(1.2179,N) + 
		(indF & topsoil)*rep(1.1012,N) + (indF & !topsoil)* rep(1.0861,N) +
		(indVF & topsoil)*rep(1.1033,N) + (indVF & !topsoil)*rep(1.0730,N)
par$n[indO] <- 1.2039

par$m <- 1-1/par$n

par$L <- (indC & topsoil)*rep(1.25,N) + (indC & !topsoil)* rep(1.25,N) +
		(indM & topsoil)*rep(-2.3421,N) + (indM & !topsoil)*rep(-0.7437,N) +
		(indMF & topsoil)*rep(-0.5884,N) + (indMF & !topsoil)*rep(0.5,N) + 
		(indF & topsoil)*rep(-1.9772,N) + (indF & !topsoil)* rep(-3.7124,N) +
		(indVF & topsoil)*rep(2.5,N) + (indVF & !topsoil)*rep(0.0001,N)
par$L[indO] <- 0.4000

par$Ks <- (indC & topsoil)*rep(60,N) + (indC & !topsoil)* rep(70.000,N) +
		(indM & topsoil)*rep(12.061,N) + (indM & !topsoil)*rep(10.755,N) +
		(indMF & topsoil)*rep(2.272,N) + (indMF & !topsoil)*rep(4.000,N) + 
		(indF & topsoil)*rep(24.800,N) + (indF & !topsoil)* rep(8.500,N) +
		(indVF & topsoil)*rep(15.000,N) + (indVF & !topsoil)*rep(8.235,N)
par$Ks[indO] <- 8.000

return(as.data.frame(par))

}
