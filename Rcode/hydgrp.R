# Hydrological soil group for SWAT
# 
# Author: G.Tóth (SPSS script translated by M. Weynants)
# A2/B2 overlap in GT's script : see what is wrong
###############################################################################
hydgrp <- function(top,sub,IL) {

hyd <- rep("?",length(IL))
#### my version based on depth to impermeable layer (IL) and HYPRES Ksat (based on texture classes)

# Based on the values defining HSG given in Chapter 7 of National Engineering Handbook, 
# none of the HYPRES Ks values falls into A class. However let's say that coarse texture profile does
# Limits of HSG least transmissive layer (mm/hr) are set to:
# w/o IL (IL%in%c(1,2): A>24; 24>=B>9; 9>=C>1.4; D<1.4
# w 50<IL<100 (IL%in%c(3,4)): B>24; 24>=C>2.4
# D: IL<50 OR Ks<2.4
	
	indA <- (IL%in%c(1,2) & (top==1 & sub%in%c(0,1) | 
					(top==0 & sub==1))) |
			(IL==0 & (top%in%c(0,1) & sub==1)) |
			(IL==0 & top==1 & sub==0)
	
	indB <- (IL %in% c(3) & (top==1 & sub%in%c(0,1))) | 
			(IL%in%c(0,1,2) &  top==4 & sub==1 ) |
			(IL%in%c(1,2) & top==4 & sub==0)
	
	indC <- (IL%in%c(0,3) & ((top%in%c(2,4,5,9) & sub!=3) | 
					(!(top%in%c(0,3)) & sub%in%c(2,4,5,9)) | 
					(!top%in%c(0,1,3) & sub==0) |
					(top==0 & sub%in%c(2,4,5,9))) ) |
			(IL%in%c(0,1,2) & ( top%in%c(2,5,9) | 
					(!(top%in%c(0,3)) & sub%in%c(2,3,4,5,9)) |
					(top==0 & sub%in%c(2,3,4,5,9)) |
					(top%in%c(2,5) & sub==0))) |
			(IL%in%c(1,2) & top==0 & sub==0)
	
	indD <- (IL==4) | (top==3) | (top==0 & sub==3) | 
			(IL==3 & top==0 & sub==0) | 
			(IL==3 & sub==3)
	
	indNA <- top==0 & sub==0 & IL==0
	
	a<-cbind(indA,indB,indC,indD,indNA)
	b<-rowSums(a)
	c<-cbind(as.vector(IL),as.vector(top),as.vector(sub))
	ind<-which(b>1)

hyd[indA] <- "A";hyd[indB]<-"B";hyd[indC]<-"C";hyd[indD]<-"D";hyd[indNA]<-NA
return(hyd)
#### Gergely's version
#indA <- ((IL == 0 | IL == 1) & TEXTSRFDOM == 1 & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2)) | 
#((IL == 2) & (TEXTSRFDOM == 1  | TEXTSRFDOM == 2  | TEXTSRFDOM == 4  |  TEXTSRFDOM == 5 ) & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2)) | 
# ((IL == 3) & TEXTSRFDOM == 1 & TEXTSUBDOM == 1)
#hyd[indA] <- "A"
#
#indB <- ((IL == 0 | IL == 1) &  (TEXTSRFDOM == 2  | TEXTSRFDOM == 4) & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2)) | 
#((IL == 2) & (TEXTSRFDOM == 1  | TEXTSRFDOM == 2  | TEXTSRFDOM == 4) & (TEXTSUBDOM == 4  |  TEXTSUBDOM == 5)) | 
# ((IL == 3) & TEXTSRFDOM == 1 & TEXTSUBDOM == 2) | 
# ((IL == 3) & (TEXTSRFDOM == 2  | TEXTSRFDOM == 4) & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2))
#hyd[indB] <- "B"
#
#indC <- ((IL == 0 | IL == 1) &  TEXTSRFDOM == 3 & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2  |  TEXTSUBDOM == 3 |  TEXTSUBDOM == 4 |  TEXTSUBDOM == 5)) | 
#((IL == 0 | IL == 1) &  (TEXTSRFDOM == 1  | TEXTSRFDOM == 2 | TEXTSRFDOM == 4) & (TEXTSUBDOM == 3  |  TEXTSUBDOM == 4  |  TEXTSUBDOM == 5)) | 
#((IL == 2) & TEXTSRFDOM == 3 & (TEXTSUBDOM == 1  | TEXTSUBDOM == 2  | TEXTSUBDOM == 3  | TEXTSUBDOM == 4  |  TEXTSUBDOM == 5)) | 
#((IL == 2) & (TEXTSRFDOM == 1  | TEXTSRFDOM == 2  | TEXTSRFDOM == 4) & TEXTSUBDOM == 3) | 
# ((IL == 3)& TEXTSRFDOM == 3 & (TEXTSUBDOM == 1  | TEXTSUBDOM == 2  | TEXTSUBDOM == 3  | TEXTSUBDOM == 4  |  TEXTSUBDOM == 5)) | 
# ((IL == 3) &  (TEXTSRFDOM == 1  | TEXTSRFDOM == 2 | TEXTSRFDOM == 4) & (TEXTSUBDOM == 3  |  TEXTSUBDOM == 4  |  TEXTSUBDOM == 5))
#hyd[indC] <- "C"
#
#indD <- ((IL == 0 | IL == 1) &  TEXTSRFDOM == 5 & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2  |  TEXTSUBDOM == 3 |  TEXTSUBDOM == 4 |  TEXTSUBDOM == 5)) | 
#((IL == 2) &  TEXTSRFDOM == 5 & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2  |  TEXTSUBDOM == 3 |  TEXTSUBDOM == 4 |  TEXTSUBDOM == 5)) | 
# ((IL == 3) &  TEXTSRFDOM == 5 & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2  |  TEXTSUBDOM == 3 |  TEXTSUBDOM == 4 |  TEXTSUBDOM == 5)) | 
# ((IL == 4) &  (TEXTSRFDOM == 1  | TEXTSRFDOM == 2 | TEXTSRFDOM == 3 | TEXTSRFDOM == 4 | TEXTSRFDOM == 5) & (TEXTSUBDOM == 1  |  TEXTSUBDOM == 2  |  TEXTSUBDOM == 3 |  TEXTSUBDOM == 4 |  TEXTSUBDOM == 5))
#hyd[indD] <- "D"
#
#indA2 <- (IL == 0  & TEXTSRFDOM == 1 & TEXTSUBDOM == 1) | 
#(IL == 1  & TEXTSRFDOM == 1 & TEXTSUBDOM == 9) | 
#(IL == 2  & TEXTSRFDOM == 2 & TEXTSUBDOM == 9) | 
#(IL == 1  & TEXTSRFDOM == 9 & TEXTSUBDOM == 2) | 
#((IL == 1  | IL == 2)  &  (TEXTSRFDOM == 1  | TEXTSRFDOM == 2) & TEXTSUBDOM == 0)
#
#indB2 <- (IL == 0  & TEXTSRFDOM == 2 & TEXTSUBDOM == 2) | 
#(IL == 1  & TEXTSRFDOM == 2 & TEXTSUBDOM == 9) | 
#(IL == 2  & TEXTSRFDOM == 2 & TEXTSUBDOM == 9) | 
#(IL == 1  & TEXTSRFDOM == 9 & TEXTSUBDOM == 2) | 
#(IL == 0 &  (TEXTSRFDOM == 1  | TEXTSRFDOM == 2) & TEXTSUBDOM == 0)
#
#indC2 <- (IL == 2  & TEXTSRFDOM == 0 & TEXTSUBDOM == 0) | 
#(IL == 3  & TEXTSRFDOM == 9 & (TEXTSUBDOM == 1 | TEXTSUBDOM == 2)) | 
#(IL == 2  & TEXTSRFDOM == 9 & TEXTSUBDOM == 4) | 
#(IL == 1  & TEXTSRFDOM == 0 & TEXTSUBDOM == 0) | 
#(IL == 1  & TEXTSRFDOM == 0 & TEXTSUBDOM == 2) | 
#(IL == 3  & TEXTSRFDOM == 1 & TEXTSUBDOM == 9) | 
#(IL == 3  & TEXTSRFDOM == 2 & TEXTSUBDOM == 9) | 
#(IL == 1  & TEXTSRFDOM == 3 & TEXTSUBDOM == 9) | 
#(IL == 0  & TEXTSRFDOM == 4 & TEXTSUBDOM == 4) | 
#(IL == 4  & TEXTSRFDOM == 4 & TEXTSUBDOM == 9) | 
#(IL == 1  & TEXTSRFDOM == 9 & TEXTSUBDOM == 0) | 
#(IL == 2  & TEXTSRFDOM == 9 & TEXTSUBDOM == 1) | 
#(IL == 3  & TEXTSRFDOM == 9 & TEXTSUBDOM == 3) | 
#(IL == 1  & TEXTSRFDOM == 9 & TEXTSUBDOM == 4) | 
#(IL == 2  & TEXTSRFDOM == 9 & TEXTSUBDOM == 1) | 
#(IL == 3  & TEXTSRFDOM == 9 & TEXTSUBDOM == 3) | 
#(IL == 1  & TEXTSRFDOM == 9 & TEXTSUBDOM == 4) | 
#(IL == 1  & TEXTSRFDOM == 9 & TEXTSUBDOM == 9) | 
#((IL == 3  | IL == 4) & (TEXTSRFDOM == 1 | TEXTSRFDOM == 2) & TEXTSUBDOM == 0) | 
#((IL == 0  | IL == 1  | IL == 2) & (TEXTSRFDOM == 3 | TEXTSRFDOM == 4 | TEXTSRFDOM == 5) & TEXTSUBDOM == 0) | 
#(IL == 1  & TEXTSRFDOM == 0 & TEXTSUBDOM == 1)
#
#indD2 <- ((IL == 3 |  IL == 4)  & TEXTSRFDOM == 0 & TEXTSUBDOM == 0) | 
# (IL == 3  & TEXTSRFDOM == 0 & TEXTSUBDOM == 3) | 
#(IL == 4  & TEXTSRFDOM == 1 & TEXTSUBDOM == 9) | 
#(IL == 1  & TEXTSRFDOM == 4 & TEXTSUBDOM == 9) | 
#(IL == 3  & TEXTSRFDOM == 9 & TEXTSUBDOM == 0) | 
#(IL == 4  & TEXTSRFDOM == 9 & TEXTSUBDOM == 1) | 
#(IL == 4  & TEXTSRFDOM == 9 & TEXTSUBDOM == 9) | 
#((IL == 3 | IL == 4) &  (TEXTSRFDOM == 3  | TEXTSRFDOM == 4 | TEXTSRFDOM == 5) & TEXTSUBDOM == 0)
#
#indNA <- (IL == 0  & TEXTSRFDOM == 0 & TEXTSUBDOM == 0) | 
#(IL == 0  & TEXTSRFDOM == 9 & TEXTSUBDOM == 0) | 
#(IL == 0  & TEXTSRFDOM == 9 & TEXTSUBDOM == 9) 
#hyd[indNA] <- "SYSMIS"
#return(hyd)
}
