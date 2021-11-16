# Create SWAT.SOL input layer from European Soil Database
# 
# Author: M. Weynants, 2011
# Last update: 2012/02/29
###############################################################################

require("foreign");require('ggplot2')

# Import .dbf table

# STU: Soil Typological Units descriptions
fname <- file.path("../","sgdbe","stu_sgdbe.dbf")
stu <- read.dbf(fname,as.is=TRUE) # 35 attributes, 5262 rows
# SMU: Soil Mapping Units: dominant values (if several stu's in one smu share 
# the same value, purity changes), purity and confidence
fname <- file.path("../","sgdbe","smu_sgdbe.dbf")
smu <- read.dbf(fname,as.is=TRUE) # 119 attributes (smu, nonsoil, 39x3), 3856 rows
# STU inferred properties + confidence level
fname <- file.path("../","sgdbe","stu_ptrb.dbf")
stu.ptrb <- read.dbf(fname,as.is=TRUE) # 77 attributes: stu, inferred properties + CL
# SMU (dominant value) inferred properties
fname <- file.path("../","sgdbe","smu_ptrdb.dbf")
smu.ptrb <- read.dbf(fname,as.is=TRUE) # 109 attributes: smu, 36x3 properties
# SMU to which each STU is affected
fname <- file.path("../","sgdbe","stuorg.dbf")
stu.org <- read.dbf(fname,as.is=TRUE) #
# Estimated profiles/horizons from SPADBE
# each profile is associated to one SMU and one STU
fname <- file.path("../../ESDB/ESDB_v2_CD/esdb/Spadbe/est/est_prof.dbf")
est.prof <- read.dbf(fname,as.is=TRUE) # 58 attributes, 588 rows
fname <- file.path("../../ESDB/ESDB_v2_CD/esdb/Spadbe/est/est_hor.dbf")
est.hor <- read.dbf(fname,as.is=TRUE) # 34 attributes
# link type between profiles and stu's: 1 explicit; 2 implicit
fname <- file.path("../../ESDB/ESDB_v2_CD/esdb/Spadbe/est/eststulk.dbf")
est.stulk <- read.dbf(fname,as.is=TRUE)

# source MyFunctions
source("HYPRESclass.R")
source("HYPREScont.R")
source("MV.R")
# source the local functions
source("hydgrp.R")
source("solzmx.R")
source("textdepchg.R")
source("av.text.R")
source("albedo.R")
source("usleK.R")
source("OCsub.R")

# Is there an estimated profile for each stu?
stup <- est.stulk$STU
stug <- stu$STU
sum(!(stug%in%stup)) #=> 5023 stu are NOT associated with a profile!!! only 239 are!
# and I don't have the links for the measured profiles...

# clean incorrect values
stu$IL[stu$IL==9]<-0

# SWAT inputs:
#FIELD		DEF
#SNAM		Soil name
#HYDGRP		Soil Hydrologic Group
#SOL_ZMX	Maximum rooting depth of soil profile.
#ANION_EXCL	Fraction of porosity (void space) from which anions are excluded.
#SOL_CRK	[OPTIONAL] Crack volume potential of soil.
#TEXTURE	[OPTIONAL] Texture of soil layer.
#NLAYERS	Number of layers in the soil.
#SOL_Z1		Depth from soil surface to bottom of layer.
#SOL_BD1	Moist bulk density.
#SOL_AWC1	Available water capacity of the soil layer.
#SOL_K1		Saturated hydraulic conductivity.
#SOL_CBN1	Organic carbon content .
#CLAY1		Clay content.
#SILT1		Silt content.
#SAND1		Sand content.
#ROCK1		Rock fragment content.
#SOL_ALB1	Moist soil albedo.
#USLE_K1	USLE equation soil erodibility (K) factor.
#SOL_EC1	[Not currently active] Electrical conductivity.
#NUMLAYER	The layer being displayed.

# data stored in swat.sol (format of usersoil table in SWATxxxx.mdb)
# SNAM: stu code
swat.sol <- data.frame(MUID=0,SEQN=0,SNAM=stu$STU,
		S5ID=0,CMPPCT=0)
nr<-nrow(swat.sol)
swatZ <- solzmx(roo=stu$ROO,il=stu$IL,wr=stu$WR,dr=stu.ptrb$DR,aglim=stu$AGLIM1)
# modify swatZ for non soil (+ hidden non soil)
no.soil <- as.numeric(as.factor(stu[,5]))%in%(1:6) & !is.na(stu[,5])
temp<-t(swatZ[no.soil,]);temp[,]<-c(30,30,0,1);swatZ[no.soil,]<-t(temp)
# stu 70019
swatZ[stu$STU==70019,]<-c(60,30,60,2)
swat.sol$NLAYERS <- swatZ$nlayers
# hydrological group
HYDGRP <- hydgrp(stu$TEXTSRFDOM,stu$TEXTSUBDOM,stu$IL)
# treat is.na(HYDRGRP)
# no soil => HYDGRP=D
HYDGRP[no.soil]<-"D"
# stu 70019 => HYDGRP=C
HYDGRP[stu$STU==70019]<-"C"
# other NA????
ind.na <- is.na(HYDGRP)
swat.sol$HYDGRP<-HYDGRP
# sol_ZMX
swat.sol$SOL_ZMX <- swatZ$sol_zmx*10
swat.sol$ANION_EXCL <- rep(0.5,nr) 		# optional. default value
swat.sol$SOL_CRK <- rep(0.5,nr) 		# optional # makes sense for clayey soils and soils senisitive to change of volume (vertisol etc.)
# there's a ptr output MIN_TOP. classes MS, S, TO
swat.sol$TEXTURE <- rep(NA,nr)			# not processed

# usersoil contains information for a maximum of 10 layers
for (iL in 1:10){
	for (iF in c("SOL_Z","SOL_BD","SOL_AWC","SOL_K","SOL_CBN","CLAY","SILT",
			"SAND","ROCK","SOL_ALB","USLE_K","SOL_EC")){
		eval(parse(text=paste("swat.sol$",iF,as.character(iL)," <- rep(0,nr)",sep="")))
	}
}

# for each stu extract SWAT input from ptr and hypres and sroe in tmp
tmp <- matrix(0,nrow(swat.sol),24)

# topsoil: SOL_ZMX cm adapted as a function of TEXTDEPCHG
tmp[,1]<-swatZ$sol_z1*10
#tmp1 <- textdepchg(stu$TEXTDEPCHG)*10

# BD: ptr PD_TOP
PDclass <- stu.ptrb$PD_TOP
tmp[(PDclass=="L"),2]<-1.1
tmp[(PDclass=="M"),2]<-1.6
tmp[(PDclass=="H"),2]<-1.9

# SOL_CBN ptr OC_TOP
CBNclass <- stu.ptrb$OC_TOP
CBNclass <- factor(CBNclass, levels = c("V","L","M","H"))
tmp[(CBNclass=="V"),5]<-0.1 # <1%
tmp[(CBNclass=="L"),5]<-1.5 # 1-2%
tmp[(CBNclass=="M"),5]<-4   # 2-6%
tmp[(CBNclass=="H"),5]<-10  # >6%
# modified ptr21, but needs agjustement for climate and LULC
#CBNclass <- ptr21mod(stu,stu.ptrb)
#tmp[(CBNclass==1),5]<-0.1 # <1%
#tmp[(CBNclass==2),5]<-1.5 # 1-2%
#tmp[(CBNclass==3),5]<-4   # 2-6%
#tmp[(CBNclass==4),5]<-10  # 6-18%
#tmp[(CBNclass==5),5]<-24  # 18-30%
#tmp[(CBNclass==6),5]<-35  # >30%
# BUT in swat, max OC is 10 anyway...  

# sgdbe dominant surface texture 
txt <- as.numeric(as.character(stu$TEXTSRFDOM)); txt[txt==0]<-stu.ptrb$TEXT[txt==0];txt[txt==8]<-9
# stu 70011 is a HSge => 9
txt[stu$STU==70011]<-9
# stu 70019 set to medium texture
txt[stu$STU==70019]<-2
# set medium texture to organic soils 
txto <- txt; txto[txt==9]<-2
texture <- av.text(txto)
tmp[,6] <- texture$clay # SOL_CLAY
tmp[,7] <- texture$silt # SOL_SILT
tmp[,8] <- texture$sand # SOL_SAND

# organic soils: OC>18%
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# ptr VS (volume of stones)
tmp[,9] <- as.numeric(as.vector(stu.ptrb$VS)) # SOL_ROCK

# AWC and SOL_K obtained from Hypres class ptf
parMV <- HYPRESclass(texture=txt,topsoil=rep(1,nrow(stu)))
FC <- MV(parMV,10^(2.5));WP <- MV(parMV,10^(4.2))
tmp[,3] <- FC$theta-WP$theta # AWC
tmp[,4] <- parMV$Ks/2.4 # SOL_K (mm/hr)

tmp[,10] <- albedo(tmp[,5])# SOL_ALB roughly estimated from SOL_CBN

# USLE_K first computed based on SWAT documentation, but outpu too low
#tmp[,11] <- usleK(msilt=tmp[,7],mvfs=NA,mc=tmp[,6],om=tmp[,5]*1.74,#
#		struct=7,Ks=tmp[,4],msand=tmp[,8]) # USLE_K
# usleK could be based on PTR ERODIBILITY (623)...
# values based on http://www.iwr.msu.edu/~ouyangda/rusle/k_factor.htm 
tmp[stu.ptrb$ERODIBI==0,11]<-0.05
tmp[stu.ptrb$ERODIBI==1,11]<-0.05
tmp[stu.ptrb$ERODIBI==2,11]<-0.15
tmp[stu.ptrb$ERODIBI==3,11]<-0.30
tmp[stu.ptrb$ERODIBI==4,11]<-0.4
tmp[stu.ptrb$ERODIBI==5,11]<-0.6

#MV<-HYPREScont(silt=est.hor$TEXT_20+est.hor$TEXT_50,clay=est.hor$TEXT_2,
#		D=est.hor$BD,OM=est.hor$OM,
#		topsoil=ifelse(est.hor$HOR_NUM==1,1,0))
#ind.hor<-!is.na(MV$Ks)
#tst <- usleK(msilt=est.hor$TEXT_20[ind.hor]+est.hor$TEXT_50[ind.hor],
#		mvfs=est.hor$TEXT_200[ind.hor],mc=est.hor$TEXT_2[ind.hor],
#		om=est.hor$OM[ind.hor],struct=est.hor$STRUCT[ind.hor],Ks=MV$Ks[ind.hor]/2.4)
#msilt=est.hor$TEXT_20+est.hor$TEXT_50
#mvfs=est.hor$TEXT_200;mc=est.hor$TEXT_2
#om=est.hor$OM;struct=est.hor$STRUCT;Ks=MV$Ks/2.4

# subsoil
ind <- swatZ$nlayers>1
tmp[ind,13]<-swatZ$sol_z2[ind]*10 # max swat depth adapted as a function of ROO and TEXTDEPCHG
# BD: ptr PD_SUB
PDclass <- stu.ptrb$PD_SUB		
tmp[ind&(PDclass=="L"),14]<-1.1
tmp[ind&(PDclass=="M"),14]<-1.6
tmp[ind&(PDclass=="H"),14]<-1.9
# if TEXTSUBDOM==0, extent TEXTSURFDOM
TSD<-stu$TEXTSUBDOM; TSD[TSD==0]<-txt[TSD==0]
# SOL_CBN: adapted from Hiederer 2009
tmp[ind,17]<-OCsub(OCtop=tmp[ind,5],LC=stu.ptrb$USE[ind],
		TEXT1=txt[ind],TEXT2=TSD[ind])
# sgdbe dominant subsoil texture
# set medium texture to organic soils
TSDo <- TSD; TSDo[TSD==9]<-2
texture_sub <- av.text(TSDo)
tmp[ind,18] <- texture_sub$clay[ind] # SOL_CLAY2
tmp[ind,19] <- texture_sub$silt[ind] # SOL_SILT2
tmp[ind,20] <- texture_sub$sand[ind] # SOL_SAND2
tmp[ind,21] <- as.numeric(as.vector(stu.ptrb$VS[ind])) # SOL_ROCK
# AWC and SOL_K obtained from Hypres class ptf
parMV <- HYPRESclass(texture=TSD,topsoil=rep(0,nrow(stu)))
FC <- MV(parMV,10^(2.5));WP <- MV(parMV,10^(24.2))
tmp[ind,15] <- FC$theta[ind]-WP$theta[ind] # AWC
tmp[ind,16] <- parMV$Ks[ind]/2.4 # SOL_K (mm/hr)
tmp -> swat.sol[,(8+4):(31+4)]

# TEXTURE: texture class of all layers
swat.sol$TEXTURE[!ind] <- text.code(txt[!ind])
swat.sol$TEXTURE[ind] <- paste(text.code(txt[ind]),"-",text.code(TSD[ind]),sep="")

# summary and final corrections
# still some stu w/o txt: all non-soil and ind6 + stu 70011 (HSge=>txt=9), 70019 (RGha),
# 4210009 (NA), 4401565 (NA), 4402004 (NA)
# Non-soil: leave empty texture, clean SOL_CBN1, set SOL_AWC1=0, set CLAY1=100?
swat.sol[no.soil,14:16]<-0;swat.sol[no.soil,17]<-100
# stu 70011, set txt=9 -> done above
# stu 70019. Only 3 other stu RGha, with very different properies... so not very helpful...
ind.RGha <- stu$WRBFU=="RGha" & !is.na(stu$WRBFU)
# set sol_zmx to 600 (most frequent) -> done above
# the most frequent texture class in all stu is 2 (M) -> done above
# 3 remaining ones: clean evrything and leave like that?
swat.sol[stu$STU %in% c(4210009,4401565,4402004),13:length(swat.sol)]<-0
# texture of organic soils?
# set texture to medium (txt=2) -> done above
#
# HYDGRP: use texture and profile depth to devise some value...
HYDGRP <- swat.sol$HYDGRP
ind.na <- is.na(HYDGRP)
# heavy and organic soils => D
HYDGRP[ind.na & txt%in%c(5,9)] <- "D"
# coarse soils => A
HYDGRP[ind.na & txt==1] <- "A"
HYDGRP[ind.na & txt==2] <- "B"
HYDGRP[ind.na & txt%in%c(3,4)] <- "C"
HYDGRP -> swat.sol$HYDGRP
# 
# USLE_K1: 35 missing values => changed the method: switched to ptr 623 -> done above

# STU level
swat.sol -> swat.sol.stu
# SMU level: dominant STU
# find dominant STU
# unique smu
Domstu <- data.frame("SMU"=unique(smu$SMU))
for (ismu in 1:length(Domstu$SMU)){
	ind <- stu.org$SMU %in% Domstu$SMU[ismu]
	if (sum(ind<1)){ 
		#
		ind1<-ind&(stu.org$PCAREA==max(stu.org$PCAREA[ind]))
		ifelse(sum(ind1)>1,Domstu$Flag[ismu]<-TRUE,Domstu$Flag[ismu]<-FALSE)
	} else {ind1<-ind} 
	Domstu$STU[ismu] <- stu.org$STU[ind1][1]
	Domstu$PCAREA[ismu]<-stu.org$PCAREA[ind1][1]	
}
ind <- match(Domstu$STU,swat.sol$SNAM)
swat.sol.smu <- swat.sol[ind,]
swat.sol.smu$SNAM <- Domstu$SMU

# export table in dbf
write.dbf(swat.sol.smu,"usersoil_SMU.dbf")
write.csv(swat.sol.smu,"usersoil_SMU.csv",row.names=FALSE)

# Histogram of PCarea
p<-ggplot(Domstu,aes(x=PCAREA,fill=Flag)) + 
		geom_histogram(binwidth=10,colour='black') + scale_fill_grey()
ggsave(filename="hist.pdf",plot=p)

save("swat.sol",file="swat.RData")
