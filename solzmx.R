# Maximum rooting depth for SWAT (mm): 
# rooting depth restrictions: presence of an impermeable layer, waterlogging, rock or other obstacle to roots
# 
# Author: M. Weynants
###############################################################################
solzmx <- function(roo,il,wr,dr,aglim){
# input:
# roo is the depth class of an obstacle to roots of the stu
# output:
# zmx is the maximum root depth of soil profile (mm). If no depth is specified, 
# the model assumes the roots can develop throughout the entire depth of the profile
zmx<-rep(120, length(roo))

# topsoil defaults to 30
sol_z1 <- rep(30,length(roo))
sol_z1[il==4 | wr==4 | dr=="S"] <- 20 
sol_z1[roo==6] <- 10

# subsoil defaults to 350 (SWAT maximum)
sol_z2 <- rep(120,length(roo))
temp <- matrix(120,length(roo),5)
# rooting depth restriction
temp[roo==2,1] <-70; temp[roo==3,1]<-50; temp[roo==4,1]<-NA; temp[roo==5,1] <- 40
# impermeable layer restriction
temp[il==3,2] <- 60; temp[il==4,2] <- 30;
# Agriculture limitation
temp[aglim==4,3] <- NA; temp[aglim==6,3] <- 50; temp[aglim%in%c(11,16:18),3] <- 80;
# Depth to rock restriction
temp[dr=="M",4] <- 60; temp[dr=="D",4] <- 100;
# water regime restriction
temp[wr==3,5] <- 40
# Most restrictive condition wins
sol_z2 <- pmin(sol_z2,temp[,1],temp[,2],temp[,3],temp[,4],temp[,5])
sol_z2[sol_z1<30] <- NA

sol_zmx <- pmax(sol_z1,sol_z2,na.rm=TRUE)
nlayers<-rep(1,length(roo)); nlayers[!is.na(sol_z2)]<-2


return(data.frame(sol_zmx,sol_z1,sol_z2,nlayers))

## ------------------------
#ind<-roo==0 # no information...
#zmx[ind]<-NA
#
#ind <- roo %in% c(0,1) #no info or no obstacle within 80 cm
#zmx[ind]<-3500 # maximum value accepted by SWAT
#
#ind <- roo==2 # obstacle between 60-80 cm
#zmx[ind]<-700
#
#ind <- roo==3 # obstacle between 40-60 cm
#zmx[ind]<-500
#
#ind <- roo==4 # obstacle between 20-40 cm
#zmx[ind]<-300
#
#ind <- roo==5 # obstacle between 0-80 cm
#zmx[ind]<-400
#
#ind <- roo==6 # obstacle between 0-20 cm
#zmx[ind]<-100
#
#return(zmx)
}


