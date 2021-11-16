# PedoTransfer Rules of the European Soil DataBase
# 
# Author: M. Weynants based on 
###############################################################################
ptrdbe <- function(stu) {
	SN <- split3(stu$FAO85FU);names(SN)<-c("SN1","SN2","SN3")
	MAT1 <- split3(stu$MAT1);names(MAT1)<-c("PM11","PM12","PM13")
	MAT2 <- split3(stu$MAT1);names(MAT2)<-c("PM21","PM22","PM23")
	df <- cbind(stu,SN,MAT1,MAT2)
	rm("MAT1","MAT2")
	rm(list=ls(pattern="SN"))
	a <- rule1(df)
	AGLIM1NNI <- rule2(df$AGLIM1)
	AGLIM2NNI <- rule2(df$AGLIM2) # same rule on diff input var
	b <- rule11(df$USE_DOM) # b$USE and b$USE.CL
	ALT_MIN <- rule121(df$ZMIN)
	ALT_MAX <- rule121(df$ZMAX)
	# merge results of rules with df
	df <- cbind(df,a,AGLIM1NNI,AGLIM2NNI,b,ALT_MIN,ALT_MAX)
	rm(list=c("a","b",ls(pattern="A")))
	c <- rule12(df$COUNRTY,df$ALT_MIN,df$ALT_MAX) # c$ALT and c$ALT.CL
	MAT1HEV <- rule13(df$MAT1)
	ATC <- rule211(df$AT)
	# merge results of rules with df
	df <- cbind(df,c,MAT1HEV,ATC)
	rm("c",MAT1HEV,ATC)
	a<-rule21(df)
	df <-cbind(df,a)
	rm(a)
	df[,(length(df)+1):(length(df)+2)] <- rule22(df)
	df[,(length(df)+1):(length(df)+2)] <- rule311(df)
	df[,(length(df)+1):(length(df)+2)] <- rule312(df)
	df[,(length(df)+1):(length(df)+2)] <- rule313(df)
}
split3 <- function(F){
	# split FAO85FU into components
	#(data.frame(do.call(rbind,strsplit(foo,""))) doesn't work because not always same number of char)
	tmp<-strsplit(as.character(F),NULL)
	F1<-F2<-F3<-rep(NA,length(tmp))
	for (i in 1:length(tmp)){
		F1[i] <- tmp[[i]][1]
		try(F2[i] <- tmp[[i]][2],silent=TRUE)
		try(F3[i] <- tmp[[i]][3],silent=TRUE)
	}
	return(data.frame(F1,F2,F3))
}

rule1 <- function(df){
attach(df)
# Rule 1, type expert:
# Input data:
a <- SN1
b <- SN2
c <- SN3
d <- TEXTSRFDOM
e <- TEXTSRFSEC
f <- PM11
g <- PM12
h <- PM13
detach(2)
# Output data:
# TEXT (1,1,I,0)
# TEXT.CL (Confidence level: 1 1 C 0)
i<-j<-rep(NA,length(a))

#		  |a|b|c|d|e|f|g|h|i|j|  AUTHOR(S) |UPD DATE|
#		  |_|_|_|_|_|_|_|_|_|_|____________|________|
#		 1|*|*|*|1|*|*|*|*|1|h|CL          |11/07/00|
ind <- d==1; i[ind] <- 1; j[ind] <- "h";
#		 2|*|*|*|2|*|*|*|*|2|h|CL          |11/07/00|
ind <- d==2; i[ind] <- 2; j[ind] <- "h";
#		 3|*|*|*|3|*|*|*|*|3|h|CL          |11/07/00|
ind <- d==3; i[ind] <- 3; j[ind] <- "h";
#		 4|*|*|*|4|*|*|*|*|4|h|CL          |11/07/00|
ind <- d==4; i[ind] <- 4; j[ind] <- "h";
#		 5|*|*|*|5|*|*|*|*|5|h|CL          |11/07/00|
ind <- d==5; i[ind] <- 5; j[ind] <- "h";
#		 6|*|*|*|9|*|*|*|*|6|h|CL          |03/07/00|
ind <- d==9; i[ind] <- 6; j[ind] <- "h";
#		 7| | | |0|0| | | |0|h|CL          |29/09/00|
ind <- d==0 & e==0; i[ind] <- 0; j[ind] <- "h";
#		 8|B| | |0|*|1|1|0|2|l|JJL         |22/09/00|
ind <- a=="B" & d==0 & f==1 & g==1 & h==0; i[ind] <- 2; j[ind] <- "l";
#		10|B|d| |0|*| | | |2|l|JJL         |22/09/00|
ind <- a=="B" & b=="d" & d==0; i[ind] <- 2; j[ind] <- "l";
#		 9|B|d| |0|*|7|3|9|1|l|JJL         |22/09/00|
ind <- a=="B" & b=="d" & d==0 & f==7 & g==3 & h==9; i[ind] <- 1; j[ind] <- "l";
#		12|B|e| |0|*| | | |2|l|JJL         |22/09/00|
ind <- a=="B" & b=="e" & d==0; i[ind] <- 2; j[ind] <- "l";
#		11|B|e| |0|*|3|1|9|4|l|JJL         |22/09/00|
ind <- a=="B" & b=="e" & d==0 & f==3 & g==1 & h==9; i[ind] <- 4; j[ind] <- "l";
#		13|B|e|c|0|*|2|1|2|2|l|JJL         |22/09/00|
ind <- a=="B" & b=="e" & c=="c" & d==0 & f==2 & g==1 & h==2; i[ind] <- 2; j[ind] <- "l";
#		14|B|e|f|0|*|1|2|0|4|l|JJL         |22/09/00|
ind <- a=="B" & b=="e" & c=="f" & d==0 & f==1 & g==2 & h==0; i[ind] <- 4; j[ind] <- "l";
#		15|B|g|g|0|*| | | |2|l|JJL         |22/09/00|
ind <- a=="B" & b=="g" & c=="g" & d==0; i[ind] <- 2; j[ind] <- "l";
#		16|g| | |0|*|*|*|*|7|h|CL          |11/07/00|
ind <- a=="g" & d==0; i[ind] <- 7; j[ind] <- "h";
#		17|g| | |9|*|*|*|*|7|h|CL          |11/07/00|
ind <- a=="g" & d==9; i[ind] <- 7; j[ind] <- "h";
#		18|G|c|s|0|*|2|1|2|3|l|JJL         |22/09/00|
ind <- a=="G" & b=="c" & c=="s" & d==0 & f==2 & g==1 & h==2; i[ind] <- 3; j[ind] <- "l";
#		19|G|d| |0|*|7|3|9|2|l|JJL         |22/09/00|
ind <- a=="G" & b=="d" & d==0 & f==7 & g==3 & h==9; i[ind] <- 2; j[ind] <- "l";
#		20|G|d|s|0|*|3|1|9|2|l|JJL         |22/09/00|
ind <- a=="G" & b=="d" & c=="s" & d==0 & f==3 & g==1 & h==9; i[ind] <- 2; j[ind] <- "l";
#		21|G|e| |0|*|9|1|0|5|l|JJL         |22/09/00|
ind <- a=="G" & b=="e" & d==0 & f==9 & g==1 & h==0; i[ind] <- 5; j[ind] <- "l";
#		22|G|e|s|0|*|2|1|2|3|l|JJL         |22/09/00|
ind <- a=="G" & b=="e" & c=="s" & d==0 & f==2 & g==1 & h==2; i[ind] <- 3; j[ind] <- "l";
#		23|G|e|s|0|*|3|1|9|4|l|JJL         |22/09/00|
ind <- a=="G" & b=="e" & c=="s" & d==0 & f==3 & g==1 & h==9; i[ind] <- 4; j[ind] <- "l";
#		24|G|h|*|0|*|*|*|*|8|h|CL          |11/07/00|
ind <- a=="G" & b=="h" & d==0; i[ind] <- 8; j[ind] <- "h";
#		25|G|h|*|9|*|*|*|*|8|h|CL          |11/07/00|
ind <- a=="G" & b=="h" & d==9; i[ind] <- 8; j[ind] <- "h";
#		26|G|i|*|9|*|*|*|*|8|h|CL          |11/07/00|  
ind <- a=="G" & b=="i" & d==9; i[ind] <- 8; j[ind] <- "h";
#		27|G|m|*|0|*|*|*|*|8|h|CL          |11/07/00|
ind <- a=="G" & b=="m" & d==0; i[ind] <- 8; j[ind] <- "h";
#		28|G|m|*|9|*|*|*|*|8|h|CL          |11/07/00|
ind <- a=="G" & b=="m" & d==9; i[ind] <- 8; j[ind] <- "h";
#		29|I|*|*|9|1|*|*|*|1|m|CL          |11/07/00|
ind <- a=="I" & d==9 & e==1; i[ind] <- 1; j[ind] <- "m";
#		30|I|c| |0|*|2|0|0|2|l|JJL         |22/09/00|
ind <- a=="I" & b=="c" & d==0 & f==2 & g==0 & h==0; i[ind] <- 2; j[ind] <- "l";
#		31|I| | |0|*| | | |1|l|JJL         |22/09/00|
ind <- a=="I" & d==0; i[ind] <- 1; j[ind] <- "l";
#		32|I| | |0|0|2|1|0|2|l|JJL         |29/09/00|
ind <- a=="I" & d==0 & e==0 & f==2 & g==1 & h==0; i[ind] <- 2; j[ind] <- "l";
#		33|I| | |0|0|3|1|2|4|l|JJL         |29/09/00|
ind <- a=="I" & d==0 & e==0 & f==3 & g==1 & h==2; i[ind] <- 4; j[ind] <- "l";
#		34|I| | |0|0|4|5|1|2|l|JJL         |29/09/00|
ind <- a=="I" & d==0 & e==0 & f==4 & g==5 & h==1; i[ind] <- 2; j[ind] <- "l";
#		35|I| | |0|0|4|5|4|2|l|JJL         |29/09/00|
ind <- a=="I" & d==0 & e==0 & f==4 & g==5 & h==4; i[ind] <- 2; j[ind] <- "l";
#		36|I| | |0|0|5|1|2|2|l|JJL         |29/09/00|
ind <- a=="I" & d==0 & e==0 & f==5 & g==1 & h==2; i[ind] <- 2; j[ind] <- "l";
#		37|I| | |0|0|6|0|0|1|l|JJL         |29/09/00|
ind <- a=="I" & d==0 & e==0 & f==6 & g==0 & h==0; i[ind] <- 1; j[ind] <- "l";
#		38|J|c|g|0|*|1|2|0|3|l|JJL         |22/09/00|
ind <- a=="J" & b=="c" & c=="g" & d==0 & f==1 & g==2 & h==0; i[ind] <- 3; j[ind] <- "l";
#		39|J|e|g|0|*|3|1|9|4|l|JJL         |22/09/00|
ind <- a=="J" & b=="e" & c=="g" & d==0 & f==3 & g==1 & h==9; i[ind] <- 4; j[ind] <- "l";
#		40|J|m|g|9|*|*|*|*|8|m|CL          |11/07/00|
ind <- a=="J" & b=="m" & c=="g" & d==9; i[ind] <- 8; j[ind] <- "m";
#		41|L|c| |0|*| | | |2|l|JJL         |22/09/00|
ind <- a=="L" & b=="c" & d==0; i[ind] <- 2; j[ind] <- "l";
#		42|L|g|s|0|*|3|1|3|5|l|JJL         |22/09/00|
ind <- a=="L" & b=="g" & c=="s" & d==0 & f==3 & g==1 & h==3; i[ind] <- 5; j[ind] <- "l";
#		43|L|g|s|0|*| | | |2|l|JJL         |22/09/00|
ind <- a=="L" & b=="g" & c=="s" & d==0; i[ind] <- 2; j[ind] <- "l";
#		44|L|o| |0|*|2|1|2|2|l|JJL         |22/09/00|
ind <- a=="L" & b=="o" & d==0 & f==2 & g==1 & h==2; i[ind] <- 2; j[ind] <- "l";
#		45|L|o| |0|*|3|1|3|4|l|JJL         |22/09/00|
ind <- a=="L" & b=="o" & d==0 & f==3 & g==1 & h==3; i[ind] <- 4; j[ind] <- "l";
#		46|L|o| |0|*|3|1|9|4|l|JJL         |22/09/00|
ind <- a=="L" & b=="o" & d==0 & f==3 & g==1 & h==9; i[ind] <- 4; j[ind] <- "l";
#		47|O|*|*|0|*|*|*|*|8|h|CL          |11/07/00|
ind <- a=="O" & d==0; i[ind] <- 8; j[ind] <- "h";
#		48|O|*|*|9|*|*|*|*|8|h|CL          |11/07/00|
ind <- a=="O" & d==9; i[ind] <- 8; j[ind] <- "h";
#		49|P|o| |0|*| | | |1|l|JJL         |22/09/00|
ind <- a=="P" & b=="o" & d==0; i[ind] <- 1; j[ind] <- "l";
#		50|P|p|*|0|*|*|*|*|8|h|CL          |11/07/00|
ind <- a=="P" & b=="p" & d==0; i[ind] <- 8; j[ind] <- "h";
#		51|P|p|*|9|*|*|*|*|8|h|CL          |11/07/00|
ind <- a=="P" & b=="p" & d==9; i[ind] <- 8; j[ind] <- "h";
#		52|Q|c| |0|*| | | |1|m|JJL         |22/09/00|
ind <- a=="Q" & b=="c" & d==0; i[ind] <- 1; j[ind] <- "m";
#		53|r| | |0|*|*|*|*|7|h|CL          |11/07/00|
ind <- a=="r" & d==0; i[ind] <- 7; j[ind] <- "h";
#		54|r| | |9|*|*|*|*|7|h|CL          |11/07/00|
ind <- a=="r" & d==9; i[ind] <- 7; j[ind] <- "h";
#		55|R|c| |0|*|1|2|0|2|l|JJL         |22/09/00|
ind <- a=="R" & b=="c" & d==0 & f==1 & g==2 & h==0; i[ind] <- 2; j[ind] <- "l";
#		56|U|*|*|0|*|*|*|*|8|m|CL          |11/07/00|
ind <- a=="U" & d==0; i[ind] <- 8; j[ind] <- "m";
#		57|U|*|*|9|*|*|*|*|8|m|CL          |11/07/00|
ind <- a=="U" & d==9; i[ind] <- 8; j[ind] <- "m";
#		58|W| | |0|*|3|1|3|4|l|JJL         |22/09/00|
ind <- a=="B" & b=="e" & d==0; i[ind] <- 2; j[ind] <- "l";
TEXT<-i
TEXT.CL<-j
return(data.frame(TEXT,TEXT.CL))
}

rule2 <- function(AGLIM1){
# Dominant limitation to agricultural use
# Input data:
a = AGLIM1
# Output data:
b = rep(NA,length(a)) #AGLIM1NNI

# there are errors in the ptr file!!!

ind <- a %in% c(1:10)
b[ind] <- a[ind]
ind <- a==11; b[ind] <- 20
ind <- a==12; b[ind] <- 21
ind <- a==13; b[ind] <- 22
ind <- a==14; b[ind] <- 30
ind <- a==15; b[ind] <- 31
ind <- a %in% c(16,17,18); b[ind] <- 20 # considering duripan, petroferric and perfmafrost as fragic horizons
ind <- a== 0; b[ind] <- 1# when no info, let's say there is no limitation

AGLIM1NNI <- b
return(AGLIM1NNI)
}

rule3 <- rule2

rule11 <- function(USE){
# regroup land use classes
a <- USE
#		  |a |b |c|  AUTHOR(S) |UPD DATE|
#		 1|* |# |v|JDA         |19/12/93|
b <- c <- rep(NA,length(a))
#		 2| 1|MG|h|JMH-LV      |26/08/93|
#		 3| 2|C |h|JMH-LV      |26/08/93|
#		 4| 3|C |h|JMH-LV      |26/08/93|
#		 5| 4|SN|h|JMH-LV      |26/08/93|
#		 6| 5|SN|h|JMH-LV      |26/08/93|
#		 7| 6|C |h|JMH-LV      |26/08/93|
#		 8| 7|C |h|JMH-LV      |26/08/93|
#		 9| 8|SN|h|JMH-LV      |26/08/93|
#		10| 9|SN|h|JMH-LV      |26/08/93|
#		11|10|SN|h|JMH-LV      |26/08/93|
#		12|11|HG|h|JMH-LV      |26/08/93|
#		13|12|C |h|JMH-LV      |26/08/93|
#		14|13|C |h|JMH-LV      |26/08/93|
#		15|14|C |h|JMH-LV      |26/08/93|
#		16|15|C |h|JMH-LV      |26/08/93|
#		17|16|C |h|JMH-LV      |26/08/93|
#		18|17|C |h|JMH-LV      |26/08/93|
#		19|18|SN|h|JMH-LV      |26/08/93|
#		20|19|SN|h|JMH-LV      |26/08/93|
#		21|20|C |h|JMH-LV      |26/08/93|
#		22|21|C |h|JMH-LV      |26/08/93|
ind <- a==1; b[ind] <- "MG"; c[ind] <- "h"
ind <- a %in% c(2,3,6,7,12:17,20,21); b[ind] <- "C"; c[ind] <- "h"
ind <- a %in% c(4,5,8:10,18,19); b[ind] <- "SN"; c[ind] <- "h"
ind <- a==11; b[ind] <- "HG"; c[ind] <- "h"
b -> USE; c -> USE.CL
return(data.frame(USE,USE.CL))
}

rule121 <- function(Z){
# altitude with 100m accuracy
# input
a <- Z
# output
b <- round(a/100)*100
# no information:
ind <- a==-999; b[ind] <- -999
ind <- b==0; b[ind] <- 1
b -> ALT
return(ALT)
}

rule12 <- function(COUNTRY,ALT_MIN,ALT_MAX){
# class of elevation depending on the country + conf level
# input:
a <- COUNTRY
b <- ALT_MIN
c <- ALT_MAX
# output
# default value: highland
d <- rep("U",length(b)); e <- rep("m",length(b))
ind <- b==-999 | c==-999; d[ind] <- "#";e[ind] <- "v"
ind <- c>0 & c<=300; d[ind]<-"L";e[ind]<-"h"
ind <- c==400; d[ind]<-"L";e[ind]<-"m"
ind <- c==500; d[ind]<-"L";e[ind]<-"l"
# country specific:
inda <- a %in% c("PO","SP","AL","MK","GR","NL")
ind <- c>0 & c<=1000; d[inda&ind]<-"L";e[inda&ind]<-"h";
ind <- c==1100; d[inda&ind]<-"L";e[inda&ind]<-"m";
ind <- c==1200; d[inda&ind]<-"L";e[inda&ind]<-"l";
d -> ALT; e -> ALT.CL
return(data.frame(ALT,ALT.CL))
}

rule13 <- function(MAT1){
# Dominant parent material code translation
# input:
a <- MAT1
# output:
b <- rep(NA,length(a))
# i don't see the point of this rule if it has no forward chaining rules...
}

rule211 <- function(AT){
# accumulated temperature class
# no information!
return("#")
}

rule21 <- function(df){
# topsoil organic carbon
# input
attach(df)
a <- SN1
b <- SN2
c <- SN3
d <- TEXT
e <- USE
f <- ATC
detach(2)
# output
# OC_TOP and CL
#		  |a|b|c|d|e |f|g|h|  AUTHOR(S) |UPD DATE|
#		  |_|_|_|_|__|_|_|_|____________|________|
#		 1|*|*|*|*|* |*|L|l|LV-JMH      |13/09/93|
g <- rep("L",length(a)); h <- rep("l",length(g))
#		 2|*|*|*|1|* |*|V|m|LV-JMH      |13/09/93|
ind <- d==1; g[ind] <- "V"; h[ind] <- "m"
#		 3|*|*|*|1|C |L|L|m|JMH         |11/03/94|
ind2 <- e=="C" & f=="L"; g[ind&ind2] <- "L";
#		 4|*|*|*|2|* |*|L|m|LV-JMH      |13/09/93|
#		 5|*|*|*|3|* |*|L|m|LV-JMH      |13/09/93|
ind <- d==2 | d==3; g[ind]<-"L";h[ind]<-"m"
#		 6|*|*|*|1|C |H|V|h|LV          |11/03/94|
#		 7|*|*|*|2|C |H|V|h|LV          |11/03/94|
#		 8|*|*|*|3|C |H|V|h|LV          |11/03/94|
ind <- (d==1 | d==2 | d==3) & e=="C" & f=="H"; g[ind]<-"V";h[ind]<-"h"
#		 9|*|*|*|4|* |*|M|m|LV-JMH      |13/09/93|
#		10|*|*|*|5|* |*|M|m|LV-JMH      |13/09/93|
ind <- d==4 | d==5; g[ind]<-"M";h[ind]<-"m"
#		11|*|*|*|4|C |H|L|m|LV          |13/09/93|
#		12|*|*|*|5|C |H|L|m|LV          |27/07/94|
ind <- (d==4 | d==5) & e=="C" & f=="H"; g[ind]<-"L";h[ind]<-"m"
#		13|*|*|*|4|MG|H|L|m|LV          |27/07/94|
ind <- d==4 & e=="MG" & f=="H";g[ind]<-"L";h[ind]<-"m"
#		14|*|*|*|5|M |H|L|m|LV          |27/07/94|
ind <- d==5 & e=="M" & f=="H";g[ind]<-"L";h[ind]<-"m"
#		15|*|*|*|4|MG|M|L|m|LV          |27/07/94|
ind <- d==4 & e=="MG" & f=="M";g[ind]<-"L";h[ind]<-"m"
#		16|*|*|*|5|M |M|L|m|LV          |27/07/94|
ind <- d==5 & e=="M" & f=="M";g[ind]<-"L";h[ind]<-"m"
#		17|*|*|*|4|C |M|L|m|LV          |13/09/93|
#		18|*|*|*|5|C |M|L|m|LV          |27/07/94|
ind <- (d==4 | d==5) & e=="C" & f=="M";g[ind]<-"L";h[ind]<-"m"
#		19|*|*|*|*|SN|*|M|l|LV          |13/09/93|
ind <- f=="SN"; g[ind]<-"M"; h[ind]<-"l"
#		20|*|*|*|1|SN|H|V|m|LV-JMH      |13/09/93|
ind <- d==1 & e=="SN" & f=="H";g[ind]<-"V";h[ind]<-"m"
#		21|*|*|*|1|SN|M|L|m|LV          |13/09/93|
ind <- d==1 & e=="SN" & f=="M";g[ind]<-"L";h[ind]<-"m"
#		22|*|*|*|2|SN|M|M|m|LV          |13/09/93|
#		23|*|*|*|3|SN|M|M|m|LV          |13/09/93|
ind <- (d==2 |d==3) & e=="SN" & f=="M";g[ind]<-"M";h[ind]<-"m"
#		24|*|*|*|1|SN|L|L|m|LV          |13/09/93|
ind <- d==1 & e=="SN" & f=="L";g[ind]<-"L";h[ind]<-"m"
#		25|*|*|*|2|SN|L|M|m|LV          |13/09/93|
#		26|*|*|*|3|SN|L|M|m|LV          |13/09/93|
ind <- (d==2 | d==3) & e=="SN" & f=="L";g[ind]<-"M";h[ind]<-"m"
#		27|*|*|*|2|SN|H|L|m|LV          |13/09/93|
#		28|*|*|*|3|SN|H|L|m|LV          |13/09/93|
#		29|*|*|*|4|SN|H|L|m|LV          |13/09/93|
#		30|*|*|*|5|SN|H|L|m|LV          |13/09/93|
ind <- d%in%c(2,3,4,5) & e=="SN" & f=="H";g[ind]<-"L";h[ind]<-"m"
#		31|*|*|*|4|SN|M|M|m|LV          |13/09/93|
#		32|*|*|*|5|SN|M|M|m|LV          |13/09/93|
ind <- d%in%c(4,5) & e=="SN" & f=="M";g[ind]<-"M";h[ind]<-"m"
#		33|*|*|*|4|SN|L|H|m|LV          |13/09/93|
#		34|*|*|*|5|SN|L|H|m|LV          |13/09/93|
ind <- d%in%c(4,5) & e=="SN" & f=="L";g[ind]<-"H";h[ind]<-"m"
#		35|L|g|*|*|SN|*|M|m|LV          |23/08/93|
ind <- a=="L" & b=="g" & e=="SN";g[ind]<-"M";h[ind]<-"m"
#		36|L|g|*|1|SN|*|L|m|LV          |23/08/93|
ind1 <- d==1;g[ind&ind1]<-"L";
#		37|L|c|*|2|C |M|L|l|JMH-LV      |15/06/93|
#		38|L|c|*|2|MG|M|L|l|JMH-LV      |27/07/94|
#		39|L|c|*|3|C |M|L|l|JMH-LV      |15/06/93|
#		40|L|c|*|3|MG|M|L|l|JMH-LV      |27/07/94|
#		41|B|c|*|2|C |M|L|l|JMH-LV      |15/06/93|
#		42|B|c|*|2|MG|M|L|l|JMH-LV      |27/07/94|
#		43|B|c|*|3|C |M|L|l|JMH-LV      |15/06/93|
#		44|B|c|*|3|MG|M|L|l|JMH-LV      |27/07/94|
ind <- a%in%c("L","B") & b=="c" & d%in%c(2,3)& e%in%c("C","MG") & f=="M"; g[ind]<-"L";h[ind]<-"l"
#		45|B|h|*|*|* |*|M|m|LV          |23/08/93|
ind <- a=="B" & b=="h"; g[ind]<-"M";h[ind]<-"m"
#		46|*|g|g|*|SN|*|M|m|JMH-LV      |15/06/93|
#		47|*|g|g|*|MG|*|M|m|JMH-LV      |27/07/94|
ind <- b=="g" & c=="g" & e%in%c("SN","MG"); g[ind]<-"M";h[ind]<-"m"
#		48|*|g|g|1|SN|*|L|m|JMH-LV      |15/06/93|
#		49|*|g|g|1|MG|*|L|m|JMH-LV      |27/07/94|
ind1 <- d==1; g[ind&ind1]<-"L";
#		50|*|g|g|4|SN|*|H|m|LV          |13/09/93|
#		51|*|g|g|5|SN|*|H|m|LV          |13/09/93|
ind <- b=="g" & c=="g" & d%in%c(4,5) & e=="SN"; g[ind]<-"H";h[ind]<-"m"
#		52|*|g|s|*|SN|*|M|m|JMH-LV      |15/06/93|
#		53|*|g|s|*|MG|*|M|m|JMH-LV      |27/07/94|
ind <- b=="g" & c=="s" & e%in%c("SN","MG"); g[ind]<-"M";h[ind]<-"m"
#		54|*|g|s|1|SN|*|L|m|JMH-LV      |15/06/93|
#		55|*|g|s|1|MG|*|L|m|JMH-LV      |27/07/94|
ind1 <- d==1;g[ind&ind1]<-"L";
#		56|*|g|s|4|SN|*|H|m|LV          |13/09/93|
#		57|*|g|s|5|SN|*|H|m|LV          |13/09/93|
ind <- b=="g" & c=="g" & d%in%c(4,5) & e=="SN"; g[ind]<-"H";h[ind]<-"m"
#		58|G|*|*|*|* |*|M|l|JMH-LV      |13/09/93|
ind <- a=="G"; g[ind]<-"M";h[ind]<-"l"
#		59|G|m|f|2|SN|*|H|m|JMH-LV      |15/06/93|
#		60|G|m|f|3|SN|*|H|m|JMH-LV      |15/06/93|
#		61|G|m|f|4|SN|*|H|m|JMH-LV      |15/06/93|
#		62|G|m|f|5|SN|*|H|m|JMH-LV      |15/06/93|
ind <- a=="G" & b=="m" & c=="f" & d%in%c(2:5) & e=="SN";g[ind]<-"H";h[ind]<-"m"
#		63|G|h|*|2|SN|*|H|m|JMH-LV      |15/06/93|
#		64|G|h|*|3|SN|*|H|m|JMH-LV      |15/06/93|
#		65|G|h|*|4|SN|*|H|m|JMH-LV      |15/06/93|
#		66|G|h|*|5|SN|*|H|m|JMH-LV      |15/06/93|
ind <- a=="G" & b=="h" & d%in%c(2:5) & e=="SN";g[ind]<-"H";h[ind]<-"m"
#		67|G|*|s|2|SN|M|H|m|LV          |15/06/93|
#		68|G|*|s|3|SN|M|H|m|LV          |15/06/93|
#		69|G|*|s|4|SN|M|H|m|LV          |15/06/93|
#		70|G|*|s|5|SN|M|H|m|LV          |15/06/93|
#		71|G|*|s|2|SN|L|H|m|LV          |15/06/93|
#		72|G|*|s|3|SN|L|H|m|LV          |15/06/93|
#		73|G|*|s|4|SN|L|H|m|LV          |15/06/93|
#		74|G|*|s|5|SN|L|H|m|LV          |15/06/93|
ind <- a=="G" & c=="s" & d%in%c(2:5) & e=="SN" & f%in%c("M","L");g[ind]<-"H";h[ind]<-"m"
#		75|J|*|*|*|* |*|M|l|JMH-LV      |13/09/93|
ind<-a=="J";g[ind]<-"M";h[ind]<-"l"
#		76|J|*|*|1|* |*|L|m|JMH         |09/06/93|
ind<-a=="J" & d==1;g[ind]<-"L";h[ind]<-"m"
#		77|J|*|*|2|C |H|L|m|LV          |23/08/93|
#		78|J|*|*|3|C |H|L|m|LV          |23/08/93|
ind<-a=="J" & d%in%c(2,3) & d=="C" & e=="H";g[ind]<-"L";h[ind]<-"m"
#		79|J|*|g|*|* |*|M|m|JMH         |09/06/93|
ind<-a=="J" & c=="g";g[ind]<-"M";h[ind]<-"m"
#		80|J|*|g|4|SN|M|H|m|JMH-LV      |13/09/93|
#		81|J|*|g|4|SN|L|H|m|JMH-LV      |13/09/93|
#		82|J|*|g|5|SN|M|H|m|JMH-LV      |13/09/93|
#		83|J|*|g|5|SN|L|H|m|JMH-LV      |13/09/93|
ind<-a=="J" & c=="g" & d%in%c(4,5) & e=="SN" & f%in%c("M","L");g[ind]<-"H";h[ind]<-"m"
#		84|J|t|*|*|* |*|M|m|JMH         |09/06/93|
ind<-a=="J" & b=="t";g[ind]<-"M";h[ind]<-"m"
#		85|J|t|*|2|SN|*|H|m|JMH         |09/06/93|
#		86|J|t|*|3|SN|*|H|m|JMH         |09/06/93|
#		87|J|t|*|4|SN|*|H|m|JMH         |09/06/93|
#		88|J|t|*|5|SN|*|H|m|JMH         |09/06/93|
ind1<-d%in%c(2:5) & e=="SN";g[ind]<-"H";h[ind]<-"m"
#		89|H|*|*|*|* |*|M|m|JMH         |09/06/93|
#		90|H|*|*|1|C |*|L|m|JMH         |09/06/93|
#		91|H|*|*|1|MG|*|L|m|JMH         |27/07/94|
#		92|K|*|*|*|* |*|M|m|JMH         |09/06/93|
#		93|K|*|*|1|C |*|L|m|JMH         |09/06/93|
#		94|K|*|*|1|MG|*|L|m|JMH         |27/07/94|
#		95|E|*|*|*|* |*|M|m|JMH         |09/06/93|
#		96|E|*|*|1|C |*|L|m|JMH         |09/06/93|
#		97|E|*|*|1|MG|*|L|m|JMH         |27/07/94|
#		 98|E|*|*|2|SN|L|H|h|JMH-LV      |13/09/93|
#		 99|E|*|*|3|SN|L|H|h|JMH-LV      |13/09/93|
#		100|E|*|*|4|SN|L|H|h|JMH-LV      |13/09/93|
#		101|E|*|*|5|SN|L|H|h|JMH-LV      |13/09/93|
#		102|U|*|*|*|* |*|M|m|JMH-LV      |15/06/93|
#		103|U|*|*|1|C |*|L|m|JMH         |09/06/93|
#		104|U|*|*|1|MG|*|L|m|JMH         |27/07/94|
#		105|U|*|*|2|SN|L|H|h|JMH         |09/06/93|
#		106|U|*|*|3|SN|L|H|h|JMH         |09/06/93|
#		107|U|*|*|4|SN|L|H|h|JMH         |09/06/93|
#		108|U|*|*|5|SN|L|H|h|JMH         |09/06/93|
ind<-a%in%c("H","K","E","U");g[ind]<-"M";h[ind]<-"m"
ind<-a%in%c("H","K","E","U") & d==1 & e%in%c("C","MG");g[ind]<-"L";h[ind]<-"m"
ind <- a%in%c("E","U") & d%in%c(2:5) & e=="SN" & f=="L";g[ind]<-"H";h[ind]<-"h"
#		109|I|*|*|*|* |*|V|m|JMH-LV      |13/09/93|
#		110|I|*|*|4|* |*|L|m|JMH         |09/06/93|
#		111|I|*|*|5|* |*|L|m|JMH         |09/06/93|
#		112|R|*|*|*|* |*|V|m|JMH         |09/06/93|
#		113|R|*|*|2|* |*|L|m|JMH-LV      |23/08/93|
#		114|R|*|*|3|* |*|L|m|JMH-LV      |23/08/93|
#		115|R|*|*|4|* |*|L|m|JMH-LV      |23/08/93|
#		116|R|*|*|5|* |*|L|m|JMH-LV      |23/08/93|
ind <- a%in%c("R","I");g[ind]<-"V";h[ind]<-"m"
ind <- a%in%c("R","I") & d%in%c(4,5);g[ind]<-"L"
ind <- a=="R" & d%in%c(2,3);g[ind]<-"L"
#		117|O|*|*|*|* |*|H|h|JMH         |09/06/93|
ind <- a=="O";g[ind]<-"H";h[ind]<-"h"
#		118|Q|*|*|*|* |*|L|m|JMH-LV      |13/09/93|
ind <- a=="Q";g[ind]<-"L";h[ind]<-"m"
#		119|Q|*|*|*|* |H|V|h|JMH-LV      |13/09/93|
ind <- a=="Q" & f=="H";g[ind]<-"V";h[ind]<-"h"
#		120|Q|*|*|*|C |M|V|h|JMH-LV      |13/09/93|
#		121|Q|*|*|*|MG|M|V|h|JMH-LV      |27/07/94|
ind <- a=="Q" & e%in%c("C","M") & f=="M";g[ind]<-"V";h[ind]<-"h"
#		122|Q|*|*|*|SN|L|M|m|JMH-LV      |13/09/93|
ind <- a=="Q" & e=="SN" & f=="L";g[ind]<-"M";h[ind]<-"m"
#		123|F|*|*|*|* |*|L|l|JMH         |09/06/93|
ind <- a=="F";g[ind]<-"L";h[ind]<-"l"
#		124|X|*|*|*|* |*|V|m|JMH         |09/06/93|
ind <- a=="X";g[ind]<-"V";h[ind]<-"m"
#		125|X|*|*|4|SN|*|L|m|JMH-LV      |23/08/93|
#		126|X|*|*|5|SN|*|L|m|JMH-LV      |23/08/93|
ind <- a=="X" & d%in%c(4,5) & e=="SN";g[ind]<-"L";h[ind]<-"m"
#		127|Z|*|*|*|* |*|L|m|JMH         |09/06/93|
ind<-a=="Z";g[ind]<-"L";h[ind]<-"m"
#		128|Z|*|*|1|* |*|V|h|JMH         |09/06/93|
ind<-a=="Z" & d==1;g[ind]<-"V";h[ind]<-"h"
#		129|Z|*|*|4|* |M|M|l|JMH-LV      |13/09/93|
#		130|Z|*|*|4|* |L|M|l|JMH-LV      |13/09/93|
#		131|Z|*|*|5|* |M|M|l|JMH-LV      |13/09/93|
#		132|Z|*|*|5|* |L|M|l|JMH-LV      |13/09/93|
ind<-a=="Z" & d%in%c(4,5) & f%in%c("M","L");g[ind]<-"M";h[ind]<-"l"
#		133|W|*|*|*|* |*|M|m|JMH-LV      |13/09/93|
ind<-a=="W";g[ind]<-"M";h[ind]<-"m"
#		134|W|*|*|*|C |H|L|l|LV          |13/09/93|
#		135|W|*|*|*|C |M|L|l|LV          |13/09/93|
ind<-a=="W" & e=="C" & f%in%c("H","M");g[ind]<-"L";h[ind]<-"l"
#		136|W|*|*|1|* |*|L|m|JMH         |09/06/93|
ind<-a=="W" & d==1;g[ind]<-"L";h[ind]<-"m"
#		137|W|*|*|1|C |H|V|l|LV          |13/09/93|
#		138|W|*|*|1|C |M|V|l|LV          |13/09/93|
ind<-a=="W" & d==1 & e=="C" & f%in%c("H","M");g[ind]<-"V";h[ind]<-"l"
#		139|p| | |*|* |*|M|m|JMH         |09/06/93|
#		140|V|*|*|*|* |*|M|m|JMH         |09/06/93|
ind <- a%in%c("p","V");g[ind]<-"M";h[ind]<-"m"
#		141|V|*|*|*|C |H|L|m|LV          |13/09/93|
#		142|V|*|*|*|MG|H|L|m|LV          |27/07/94|
ind <- a=="V" & e%in%c("C","MG") & f=="H";g[ind]<-"L";h[ind]<-"m"
#		143|r| | |*|* |*|V|h|JMH         |09/06/93|
ind<-a=="r";g[ind]<-"V";h[ind]<-"h"
#		144|T|*|*|*|* |*|H|h|JMH         |09/06/93|
ind<-a=="T";g[ind]<-"H";h[ind]<-"h"
#		145|T|v|*|*|* |*|M|h|JMH         |09/06/93|
ind<-a=="T" & b=="v";g[ind]<-"M";h[ind]<-"h"
#		146|P|*|*|*|* |*|M|m|JMH         |09/06/93|
ind<-a=="P";g[ind]<-"M";h[ind]<-"m"
#		147|P|g|*|*|SN|L|H|h|LV          |13/09/93|
#		148|P|g|s|*|SN|*|H|h|LV          |13/09/93|
#		149|P|p|*|*|SN|L|H|h|JMH-LV      |23/08/93|
#		150|P|g|h|*|* |*|H|h|LV          |15/06/93|
ind <- a=="P" & b%in%c("g","p") & e=="SN" & f=="L";g[ind]<-"H"; h[ind]<-"h"
ind <- a=="P" & b=="g" & ((c=="s" & e=="SN")|(c=="h"));g[ind]<-"H"; h[ind]<-"h"
g -> OC_TOP; h -> OC_TOP.CL
return(data.frame(OC_TOP,OC_TOP.CL))
}

rule22 <- function(df){
# Peat
attach(df)
a<-SN1;b<-SN2;c<-SN3
detach(2)
#
#			  |a|b|c|d|e|  AUTHOR(S) |UPD DATE|
#			  |_|_|_|_|_|____________|________|
#			 1|*|*|*|N|h|LV-EVR      |18/08/93|
d <- rep("N",length(a)); e <- rep("h",length(a)) 
#			 2|O|*|*|Y|h|LV-EVR      |18/08/93|
ind <- a=="O"; d[ind]<-"Y";
#			 3|J|t|*|N|l|LV-EVR      |18/08/93|
#			 4|J|*|g|N|l|LV-EVR      |18/08/93|
ind <- a=="J" & ((b=="t")|(c=="g")); e[ind]<-"l";
#			 5|Z|g|*|N|l|LV-EVR      |18/08/93|
ind<-a=="Z" & b=="g";e[ind]<-"l"
#			 6|G|i|*|Y|h|CL          |19/11/97|
#			 7|G|i|h|Y|h|CL          |19/11/97|
#			 8|G|h|*|N|l|LV-EVR      |18/08/93|
#			 9|G|h|h|Y|h|LV-EVR      |18/08/93|
ind <- a=="G" & b=="i";d[ind]<-"Y"
ind <- a=="G" & b=="h"; e[ind]<="l"
ind <- a =="G" & b%in%c("i","h") & c=="h";d[ind]<-"Y"
#			10|P|p|*|N|l|LV-EVR      |18/08/93|
#			11|P|g|*|N|l|LV-EVR      |18/08/93|
ind<-a=="P" & b%in%c("p","g");e[ind]<-"l"
#			12|P|g|h|Y|h|LV-EVR      |18/08/93|
ind<-a=="P" & b=="g" & c=="h";d[ind]<-"Y";e[ind]<-"h"
#			13|P|h|*|N|m|LV-EVR      |18/08/93|
ind<-a=="P" & b=="h";e[ind]<-"m"
d -> PEAT; e-> PEAT.CL
return(data.frame(PEAT,PEAT.CL))
}

rule311 <- function(df){
# Soil profile differentiation
attach(df)
a <- SN1; b <- SN2; c <- SN3
detach(df)
d <- e <- rep(NA,length(a))

#		  |a|b|c|d|e|  AUTHOR(S) |UPD DATE|
#		 1|A|*|*|H|h|MJ-DK-EV    |20/08/93|
ind <- a=="A"; d[ind]<- "H";e[ind]<-"h"
#		 2|B|*|*|O|h|MJ-DK-EV    |20/08/93|
ind <- a=="B"; d[ind]<- "O";e[ind]<-"h"
#		 3|B|d|*|L|m|MJ-DK-EV    |20/08/93|
ind <- a=="B" & b=="d"; d[ind]<-"L";e[ind]<-"m"
#		 4|B|d|a|O|h|MJ-DK-EV    |20/08/93|
ind1<-c=="a";d[ind&ind1]<-"O";e[ind]<-"h"
#		 5|B|g|g|L|l|MJ-DK-EV    |20/08/93|
ind <- a=="B" & b=="g" & c=="g"; d[ind]<-"L";e[ind]<-"l"
#		 6|B|*|s|L|m|MJ-DK-EV    |20/08/93|
ind<- a=="B" & c=="s";d[ind]<-"L";e[ind]<-"m"
#		 7|C|*|*|O|h|MJ-DK       |01/08/96|
ind <- a=="C";d[ind]<-"O";e[ind]<-"h";
#		 8|C|l|*|L|m|MJ-DK       |01/08/96|
#		 9|C|k|*|L|m|MJ-DK       |01/08/96|
ind <- a=="C" & b%in%c("l","k");d[ind]<-"L";e[ind]<-"m"
#		10|D|*|*|H|h|MJ-DK-EV    |20/08/93|
ind<-a=="D";d[ind]<-"H";e[ind]<-"h"
#		11|D|e|*|H|m|MJ-DK-EV    |20/08/93|
ind<-a=="D" & b=="e";e[ind]<-"m"
#		12|E|*|*|O|h|MJ-DK-EV    |20/08/93|
ind<-a=="E";d[ind]<-"O";e[ind]<-"h"
#		13|F|o|*|H|h|MJ-DK-EV    |20/08/93|
ind<-a=="F" & b=="o";d[ind]<-"H";e[ind]<-"h"
#		14|G|*|*|O|h|MJ-DK-EV    |20/08/93|
ind<-a=="G";d[ind]<-"O";e[ind]<-"h"
#		15|G|d|*|O|m|MJ-DK-EV    |20/08/93|
ind<-a=="G"&b=="d";d[ind]<-"O";e[ind]<-"m"
#		16|G|d|f|O|m|MJ-DK-EV    |20/08/93|
#		17|G|h|t|O|m|MJ-DK-EV    |20/08/93|
ind<-a=="G"&b=="h"&c=="t";d[ind]<-"O";e[ind]<-"m"
#		18|H|*|*|O|m|MJ-DK-EV    |20/08/93|
ind<-a=="H";d[ind]<-"O";e[ind]<-"m"
#		19|H|l|*|L|h|MJ-DK-EV    |20/08/93|
ind<-a=="H"&b=="l";d[ind]<-"L";e[ind]<-"h"
#		20|I|*|*|O|h|MJ-DK-EV    |20/08/93|
#		21|J|*|*|O|h|MJ-DK-EV    |20/08/93|
#		22|J|t|*|O|m|MJ-DK-EV    |20/08/93|
ind<-a=="G"&b=="t";d[ind]<-"O";e[ind]<-"m"
#		23|K|*|*|O|h|MJ-DK-EV    |20/08/93|
ind<-a%in%c("I","J","K");d[ind]<-"O";e[ind]<-"h"
#		24|K|l|*|L|h|MJ-DK-EV    |20/08/93|
ind<-a=="K" & b=="l";d[ind]<-"L";e[ind]<-"h"
#		25|L|*|*|H|h|MJ-DK-EV    |20/08/93|
ind<-a=="L";d[ind]<-"H";e[ind]<-"h"
#		26|L|c|*|L|h|MJ-DK-EV    |20/08/93|
#		27|L|k|*|L|m|MJ-DK-EV    |20/08/93|
ind<-a=="L" & b=="k";d[ind]<-"L";e[ind]<-"m"
#		28|L|v|*|L|h|MJ-DK-EV    |20/08/93|
ind<-a=="L" & b%in%c("c","v");d[ind]<-"L";e[ind]<-"h"
#		29|L|v|c|L|m|MJ-DK-EV    |20/08/93|
#		30|L|v|k|L|m|MJ-DK-EV    |20/08/93|
ind<-a=="L" & b=="v" & c%in%c("c","k");d[ind]<-"L";e[ind]<-"m"
#		31|L|a|p|H|h|MJ-DK-EV    |20/08/93|
#		32|L|g|p|H|h|MJ-DK-EV    |20/08/93|
ind <- a=="L" & b%in%c("a","p") & c=="p";d[ind]<-"H";e[ind]<-"h"
#		33|M|o|*|O|h|MJ-DK       |01/08/96|
ind<-a=="M"&b=="o";d[ind]<-"O";e[ind]<-"h"
#		34|O|*|*|O|h|MJ          |29/12/93|
ind<-a=="O";d[ind]<-"O";e[ind]<-"h"
#		35|P|*|*|L|h|MJ-DK-EV    |20/08/93|
ind<-a=="P";d[ind]<-"L";e[ind]<-"h"
#		36|P|h|*|H|h|MJ-DK-EV    |20/08/93|
#		37|P|o|*|H|h|MJ-DK-EV    |20/08/93|
ind<-a=="P" & b%in%c("h","o");d[ind]<-"H";e[ind]<-"h"
#		38|Q|*|*|O|h|MJ-DK-EV    |20/08/93|
ind<-a=="Q";d[ind]<-"O";e[ind]<-"h"
#		39|Q|c|s|L|m|MJ-DK-EV    |20/08/93|
#		40|Q|l|*|L|m|MJ-DK-EV    |20/08/93|
ind<-a=="Q" & ((b=="c"&c=="s")|(b=="l"));d[ind]<-"L";e[ind]<-"m"
#		41|R|*|*|O|h|MJ-DK-EV    |20/08/93|
ind<-a=="R";d[ind]<-"O";e[ind]<-"h"
#		42|S|*|*|H|m|MJ-DK       |01/08/96|
#		43|S|g|*|H|m|MJ-DK       |01/08/96|
ind<-a=="S";d[ind]<-"H";e[ind]<-"m"
#		44|T|*|*|O|h|MJ-DK-EV    |20/08/93|
#		45|U|*|*|O|h|MJ-DK-EV    |20/08/93|
ind<-a%in%c("T","U","V","X","Z","r");d[ind]<-"O";e[ind]<-"h"
#		46|U|l|*|L|l|MJ-DK-EV    |20/08/93|
ind<-a=="U" & b=="l";d[ind]<-"L";e[ind]<-"l"
#		47|V|*|*|O|h|MJ-DK-EV    |20/08/93|
#		48|W|*|*|H|h|MJ-DK-EV    |20/08/93|
#		49|W|e|*|H|h|MJ-DK-EV    |20/08/93|
ind<-a=="W";d[ind]<-"H";e[ind]<-"h"
#		50|X|*|*|O|h|MJ-DK-EV    |20/08/93|
#		51|Z|*|*|O|h|MJ-DK-EV    |20/08/93|
#		52|p|*|*|L|m|MJ          |29/12/93|
ind<-a=="p";d[ind]<-"L";e[ind]<-"m"
#		53|r| | |O|h|MJ          |29/12/93|
DIFF <- d;DIFF.CL<-e
return(data.frame(DIFF,DIFF.CL))
}

rule312<-function(df){
# Profile mineralogy
attach(df)
a<-SN1;b<-SN2;c<-SN3
detach(2)
d<-e<-rep(NA,length(a))
#		  |a|b|c|d |e|  AUTHOR(S) |UPD DATE|
#		 1|A|*|*|C |h|MJ-DK-EV    |20/08/93|
ind <- a=="A";d[ind]<-"C";e[ind]<-"h"
#		 2|B|*|*|ND|h|MJ-DK-EV    |20/08/93|
ind <- a%in%c("B","C","E","G","I","J","K","O","Q","R","T","U","V","X","Z","r")
d[ind]<-"ND";e[ind]<-"h"
#		 3|B|d|*|C |m|DK          |27/07/94|
#		 4|B|d|a|ND|h|MJ-DK-EV    |20/08/93|
#		 5|B|g|g|M |l|MJ-DK-EV    |20/08/93|
#		 6|B|*|s|C |m|MJ-DK-EV    |20/08/93|
ind <- a=="B" & ((b==d & c!="a")|(c=="s"));d[ind]<-"C";e[ind]<-"m"
ind<-a=="B"&b=="g"&c=="g";d[ind]<-"M";e[ind]<-"l"
#		 7|C|*|*|ND|h|MJ-DK       |01/08/96|
#		 8|C|l|*|M |m|MJ-DK       |01/08/96|
ind <-a=="C"&b=="l";d[ind]<-"M";e[ind]<-"m"
#		 9|C|k|*|C |m|MJ-DK       |01/08/96|
ind <-a=="C"&b=="l";d[ind]<-"C";e[ind]<-"m"
#		10|D|*|*|MC|h|MJ-DK-EV    |20/08/93|
ind <-a=="D";d[ind]<-"MC";e[ind]<-"h"
#		11|D|e|*|MC|m|MJ-DK-EV    |20/08/9h|
ind <-a=="D"&b=="e";d[ind]<-"MC";e[ind]<-"m"
#		12|E|*|*|ND|h|MJ-DK-EV    |20/08/93|
#		13|F|o|*|C |h|MJ-DK-EV    |20/08/93|
ind <-a=="F"&b=="o";d[ind]<-"C";e[ind]<-"h"
#		14|G|*|*|ND|h|MJ-DK-EV    |20/08/93|
#		15|G|d|*|ND|m|MJ-DK-EV    |20/08/93|
#		16|G|d|f|ND|m|MJ-DK-EV    |20/08/93|
#		17|G|h|t|ND|m|MJ-DK-EV    |20/08/93|
ind <-a=="G"&((b=="d") | (b=="h"&c=="t"));d[ind]<-"ND";e[ind]<-"m"
#		18|H|*|*|ND|m|MJ-DK-EV    |20/08/93|
ind <-a=="H";d[ind]<-"ND";e[ind]<-"m"
#		19|H|l|*|M |h|MJ-DK-EV    |20/08/93|
ind <-a%in%c("H","K") & b=="l";d[ind]<-"M";e[ind]<-"h"
#		20|I|*|*|ND|h|MJ-DK-EV    |20/08/93|
#		21|J|*|*|ND|h|MJ-DK-EV    |20/08/93|
#		22|J|t|*|ND|m|MJ-DK-EV    |20/08/93|
ind <-a=="J" & b=="t";d[ind]<-"ND";e[ind]<-"m"
#		23|K|*|*|ND|h|MJ-DK-EV    |20/08/93| =>2
#		24|K|l|*|M |h|MJ-DK-EV    |20/08/93| =>19
#		25|L|*|*|M |h|MJ-DK-EV    |20/08/93|
#		26|L|c|*|M |h|MJ-DK-EV    |20/08/93|
ind <-a=="L";d[ind]<-"M";e[ind]<-"h"
#		27|L|k|*|M |m|MJ-DK-EV    |20/08/93|
#		28|L|v|*|M |h|MJ-DK-EV    |20/08/93|
#		29|L|v|c|M |m|MJ-DK-EV    |20/08/93|
#		30|L|v|k|M |m|MJ-DK-EV    |20/08/93|
ind <-a=="L" & ((b=="k")|(b=="v"&c%in%c("c","k")));d[ind]<-"M";e[ind]<-"m"
#		31|L|a|p|MC|h|MJ-DK-EV    |20/08/93|
#		32|L|g|p|MC|h|MJ-DK-EV    |20/08/93|
ind <-a=="L" & b%in%c("a","p")&c=="p";d[ind]<-"MC";e[ind]<-"h"
#		33|M|o|*|ND|h|MJ-DK       |01/08/96|
ind <-a=="M"&b=="o";d[ind]<-"ND";e[ind]<-"h"
#		34|O|*|*|ND|h|MJ          |29/12/93| =>2
#		35|P|*|*|C |h|MJ-DK-EV    |20/08/93|
#		36|P|h|*|C |h|MJ-DK-EV    |20/08/93|
#		37|P|o|*|C |h|MJ-DK-EV    |20/08/93|
ind <-a=="P";d[ind]<-"C";e[ind]<-"h"
#		38|Q|*|*|ND|h|MJ-DK-EV    |20/08/93| =>2
#		39|Q|c|s|C |m|MJ-DK-EV    |20/08/93|
ind <-a=="Q" & b=="c" & c=="s";d[ind]<-"C";e[ind]<-"m"
#		40|Q|l|*|M |m|MJ-DK-EV    |20/08/93|
ind <-a=="Q" & b=="l";d[ind]<-"M";e[ind]<-"m"
#		41|R|*|*|ND|h|MJ-DK-EV    |20/08/93| =>2
#		42|S|*|*|C |m|MJ-DK       |01/08/96|
ind <-a=="S";d[ind]<-"C";e[ind]<-"m"
#		43|S|g|*|C |h|MJ-DK       |01/08/96|
ind <-a=="S" & b=="g";d[ind]<-"C";e[ind]<-"h"
#		44|T|*|*|ND|h|MJ-DK-EV    |20/08/93| =>2
#		45|U|*|*|ND|h|MJ-DK-EV    |20/08/93| =>2
#		46|U|l|*|M |l|MJ-DK-EV    |20/08/93|
ind <-a=="U" & b=="l";d[ind]<-"M";e[ind]<-"l"
#		47|V|*|*|ND|h|MJ-DK-EV    |20/08/93| =>2
#		48|W|*|*|MC|h|MJ-DK-EV    |20/08/93|
ind <-a=="W";d[ind]<-"MC";e[ind]<-"h"
#		49|W|e|*|M |h|MJ-DK-EV    |20/08/93|
ind <-a=="W" & b=="e";d[ind]<-"M";e[ind]<-"h"
#		50|X|*|*|ND|h|MJ-DK-EV    |20/08/93| =>2
#		51|Z|*|*|ND|h|MJ-DK-EV    |20/08/93| =>2
#		52|p|*|*|MC|m|MJ          |29/12/93|
ind <-a=="p";d[ind]<-"MC";e[ind]<-"m"
#		53|r| | |ND|h|MJ          |29/12/93| =>2
d -> MIN
e -> MIN.CL
return(data.frame(MIN,MIN.CL))
}

rule313<-function(df){
# Topsoil mineralogy. (MIN_TOP)
attach(df)
a<-PM11;b<-PM12;c<-PM13;d<-MIN
detach(2)
#
e<-f<-rep(NA,length(a))
#			  |a|b|c|d |e |f|  AUTHOR(S) |UPD DATE|
#			 1|1|*|*|* |M |h|MJ-DK-EV    |20/08/93|
ind<-a%in%c(1,2);e[ind]<-"M";f[ind]<-"h"
#			 2|1|*|*|C |KQ|m|MJ-DK-EV    |20/08/93|
ind<-a==1 & d=="C";e[ind]<-"KQ";f[ind]<-"m"
#			 3|1|1|1|M |KQ|h|MJ-DK-EV    |20/08/93|
ind<-a==1 & b==1 & c==1 & d=="M";e[ind]<-"KQ";f[ind]<-"m"
#			 4|1|1|3|ND|MS|m|MJ-DK-EV    |20/08/93|
#			 5|1|2|*|ND|MS|m|MJ-DK-EV    |20/08/93|
ind<-a==1 & ((b==1 & c==3)|(b==2))& d=="ND";e[ind]<"MS";f[ind]<-"m"
#			 6|2|*|*|* |M |h|MJ-DK-EV    |20/08/93| =>1
#			 7|2|1|7|* |MS|m|MJ-DK-EV    |20/08/93|
#			 8|2|1|7|M |M |h|MJ-DK-EV    |20/08/93|
#			 9|2|2|0|* |MS|m|MJ-DK-EV    |20/08/93|
#			10|2|2|0|M |M |m|MJ-DK-EV    |20/08/93|
ind<-a==2 & ((b==1 & c==7)|(b==2 & c==0)) & d!="M";e[ind]<-"MS";f[ind]<-"m"
ind<-a==2 & b==b & c==0 & d=="M";f[ind]<="m"
#			11|2|3|*|* |MS|h|MJ-DK-EV    |20/08/93|
#			12|2|3|*|M |M |h|MJ-DK-EV    |20/08/93|
ind<-a==2 & b==3 & d!="M";e[ind]<"M";f[ind]<-"h"	
#			13|3|*|*|* |MK|m|MJ-DK-EV    |20/08/93|
ind<-a==3;e[ind]<"MQ";f[ind]<-"m"
#			14|3|1|0|C |KQ|h|MJ-DK-EV    |20/08/93|
#			15|3|1|3|C |KX|h|MJ-DK-EV    |20/08/93|
#			16|3|1|4|C |KX|h|MJ-DK-EV    |20/08/93|
#			17|3|2|*|* |M |m|MJ-DK-EV    |20/08/93|
#			18|3|2|1|* |MK|h|MJ-DK-EV    |20/08/93|
#			19|3|3|0|* |M |h|MJ-DK-EV    |20/08/93|
#			20|3|3|0|C |KX|m|MJ-DK-EV    |20/08/93|
#			21|3|3|1|* |KX|h|MJ-DK-EV    |20/08/93|
#			22|3|3|2|* |KX|h|MJ-DK-EV    |20/08/93|
#			23|3|3|2|C |KQ|h|MJ-DK-EV    |20/08/93|
#			24|3|3|3|* |M |h|MJ-DK-EV    |20/08/93|
#			25|3|4|*|* |M |m|MJ-DK-EV    |20/08/93|
#			26|3|5|*|* |M |m|MJ-DK-EV    |20/08/93|
ind<-a==3 & ((b%in%c(2,4,5))|(b==3 & c%in%c(0,3)))
ind<-a==3 & ((b==1&c==0)|(b==3&c==2&d=="C"));d[ind]<-"KQ";e[ind]<-"h"
#			27|4|*|*|* |M |m|MJ-DK-EV    |20/08/93|
#			28|4|*|*|C |KQ|h|MJ-DK-EV    |20/08/93|
#			29|4|1|*|* |MK|m|MJ-DK-EV    |20/08/93|
#			30|4|1|*|C |KQ|h|MJ-DK-EV    |20/08/93|
#			31|4|5|2|* |KX|h|MJ-DK-EV    |20/08/93|
#			32|4|5|2|C |KQ|h|MJ-DK-EV    |20/08/93|
#			33|5|*|*|* |M |m|MJ-DK-EV    |20/08/93|
#			34|5|*|*|C |KQ|m|MJ-DK-EV    |20/08/93|
#			35|5|1|1|* |MK|h|MJ-DK-EV    |20/08/93|
#			36|5|1|1|C |KQ|h|MJ-DK-EV    |20/08/93|
#			37|5|2|*|ND|MS|h|MJ-DK-EV    |20/08/93|
#			38|5|2|*|M |M |h|MJ-DK-EV    |20/08/93|
#			39|5|2|2|* |M |h|MJ-DK-EV    |20/08/93|
#			40|5|2|3|* |M |h|MJ-DK-EV    |20/08/93|
#			41|5|2|2|M |MK|h|MJ-DK-EV    |20/08/93|
#			42|6|*|*|* |M |m|MJ-DK-EV    |20/08/93|
#			43|6|*|*|C |KQ|h|MJ-DK-EV    |20/08/93|
#			44|6|4|0|ND|MS|m|MJ-DK-EV    |20/08/93|
#			45|7|*|*|ND|M |m|MJ-DK-EV    |20/08/93|
#			46|7|*|*|M |MK|m|MJ-DK-EV    |20/08/93|
#			47|7|*|*|C |KQ|h|MJ-DK-EV    |20/08/93|
#			48|7|0|0|MC|MK|m|MJ          |04/01/94|
#			49|7|3|9|MC|MK|m|MJ          |04/01/94|
#			50|7|4|1|MC|M |m|MJ          |04/01/94|
#			51|7|2|*|ND|MS|m|MJ-DK-EV    |20/08/93|
#			52|7|2|*|M |M |h|MJ-DK-EV    |20/08/93|
#			53|7|4|*|M |M |h|MJ-DK-EV    |20/08/93|
#			54|7|5|0|M |M |m|MJ-DK-EV    |20/08/93|
#			55|7|5|0|C |KQ|m|MJ-DK-EV    |20/08/93|
#			56|8|*|*|* |TO|m|MJ-DK-EV    |20/08/93|
#			57|8|1|0|* |TV|m|MJ-DK-EV    |20/08/93|
#			58|8|1|0|C |KX|m|MJ-DK-EV    |20/08/93|
#			59|8|3|0|* |TV|h|MJ-DK-EV    |20/08/93|
#			60|8|3|0|M |TO|m|MJ-DK-EV    |20/08/93|
#			61|9|0|0|C |NA|h|DK          |02/08/94|
#			62|9|0|0|M |M |l|MJ          |04/01/94|
#			63|9|0|0|ND|M |l|MJ          |04/01/94|
#			64|9|0|1|* |NA|h|DK          |31/07/96|
#			65|9|0|2|* |NA|h|DK          |31/07/96|
#			66|9|1|0|C |NA|h|MJ          |04/01/94|
#			67|9|1|0|ND|NA|h|MJ          |04/01/94|
#			68| | | |* |NA|h|DK-JD       |01/08/96|
#			69|7|3|1|MC|MK|m|DK          |20/12/94|
#			70|7|4|0|MC|MK|m|DK          |20/12/94|
}