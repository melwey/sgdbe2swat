# Modified PTR 21 (OC_TOP), Jones et al. 2005. Rule provided by Roland Hiederer
# 
# Author: M. Weynants
###############################################################################
ptr21mod <- function(stu, stu.ptrb){
	T <- stu.ptrb$TEXT; U <- stu.ptrb$USE
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
	SN <- split3(stu$FAO85FU);names(SN)<-c("SN1","SN2","SN3")
	SN1<-SN$SN1;SN2<-SN$SN2;SN3<-SN$SN3
	
	# General rule
	OC_TOP <- rep(2,nrow(stu))
	OC_TOP[T==1] <- 1
	OC_TOP[T==1 & U=="C"] <- 2
	OC_TOP[T%in%c(2,3) & U=="C"] <- 1
	OC_TOP[T%in%c(4,5)] <- 3
	OC_TOP[T%in%c(4,5) & U%in%c("C","MG")] <- 2
	OC_TOP[U=="SN"] <- 3
	OC_TOP[T==1 & U=="SN"] <- 2
	OC_TOP[SN1=="L" & SN2=="g" & U%in%c("SN","MG")]<- 3
	OC_TOP[SN1=="L" & SN2=="g" & T==1 & U=="SN"]<- 2
	OC_TOP[SN1=="L" & SN2=="c" & T%in%c(2,3) & U=="C"]<- 2
	OC_TOP[SN1=="L" & SN2=="c" & T%in%c(2,3) & U=="MG"]<- 3
	OC_TOP[SN1=="L" & SN2=="c" & T%in%c(4,5)]<- 3
	OC_TOP[SN1=="B" & SN2=="c" & T%in%c(2,3) & U=="C"]<-2
	OC_TOP[SN1=="B" & SN2=="c" & T==3 & U=="MG"]<-3
	OC_TOP[SN1=="B" & SN3=="h"]<-3
	OC_TOP[SN1=="B" & SN3=="h" & T%in%c(3,4,5) & U=="SN"]<-4
	OC_TOP[SN1=="B" & SN2%in%c("h","d")]<-3
	OC_TOP[SN1=="B" & SN2=="h" & T%in%c(3,4,5) & U=="SN"]<-4
	OC_TOP[SN1=="B" & SN2=="d" & T==4 & U=="SN"]<-4
	OC_TOP[SN1=="B" & SN2=="d" & T==5]<-4
	OC_TOP[SN1=="B" & SN2=="d" & SN3=="s" & T%in%c(2,3) & U=="SN"]<-4
	OC_TOP[SN1=="B" & SN2=="e" & SN3=="a" & T%in%c(4,5) & U=="SN"]<-4
	OC_TOP[SN2=="g" & SN3%in%c("g","s") & U%in%c("SN","MG")]<-3
	OC_TOP[SN2=="g" & SN3%in%c("g","s") & T==1 & U%in%c("SN","MG")]<-2
	OC_TOP[SN2=="g" & SN3%in%c("g","s") & T%in%c(4,5) & U=="SN"]<-4
	OC_TOP[SN1=="G"]<-3
	OC_TOP[SN1=="G" & SN2=="d" & U=="SN"]<-5
	OC_TOP[SN1=="G" & SN2=="d" & T%in%c(2,3,4) & U=="SN"]<-4
	OC_TOP[SN1=="G" & SN2=="d" & T==5 & U=="SN"]<-5
	OC_TOP[SN1=="G" & SN2=="h" & U=="SN"]<-4
	OC_TOP[SN1=="G" & SN2=="h" & T%in%c(4,5) & U=="SN"]<-5
	OC_TOP[SN1=="G" & SN3=="s" & U=="SN"]<-4
	OC_TOP[SN1=="G" & SN3=="s" & T==1 & U=="SN"]<-3
	OC_TOP[SN1=="G" & SN2=="m" & SN3=="f" & T%in%c(2,3) & U=="SN"]<-4
	OC_TOP[SN1=="G" & SN2=="m" & SN3=="f" & T%in%c(4,5) & U=="SN"]<-5
	OC_TOP[SN1=="G" & SN2=="f" & SN3=="m" & T%in%c(2,3) & U=="SN"]<-4
	OC_TOP[SN1=="G" & SN2=="f" & SN3=="m" & T%in%c(4,5) & U=="SN"]<-5
	OC_TOP[SN1=="J"] <- 3
	OC_TOP[SN1=="J" & T==1] <- 2
	OC_TOP[SN1=="J" & T%in%c(2,3) & U=="C"] <- 2
	OC_TOP[SN1=="J" & SN3=="g"] <- 3
	OC_TOP[SN1=="J" & SN3=="g" & T==4 & U=="SN"] <- 3
	OC_TOP[SN1=="J" & SN3=="g" & T==5 & U=="SN"] <- 4
	OC_TOP[SN1=="J" & SN2=="t"] <- 3
	OC_TOP[SN1=="J" & SN2=="t" & U=="SN"] <- 4
	OC_TOP[SN1=="J" & SN2=="t" & T==1 & U=="SN"] <- 3
	OC_TOP[SN1%in%c("H","K","E")]<-3
	OC_TOP[SN1%in%c("H","K","E") & T==1 & U%in%c("C","MG")]<-2
	OC_TOP[SN1=="E" & T%in%c(4,5) & U=="SN"]<-4
	OC_TOP[SN1=="U"]<-4
	OC_TOP[SN1=="U" & T==1 & U=="C"]<-2
	OC_TOP[SN1=="U" & T==1 & U=="MG"]<-3
	OC_TOP[SN1=="U" & U=="SN"]<-4
	OC_TOP[SN1=="U" & T==1 & U=="SN"]<-3
	OC_TOP[SN1%in%c("I","R")]<-1
	OC_TOP[SN1=="I" & T%in%c(4,5)]<-2
	OC_TOP[SN1=="R" & T%in%c(2,3,4,5)]<-2
	OC_TOP[SN1=="R" & SN2=="c" & U%in%c("SN","MG")]<-3
	OC_TOP[SN1=="O"]<-5
	OC_TOP[SN1=="O" & U=="C"]<-4
	OC_TOP[SN1=="O" & SN2=="d" & U=="SN"]<-6
	OC_TOP[SN1%in%c("Q","F")]<-2
	OC_TOP[SN1=="Q" & U=="C"]<-1
	OC_TOP[SN1=="Q" & U%in%c("MG","SN")]<-3
	OC_TOP[SN1=="X"]<-1
	OC_TOP[SN1=="X" & T%in%c(4,5) & U=="SN"]<-2
	OC_TOP[SN1=="Z"]<-2
	OC_TOP[SN1=="Z" & T==1]<-1
	OC_TOP[SN1=="Z" & T%in%c(4,5)]<-3
	OC_TOP[SN1=="W"]<-3
	OC_TOP[SN1=="W" & (U=="C" | T==1)]<-2
	OC_TOP[SN1=="W" & T==1 & U=="C"]<-1
	OC_TOP[SN1=="p"]<-3
	OC_TOP[SN1=="V"]<-3
	OC_TOP[SN1=="V" & U%in%c("C","MG")]<-2
	OC_TOP[SN1=="r"]<-1
	OC_TOP[SN1=="T"]<-4
	OC_TOP[SN1=="T" & SN2=="h" & U=="C"]<-3
	OC_TOP[SN1=="T" & SN2=="v"]<-3
	OC_TOP[SN1=="P"]<-3
	OC_TOP[SN1=="P" & SN2=="g" & U=="SN"]<-4
	OC_TOP[SN1=="P" & SN2=="g" & SN3=="s"]<-3
	OC_TOP[SN1=="P" & SN2=="p" & U%in%c("SN","MG")]<-5
	OC_TOP[SN1=="P" & SN2=="g" & SN3=="h"]<-4
	#ID_CNT  SN1 SN2 SN3 TEXT USE OCTOP
	#1 99 99 99 99 99 2
	#1 99 99 99 1 99 1
	#1 99 99 99 1 C 2
	#1 99 99 99 2 C 1
	#1 99 99 99 3 C 1
	#1 99 99 99 4 99 3
	#1 99 99 99 4 C 2
	#1 99 99 99 4 MG 2
	#1 99 99 99 5 99 3
	#1 99 99 99 5 C 2
	#1 99 99 99 5 MG 2
	#1 99 99 99 99 SN 3
	#1 99 99 99 1 SN 2
	#1 cL g 99 99 SN 3
	#1 cL g 99 99 MG 3
	#1 cL g 99 1 SN 2
	#1 cL c 99 2 C 2
	#1 cL c 99 2 MG 3
	#1 cL c 99 3 C 2
	#1 cL c 99 3 MG 3
	#1 cL c 99 4 99 3
	#1 cL c 99 5 99 3
	#1 cB c 99 2 C 2
	#1 cB c 99 3 C 2
	#1 cB c 99 3 MG 3
	#1 cB 99 h 99 99 3
	#1 cB 99 h 3 SN 4
	#1 cB 99 h 4 SN 4
	#1 cB 99 h 5 SN 4
	#1 cB h 99 99 99 3
	#1 cB h 99 3 SN 4
	#1 cB h 99 4 SN 4
	#1 cB h 99 5 SN 4
	#1 cB d 99 99 99 3
	#1 cB d 99 4 SN 4
	#1 cB d 99 5 99 4
	#1 cB d s 2 SN 4
	#1 cB d s 3 SN 4
	#1 cB e a 4 99 4
	#1 cB e a 5 99 4
	#1 99 g g 99 SN 3
	#1 99 g g 99 MG 3
	#1 99 g g 1 SN 2
	#1 99 g g 1 MG 2
	#1 99 g g 4 SN 4
	#1 99 g g 5 SN 4
	#1 99 g s 99 SN 3
	#1 99 g s 99 MG 3
	#1 99 g s 1 SN 2
	#1 99 g s 1 MG 2
	#1 99 g s 4 SN 4
	#1 99 g s 5 SN 4
	#1 cG 99 99 99 99 3
	#1 cG d 99 99 SN 5
	#1 cG d 99 2 SN 4
	#1 cG d 99 3 SN 4
	#1 cG d 99 4 SN 4
	#1 cG d 99 5 SN 5
	#1 cG h 99 99 SN 4
	#REM 1 cG h 99 1 99 3
	#1 cG h 99 4 SN 5
	#1 cG h 99 5 SN 5
	#1 cG 99 s 99 SN 4
	#1 cG 99 s 1 SN 3
	#1 cG m f 2 SN 4
	#1 cG m f 3 SN 4
	#1 cG m f 4 SN 5
	#1 cG m f 5 SN 5
	#1 cG f m 2 SN 4
	#1 cG f m 3 SN 4
	#1 cG f m 4 SN 5
	#1 cG f m 5 SN 5
	#1 cJ 99 99 99 99 3
	#1 cJ 99 99 1 99 2
	#1 cJ 99 99 2 C 2
	#1 cJ 99 99 3 C 2
	#1 cJ 99 g 99 99 3
	#1 cJ 99 g 4 SN 3
	#1 cJ 99 g 5 SN 4
	#1 cJ t 99 99 99 3
	#1 cJ t 99 99 SN 4
	#1 cJ t 99 1 SN 3
	#1 cH 99 99 99 99 3
	#1 cH 99 99 1 C 2
	#1 cH 99 99 1 MG 2
	#1 cK 99 99 99 99 3
	#1 cK 99 99 1 C 2
	#1 cK 99 99 1 MG 2
	#1 cE 99 99 99 99 3
	#1 cE 99 99 1 C 2
	#1 cE 99 99 1 MG 2
	#1 cE 99 99 4 SN 4
	#1 cE 99 99 5 SN 4
	#1 cU 99 99 99 99 4
	#1 cU 99 99 1 C 2
	#1 cU 99 99 1 MG 3
	#1 cU 99 99 99 SN 4
	#1 cU 99 99 1 SN 3
	#1 cI 99 99 99 99 1
	#1 cI 99 99 4 99 2
	#1 cI 99 99 5 99 2
	#1 cR 99 99 99 99 1
	#1 cR 99 99 2 99 2
	#1 cR 99 99 3 99 2
	#1 cR 99 99 4 99 2
	#1 cR 99 99 5 99 2
	#1 cR c 99 99 SN 3
	#1 cR c 99 99 MG 3
	#1 cO 99 99 99 99 5
	#1 cO d 99 99 C 4
	#1 cO d 99 99 SN 6
	#1 cQ 99 99 99 99 2
	#1 cQ 99 99 99 C 1
	#1 cQ 99 99 99 MG 3
	#1 cQ 99 99 99 SN 3
	#1 cF 99 99 99 99 2
	#1 cX 99 99 99 99 1
	#1 cX 99 99 4 SN 2
	#1 cX 99 99 5 SN 2
	#1 cZ 99 99 99 99 2
	#1 cZ 99 99 1 99 1
	#1 cZ 99 99 4 99 3
	#1 cZ 99 99 5 99 3
	#1 cW 99 99 99 99 3
	#1 cW 99 99 99 C 2
	#1 cW 99 99 1 99 2
	#1 cW 99 99 1 C 1
	#1 sp 99 99 99 99 3
	#1 cV 99 99 99 99 3
	#1 cV 99 99 99 C 2
	#1 cV 99 99 99 MG 2
	#1 sr 99 99 99 99 1
	#1 cT 99 99 99 99 4
	#1 cT h 99 99 C 3
	#1 cT v 99 99 99 3
	#1 cP 99 99 99 99 3
	#1 cP g 99 99 SN 4
	#1 cP g s 99 99 3
	#1 cP p 99 99 SN 5
	#1 cP p 99 99 MG 5
	#1 cP g h 99 99 4
	OC_TOP
}


