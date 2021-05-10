#
#
# R version originally written by by C. Laize CEH in early 2012
# Based on FORTRAN PROGRAM TEMPGRID
# For QC and traceability, followed the FORTRAN code as closely as possible; not necessarily best R code
# Converted to calc.temps function by M Dunbar, Environment Agency April 2018

# Modified by T Loveday to avoid errors on east coast, Environment Agency May 2019 
############################################
#

  
#SUBROUTINE AVCALL
#implemented in R as a function

AVCALL<-function(ME1, ME2, MN1, MN2, KE, KN){
np<-0
tsum<-0
rsum<-0
smean<-0.000000
srange<-0.000000
subsetATG <-NULL
subsetATG <- subset(AirTempGrid,  Easting >= ME1 & Easting <= ME2 & Northing >= MN1 & Northing <= MN2)

subsetATG$D<-(subsetATG$Easting-(KE+25))^2+(subsetATG$Northing-(KN+25))^2
 
ifelse(subsetATG$D!=0.000000, subsetATG$DS<-1/subsetATG$D, subsetATG$DS<-0.01)

smean<- sum(subsetATG$TEMPM/subsetATG$D)/sum(subsetATG$DS)
srange<- sum(subsetATG$TEMPR/subsetATG$D)/sum(subsetATG$DS)

np<-nrow(subsetATG)
c(np, smean, srange)

}

#Program to calculate mean temperature and annual temperature

calc.temps <- test1<-function(coordinates){
# coordinates is a data frame with three columns
# SITE_ID
# EASTING4
# NORTHING4
  
# this function will know about AirTempGrid because it will have been defined in the global environment just
# before the function has been called. This isn't great programming practice, but will do for now.
  
NP<-NULL
SMEAN<-NULL
SRANGE<-NULL
ME1<-NULL
ME2<-NULL
KE<-NULL
MN1<-NULL
MN2<-NULL
KN<-NULL



TempGrid_out<-NULL



for (l in c(1:nrow(coordinates))) {#0
# for (l in c(4:4)) {#0
# print(coordinates$Site_ID[l])
if(nchar(coordinates$Site_ID[l])<4) {
  for(z in c(1:(4-nchar(coordinates$Site_ID[l])))){
    coordinates$Site_ID[l]<-paste("0", coordinates$Site_ID[l],sep="")
  }
}

IGEAST<-coordinates$Easting4[l]    #============================================================
						#remember to change back
IGNORTH<-coordinates$Northing4[l]  #============================================================

KE<-IGEAST-25
KN<-IGNORTH-25

#find nearest 5km-point to sw and distances e and n from that
#although this point might be the below the site location as the first four are derived by adding 50 to east and nort

KSQE<-(trunc(KE/50)*50)+25
KSQN<-(trunc(KN/50)*50)+25

IREME<-IGEAST-KSQE
IREMN<-IGNORTH-KSQN

#test if at a 5km-point or a vertical or horizontal between
if (IREME==0 & IREMN==0) {#1
	#print("if 1")
	ME1<-KSQE
	ME2<-ME1
	MN1<-KSQN
	MN2<-MN1
        subsetATG1 <- subset(AirTempGrid,  Easting >= ME1 & Easting <= ME2 & Northing >= MN1 & Northing <= MN2)
	NP<-nrow(subsetATG1) ;SMEAN<-subsetATG1$TEMPM ;SRANGE<-subsetATG1$TEMPR
	if(NP==1){#2
		#print("if 2")
		TMEAN<-SMEAN
		TRANGE<-SRANGE
	} else {#2
		#print("else 2")
		ME1<-ME1-50
		ME2<-ME2+50
		MN1<-MN1-50
		MN2<-MN2+50
		avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
		if(NP>3){#3
			#print("if 3")
			TMEAN<-SMEAN
			TRANGE<-SRANGE
		} else {#3
			#print("else 3")
			ME1<-ME1-50
			ME2<-ME2+50
			MN1<-MN1-50
			MN2<-MN2+50		
			avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
			if(NP<4){TMEAN<-0.0;TRANGE<-0.0} else {TMEAN<-SMEAN;TRANGE<-SRANGE}#4
			}#3
		}#2	
} else {#1
		if (IREME==0) {#5
			#print("if 5")
			ME1<-KSQE
			ME2<-ME1
			MN1<-KSQN
			MN2<-MN1+50
			avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
			if(NP==2){#6
				#print("if 6")
				TMEAN<-SMEAN
				TRANGE<-SRANGE
			} else {#6
				#print("else 6")
				ME1<-ME1-50
				ME2<-ME2+50
				avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
				if(NP>3){#7
					#print("if 7")
					TMEAN<-SMEAN
					TRANGE<-SRANGE
				} else {#7
					#print("else 7")
					MN1<-MN1-50
					MN2<-MN2+50
					avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
					if(NP<4){print("if 8");TMEAN<-0.0;TRANGE<-0.0} else {
					  #print("else 8");
					  TMEAN<-SMEAN;
					  TRANGE<-SRANGE}#8
					}#7	
				}#6	
		} else {#5
				if (IREMN==0) {#9
					#print("if 9")
					ME1<-KSQE
					ME2<-ME1+50
					MN1<-KSQN
					MN2<-MN1
					avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
					if(NP==2){#10
						#print("if 10")
						TMEAN<-SMEAN
						TRANGE<-SRANGE
					} else {#10
						#print("else 10")
						MN1<-MN1-50
						MN2<-MN2+50
						avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
						if(NP>3){#11
							#print("if 11")
							TMEAN<-SMEAN
							TRANGE<-SRANGE
						} else {#11
							#print("else 11")
							ME1<-ME1-50
							ME2<-ME2+50
							avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
							if(NP<4){print("if 12");TMEAN<-0.0;TRANGE<-0.0} else {
							  #print("else 12");
							  TMEAN<-SMEAN;
							  TRANGE<-SRANGE}#12
							}#11
						}#10
				} else {#9
						#must interpolate between 4 values
						ME1<-KSQE
						ME2<-ME1+50
						MN1<-KSQN
						MN2<-MN1+50
						avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
						if(NP>2) {#13
							#print("if 13")
							TMEAN<-SMEAN
							TRANGE<-SRANGE
						} else {#13
								#print("else 13")
								ME1<-ME1-50
								ME2<-ME2+50
								MN1<-MN1-50
								MN2<-MN2+50
								avcall<-AVCALL(ME1, ME2, MN1, MN2, KE, KN);NP<-avcall[1];SMEAN<-avcall[2];SRANGE<-avcall[3]
								if(NP<4){print("if 14");TMEAN<-0.0;TRANGE<-0.0} else {
								  #print("else 14");
								  TMEAN<-SMEAN;TRANGE<-SRANGE}#14	
							}#13
					}#9	
			}#5
	}#1
#}
#}
#print(paste("NP = ", NP, sep=""))
#print(paste(coordinates[l,1],NP, TMEAN, TRANGE))
TempGrid_out<-rbind(TempGrid_out, cbind(coordinates[l,], TMEAN, TRANGE))
}#0
TempGrid_out<-as.data.frame(TempGrid_out)
#names(TempGrid_out)<-c("Site", "East", "North", "TMEAN", "TRANGE") # this line removed as it's stopping the function return working
#so we deal with the names back in the calling code


}



