##################
#### Packages ####
##################
library(foreach)

######################
#### Read in Data ####
######################
dc <- read.csv("~/downloads/finland (1).csv", header=TRUE) # read finland.csv
newdc<-subset(dc, select=c(year, dm, grep("v", names(dc))))
newdc <- newdc[,c(-3)]

#####################################
#### Subset to years of interest ####
#####################################
dc.sub <- subset(newdc, year==1983 | year==1987 | year==1991 | year==1995)

##################################################
#### Change # of Votes to Zero if Beneath Threshold ####
##################################################
dc.sub$sumv <- rowSums(dc.sub[,grep("v", names(newdc))], na.rm=TRUE) # Caculate total number of votes in each district
dc.sub$threshv <- (.04)*dc.sub$sumv # change threshold here!

rt<-nrow(dc.sub[,3:length(dc.sub)])
ct<-ncol(dc.sub)
for(i in 1:rt) { #by row
	for(j in 3:ct){ #by col
		if(is.na(dc.sub[i,j])==FALSE & dc.sub[i,j]<dc.sub[i,ct]){
		dc.sub[i,j]<-NA
}
}
}

######################
#### The Function ####
######################
dHont <- function( candidates, votes, seats ){
    tmp <- data.frame(
                candidates = rep( candidates, each = seats ),
                scores     = as.vector(sapply( votes, function(x) x /
1:seats ))
                )
    tmp <- tmp$candidates[order( - tmp$scores )] [1:seats]
    table(tmp)
}

#############################
#### Apply to data.frame ####
#############################
DfSeats<-as.data.frame(foreach(k=1:nrow(dc.sub), .combine=rbind) %do% dHont(candidates=names(dc.sub[k,3:45]),votes= dc.sub[k,3:45], dc.sub[k,2]))

########################
#### Export to .csv ####
########################
write.csv(DfSeats, file ="~/FileName.csv")

