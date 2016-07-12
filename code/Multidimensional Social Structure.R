# Contents: This source file contains functions which allow the user to calculate a number of multidimensional measures of social structure using survey or census data. Some of the scholars who have developed these measures are cited at the end of this document. 

# Instructions: Data should be in a observation-row format and identity variables should be class factor. 

##################
# One-way Measures #
##################
# Fractionalization
frac<-function(identity){
	tab<-as.numeric(table(identity, useNA="no")) # Create counts for each group
	tab1<-tab/length(identity) # Convert counts to percentages
	tab2<-(tab1)^2 # Square each percentage
	return(1-sum(tab2)) # Return this measures subtracted from unity
}

# Bipolarization
bipol<-function(identity){
	tab<-as.numeric(table(identity, useNA="no")) # Create counts for each group
	tab1<-tab/length(identity) # Convert counts to percentages
	return(1-sum(tab1* ((.5- tab1)/.5)^2))
}

##################
# Two-way Measures #
##################
# Subgroup-Fractionalization
frac2<-function(identity1, identity2){
	tab<-as.numeric(table(identity1, identity2, useNA="no")) # Create counts for each subgroup
    tab1<-tab/length(identity1) # Convert counts to percentages
	tab2<-(tab1)^2 # Square each percentage        
	return(1-sum(tab2)) # Return this measures subtracted from unity
    }

# Cross-cuttingness (two categorical variables)
cc2<-function(identity1, identity2){
	cc.tab <- as.matrix(table(as.numeric(identity1),as.numeric(identity2), useNA="no"))
	cc.v <- NA
	if(min(dim(cc.tab))>1){
	chi.stat<-as.numeric(chisq.test(cc.tab)$statistic)
	cc.v<-1-sqrt(as.numeric(chi.stat)/(length(identity1)*(min(dim(cc.tab))-1)))
	}
	return(cc.v)
}

# Calculate Ordinal Cross-Cuttingness
# One ordinal variable, one categorical variable
cc.o<-function(identity1, ordinal.identity2){
	cc.tab <- as.matrix(table(as.numeric(identity1),as.numeric(ordinal.identity2)))
	cc.v <- NA
	if(min(dim(cc.tab))>1){	
	k.stat<-as.numeric(kruskal.test(g=identity1, x=as.numeric(ordinal.identity2), na.action=na.omit)$statistic)
	cc.v<-1-sqrt(as.numeric(k.stat)/(sum(cc.tab)*(min(dim(cc.tab))-1)))
	}
	return(cc.v)
}


####################
# Three-way Measures #
####################
# Fractionalization
frac3<-function(identity1, identity2, identity3){
	tab<-as.numeric(table(identity1, identity2, identity3, useNA="no")) # Create counts for each subgroup
        tab1<-tab/length(identity1) # Convert counts to percentages
	tab2<-(tab1)^2 # Square each percentage        
	return(1-sum(tab2)) # Return this measures subtracted from unity
    }

# Cross-cuttingness (three categorical variables)
cc2<-function(identity1, identity2, identity3){
	cc.tab <- as.matrix(table(as.numeric(identity1),as.numeric(identity2),as.numeric(identity3), useNA="no"))
	cc.v <- NA
	if(min(dim(cc.tab))>1){
	chi.stat<-as.numeric(chisq.test(cc.tab)$statistic)
	cc.v<-1-sqrt(as.numeric(chi.stat)/(length(identity1)*(min(dim(cc.tab))-1)))
	}
	return(cc.v)
}





#####
# Citations
# 1. Alesina, A. et al., 1999. Public goods and ethnic divisions,

# 2. Herﬁndahl, O.C., 1950. Herﬁndahl: Concentration in the US steel industry - Google Scholar. Unpublished doctoral dissertation.

# 3. Hirschman, A.O., 1980. National Power and the Structure of Foreign Trade, Univ of California Press.

# 4. Montalvo, J. & Reynal-Querol, M., 2002. The effect of ethnic and religious conflict on growth.

# 5. Selway, J.S., 2011. The Measurement of Cross-cutting Cleavages and Other Multidimensional Cleavage Structures. Political Analysis, 19(1), pp.48–65.
