# 	Gero Dolfus
# 	University of Helsinki, HECER
# 	Start: November 05, 2013.
#
#	First look at regional accounts from
#	Statistics Finland.
#	Public Data only.
#
#	Links:
#		
# 	Tuotanto ja työllisyys seutukunnittain 1975-2008*, 20 toimialaa


# Clear workspace.
rm(list = ls())
# Set the name of the directory where the data is.
dirname.data <- "~/RRR_finn/data/statfin/regional/"
# Set the name of the directory for saving LaTeX tables.
dirname.tab<-"~/RRR_finn/tables"

# Read the data into memory.
dat	<-read.csv(paste(dirname.data,'aaa-panel.csv',sep=''))
indust<-read.csv(paste(dirname.data,'aaa-indust.csv',sep=''))
places<-read.csv(paste(dirname.data,'aaa-places.csv',sep=''))
years<-read.csv(paste(dirname.data,'aaa-years.csv',sep=''))

# Make these vectors.
indust<-indust[,2]
places<-places[,2]
years<-years[,2]

# 

tmp<-dat[2:length(places),]$output
mean(tmp)
sd(tmp)

tmpseries<-matrix(NA,nrow=length(years),ncol=4)

for(i in 1:length(years)){
	
	start<-2+(i-1)*length(places)
	end<-length(places)*(i)
	# Output
	tmpseries[i,1]<-mean(dat[start:end,]$output)
	tmpseries[i,2]<-sd(dat[start:end,]$output)
	# Total
}
	
rm(i,start,end)

# Load the package for creating LaTeX tables.
library(xtable)

tmptable<-tmpseries
rownames(tmptable)<-years
colnames(tmptable)<-c('mean','sd','mean','sd')

tmptextable<-xtable(tmptable, caption='Regional Accounts -- Output', align=rep('r',ncol(tmptable)+1), label='regacc-output')

sink(file=paste(dirname.tab,'firstlook.tex'))
sink() # this ends the sinking



rm(tmpseries,tmptable,tmptextable)
