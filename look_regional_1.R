# 	Gero Dolfus
# 	University of Helsinki, HECER
# 	Start: November 05, 2013.
#
#	First look at regional accounts from
#	Statistics Finland.
#	Public Data only.
#
#	Links: http://193.166.171.75/Dialog/info.asp?File=060_altp_tau_106_fi.px&path=../Database/StatFin/kan/altp/&ti=Tuotanto+ja+ty%F6llisyys+seutukunnittain+1975%2D2008%2A%2C+20+toimialaa&lang=3&ansi=1&multilang=
#		
# 	Tuotanto ja työllisyys seutukunnittain 1975-2008*, 20 toimialaa

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Setup.
#
# - - - - - - - - - - - - - - - - - - - - - - 

# Clear workspace.
rm(list = ls())
# Load the package for creating LaTeX tables.
library(xtable)


# Set the name of the directory where the data is.
dirname.data <- "~/RRR_finn/data/statfin/regional/"
# Set the name of the directory for saving LaTeX tables.
dirname.tab<-"~/RRR_finn/tables/"


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Read the data into memory.
#
# - - - - - - - - - - - - - - - - - - - - - - 


dat	<-read.csv(paste(dirname.data,'aaa-panel.csv',sep=''))
# These are useful too.
indust<-read.csv(paste(dirname.data,'aaa-indust.csv',sep=''))
places<-read.csv(paste(dirname.data,'aaa-places.csv',sep=''))
years<-read.csv(paste(dirname.data,'aaa-years.csv',sep=''))
vars<-read.csv(paste(dirname.data,'aaa-vars.csv',sep=''))

# Make these vectors.
indust<-indust[,2]
places<-places[,2]
years<-years[,2]

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		First check.
#
# - - - - - - - - - - - - - - - - - - - - - - 

tmp<-dat[2:length(places),]$output
mean(tmp)
sd(tmp)

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Create statistics by year
#		for all industries and all places.
#
# - - - - - - - - - - - - - - - - - - - - - - 


tmp.series<-matrix(NA,nrow=length(years),ncol=4)

for(i in 1:length(years)){

	# First and last place.
	# (The first entry is total.)
	
	start<-2+(i-1)*length(places)
	end<-length(places)*(i)
	
	# Output
	tmp.series[i,1]<-mean(dat[start:end,]$output)
	tmp.series[i,2]<-sd(dat[start:end,]$output)
	# Total Employment
	tmp.series[i,3]<-mean(dat[start:end,]$empl_tot)
	tmp.series[i,4]<-sd(dat[start:end,]$empl_tot)
	
}
	
rm(i,start,end)


tmp.table<-tmp.series
rownames(tmp.table)<-years
colnames(tmp.table)<-c('mean','sd','mean','sd')

tmp.textable<-xtable(tmp.table, caption='Regional Accounts -- Output', align=rep('r',ncol(tmp.table)+1), label='regacc-output')

# names(tmp.textable)=c("\multicolumn{2}{c}{Output}","\multicolumn{2}{c}{Employment}")

sink(file=paste(dirname.tab,'aaa-all-all-years',sep=''))
tmp.textable
sink() # this ends the sinking

write.csv(tmp.table,paste(dirname.data,'aaa-output-employment-all-all.csv',sep=''))

rm(tmp.series,tmp.table,tmp.textable)



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Create statistics by industry and year.
#
# - - - - - - - - - - - - - - - - - - - - - - 

tmp.series<-matrix(NA,nrow=length(years),ncol=length(indust))
# tmp.series<-matrix(NA,nrow=length(years),ncol=2)
tmp.ind<-NULL
tmp.first<-NULL
tmp.last<-NULL
for(i in 1:length(indust)){
# for(i in 1:2){
	# Number of observations year by place i.e. per industry.
	# Correct for removing total: 
	# (number of year times number of places minus one).
	tmp.ind<-1+(length(years)*length(places)-1)
	# *(i-1)
	
	for(j in 1:length(years)){
	
	# First and last place.
	# (The first entry is total.)
	tmp.first<-2+(j-1)*length(places)
	tmp.last<-length(places)*(j)
	tmp.first<-tmp.first+tmp.ind*(i-1)
	tmp.last<-tmp.last+tmp.ind*(i-1)
	
	# Average across places.
	tmp.series[j,i]<-mean(dat[tmp.first:tmp.last,]$output)
	# tmp.series[j,2]<-sd(dat[tmp.first: tmp.last,]$output)
	
	}
	
}


rm(i,j,tmp.ind,tmp.first,tmp.last)


tmp.table<-tmp.series
rownames(tmp.table)<-years
colnames(tmp.table)<-indust

tmp.textable<-xtable(tmp.table, caption='Regional Accounts -- Output by Industries', align=rep('r',ncol(tmp.table)+1), label='regacc-output-indust')
sink(file=paste(dirname.tab,'aaa-output-indust-all-years.tex',sep=''))
tmp.textable
sink() # this ends the sinking

# tmp.df<-data.frame(years,tmp.table,row.names=1:dim(tmp.table)[1])
tmp.df<-data.frame(years,tmp.table)


# Housekeeping.

write.csv(tmp.df,paste(dirname.data,'aaa-output-indust-all-years.csv',sep=''))



rm(tmp.series,tmp.table,tmp.df)





# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Create statistics by region and year.
#
# - - - - - - - - - - - - - - - - - - - - - - 

tmp.series<-matrix(NA,nrow=length(years),ncol=length(places))


for(i in 1:length(years)){
	for(j in 1:length(places)){
		tmp.i<-1+(i-1)*length(places)
		tmp.i<-tmp.i+(j-1)
		tmp.series[i,j]<-dat$output[tmp.i]
	}
}


rm(i,j,tmp.i)


tmp.table<-tmp.series
rownames(tmp.table)<-as.character(years)
colnames(tmp.table)<-places


tmp.textable<-xtable(tmp.table, caption='Regional Accounts -- Output by Regions', align=rep('r',ncol(tmp.table)+1), label='regacc-output-regions')
sink(file=paste(dirname.tab,'aaa-output-all-places-years.tex',sep=''))
tmp.textable
sink() # this ends the sinking

rownames(tmp.table)<-NULL
tmp.df<-data.frame(years,tmp.table)
# colnames(tmp.df)<-c('places',as.character(years))

# Housekeeping.

write.csv(tmp.df,paste(dirname.data,'aaa-output-all-places-years.csv',sep=''))




