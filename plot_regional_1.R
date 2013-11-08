# 	Gero Dolfus
# 	University of Helsinki, HECER
# 	Start: November 06, 2013.
#
#	First look at regional accounts from
#	Statistics Finland.
#	Public Data only.
#
#	Plots.
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
# Load the package for making fancy plots.
library(ggplot2)
# Load the package for preparing data frames for plots.
library(reshape)


# Set the name of the directory where the data is.
dirname.data <- "~/RRR_finn/data/statfin/regional/"
# Set the name of the directory for the pics.
dirname.pics <- "~/RRR_finn/pics/"



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Read the data into memory.
#
# - - - - - - - - - - - - - - - - - - - - - - 


df.indust<-read.csv(paste(dirname.data,'aaa-output-indust-all-years-real.csv',sep=''))

# Remove superfluous stuff and total.
df.indust$all<-NULL
df.indust$erittelematon<-NULL

df.places<-read.csv(paste(dirname.data,'aaa-output-all-places-years-real.csv',sep=''))


# These are useful too.
indust<-read.csv(paste(dirname.data,'aaa-indust.csv',sep=''))
places<-read.csv(paste(dirname.data,'aaa-places.csv',sep=''))
years<-read.csv(paste(dirname.data,'aaa-years.csv',sep=''))
vars<-read.csv(paste(dirname.data,'aaa-vars.csv',sep=''))

# Make these vectors.
indust<-indust$indust
places<-places$places
years<-years$years


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		By industries.
#
# - - - - - - - - - - - - - - - - - - - - - - 


tmp.df<-df.indust

# Prepare the data frame for using ggplot.
tmp.df<-melt(tmp.df,id='years',variable_name='industries')

tmp.plot<-ggplot(tmp.df, aes(years,value)) + geom_line(aes(colour = industries))

# tmp.plot+ scale_y_discrete(breaks=years,labels=as.character(years))

tmp.plot

rm(tmp.df)

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		By places.
#
# - - - - - - - - - - - - - - - - - - - - - - 


# - - - - - - - - - - - - - - -
#	All places in one pictures.
# - - - - - - - - - - - - - - - 

tmp.df<-df.places[,c(-2,-3)]

# Prepare the data frame for using ggplot.
tmp.df<-melt(tmp.df,id='years',variable_name='seutukunnat')
tmp.plot<-ggplot(tmp.df, aes(years,value)) + geom_line(aes(colour = seutukunnat))
tmp.plot

rm(tmp.df)

# - - - - - - - - - - - - - - -
#	Sort the places.
# - - - - - - - - - - - - - - - 




tmp.small<-matrix(NA,nrow=length(places),ncol=1)
tmp.big<-tmp.small
counter.small<-0
counter.big<-counter.small

# Remove years column and "all regions."
tmp.df<-df.places[length(years),c(-1,-2)]
# Remove outliers.
tmp.df$"Helsinki"<-NULL
tmp.df$"Turku"<-NULL
tmp.df$"Tampere"<-NULL

# Find the mean value.
tmp.v<-tmp.df[1,]
tmp.v<-as.numeric(as.character(tmp.v))
tmp.mean<-mean(tmp.v)

for(i in 1:dim(tmp.df)[2]){
	if(tmp.df[i]<tmp.mean){
		counter.small<-counter.small+1
		tmp.small[counter.small]<-colnames(tmp.df[i])[1]
		}
		else{
		counter.big<-counter.big+1
		tmp.big[counter.big]<-colnames(tmp.df[i])[1]
		}
}

tmp.big <- tmp.big[!is.na(tmp.big)]
tmp.small <- tmp.small[!is.na(tmp.small)]

rm(i,counter.small,counter.big,tmp.df)

df.small<-df.places[,tmp.small]
df.big	<-df.places[,tmp.big]
df.small<-data.frame(years,df.small)
df.big<-data.frame(years,df.big)

# Regions below the mean output level.
df.small<-melt(df.small,id='years',variable_name='seutukunnat')
tmp.plot<-ggplot(df.small, aes(years,value)) + geom_line(aes(colour = seutukunnat))


pdf(paste(dirname.pics,'aaa-all-small-years-real.pdf',sep=''))
tmp.plot
dev.off()

# Regions above the mean.
tmp.df<-melt(df.big,id='years',variable_name='seutukunnat')
# Plot.
tmp.plot<-ggplot(tmp.df, aes(years,value)) + geom_line(aes(colour = seutukunnat))

pdf(paste(dirname.pics,'aaa-all-big-years-real.pdf',sep=''))
tmp.plot
dev.off()



tmp.tur<-df.places$"Turku"
tmp.tam<-df.places$"Tampere"


# Turku and Tampere.
tmp.df<-data.frame(years,tmp.tur,tmp.tam)
tmp.df<-melt(tmp.df,id='years',variable_name='seutukunnat')
tmp.plot<-ggplot(tmp.df, aes(years,value)) + geom_line(aes(colour = seutukunnat))

pdf(paste(dirname.pics,'aaa-all-tur-tam-years-real.pdf',sep=''))
tmp.plot
dev.off()

# Helsinki.
tmp.df<-data.frame(years,df.places$"Helsinki")
tmp.df<-melt(tmp.df,id='years',variable_name='seutukunnat')
tmp.plot<-ggplot(tmp.df, aes(years,value)) + geom_line(aes(colour = seutukunnat))

 # + scale_fill_discrete(name='',breaks=tmp.df,labels="Helsinki"))

pdf(paste(dirname.pics,'aaa-all-hel-years-real.pdf',sep=''))
tmp.plot
dev.off()