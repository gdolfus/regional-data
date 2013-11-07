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


df.indust<-read.csv(paste(dirname.data,'aaa-output-indust-all-years.csv',sep=''))

# Remove superfluous stuff and total.
df.indust$X<-NULL
df.indust$all<-NULL
df.indust$erittelematon<-NULL

df.places<-read.csv(paste(dirname.data,'aaa-output-all-places-years.csv',sep=''))

# Remove superfluous stuff.
df.places$X<-NULL
# Make it pretty.
# colnames(df.places)<-c('places',as.character(years))


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
# 		Plot some stuff.
#
# - - - - - - - - - - - - - - - - - - - - - - 


# By industries.

tmp.df<-df.indust

# Prepare the data frame for using ggplot.
tmp.df<-melt(tmp.df,id='years',variable_name='industries')

tmp.plot<-ggplot(tmp.df, aes(years,value)) + geom_line(aes(colour = industries))

# tmp.plot+ scale_y_discrete(breaks=years,labels=as.character(years))

tmp.plot

rm(tmp.df)

# By regions.

tmp.df<-df.places[,c(-2,-3)]

# Prepare the data frame for using ggplot.
tmp.df<-melt(tmp.df,id='years',variable_name='seutukunnat')

tmp.plot<-ggplot(tmp.df, aes(years,value)) + geom_line(aes(colour = seutukunnat))

# tmp.plot+ scale_y_discrete(breaks=years,labels=as.character(years))

tmp.plot

rm(tmp.df)