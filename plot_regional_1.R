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
# Load the package for creating LaTeX tables.
library(xtable)
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


tmp.df<-read.csv(paste(dirname.data,'bbb-output-by-indust.csv',sep=''))

# Remove total.
tmp.df$X<-NULL
tmp.df$all<-NULL



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Plot some stuff.
#
# - - - - - - - - - - - - - - - - - - - - - - 


# By industries.


# Prepare the data frame for using ggplot.
tmp.df<-melt(tmp.df,id='years',variable_name='industries')

tmp.plot<-ggplot(tmp.df, aes(years,value)) + geom_line(aes(colour = industries))

# tmp.plot+ scale_y_discrete(breaks=years,labels=as.character(years))

tmp.plot