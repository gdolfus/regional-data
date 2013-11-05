# 	Gero Dolfus
# 	University of Helsinki, HECER
# 	Start: November 04, 2013.
#
#	Read data from the regional accounts from
#	Statistics Finland.
#	Public Data only.
#
#	The data has to be in the format
# 	one price, all regions, one sector
# 	one industry, all variables, one year
#
#	Links: http://193.166.171.75/Dialog/info.asp?File=060_altp_tau_106_fi.px&path=../Database/StatFin/kan/altp/&ti=Tuotanto+ja+ty%F6llisyys+seutukunnittain+1975%2D2008%2A%2C+20+toimialaa&lang=3&ansi=1&multilang=
#		
# 	Tuotanto ja työllisyys seutukunnittain 1975-2008*, 20 toimialaa


# Clear workspace.
rm(list = ls())

dirname.data <- "~/RRR_finn/data/statfin/regional/"

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 	Name observations, variables, and files.
#
# - - - - - - - - - - - - - - - - - - - - - - 
years <- as.character(1985:1995)
places <- read.table(paste(dirname.data, "seutukunnat.txt", 
	sep = ""))
places <- places[, 1]
indust <- read.csv(paste(dirname.data, "toimialat.csv", sep = ""))
indust <- c("maa", "metsa", "kala", "elintar", "puu", 
	"perus", "muu_teoll", "raken", "tukku", "majoitus", 
	"kuljetus", "rahoitus", "kiinteisto", "asuntojen", 
	"hallinto", "koulutus", "terveys", "muut_palv", 
	"kotipal", "erittelematon")

vars.original <- c("Tuotos perushintaan", "Valituotekaytto ostajanhintaan", 
 "Arvonlisays, brutto perushintaan", "Palkansaajakorvaukset", 
 "Tyolliset", "Yrittajat", "Palkansaajat", "Tehdyt tyotunnit", 
 "Yrittajien tehdyt tyotunnit", "Palkansaajien tehdyt tyotunnit", 
 "Kiintean paaoman bruttomuodostus")
vars <- c("output", "input", "value_added", "wage", 
	"empl_tot", "empl_entre", "empl_workers", "hrs", "hrs_entre", "hrs_empl", 
	"fixed_capital")

files <- NULL
obs <- matrix(NA, nrow = length(places), ncol = length(years) * 
	length(indust))
jcounter <- 0

#for (i in indust) {
for (i in c("all", "maa")) {
	industplace <- paste(i, "-", as.character(places), 
		sep = "")
	for (j in years) {
		jcounter <- jcounter + 1
		for (k in places) {
			tmp <- paste(industplace, "-", j, sep = "")
			obs[, jcounter] <- tmp	
		}
		tmp <- paste(i, "-", j, ".csv", sep = "")
			print(tmp)
			files <- rbind(files, tmp)
	}
}

rm(i, j, k, industplace, tmp)

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 	Read the files.
#
# - - - - - - - - - - - - - - - - - - - - - - 

start <- "yes"
counter <- 0

for (i in files) {
	dat <- read.csv(paste(dirname.data, i, sep = ""), header = F)
	tmp <- dat[-c(1, 2, 3, 4, 5, 6, 7, 87:96), -c(1, 
		2, 3, 4)]
	counter <- counter + 1
	rownames(tmp) <- obs[, counter]

	if (start == "yes") {
		tmpall <- rep(tmp)
		start <- "no"
	}

	tmpall <- rbind(tmpall, tmp)
}

rm(i, dat, tmp, start)

# Remove the double entry.
tmpall <- tmpall[-c(1:length(places)), ]

# Add the column names.
colnames(tmpall) <- vars

# Convert factors to numerical.
tmpall$output <- as.numeric(as.character(tmpall$output))
tmpall$input <- as.numeric(as.character(tmpall$input))
tmpall$value_added <- as.numeric(as.character(tmpall$value_added))
tmpall$wage <- as.numeric(as.character(tmpall$wage))
tmpall$empl_tot <- as.numeric(as.character(tmpall$empl_tot))
tmpall$entre <- as.numeric(as.character(tmpall$empl_entre))
tmpall$empl <- as.numeric(as.character(tmpall$empl_workers))
tmpall$hrs <- as.numeric(as.character(tmpall$hrs))
tmpall$hrs_entre <- as.numeric(as.character(tmpall$hrs_entre))
tmpall$hrs_empl <- as.numeric(as.character(tmpall$hrs_empl))
tmpall$fixed_capital <- as.numeric(as.character(tmpall$fixed_capital))


# Housekeeping.
write.csv(tmpall, paste(dirname.data,"aaa-panel.csv",sep=''))
write.csv(indust, paste(dirname.data,"aaa-indust.csv",sep=''))
write.csv(as.character(places),paste(dirname.data,"aaa-places.csv",sep=''))
write.csv(years,paste(dirname.data,"aaa-years.csv",sep=''))
write.csv(cbind(vars.original,vars),paste(dirname.data,"aaa-vars.csv",sep=''))



# Clean up.
rm(list = ls())
