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
indust <- read.csv(paste(dirname.data, "toimialat.csv", 
	sep = ""))
indust <- c("all","maa", "metsa", "kala", "elintar", "puu", 
	"metal", "muu_teoll", "raken", "tukku", "majoitus", 
	"kuljetus", "rahoitus", "kiinteisto", "asuntojen", 
	"hallinto", "koulutus", "terveys", "muut_palv", 
	"kotipalv", "erittelematon")

vars.original <- c("Tuotos perushintaan", "Valituotekaytto ostajanhintaan", 
	"Arvonlisays, brutto perushintaan", "Palkansaajakorvaukset", 
	"Tyolliset", "Yrittajat", "Palkansaajat", "Tehdyt tyotunnit", 
	"Yrittajien tehdyt tyotunnit", "Palkansaajien tehdyt tyotunnit", 
	"Kiintean paaoman bruttomuodostus")
vars <- c("output", "input", "value_added", "wage", 
	"empl_tot", "empl_entre", "empl_workers", "hrs", 
	"hrs_entre", "hrs_empl", "fixed_capital")

files <- NULL
obs <- matrix(NA, nrow = length(places), ncol = length(years) * 
	length(indust))
jcounter <- 0

for (i in indust) {
# for (i in c("all", "maa")) {
	industplace <- paste(i, "-", as.character(places), 
		sep = "")
	for (j in years) {
		jcounter <- jcounter + 1
		for (k in places) {
			tmp <- paste(industplace, "-", j, sep = "")
			obs[, jcounter] <- tmp
		}
		tmp <- paste(i, "-", j, ".csv", sep = "")
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
	dat <- read.csv(paste(dirname.data, i, sep = ""), 
		header = F)
	tmp <- dat[-c(1, 2, 3, 4, 5, 6, 7, 87:96), -c(1, 
		2, 3, 4)]
	counter <- counter + 1
	rownames(tmp) <- obs[, counter]

	if (start == "yes") {
		tmp.all <- rep(tmp)
		start <- "no"
	}

	tmp.all <- rbind(tmp.all, tmp)
}

rm(i, dat, tmp, start)

# Remove the double entry.
tmp.all <- tmp.all[-c(1:length(places)), ]

# Add the column names.
colnames(tmp.all) <- vars

# Convert factors to numerical.
tmp.all$output <- as.numeric(as.character(tmp.all$output))
tmp.all$input <- as.numeric(as.character(tmp.all$input))
tmp.all$value_added <- as.numeric(as.character(tmp.all$value_added))
tmp.all$wage <- as.numeric(as.character(tmp.all$wage))
tmp.all$empl_tot <- as.numeric(as.character(tmp.all$empl_tot))
tmp.all$entre <- as.numeric(as.character(tmp.all$empl_entre))
tmp.all$empl <- as.numeric(as.character(tmp.all$empl_workers))
tmp.all$hrs <- as.numeric(as.character(tmp.all$hrs))
tmp.all$hrs_entre <- as.numeric(as.character(tmp.all$hrs_entre))
tmp.all$hrs_empl <- as.numeric(as.character(tmp.all$hrs_empl))
tmp.all$fixed_capital <- as.numeric(as.character(tmp.all$fixed_capital))


# Housekeeping.
write.csv(tmp.all, paste(dirname.data, "aaa-panel.csv", 
	sep = ""))
write.csv(indust, paste(dirname.data, "aaa-indust.csv", 
	sep = ""))
write.csv(as.character(places), paste(dirname.data, 
	"aaa-places.csv", sep = ""))
write.csv(years, paste(dirname.data, "aaa-years.csv", 
	sep = ""))
write.csv(cbind(vars.original, vars), paste(dirname.data, 
	"aaa-vars.csv", sep = ""))



# Clean up.
rm(list = ls())
