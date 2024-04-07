library(tidyverse)
library(lubridate)
library(janitor)
library(naniar)
library(data.table)

debug <- FALSE

# Setting up Source path, where export is stored, if not already set
if (!exists("data_path")){
	data_path <- "./" # <- default path to export files
}

if (!dir.exists(file.path(data_path))) {
	stop(paste("Directory does not exist:", data_path))
}

# Setting up result path, where the result file will be stored, if not already set
if (!exists("result_path")){
	result_path <- "./" # <- default path to result files
}

# create result directory if not existent
if (!dir.exists(file.path(result_path))) {
	dir.create(file.path(result_path))
	if (debug) print(paste("Create dir:",result_path))
}


# read all csv files
paths <- list.dirs(data_path, recursive = FALSE) %>% sort(decreasing = TRUE)
# define result file, it will be stored one 
save_file <- paste0(result_path,"/DZIFTx.RData")

# function to read a CSV file (using fread, as it is a more robust csv parser)
# usage:  readCSV(filename, TRUE)
readCSV <- function(filename, header, stringsAsFactors = TRUE, na.strings = "NA") {
	content <- fread(
		file = filename, 
		head = header, 
		sep = ";", 
		encoding = "UTF-8", 
		integer64 = "character", 
		stringsAsFactors = stringsAsFactors, 
		na.strings = na.strings) %>% 
		as_tibble() %>% 
		mutate_all(as.character)
}

# Files of interest
filter_files <- c(
	"General_information.csv",
	"Post-operative_course.csv",
	"BImmunosuppressive_Drugs.csv",
	"FImmunosuppressive_Drugs.csv",
	"TransplantProcOver.csv|Transplant_proceedings_overview.csv",
	"Conditioning_Regimen.csv",
	"AntiInfProphy.csv|Anti_infective_prophylaxis.csv",
	"BacterialComp.csv|Bac_Bacterial_complications.csv",
	"Vir_Viral_complications.csv",
	"ParasiteComp.csv|Par_Parasite_complications.csv",
	"Myc_Fungal_complications.csv",
	"Recipient_Information.csv"
)

# helper function to prune data (filter according to status, add erfassungsdatum if missing and add center and organ)
prune_data <- function(daten, daten_meta, path, file){
	
	if ("Status" %in% names(daten)) {
		daten <- daten %>% 
			filter(Status >= 0)  
	} else {
		daten <- daten
	}
	
	if (!"Erfassungsdatum" %in% names(daten)) {
		daten <- daten %>% 
			mutate(Erfassungsdatum = NA)
	}
	
	if (nrow(daten) > 0) {
		daten <- daten %>% 
			mutate(center = case_when(grepl("HD", path)  ~ "HD",
																grepl("KUM", path) ~ "KUM",
																grepl("MRI", path) ~ "MRI",
																grepl("TUE", path) ~ "TUE",
																grepl("MHH", path) ~ "MHH",
																grepl("HDP", path) ~ "HDP",
																grepl("MHHP", path) ~ "MHH"),
						 organ = case_when(grepl("Niere", file) ~ "Niere",
						 									grepl("Herz", file) ~ "Herz",
						 									grepl("Stamm", file) ~ "Stammzellen",
						 									grepl("Pankreas", file) ~ "Pankreas",
						 									grepl("Lunge", file) ~ "Lunge",
						 									grepl("Leber", file) ~ "Leber")) %>% 
			rename(PatientID = starts_with("Patient")) %>% 
			gather(Feld, value, -PatientID, -Erfassungsdatum, -Dokument, -Container, -center, -organ, -Status, -file) %>% 
			left_join(daten_meta, by = "Feld") %>% 
			filter(!is.na(Pflichtfeld)) %>% 
			mutate(value = as.character(value))
	} else {
		daten <- NULL
	}
	
	return(daten)
}

# function to read every record and merge with meta data and entry state
calc_missperc <- function(daten, daten_entry, daten_meta, path, file){
	
	# status message
	status_mess <- daten %>% 
		distinct(file) %>% 
		pull() %>% 
		basename() %>% 
		tools::file_path_sans_ext() %>% 
		basename() %>% 
		tools::file_path_sans_ext()
	
	if (debug) print(paste("Lese Record:", status_mess))
	
	# prune data and data_entry
	daten <- prune_data(daten, daten_meta, path, file)
	daten_entry <- prune_data(daten_entry,daten_meta, path, file)
	daten_entry <- daten_entry %>% 
		select(PatientID, Dokument, Feld, entry_value = value)
	
	# join data and data_entry
	daten <- daten %>% 
		left_join(daten_entry,
							by = c("PatientID", "Feld", "Dokument"))
	
	return(daten)
}

# Function to read all files in recent export (file, path)
sum_up_miss <- function(file, path) {
	
	if (debug) print(paste("Read path:", path, ", file:", file))
	
	files1 <- list.files(pattern = file, path = path, recursive = TRUE)
	data1 <- lapply(files1, function(x) readCSV(paste0(path, "/", x), TRUE) %>% 
										mutate(file = x) %>% 
										filter(Status >= 0))
	
	files1e <- gsub(".csv", "(E).csv", files1)
	data1e <- lapply(files1e, function(x) readCSV(paste0(path, "/", x), TRUE) %>% 
									 	mutate(file = x) %>% 
									 	filter(Status >= 0))
	
	files1m <- gsub(".csv", "(M).csv", files1)
	data1m <- lapply(files1m, function(x) readCSV(paste0(path, "/", x), TRUE) %>% 
									 	select(Feld, Pflichtfeld))
	
	if (debug) print(paste(file, length(data1), "files read"))  
	
	gen_info <- NULL
	for (i in seq_along(data1)){
		gen_info[[i]] <- calc_missperc(data1[[i]], data1e[[i]], data1m[[i]], path, file = file)  
	}
	
	bind_rows(gen_info) %>%
		ungroup() %>% 
		mutate(file = file,
					 path = path)
}

# iterate over all files in the export directory
for (export_recent in paths){
	
	# Entry state file - for later merge
	entry_file <-
		list.files(path = export_recent,
							 pattern = "EntryStates",
							 recursive = TRUE) %>%
		enframe(name = NULL) %>%
		slice(1) %>%
		pull(value)
	
	entry <- readCSV(paste0(export_recent, "/", entry_file), TRUE) %>% 
		filter(grepl("Zustand", Beschreibung)) %>% 
		select(2:4)
	
	dataframe <- map_df(filter_files, function(x) sum_up_miss(x, path = export_recent))
	
	if (nrow(dataframe) > 1 && !is_empty(dataframe)) {
		# Import all files
		dftemp.all <- dataframe %>% 
			mutate(path = str_extract(path, "[0-9]+"),
						 path = ymd(path)) %>% 
			rename(export = path) %>% 
			left_join(
				entry %>% 
					select(-Beschreibung),
				by = c("entry_value" = "Code"))
		
		# integrate all dataframes into one global frame
		if (exists("df_clean")) {
			df_clean <-	bind_rows(df_clean, dftemp.all)
		} else{
			df_clean <- dftemp.all
		}
		
		# Import Personenvariablen
		if (debug) print("Lese Personenvariablen.")
		path <- export_recent
		perso <- list.files(pattern = "Personenvariablen.csv", path = path, recursive = TRUE) %>% 
			grep("LabBio", ., value = TRUE, invert = TRUE)
		
		patienten <- NULL
		for (i in seq_along(perso)) {
			patienten[[i]] <- readCSV(paste0(path, "/", perso[i]), TRUE)
			if (nrow(patienten[[i]]) > 0) {
				patienten[[i]] <- patienten[[i]] %>%
					mutate(file = perso[i], 
								 center = case_when(grepl("HD", path)  ~ "HD",
								 									 grepl("KUM", path) ~ "KUM",
								 									 grepl("MRI", path) ~ "MRI",
								 									 grepl("TUE", path) ~ "TUE",
								 									 grepl("MHH", path) ~ "MHH",
								 									 grepl("HDP", path) ~ "HDP",
								 									 grepl("MHHP", path) ~ "MHH"),
								 organ = case_when(grepl("Niere", file) ~ "Niere",
								 									grepl("Herz", file) ~ "Herz",
								 									grepl("Stamm", file) ~ "Stammzellen",
								 									grepl("Pankreas", file) ~ "Pankreas",
								 									grepl("Lunge", file) ~ "Lunge",
								 									grepl("Leber", file) ~ "Leber")) %>% 
					rename(PatientID = starts_with("Patient")) %>% 
					gather(Feld, value, -PatientID, -Erfassungsdatum, -center, -organ, -file) %>% 
					mutate(value = as.character(value), 
								 path = path, 
								 file = "Personenvariablen") %>% 
					select(PatientID, Feld, entry_value = value, file, path)
			} else {
				patienten[[i]] <- NULL
			}
		}
		rm(i)
		
		# integrate all personenvariablen into one global frame
		if (exists("personenvariablen")) {
			personenvariablen <-	bind_rows(personenvariablen, bind_rows(patienten))
		} else{
			personenvariablen <- bind_rows(patienten)
		}
	}
}

# save data frames
if (exists("dataframe") && !is_empty(dataframe)) {
	save(personenvariablen, df_clean, file = save_file)
} else {
	print("No dataframe extracted - probably empty export dir")
}
	
# delete local variables
rm(personenvariablen)
rm(df_clean)
rm(dftemp.all)
rm(entry)
rm(patienten)
rm(save_file)
rm(export_recent)
rm(filter_files)
rm(paths)
rm(entry_file)
rm(perso)
rm(path)
rm(dataframe)


# delete funtioncs
rm(calc_missperc)
rm(prune_data)
rm(sum_up_miss)
rm(readCSV)
