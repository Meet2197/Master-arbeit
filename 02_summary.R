# Script for Population/Summary Evaluation and Visualion --------------

# Loading used libraries --------------
library(tidyverse)
library(lubridate)
library(janitor)
theme_set(theme_light())

# Setting up result path, where the result file will be stored, if not already set
if (!exists("result_path")){
	result_path <- "./" # <- default path to result files
}

if (!dir.exists(file.path(result_path))) {
	stop(paste("Directory does not exist:", result_path))
}

dataFile <- paste0(result_path, "/DZIFTx.RData");
if (!file.exists(dataFile)){
	print(paste0(dataFile," File does not exist"))
} else {
	
	# load data
	load(dataFile)
	
	#### Report Table 1
	tab01 <- df_clean %>% 
		filter(grepl("General", file)) %>% 
		distinct(PatientID, organ, center) %>% 
		count(organ, center) %>% 
		spread(organ, n, fill = 0) %>% 
		rename(Zentrum = center) %>% 
		adorn_totals() %>% 
		adorn_totals("col")
	
	### Fig 1 Overview
	
	fig01 <- df_clean %>% 
		filter(grepl("General", file)) %>% 
		distinct(PatientID, organ, center) %>% 
		count(organ, center) %>% 
		ggplot(aes(center, n, fill = organ)) +
		geom_col() +
		scale_fill_viridis_d(option = "D") +
		coord_flip() +
		labs(x = "Zentrum", 
				 y = "Anzahl",
				 title = "",
				 fill = "Organ")
	
	### Fig 2 Cummulative
	fig02 <- df_clean %>% 
		filter(grepl("General", file)) %>% 
		distinct(PatientID, organ, center, Erfassungsdatum) %>% 
		mutate(Patient = 1,
					 date = floor_date(as.Date(Erfassungsdatum,"%d.%m.%Y"), "week")) %>% 
		group_by(date, center) %>%
		summarise(cum = sum(Patient)) %>%
		ungroup() %>%
		group_by(center) %>%
		mutate(cumsum = cumsum(cum),
					 Zentrum = center) %>%
		select(-cum) %>%
		ggplot(aes(date, cumsum, color = Zentrum)) +
		geom_line(size = 2) +
		scale_color_viridis_d(option = "D") +
		labs(x = "Datum", 
				 y = "Kumulative Anzahl",
				 title = "")
	
	
	#### Patients with 2 Organs
	report_doubles <- df_clean %>% 
		filter(grepl("General", file)) %>% 
		distinct(PatientID, organ, center) %>% 
		count(PatientID, center, sort = TRUE) %>% 
		filter(n == 2)
	warning(paste0("WARNING: ", nrow(report_doubles), " Patients are entered with 2 organs"))
	
	
	### SAVE
	file <- paste0(result_path, "/complete_summary.RData")
	save(tab01, fig01, fig02, file = file)
	
	# remove variables
	rm(file)
	rm(df_clean)
	rm(fig01)
	rm(fig02)
	rm(personenvariablen)
	rm(report_doubles)
	rm(tab01)
}
