library(tidyverse)
library(gtsummary)

#Import Data
raw_data <- readRDS(here::here("data/raw/raw_data.rds"))
#Drop unnecessary variables
edited <- raw_data |>
										subset(select = c(subject_id, clinic_name, gender, test_id, result, drive_thru_ind,
													 col_rec_tat, rec_ver_tat))


#Label data
data = edited
mutate(drive_thru_ind = factor(drivethru, labels = c("No", "Yes")))

#Create gtsummary table (table1)
table1 <- tbl_summary(
	raw_data,
	by = gender,
	include = c(age, result, drive_thru_ind, col_rec_tat,
							rec_ver_tat)) |>
	add_overall(last = TRUE)
table1

