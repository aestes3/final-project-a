library(tidyverse)
library(gtsummary)
library(here)
library(janitor)
library(Hmisc)
library(summarytools)
library(ggplot2)
library(broom.helpers)
library(cardx)
library(parameters)

#Import Data
raw_data <- readRDS(here::here("data/raw/raw_data.rds"))
#Drop unnecessary variables and NA
edited <- raw_data |>
										select(c(subject_id, age, clinic_name,
																			gender, test_id, result, drive_thru_ind,
																			col_rec_tat, rec_ver_tat)) |>
   na.omit() |>

#Label data
		mutate(drive_thru_ind = factor(drive_thru_ind,
																	 levels = c(0,1),
																	 labels = c("No", "Yes")),

					 )




label(edited$col_rec_tat) <- "Time (hours) between collection and lab received"
label(edited$rec_ver_tat) <- "Time (hours) between received and test verification"

#Export Final dataset using here
write_csv(edited, file = here("data/clean/clean.csv"))


#Create gtsummary table (table1)
table1 <- tbl_summary(
	edited,
	by = gender,
	include = c(age, result, drive_thru_ind, col_rec_tat,
							rec_ver_tat)) |>
	add_overall(last = TRUE)

summary(edited$age)
table1

#Create regression
tbl_uvregression(
	edited,
	y = age,
	include = c(result, gender, clinic_name),
	method = lm)

#Create figure
library(ggplot2)

#Previous barplot code - Obsolete
#ggplot(data=edited, aes(result)) +
#	geom_bar()
#table(edited$result)

#Boxplot with color coded results
p <- ggplot(data=edited, aes(x=result, y=age, fill= result)) +
		geom_boxplot() +
	scale_fill_manual(values=c("grey", "red", "green")) + #adds manual colors in order of result
	theme_minimal() #adds white background
p


#Boxplot creation function
#Double bracket allows for user variable to be added into graph
create_boxplot <- function(var){
	temp <- ggplot(data=edited, aes(x={{var}}, y=age, fill={{var}})) +
		geom_boxplot() +
		theme_minimal()
	return(temp)

}

create_boxplot(gender)




