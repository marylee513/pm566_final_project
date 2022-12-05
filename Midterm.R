library(readxl)
library(plyr)
library(dplyr)
library(skimr)
library(ggplot2)

metaair <- readRDS("/Users/yiping/Desktop/Epi research/OneDrive - University of Southern California/MetaChem/metaair.rds")
UPFMean_grams <- read_excel("/Users/yiping/Desktop/Epi research/USC-Liz/independent research - UPF/UPF% calculation2/UPFgrams_baseline.xlsx")

myvars <- c("ID", "cage", "male", "race2", "bmi_avg", "bmiclass", "diabetesbinary", "gluc_fasting", "gluc_120min", "hba1c")
subsetmetachem <- metaair[myvars]

vars <- merge(subsetmetachem, UPFMean_grams)

vars$UPF_quart <- cut(vars$UPFpercentage_mean,
                      breaks=c(0, 25, 50, 75, 100),
                      labels=c("1", "2", "3", "4"))

vars$UPF_cat <- cut(vars$UPFpercentage_mean,
                    breaks=c(0, 75, 100),
                    labels=c("0", "1"))

names(vars)[2] <- "Age"
names(vars)[3] <- "Male"
names(vars)[4] <- "Race"
names(vars)[5] <- "BMI_cont"
names(vars)[6] <- "BMI_cat"
names(vars)[7] <-  "Diabetes_cat"
names(vars)[8] <- "Gluc_Fasting"
names(vars)[9] <- "Gluc_120min"
names(vars)[10] <- "HbA1C"

vars <- vars %>%
  filter(!is.na(Diabetes_cat)) %>%
  filter(!is.na(Gluc_Fasting)) %>%
  filter(!is.na(Gluc_120min)) %>%
  filter(Gluc_Fasting < 200) %>%
  filter(Gluc_120min < 400)

vars <- vars %>%
  mutate(UPF_BMI = factor(
    case_when(UPF_cat == 0 & BMI_cat == "Normal" ~ "Low UPF Normal Weight",
              UPF_cat == 0 & BMI_cat == "Overweight" ~ "Low UPF Overweight",
              UPF_cat == 0 & BMI_cat == "Obese" ~ "Low UPF Obese",
              UPF_cat == 1 & BMI_cat == "Normal" ~ "High UPF Normal Weight",
              UPF_cat == 1 & BMI_cat == "Overweight" ~ "High UPF Overweight",
              UPF_cat == 1 & BMI_cat == "Obese" ~ "High UPF Obese")
  ))

vars<- vars %>%
  filter(!is.na(UPF_BMI))