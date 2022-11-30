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

table1 <- vars %>%
  group_by(UPF_quart) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table1)

table2 <- vars %>%
  group_by(UPF_cat) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table2)

table3 <- vars %>%
  group_by(BMI_cat) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table3)

table4 <- vars %>%
  group_by(UPF_BMI) %>%
  summarise(
    Group_num = n(),
  )
knitr::kable(table4)

fig1 <- 
ggplot(vars, aes(x = UPFpercentage_mean, y = BMI_cont)) + 
  geom_point() +
  labs(title = "Fig1: Scatterplot of UPF vs. BMI", x = "UPF%", y = "BMI")

fig2 <- 
ggplot(vars, aes(x = UPFpercentage_mean, y = Gluc_Fasting)) + 
  geom_point() +
  labs(title = "Fig2: Scatterplot of UPF vs. Fasting Glucose", x = "UPF%", y = "Fasting Glucose")

fig3 <- 
ggplot(vars, aes(x = UPFpercentage_mean, y = Gluc_120min)) + 
  geom_point() +
  labs(title = "Fig3: Scatterplot of UPF vs. Blood Glucose after 120 minutes", x = "UPF%", y = "Blood Glucose after 120 minutes")

vars %>%
  ggplot(aes(x = Gluc_Fasting, fill = UPF_quart)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig4: Fasting Glucose by UPF Quartiles", x = "Fasting Glucose", fill = "UPF")

vars %>%
  ggplot(aes(x = Gluc_120min, fill = UPF_quart)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig5: Blood Glucose after 120mins by UPF Quartiles", x = "Blood Glucose after 120mins", fill = "UPF")

vars %>%
  ggplot(aes(x = BMI_cont, fill = UPF_quart)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig6: BMI by UPF Quartiles", x = "BMI", fill = "UPF")

vars %>%
  ggplot(aes(x = UPF_BMI, fill = UPF_quart)) +
  geom_bar() +
  labs(title = "Fig7: UPF binary and BMI combined by UPF Quartile", x = "UPF_BMI", fill = "UPF Quartile")

vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = UPF_quart, y = Gluc_Fasting),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig8: Statistical Summary of Fasting GLucose by UPF Quartiles", x = "UPF Quartiles", y = "Fasting Glucose")

vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = UPF_quart, y = Gluc_120min),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig9: Statistical Summary of Fasting GLucose by UPF Quartiles", x = "UPF Quartiles", y = "Glucose after 120mins")

vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = BMI_cat, y = Gluc_Fasting),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig10: Statistical Summary of Fasting GLucose by UPF Quartiles", x = "BMI", y = "Fasting Glucose")

vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = BMI_cat, y = Gluc_120min),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig11: Statistical Summary of Fasting Glucose by UPF Quartiles", x = "BMI", y = "Glucose after 120mins")

