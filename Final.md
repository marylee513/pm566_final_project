Final
================
Yiping Li
2022-12-07

``` r
library(readxl)
library(plyr)
library(dplyr)
library(skimr)
library(ggplot2)
```

Introduction: In the United States, more than half of the total dietary
energy consumption has been composed by ultra-processed foods (UPFs).
Most UPFs are calorie-dense and high in sugar, salt, and unhealthy fats,
while low in protein, vitamins, and minerals. Studies have demonstrated
that the increased consumption of UPFs could result in a poor
nutritional quality of the overall diet and high chances of developing
chronic diseases. Therefore, the aim of the study focuses on exploring
the association between UPF consumption and obesity, which is the
primary study interest, and type 2 diabetes (T2DM) among young adults,
using data obtained from the Children’s Health Study (CHS).

Method: UPF Classification In this study, UPFs are defined as food items
that go through multiple manipulated process before people purchase or
eat them. A total of 1167 food items (including ingredients) were
collected at baseline visit and a total of 807 food items were collected
were collected at follow-up visit using the 24-hour dietary recalls
(24HRs). Two reviewers conducted following two processes independently.
Firstly, reviewers used NOVA classification to group food items into 4
groups: unprocessed and minimally processed foods, processed culinary
ingredients, processed foods, and ultra-processed foods (UPFs).
Secondly, reviewers grouped the first 3 groups into non-UPFs group, so
only two groups were resulted. After grouping, reviewers had
disagreements on food item classification, reviewers share own
perspectives to seek an agreement. In general, UPF classification
criteria were as following: the major ingredient of a food item; how the
food is made in the U.S. normally; the way most people get the food –
purchase from a grocery store or homemade.

The data used were obtained from the baseline visit (between 2014 and
2018) of the CHS, which contained 158 participants aging from 17 to 22
years old. The 24-hour dietary recall was used to obtain the food items
that participants consumed and to further calculate the UPF consumption
percentage (UPF%). The inclusion criteria of participants were
successfully completing the 24-hour dietary recall, BMI measurement, and
glucose measurements. UPF% equaled to the UPF consumption amount in
grams divided by total food consumption amount in grams. Demographic
variables, body mass index (BMI), UPF%, and T2DM related variables
(fasting glucose, blood glucose after 120 minutes, and HbA1c) were
included in this study. BMI was used as the reference of obesity, and
fasting glucose and blood glucose after 120 minutes were used as
references of T2DM.

``` r
metaair <- readRDS("/Users/yiping/Desktop/Epi research/OneDrive - University of Southern California/MetaChem/metaair.rds")
UPFMean_grams <- read_excel("/Users/yiping/Desktop/Epi research/USC-Liz/independent research - UPF/UPF% calculation2/UPFgrams_baseline.xlsx")
```

``` r
myvars <- c("ID", "cage", "male", "race2", "bmi_avg", "bmiclass", "diabetesbinary", "gluc_fasting", "gluc_120min", "hba1c")
subsetmetachem <- metaair[myvars]
```

``` r
vars <- merge(subsetmetachem, UPFMean_grams)
```

To better apply the numeric variable (the UPF%) in the study, the author
created 2 categorical UPF% variables. The first one has 4 groups, which
was created by evenly dividing the UPF% into 4 groups. The second one
was a binary UPF variable, which was created by assuming those who
consumed less than 50% of UPFs as low UPF consumption group and the rest
as high UPF consumption group.

``` r
vars$UPF_cat <- cut(vars$UPFpercentage_mean,
                          breaks=c(0, 25, 50, 75, 100),
                          labels=c("1", "2", "3", "4"))

vars$UPF_binary <- cut(vars$UPFpercentage_mean,
                             breaks=c(0, 50, 100),
                             labels=c("0", "1"))
```

Appropriate names of variables were given to better view the dataset.

``` r
names(vars)[2] <- "Age"
names(vars)[3] <- "Male"
names(vars)[4] <- "Race"
names(vars)[5] <- "BMI_cont"
names(vars)[6] <- "BMI_cat"
names(vars)[7] <-  "Diabetes_cat"
names(vars)[8] <- "Gluc_Fasting"
names(vars)[9] <- "Gluc_120min"
names(vars)[10] <- "HbA1C"
```

To better use the numeric variable, sex, I changed the format of it into
a categorical variable to give proper names of the groups in it.

``` r
vars$Male <- as.factor(vars$Male)
vars$Male <- revalue(vars$Male, c("0" = "Female", "1" = "Male"))
```

Participants with missing values in T2DM or fasting glucose or blood
glucose after 120 minutes were excluded. Participants with implausible
fasting glucose of greater than 200 mg/dL and blood glucose after 120
minutes of greater than 400 mg/dL were also excluded.

``` r
vars <- vars %>%
  filter(!is.na(Diabetes_cat)) %>%
  filter(!is.na(Gluc_Fasting)) %>%
  filter(!is.na(Gluc_120min)) %>%
  filter(Gluc_Fasting < 200) %>%
  filter(Gluc_120min < 400)
```

A new categorical variable (names as “UPF and BMI”), that contained all
cases of the UPF binary variable and 3 BMI categories combination was
created to explore the association between UPF consumption and BMI.

``` r
vars <- vars %>%
  mutate(UPF_BMI = factor(
    case_when(UPF_binary == 0 & BMI_cat == "Normal" ~ "Low UPF Normal Weight",
              UPF_binary == 0 & BMI_cat == "Overweight" ~ "Low UPF Overweight",
              UPF_binary == 0 & BMI_cat == "Obese" ~ "Low UPF Obese",
              UPF_binary == 1 & BMI_cat == "Normal" ~ "High UPF Normal Weight",
              UPF_binary == 1 & BMI_cat == "Overweight" ~ "High UPF Overweight",
              UPF_binary == 1 & BMI_cat == "Obese" ~ "High UPF Obese")
  ))

vars<- vars %>%
  filter(!is.na(UPF_BMI))
```

Tables were created for readers to understand the numeric values of
study of interests. Summaries tables of fasting glucose and blood
glucose after 120 minutes containing total numbers, minimum values, and
maximum values in regard to UPF 4 groups, UPF binary variable, and BMI
categories were created. The summary table of number of subjects for
each “UPF and BMI” combination was also created.

Different kinds of plots were created for readers to visualize the
exposure and outcomes in the study. Smooth lineplots of UPF% vs. BMI,
fasting glucose, and blood glucose after 120 minutes were created.
Histograms of BMI, fasting glucose, and blood glucose after 120 minutes
by sex were created. A barchart of the combination of UPF% and BMI by
UPF% 4 groups and a statistical summary graph of UPF% by BMI were also
created.

Table 1 to 3 are descriptive tables showing the number of participants,
the minimum and maximum vales of fasting glucose and blood glucose after
120 minutes. Most participants were overweight obese and ate less than
50% of UPFs. Only 4 participants ate UPF in 50-75% and only 1
participant ate UPF greater than 75%.

``` r
table1 <- vars %>%
  group_by(BMI_cat) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table1)
```

| BMI_cat    | Gluc_Fasting_min | Gluc_Fasting_max | Gluc_120min_min | GLuc_120min_max | Gluc_num |
|:-----------|-----------------:|-----------------:|----------------:|----------------:|---------:|
| Normal     |               74 |              107 |              55 |             176 |       24 |
| Obese      |               77 |              119 |              83 |             208 |       56 |
| Overweight |               76 |              107 |              55 |             200 |       73 |

``` r
table2 <- vars %>%
  group_by(UPF_cat) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    Gluc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table2)
```

| UPF_cat | Gluc_Fasting_min | Gluc_Fasting_max | Gluc_120min_min | Gluc_120min_max | Gluc_num |
|:--------|-----------------:|-----------------:|----------------:|----------------:|---------:|
| 1       |               76 |              119 |              55 |             208 |      105 |
| 2       |               74 |              110 |              70 |             200 |       43 |
| 3       |               76 |               95 |              99 |             131 |        4 |
| 4       |               91 |               91 |             135 |             135 |        1 |

``` r
table3 <- vars %>%
  group_by(UPF_binary) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table3)
```

| UPF_binary | Gluc_Fasting_min | Gluc_Fasting_max | Gluc_120min_min | GLuc_120min_max | Gluc_num |
|:-----------|-----------------:|-----------------:|----------------:|----------------:|---------:|
| 0          |               74 |              119 |              55 |             208 |      148 |
| 1          |               76 |               95 |              99 |             135 |        5 |

Table 4 combines the circumstances of UPF consumption and BMI
categories, indicating that most participants ate low UPF in their diet
and were overweight or obese.

``` r
table4 <- vars %>%
  group_by(UPF_BMI) %>%
  summarise(
    Group_num = n(),
  )
knitr::kable(table4)
```

| UPF_BMI               | Group_num |
|:----------------------|----------:|
| High UPF Obese        |         2 |
| High UPF Overweight   |         3 |
| Low UPF Normal Weight |        24 |
| Low UPF Obese         |        54 |
| Low UPF Overweight    |        70 |

Figure 1 to 3 shows UPF% vs BMI, fasting glucose, and blood glucose
after 120 minutes correspondingly. No clear patten between UPF% and
those variables interested in is seen from those figures.

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = BMI_cont)) + 
  geom_point() +
  labs(title = "Fig1: Scatterplot of UPF vs. BMI", x = "UPF%", y = "BMI") + 
  geom_smooth(method = lm, se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Final_files/figure-gfm/scatterplots%20and%20smoothed%20slineplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-1.png)<!-- -->

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = BMI_cont)) + 
  geom_line () +
  geom_smooth () +
  labs(title = "Fig1: Smooth Lineplot of UPF vs. BMI", x = "UPF%", y = "BMI") 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Final_files/figure-gfm/scatterplots%20and%20smoothed%20slineplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-2.png)<!-- -->

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = Gluc_Fasting)) + 
  geom_point() +
  labs(title = "Fig2: Scatterplot of UPF vs. Fasting Glucose", x = "UPF%", y = "Fasting Glucose") + 
  geom_smooth(method = lm, se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Final_files/figure-gfm/scatterplots%20and%20smoothed%20slineplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-3.png)<!-- -->

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = Gluc_Fasting)) + 
  geom_line () +
  geom_smooth () +
  labs(title = "Fig2: Smooth Lineplot of UPF vs. Fasting Glucose", x = "UPF%", y = "BMI") 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Final_files/figure-gfm/scatterplots%20and%20smoothed%20slineplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-4.png)<!-- -->

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = Gluc_120min)) + 
  geom_point() +
  labs(title = "Fig3: Scatterplot of UPF vs. Blood Glucose after 120 minutes", x = "UPF%", y = "Blood Glucose after 120 minutes") +
  geom_smooth(method = lm, se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Final_files/figure-gfm/scatterplots%20and%20smoothed%20slineplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-5.png)<!-- -->

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = Gluc_Fasting)) + 
  geom_line () +
  geom_smooth () +
  labs(title = "Fig3: Smooth Lineplot of UPF vs. Blood Glucose after 120 minutes", x = "UPF%", y = "Blood Glucose after 120 minutes") 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Final_files/figure-gfm/scatterplots%20and%20smoothed%20slineplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-6.png)<!-- -->

Figure 4, BMI by sex, shows that males have a right skewed distribution
while females do not.

``` r
vars %>%
  ggplot(aes(x = BMI_cont, fill = as.factor(Male))) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig4: BMI by Sex", x = "BMI", fill = "Sex")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final_files/figure-gfm/histogram,%20BMI%20by%20UPF%20quartiles-1.png)<!-- -->

Figure 5, UPF% by sex, shows that both males and females have right
skewed bell shapes, and males have high UPF%. Figure 6, UPF% by BMI,
straightly shows that more participants were overweight and consumed
less than 40% UPFs.

``` r
vars %>%
  ggplot(aes(x = UPFpercentage_mean, fill = Male)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig5: UPF% by Sex", x = "UPF%", fill = "Sex")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final_files/figure-gfm/histogram,%20glu-fasting/120mins%20by%20UPF_quart-1.png)<!-- -->

``` r
vars %>%
  ggplot(aes(x = UPFpercentage_mean, fill = BMI_cat)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig6: UPF% by BMI", x = "UPF%", fill = "BMI")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final_files/figure-gfm/histogram,%20glu-fasting/120mins%20by%20UPF_quart-2.png)<!-- -->

Figure 7 shows Table 4 in a visualized way, but it does not indicate any
association between UPF plus BMI and UPF 4 categories.

``` r
vars %>%
  ggplot(aes(x = UPF_BMI, fill = UPF_cat)) +
  geom_bar() +
  labs(title = "Fig7: UPF binary and BMI combined by UPF Categories", x = "UPF_BMI", fill = "UPF Categories")
```

![](Final_files/figure-gfm/barchart,%20UPF_BMI%20by%20UPF_quart,%20cont-1.png)<!-- -->

Figure 8 shows the primary outcome of interest BMI in regard to UPF%.
Participants consumed more UPF were overweight instead of obese,
suggesting no association between UPF% and obesity, indicating by BMI
categories.

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = BMI_cat, y = UPFpercentage_mean),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig8: Statistical Summary of UPF% by BMI Categories", x = "BMI", y = "UPF%")
```

![](Final_files/figure-gfm/statistical%20summary%20graph-1.png)<!-- -->

Conclusion: Based on the above data exploration and graphs, no clear
pattern shows that UPF consumption is associated with obesity or T2DM,
indicating by BMI categories and blood glucose. One reason can be the
number of participants is limited. The other reason might be limited
outcomes of interests to study. Only 3 variables were used to make
tables and figures to visualize. Although this study does not any
association between UPF consumption and obesity and T2DM, further
studies are needed to explore any potential relationships between UPFs
and metabolic diseases.
