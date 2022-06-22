## Polar Bear Personality Code
## Chilled-out polar bears: Reliability and validity of the five-factor personality assessment in polar bears (Ursus maritimus) in human care
## by Kerianne Chant

setwd("P:/Conservation_Research/Restricted/CRD/Research Projects/Polar Bear/Personality/Data/R/polar-bear-personality")

## Radar Charts
#### data import
library(tidyverse)
library(ggplot2)
library(reshape2)
library(fmsb)
library(yarrr)
library(irr)
library(DescTools)
library(ggpubr)
library(devtools)
library(janitor)
library(corrplot)
library(readr)
library(ggridges)
library(car)

survey_18_20 <- read.csv("Polar Bear Personality Data 2018-2020.csv")

# data wrangle----
#we need to create some vectors and dataframes that will be used throughout 
bears <- unique(survey_18_20$Bear)
female_bears <- c("Aurora", "Kaska", "Star", "Willow")
male_bears <- c("Baffin","Nanuq","Siku","Storm","York")
Years_list <- unique(survey_18_20$Year)
traits <- c("o", "c", "e", "a", "n")
Traits <- c("O", "C", "E", "A", "N")

AVG <- survey_18_20 %>%
  rowwise() %>%
  mutate(O = mean(c (O1, O2, O3, O4, O5, O6), na.rm = T)) %>%
  mutate(C = mean(c (C1, C2, C3, C4, C5, C6), na.rm = T)) %>%
  mutate(E = mean(c (E1, E2, E3, E4, E5, E6), na.rm = T)) %>%
  mutate(A = mean(c (A1, A2, A3, A4, A5, A6), na.rm = T)) %>%
  mutate(N = mean(c (N1, N2, N3, N4, N5, N6), na.rm = T))
ocean <- data.frame(AVG$Year, AVG$Bear, AVG$O, AVG$C, AVG$E, AVG$A, AVG$N)
colnames(ocean) <- c("Year", "Bear", "O", "C", "E", "A", "N")

# Functions ----

# This function builds an overlapping radar chart for a given set of bears in a year of interest
radar.chart <- function (data, year, bears, title) {
  
int_data <- data %>% 
  filter(Year == year, Bear %in% bears) %>%
  group_by(Bear) %>%
  summarise_all("mean") %>%
  mutate(across(O:N, ~ 8-.)) %>%
  tibble::column_to_rownames('Bear') %>%
  select(-Year)

int_data2 <- rbind(rep(7,4) , rep(1,4) , int_data)
par(mfrow=c(1,2))
par(bg="white")
par(mar = c(1,1,1,1))
prettynm = c(yarrr::transparent("orchid", trans.val = .6), yarrr::transparent("yellow4", trans.val = .6), yarrr::transparent("mediumseagreen", trans.val = .6), yarrr::transparent("dodgerblue", trans.val = .6), yarrr::transparent("lightcoral", trans.val = .6))
pretty = c(yarrr::transparent("orchid", trans.val = .2), yarrr::transparent("yellow4", trans.val = .2), yarrr::transparent("mediumseagreen", trans.val = .2), yarrr::transparent("dodgerblue", trans.val = .2), yarrr::transparent("lightcoral", trans.val = .2))
radarchart(int_data2, seg = 3, plwd = 4, cglty = 1, plty = 1, vlcex = 1.1, axistype = 4, caxislabels=seq(1,7,2), cglcol = "grey", axislabcol = "black", pcol = pretty, pfcol = prettynm) +
  legend(legend = bears, x = -1.1, y = 1.3, title = title, bty = "n", pch=20 , col = prettynm, text.col = "gray50", cex = 1.3, pt.cex=2) +
  plot(rnorm(10))
legend(x = -1.59, y = 1.44, "A", bty="n", cex = 2.3)
}

# This function retrieves the Kendall test results for a bear given bear
kendall.within.bear <- function (bear, data) {
  
  cor_data <- data %>%
    filter(Bear == bear) %>%
    group_by(Observer)  %>%
    summarise(across(O1:N6, mean, na.rm = TRUE)) %>%
    data.table::transpose(make.names = 'Observer', keep.names = 'Observer') %>%
    column_to_rownames(var="Observer")
  KendallW(cor_data, TRUE, test = TRUE)
  }
 
# This function retrieves the Kendall test results for a given bear and factor
kendall.by.factor <- function  (trait, bear, data) {
  byfactor_data <- data %>%
    filter(Bear == bear) %>%
    select(Observer, contains(trait), -Exhibit, -Bear, -Month, -Year) %>%
    group_by(Observer)  %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    data.table::transpose(make.names = 'Observer', keep.names = 'Observer') %>%
    column_to_rownames(var="Observer")
  KendallW(byfactor_data, TRUE, test = FALSE)
}

# This function retrieves Cronbach alpha value for a given factor, bear, and year 
btw.years.alpha <- function (trait, bear, year, data) {
  
  bear_factor <- data %>%
    filter(Year == year, Bear == bear) %>%
    select(contains(trait),-Observer, -Exhibit, -Bear, -Month, -Year)
  CronbachAlpha(bear_factor, na.rm = TRUE)
}

# This function retrieves an overall Cronbach alpha value for each factor
overall.alpha <- function (trait, data) {
  
  trait <- survey_18_20 %>% select(contains(trait), -Observer, -Exhibit, -Bear, -Month, -Year)
  CronbachAlpha(trait, na.rm = TRUE) 
}

# This function retrieves the inter item correlation data to be visualized
inter.item.cor <- function (trait, data) {
  
  trait_cor <- data %>% select(contains(trait), -Observer, -Exhibit, -Bear, -Month, -Year)
  trait_cor <- cor(trait_cor, method = "pearson", use = "complete.obs")
 
  trait_cor <- trait_cor %>% 
    melt() %>%
    select(-Var1, -Var2) %>%
    mutate(factor = trait)
}
 
# This function tests if there is a statistical difference between the sexes for
# a given factor
sex.difs.test <- function (trait, data) {
  
  t_test_data <- data %>% select(contains(trait), Bear, -Observer, -Exhibit, -Month, -Year) %>%
    melt()
  t.test(t_test_data$value ~ t_test_data$Bear)
}

# This function tests the if there is a statistical difference between the sexes for
# a given factor in a given year
sex.dif.btw <- function (trait, year, data) {
  inter_data <- data %>% filter(Year == year) %>%
    mutate(Bear = ifelse(Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")) %>%
    select(contains(trait), Bear, -Observer, -Exhibit, -Month, -Year) %>%
    melt()
  t.test(inter_data$value ~ inter_data$Bear)
}

# This fuction tests the retest repeatability among years using using the 
# Intraclass Correlation Coefficient 
repeatability <- function (bear, data) {
  
  bear <- data %>% filter(Bear == bear) %>%
    group_by(Year) %>%
    summarise(across(O1:N6, mean, na.rm = TRUE)) %>%
    data.table::transpose(make.names = 'Year', keep.names = 'Year') %>%
    column_to_rownames(var="Year")
  icc(bear, model = "twoway",
      type = "consistency",
      unit = "average", r0 = 0, conf.level = 0.95)
  bear <- cor(bear)
}

# This fuction tests the retest repeatability within a given factor and bear using the 
# Intraclass Correlation Coefficient 
repeatability.within <- function (bear, trait, data) {
  
  bear <- data %>% filter(Bear == bear) %>%
    group_by(Year) %>%
    summarise(across(O1:N6, mean, na.rm = TRUE)) %>%
    select(contains(trait), Year) %>%
    data.table::transpose(make.names = 'Year', keep.names = 'Year') %>%
    column_to_rownames(var="Year")
  icc(bear, model = "twoway",
      type = "consistency",
      unit = "average", r0 = 0, conf.level = 0.95)$value
}

# This function plots the Test-Retest Reliability for a given bear
reliability.plot <- function (bear, plot.title = NULL, data) {
  
  bear <- data %>% filter(Bear == bear) %>%
    group_by(Year) %>%
    summarise_at(vars(O:N), mean, na.rm = TRUE)  %>%
    melt(id.vars = "Year") %>%
    mutate(value = 8 - value, variable = as.factor(variable), Year = as.factor(Year))
  ggplot(bear) +
    aes(x = variable, y = value, fill = Year) +
    geom_col(position = "dodge") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_minimal() +
    coord_flip() +
    theme(text = element_text(size=20)) +
    scale_fill_manual(values = c(yarrr::transparent("lightcoral", trans.val = .3), yarrr::transparent("mediumseagreen", trans.val = .3), yarrr::transparent("dodgerblue", trans.val = .3))) +
    theme(legend.text = element_text(size = 20, face = "bold"), legend.title = element_text(size = 20)) +
    labs(title = plot.title, x = "Personality Factors", y = "Average of Ratings", cex = 1.3) +
    theme(axis.line = element_line(size = 0.5, colour = "grey90"))}

# B.1 | Radar Chart Code ----


male_radar2018 <- radar.chart(data = ocean, year = "2018", bears = male_bears, 
                              title = "Male Bears")

female_radar2018 <- radar.chart(data = ocean, year = "2018", bears = female_bears,
                            title = "Female Bears")

male_radar2019 <- radar.chart(data = ocean, year = "2019", bears = male_bears, 
                              title = "Male Bears")

female_radar2018 <- radar.chart(data = ocean, year = "2019", bears = female_bears,
                                title = "Female Bears")

# B.2 - Interrater Reliability: Kendall's Coefficient of concordance Wt - between rater difference ----

## Interrater Correlation Matrix ----
total <- aggregate(survey_18_20, by = list(survey_18_20$Observer), mean, na.rm = TRUE)
total <- data.frame(subset(total, select = -c(Year, Exhibit, Month, Observer, Bear)))
total <- data.frame(t(total))
total <- total %>% row_to_names(row_number = 1) %>%
  mutate_all(as.numeric)
total <- round(cor(total, method = "kendall"),2)
totala <- function(total){
  total[upper.tri(total)] <- NA
  return(total)
}
total2 <- totala(total)
total <- melt(total)
mean(total$value, na.rm = TRUE)
sd(total$value, na.rm = TRUE)
par(mfrow=c(1,1))
ggplot(data = total, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "White") +
  scale_fill_gradient2(low = "mediumorchid2", high = "royalblue1", mid = "powderblue",
                       midpoint = 0.5, na.value = "White", limit = c(0,1), space = "Lab",
                       name="Kendall's\nCoefficient") +
  theme(legend.key.size = 30) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12)) +
  coord_fixed() +
  labs(x = "Raters", y = " ") +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal") +
  coord_fixed(ratio = 0.8) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 2,
                               title.position = "top", title.hjust = 0.5))

## Within Each Bear across all the factors----

within_bear_results <- sapply(bears, kendall.within.bear, survey_18_20)

##### Within Each Bear and broken down within each Factor

kendall_by_factor  <- data.frame()   
for (a in bears) {output <- sapply(traits, kendall.by.factor, a, survey_18_20)
kendall_by_factor <- rbind(kendall_by_factor, output)# Store output in dataframe
}
colnames(kendall_by_factor) <- traits
rownames(kendall_by_factor) <- bears

##### Overall Mean Within The Factors
kendall_summary <- sapply(kendall_by_factor, function(y) c( "Stand dev" = sd(y), 
                         "Mean"= mean(y,na.rm=TRUE)))

## graph for visualization (not included in thesis, mean and standard deviation listed instead)
kendall_plot_by_factor <- melt(kendall_by_factor)
ggplot(kendall_plot_by_factor, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_point () +
  theme_minimal()
----------------------------------------------------------------------------------
  
# B.3 | Internal Consistency Code ----

## Internal Consistency: Cronbach's Alpha ---- 
## how well the behavioural descriptions describe each factor (e.g how well being assertive" specifically 
## describes Extraversion in polar bears)

#### Overall

overall_alpha <- data.frame(sapply(Traits, overall.alpha, survey_18_20))

#### Between The Years

cons_2018 <- data.frame()
for (a in bears) {output <- sapply(Traits, btw.years.cons, a, 2018, survey_18_20)
cons_2018 <- rbind(cons_2018, output)# Store output in dataframe
}

colnames(cons_2018) <- Traits
cons_2018$Bear <- bears
cons_2018 <- cons_2018 %>% melt(id = "Bear", value.name = "Alpha", variable.name = "Factor") %>%
  mutate(Year = 2018)

cons_2019 <- data.frame()
for (a in bears) {output <- sapply(Traits, btw.years.cons, a, 2019, survey_18_20)
cons_2019 <- rbind(cons_2019, output)# Store output in dataframe
}

colnames(cons_2019) <- Traits
cons_2019$Bear <- bears
cons_2019 <- cons_2019 %>% melt(id = "Bear", value.name = "Alpha", variable.name = "Factor") %>%
  mutate(Year = 2019)

cons_2020 <- data.frame()
for (a in bears) {output <- sapply(Traits, btw.years.cons, a, 2020, survey_18_20)
cons_2020 <- rbind(cons_2020, output)# Store output in dataframe
}

colnames(cons_2020) <- Traits
cons_2020$Bear <- bears
cons_2020 <- cons_2020 %>% melt(id = "Bear", value.name = "Alpha", variable.name = "Factor") %>%
  mutate(Year = 2020)

means <- rbind(cons_2018, cons_2019, cons_2020)

### Cronbach's Alpha Correlation Graphs (2018,2019,2020)
means$Year <- as.factor(means$Year)
means$Alpha <- as.numeric(means$Alpha)
means$Factor <- factor(means$Factor, levels = c("O", "C", "E", "A", "N"))
means <- data.frame(subset(means, (means$Alpha > 0)))
g <- ggplot(data = means, aes(x = Factor, y = Alpha, fill = Factor)) +
  geom_boxplot () +
  geom_point() +
  facet_wrap(~ Year, nrow = 3) +
  theme_minimal() +
  scale_fill_manual(values = c(yarrr::transparent("lightcoral", trans.val = .3), yarrr::transparent("gold", trans.val = .4), yarrr::transparent("mediumseagreen", trans.val = .3), yarrr::transparent("dodgerblue", trans.val = .3), yarrr::transparent("orchid", trans.val = .3))) +
  theme(axis.text = element_text(size = 12), axis.title=element_text(size=14), strip.text = element_text(size = 14, face = "bold"))
g + labs(y = "Cronbach's Alpha", x = "Personality Factors") +
  theme(legend.position = "none")+
  geom_hline(yintercept = 0.5, size = 1, color = "red") +
  theme(axis.line = element_line(size = 0.5, colour = "grey90"))

## Inter-Item Correlation ----

inter_item_cor <- lapply(traits, inter.item.cor, survey_18_20)
inter_item_cor <- data.frame(do.call(rbind, inter_item_cor))

g <- ggplot(inter_item_cor, aes(x = factor, y = value, fill = factor)) +
  geom_boxplot() +
  geom_point() +
  theme_minimal() +
  scale_fill_manual(values = c(yarrr::transparent("lightcoral", trans.val = .3), yarrr::transparent("gold", trans.val = .4), yarrr::transparent("mediumseagreen", trans.val = .3), yarrr::transparent("dodgerblue", trans.val = .3), yarrr::transparent("orchid", trans.val = .3))) +
  scale_y_continuous(limits = c(-1,1)) +
  theme(axis.text = element_text(size = 12), axis.title=element_text(size=14), strip.text = element_text(size = 12, face = "bold"))
g + labs(y = "Pearson Correlation Coefficient", x = "Personality Factors") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0.15, size = 1, color = "red") +
  geom_hline(yintercept = 0.50, size = 1, color = "red") +
  theme(axis.line = element_line(size = 0.5, colour = "grey90"))
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
# B.4 | Difference and Distribution Code----
# Difference Between the Bears and Distribution

## Levene's Test ----
# check to see if I can use One-way ANOVA (when p > 0.05) ["homogeneity of variance"]

leveneTest(O ~ Bear, AVG)
leveneTest(C ~ Bear, AVG)
leveneTest(E ~ Bear, AVG)
leveneTest(A ~ Bear, AVG)
leveneTest(N ~ Bear, AVG)

## Factor Rating Differences ----
#Analysis of Variance (ANOVA) - ONe-Way
res.aov1 <- aov(O ~ Bear, data = AVG, na.rm = TRUE)
summary(res.aov1)
res.aov <- aov(C ~ Bear, data = AVG, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(E ~ Bear, data = AVG, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(A ~ Bear, data = AVG, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(N ~ Bear, data = AVG, na.rm = TRUE)
summary(res.aov)

## Maturation Effects ----
# Analysis of Variance (ANOVA) - ONe-Way
res.aov1 <- aov(O ~ Year, data = AVG, na.rm = TRUE)
summary(res.aov1)
res.aov <- aov(C ~ Year, data = AVG, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(E ~ Year, data = AVG, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(A ~ Year, data = AVG, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(N ~ Year, data = AVG, na.rm = TRUE)
summary(res.aov)
o <- data.frame(subset(AVG, select = c(Year, O)))
o <- aggregate(o, by = list(o$Year), mean, na.rm = TRUE)
o <- data.frame(add_column(o, "O" = 8 - o$O))
c <- data.frame(subset(AVG, select = c(Year, C)))
c <- aggregate(c, by = list(c$Year), mean, na.rm = TRUE)
c <- data.frame(add_column(c, "C" = 8 - c$C))
e <- data.frame(subset(AVG, select = c(Year, E)))
e <- aggregate(e, by = list(e$Year), mean, na.rm = TRUE)
e <- data.frame(add_column(e, "E" = 8 - e$E))
a <- data.frame(subset(AVG, select = c(Year, A)))
a <- aggregate(a, by = list(a$Year), mean, na.rm = TRUE)
a <- data.frame(add_column(a, "A" = 8 - a$A))
n <- data.frame(subset(AVG, select = c(Year, N)))
n <- aggregate(n, by = list(n$Year), mean, na.rm = TRUE)
n <- data.frame(add_column(n, "N" = 8 - n$N))

## Distribution of personality factors graph----
distribution_graph <- AVG %>%
  select(Year, Bear, O, C, E, A, N) %>%
  melt(id = c("Year", "Bear")) %>%
  mutate(value = 8 - value)

distribution_graph$variable <- factor(distribution_graph$variable, levels = c("N", "A", "E", "C", "O"))
theme_set(theme_minimal())
ggplot(distribution_graph, aes(x = value, y = variable, group = variable)) +
  geom_density_ridges2(scale = 1.8, alpha = 0.8, aes(fill = variable, rel_min_height = 0.01)) +
  scale_fill_manual(values = c("orchid", "dodgerblue", "mediumseagreen", "gold", "lightcoral")) +
  theme(legend.position = "none") +
  labs(y = "Personality Factors", x = "Distribution") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18)) +
  scale_x_continuous(limits = c(0,8)) +
  geom_vline(xintercept = 4, size = 1, color = "red") +
  theme(axis.line = element_line(size = 0.5, colour = "grey90"))

## Sex Differences ---- 
#Welch Two Sample t-test

sex_difs <- survey_18_20 %>%
  mutate(Bear = ifelse(Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M"))

sex_difs_results <- sapply(Traits, sex.difs.test, sex_difs)

## Sex differences Between the years

sex_difs_btw <- list()
for (value in Years_list) {output <- sapply(Traits, sex.dif.btw, value, survey_18_20)
sex_difs_btw <- rbind(sex_difs_btw, output)
}
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# B.5 | Test Retest Repeatability Code----
## Test Retest Repeatability: Intraclass Correlation Coefficient (ICC) - between year difference

#### Bears Overall

overall_repeat <- sapply(bears, repeatability, survey_18_20)
rownames(overall_repeat) <- c("2018-2018", "2018-2019", "2018-2020", "2018-2019",
                              "2019-2019", "2019-2020", "2018-2020", "2019-2020",
                              "2020-2020")

# ***ICC(3,k) Two-way mixed, Average measure. Reliability is calculated by taking an average of the k raters measurements.
# Baffin and Star around 0.55, everyone else above 0.85 (very consistent).
#### ICC within the Factors and Bears

datalist <- list() 
icc_results <-  list()
 for (value in Traits) {output <- data.frame(sapply(bears, repeatability.within, value, survey_18_20))
 datalist[[value]] <- output
 }
icc_results <- do.call(cbind, datalist)
colnames(icc_results) <- Traits
 
icc_summary <- rbind(icc_results, (sapply(icc_results, function(y) c( "sd" = sd(y), 
                                    "mean"= mean(y,na.rm=TRUE)))))
 
## Graph for visualization of Test Retest Correlations (not included in thesis, mean and standard deviation included instead)

boxplot(icc_results, ylim = c(0,1.1))

## Test-Retest Reliability Graphs (Most consistent and Least Consistent included in thesis)

kaska2 <- reliability.plot("Kaska", plot.title = "Kaska", ocean)  