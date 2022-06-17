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

# Functions ----

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

within.bear <- function (bear, data) {
  
  cor_data <- data %>%
    filter(Bear == bear) %>%
    group_by(Observer)  %>%
    summarise(across(O1:N6, mean, na.rm = TRUE)) %>%
    data.table::transpose(make.names = 'Observer', keep.names = 'Observer') %>%
    column_to_rownames(var="Observer")
  KendallW(cor_data, TRUE, test = TRUE)
  }

##### Within Each Bear and broken down within each Factor
 
bear.by.factor <- function  (trait, bear, data) {
  byfactor_data <- data %>%
    filter(Bear == bear) %>%
    select(Observer, contains(trait), -Exhibit, -Bear, -Month, -Year) %>%
    group_by(Observer)  %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    data.table::transpose(make.names = 'Observer', keep.names = 'Observer') %>%
    column_to_rownames(var="Observer")
  KendallW(byfactor_data, TRUE, test = FALSE)
}

btw.years.cons <- function (trait, bear, year, data) {
  
  bear_factor <- data %>%
    filter(Year == year, Bear == bear) %>%
    select(contains(trait),-Observer, -Exhibit, -Bear, -Month, -Year)
  CronbachAlpha(bear_factor, na.rm = TRUE)
}

# B.1 | Radar Chart Code ----

## data wrangle ----
AVG <- survey_18_20 %>%
  rowwise() %>%
  mutate(O = mean(c (O1, O2, O3, O4, O5, O6), na.rm = T)) %>%
  mutate(C = mean(c (C1, C2, C3, C4, C5, C6), na.rm = T)) %>%
  mutate(E = mean(c (E1, E2, E3, E4, E5, E6), na.rm = T)) %>%
  mutate(A = mean(c (A1, A2, A3, A4, A5, A6), na.rm = T)) %>%
  mutate(N = mean(c (N1, N2, N3, N4, N5, N6), na.rm = T))
ocean <- data.frame(AVG$Year, AVG$Bear, AVG$O, AVG$C, AVG$E, AVG$A, AVG$N)
colnames(ocean) <- c("Year", "Bear", "O", "C", "E", "A", "N")
radar <- data.frame(AVG$O, AVG$C, AVG$E, AVG$A, AVG$N)
colnames(radar) <- c("O", "C", "E", "A", "N")
female_bears <- c("Aurora", "Kaska", "Star", "Willow")
male_bears <- c("Baffin","Nanuq","Siku","Storm","York")

male_radar2018 <- radar.chart(data = ocean, year = "2018", bears = male_bears, 
                              title = "Male Bears")

female_radar2018 <- radar.chart(data = ocean, year = "2018", bears = female_bears,
                            title = "Female Bears")

male_radar2019 <- radar.chart(data = ocean, year = "2019", bears = male_bears, 
                              title = "Male Bears")

female_radar2018 <- radar.chart(data = ocean, year = "2019", bears = female_bears,
                                title = "Female Bears")

--------------------------------------------------------------------------------
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

bears <- unique(survey_18_20$Bear)

within_bear_results <- sapply(bears, within.bear, survey_18_20)


##### Within Each Bear and broken down within each Factor

traits <- c("o", "c", "e", "a", "n")
y  <- data.frame()   

for (a in bears) {output <- sapply(traits, bear.by.factor, a, survey_18_20)
y <- rbind(y, output)# Store output in dataframe
}

colnames(y) <- traits
rownames(y) <- bears

##### Overall Mean Within The Factors
summary <- sapply(y, function(y) c( "Stand dev" = sd(y), 
                         "Mean"= mean(y,na.rm=TRUE)))

## graph for visualization (not included in thesis, mean and standard deviation listed instead)
ocean <- data.frame(o)
ocean <- data.frame(ocean, c)
ocean <- data.frame(ocean, e)
ocean <- data.frame(ocean, a)
ocean <- data.frame(ocean, n)
ocean <- melt(ocean)
ggplot(ocean, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_point () +
  theme_minimal()
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
# B.3 | Internal Consistency Code ----

## Internal Consistency: Cronbach's Alpha ---- 
## how well the behavioural descriptions describe each factor (e.g how well being assertive" specifically 
## describes Extraversion in polar bears)
#### data import

#### Overall

overall.alpha <- function (trait, data) {

trait <- survey_18_20 %>% select(contains(trait), -Observer, -Exhibit, -Bear, -Month, -Year)
CronbachAlpha(trait, na.rm = TRUE) 
}

overall_alpha <- data.frame(sapply(Traits, overall.alpha, survey_18_20))

#### Between The Years

Traits <- c("O", "C", "E", "A", "N")

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


##### Inter-Item Correlation

inter.item.cor <- function (trait, data) {
  
  trait <- survey_18_20 %>% select(contains(trait), -Observer, -Exhibit, -Bear, -Month, -Year) %>%
  trait_cor <- cor(trait, method = "pearson", use = "complete.obs")
  
  ouput <- trait_cor %>% 
    melt(trait_cor) %>%
    select(-Var1, -Var2) %>%
    mutate(factor = trait)
  return
}

o <- cor(o, method = "pearson", use = "complete.obs")
mean(o)
sd(o)

c <- cor(c, method = "pearson", use = "complete.obs")
mean(c)
sd(c)

e <- cor(e, method = "pearson", use = "complete.obs")
mean(e)
sd(e)

a <- cor(a, method = "pearson", use = "complete.obs")
mean(a)
sd(a)

n <- cor(n, method = "pearson", use = "complete.obs")
mean(n)
sd(n)

## Inter-Item Pearson Correlation Graph
o <- melt(o)
c <- melt(c)
e <- melt(e)
a <- melt(a)
n <- melt(n)
c <- data.frame(subset(c, select = -c(Var1, Var2)))
e <- data.frame(subset(e, select = -c(Var1, Var2)))
a <- data.frame(subset(a, select = -c(Var1, Var2)))
n <- data.frame(subset(n, select = -c(Var1, Var2)))
ocean <- data.frame(add_column(o,c))
ocean <- data.frame(add_column(ocean,e))
ocean <- data.frame(add_column(ocean,a))
ocean <- data.frame(add_column(ocean,n))
colnames(ocean) <- c("Var1", "Var2", "O", "C", "E", "A", "N")
ocean <- data.frame(subset(ocean, select = -c(Var1, Var2)))
ocean <- melt(ocean)
g <- ggplot(ocean, aes(x = variable, y = value, fill = variable)) +
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
## Difference Between the Bears and Distribution
#### data import

#### Data Transformation
data <- data %>%
  rowwise() %>%
  mutate(O = mean(c (O1, O2, O3, O4, O5, O6), na.rm = T)) %>%
  mutate(C = mean(c (C1, C2, C3, C4, C5, C6), na.rm = T)) %>%
  mutate(E = mean(c (E1, E2, E3, E4, E5, E6), na.rm = T)) %>%
  mutate(A = mean(c (A1, A2, A3, A4, A5, A6), na.rm = T)) %>%
  mutate(N = mean(c (N1, N2, N3, N4, N5, N6), na.rm = T))
#### Levene's Test
# check to see if I can use One-way ANOVA (when p > 0.05) ["homogeneity of variance"]
leveneTest(O ~ Bear, data)
leveneTest(C ~ Bear, data)
leveneTest(E ~ Bear, data)
leveneTest(A ~ Bear, data)
leveneTest(N ~ Bear, data)
#### Factor Rating Differences - Analysis of Variance (ANOVA) - ONe-Way
res.aov1 <- aov(O ~ Bear, data = data, na.rm = TRUE)
summary(res.aov1)
res.aov <- aov(C ~ Bear, data = data, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(E ~ Bear, data = data, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(A ~ Bear, data = data, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(N ~ Bear, data = data, na.rm = TRUE)
summary(res.aov)
#### Maturation Effects - Analysis of Variance (ANOVA) - ONe-Way
res.aov1 <- aov(O ~ Year, data = data, na.rm = TRUE)
summary(res.aov1)
res.aov <- aov(C ~ Year, data = data, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(E ~ Year, data = data, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(A ~ Year, data = data, na.rm = TRUE)
summary(res.aov)
res.aov <- aov(N ~ Year, data = data, na.rm = TRUE)
summary(res.aov)
o <- data.frame(subset(data, select = c(Year, O)))
o <- aggregate(o, by = list(o$Year), mean, na.rm = TRUE)
o <- data.frame(add_column(o, "O" = 8 - o$O))
c <- data.frame(subset(data, select = c(Year, C)))
c <- aggregate(c, by = list(c$Year), mean, na.rm = TRUE)
c <- data.frame(add_column(c, "C" = 8 - c$C))
e <- data.frame(subset(data, select = c(Year, E)))
e <- aggregate(e, by = list(e$Year), mean, na.rm = TRUE)
e <- data.frame(add_column(e, "E" = 8 - e$E))
a <- data.frame(subset(data, select = c(Year, A)))
a <- aggregate(a, by = list(a$Year), mean, na.rm = TRUE)
a <- data.frame(add_column(a, "A" = 8 - a$A))
n <- data.frame(subset(data, select = c(Year, N)))
n <- aggregate(n, by = list(n$Year), mean, na.rm = TRUE)
n <- data.frame(add_column(n, "N" = 8 - n$N))
##### Graph
data <- data.frame(subset(data, select = c(Year, Bear, O, C, E, A, N)))
data <- melt(data, id = c("Year", "Bear"))
data <- data.frame(data)
data <- data.frame(data, add_column(8 - data$value))
data <- data.frame(subset(data, select = -c(value)))
colnames(data) <- c("Year", "Bear", "variable", "value")
data$variable <- factor(data$variable, levels = c("N", "A", "E", "C", "O"))
theme_set(theme_minimal())
ggplot(data, aes(x = value, y = variable, group = variable)) +
  geom_density_ridges2(scale = 1.8, alpha = 0.8, aes(fill = variable, rel_min_height = 0.01)) +
  scale_fill_manual(values = c("orchid", "dodgerblue", "mediumseagreen", "gold", "lightcoral")) +
  theme(legend.position = "none") +
  labs(y = "Personality Factors", x = "Distribution") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18)) +
  scale_x_continuous(limits = c(0,8)) +
  geom_vline(xintercept = 4, size = 1, color = "red") +
  theme(axis.line = element_line(size = 0.5, colour = "grey90"))
## Sex Differences - Welch Two Sample t-test
rm(list=ls(all=T)) ## code to clear
data <- read_csv("C:/Users/keria/Desktop/Code/Polar Bear Personality Data 2018-2020.csv")
## Overall
# Openness
o <- data.frame(subset(data, select = c(Bear, O1, O2, O3, O4, O5, O6)))
o$Bear <- ifelse(o$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
o <- melt(o)
t.test(o$value ~ o$Bear)
# Conscientiousness
c <- data.frame(subset(data, select = c(Bear, C1, C2, C3, C4, C5, C6)))
c$Bear <- ifelse(c$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
c <- melt(c)
t.test(c$value ~ c$Bear)
# Extraversion
e <- data.frame(subset(data, select = c(Bear, E1, E2, E3, E4, E5, E6)))
e$Bear <- ifelse(e$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
e <- melt(e)
t.test(e$value ~ e$Bear)
# Agreeableness
a <- data.frame(subset(data, select = c(Bear, A1, A2, A3, A4, A5, A6)))
a$Bear <- ifelse(a$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
a <- melt(a)
t.test(a$value ~ a$Bear) ### p-value = 0.03352 (females = 4.18, males = 4.51)
# Neuroticism
n <- data.frame(subset(data, select = c(Bear, N1, N2, N3, N4, N5, N6)))
n$Bear <- ifelse(n$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
n <- melt(n)
t.test(n$value ~ n$Bear)
## Between the years
#2018
d18 <- data.frame(subset(data, Year == '2018'))
# Openness
o <- data.frame(subset(d18, select = c(Bear, O1, O2, O3, O4, O5, O6)))
o$Bear <- ifelse(o$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
o <- melt(o)
t.test(o$value ~ o$Bear)
# Conscientiousness
c <- data.frame(subset(d18, select = c(Bear, C1, C2, C3, C4, C5, C6)))
c$Bear <- ifelse(c$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
c <- melt(c)
t.test(c$value ~ c$Bear)
# Extraversion
e <- data.frame(subset(d18, select = c(Bear, E1, E2, E3, E4, E5, E6)))
e$Bear <- ifelse(e$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
e <- melt(e)
t.test(e$value ~ e$Bear)
# Agreeableness
a <- data.frame(subset(d18, select = c(Bear, A1, A2, A3, A4, A5, A6)))
a$Bear <- ifelse(a$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
a <- melt(a)
t.test(a$value ~ a$Bear)
# Neuroticism
n <- data.frame(subset(d18, select = c(Bear, N1, N2, N3, N4, N5, N6)))
n$Bear <- ifelse(n$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
n <- melt(n)
t.test(n$value ~ n$Bear)
#2019
d19 <- data.frame(subset(data, Year == '2019'))
# Openness
o <- data.frame(subset(d19, select = c(Bear, O1, O2, O3, O4, O5, O6)))
o$Bear <- ifelse(o$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
o <- melt(o)
t.test(o$value ~ o$Bear)
# Conscientiousness
c <- data.frame(subset(d19, select = c(Bear, C1, C2, C3, C4, C5, C6)))
c$Bear <- ifelse(c$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
c <- melt(c)
t.test(c$value ~ c$Bear)
# Extraversion
e <- data.frame(subset(d19, select = c(Bear, E1, E2, E3, E4, E5, E6)))
e$Bear <- ifelse(e$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
e <- melt(e)
t.test(e$value ~ e$Bear)
# Agreeableness
a <- data.frame(subset(d19, select = c(Bear, A1, A2, A3, A4, A5, A6)))
a$Bear <- ifelse(a$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
a <- melt(a)
t.test(a$value ~ a$Bear) ## p-value = 0.0035 (females = 3.63, males = 4.73)
# Neuroticism
n <- data.frame(subset(d19, select = c(Bear, N1, N2, N3, N4, N5, N6)))
n$Bear <- ifelse(n$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
n <- melt(n)
t.test(n$value ~ n$Bear) ## p-value = 0.028 (females = 4.53, males = 3.72)
#2020
d20 <- data.frame(subset(data, Year == '2020'))
# Openness
o <- data.frame(subset(d20, select = c(Bear, O1, O2, O3, O4, O5, O6)))
o$Bear <- ifelse(o$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
o <- melt(o)
t.test(o$value ~ o$Bear) ## p-value = 0.0245 (females = 4.70, males = 5.43)
# Conscientiousness
c <- data.frame(subset(d20, select = c(Bear, C1, C2, C3, C4, C5, C6)))
c$Bear <- ifelse(c$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
c <- melt(c)
t.test(c$value ~ c$Bear)
# Extraversion
e <- data.frame(subset(d20, select = c(Bear, E1, E2, E3, E4, E5, E6)))
e$Bear <- ifelse(e$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
e <- melt(e)
t.test(e$value ~ e$Bear)
# Agreeableness
a <- data.frame(subset(d20, select = c(Bear, A1, A2, A3, A4, A5, A6)))
a$Bear <- ifelse(a$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
a <- melt(a)
t.test(a$value ~ a$Bear)
# Neuroticism
n <- data.frame(subset(d20, select = c(Bear, N1, N2, N3, N4, N5, N6)))
n$Bear <- ifelse(n$Bear == c('Aurora','Star', 'Willow', 'Kaska'), "F", "M")
n <- melt(n)
t.test(n$value ~ n$Bear)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# B.5 | Test Retest Repeatability Code----
## Test Retest Repeatability: Intraclass Correlation Coefficient (ICC) - between year difference
#### data import
library(tidyverse)
library(readr)
library(irr)
library(DescTools)
library(corrplot)
library(ggplot2)
library(reshape2)
library(dplyr)
dev.off()
rm(list=ls(all=T)) ## Code To Clear
data <- read_csv("C:/Users/keria/Desktop/Code/Polar Bear Personality Data 2018-2020.csv")
#### Bears Overall
# Aurora
aurora <- data.frame(subset(data, Bear == 'Aurora'))
aurora <- aggregate(aurora, by = list(aurora$Year), mean, na.rm = TRUE)
aurora <- data.frame(subset(aurora, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
aurora <- t(aurora)
colnames(aurora) <- c("2018", "2019", "2020")
icc(aurora, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
aurora <- cor(aurora)
aurora
corrplot(aurora, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Baffin
baffin <- data.frame(subset(data, Bear == 'Baffin'))
baffin <- aggregate(baffin, by = list(baffin$Year), mean, na.rm = TRUE)
baffin <- data.frame(subset(baffin, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
baffin <- t(baffin)
colnames(baffin) <- c("2018", "2019", "2020")
icc(baffin, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
baffin <- cor(baffin)
baffin
corrplot(baffin, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Kaska
kaska <- data.frame(subset(data, Bear == 'Kaska'))
kaska <- aggregate(kaska, by = list(kaska$Year), mean, na.rm = TRUE)
kaska <- data.frame(subset(kaska, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
kaska <- t(kaska)
colnames(kaska) <- c("2018", "2019", "2020")
icc(kaska, model = "twoway", #### Kaska - Most Consistent (ICC = 0.94, p = 1.2E-19)
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
kaska <- cor(kaska)
kaska
corrplot(kaska, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Nanuq
nanuq <- data.frame(subset(data, Bear == 'Nanuq'))
nanuq <- aggregate(nanuq, by = list(nanuq$Year), mean, na.rm = TRUE)
nanuq <- data.frame(subset(nanuq, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
nanuq <- t(nanuq)
colnames(nanuq) <- c("2018", "2019", "2020")
icc(nanuq, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
nanuq <- cor(nanuq)
nanuq
corrplot(nanuq, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Siku
siku <- data.frame(subset(data, Bear == 'Siku'))
siku <- aggregate(siku, by = list(siku$Year), mean, na.rm = TRUE)
siku <- data.frame(subset(siku, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
siku <- t(siku)
colnames(siku) <- c("2018", "2019", "2020")
icc(siku, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
siku <- cor(siku)
siku
corrplot(siku, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Star
star <- data.frame(subset(data, Bear == 'Star'))
star <- aggregate(star, by = list(star$Year), mean, na.rm = TRUE)
star <- data.frame(subset(star, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
star <- t(star)
colnames(star) <- c("2018", "2019", "2020")
icc(star, model = "twoway", #### Star - Least Consistent (ICC = 0.56, p = 0.0035)
    type = "consistency", ### Baffin - also pretty low consistency (ICC = 0.55)
    unit = "average", r0 = 0, conf.level = 0.95)
star <- cor(star)
star
corrplot(star, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Storm
storm <- data.frame(subset(data, Bear == 'Storm'))
storm <- aggregate(storm, by = list(storm$Year), mean, na.rm = TRUE)
storm <- data.frame(subset(storm, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
storm <- t(storm)
colnames(storm) <- c("2018", "2019", "2020")
icc(storm, model = "twoway",
    type = "consistency",
    = "average", r0 = 0, conf.level = 0.95)
storm <- cor(storm)
storm
corrplot(storm, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Willow
willow <- data.frame(subset(data, Bear == 'Willow'))
willow <- aggregate(willow, by = list(willow$Year), mean, na.rm = TRUE)
willow <- data.frame(subset(willow, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
willow <- t(willow)
colnames(willow) <- c("2018", "2019", "2020")
icc(willow, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
willow <- cor(willow)
willow
corrplot(willow, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# York
york <- data.frame(subset(data, Bear == 'York'))
york <- aggregate(york, by = list(york$Year), mean, na.rm = TRUE)
york <- data.frame(subset(york, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
york <- t(york)
colnames(york) <- c("2018", "2019", "2020")
icc(york, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
york <- cor(york)
york
corrplot(york, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# ***ICC(3,k) Two-way mixed, Average measure. Reliability is calculated by taking an average of the k raters measurements.
# Baffin and Star around 0.55, everyone else above 0.85 (very consistent).
#### ICC within the Factors and Bears
## Openness
# Aurora
openness <- data.frame(subset(data, Bear == 'Aurora'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Baffin
openness <- data.frame(subset(data, Bear == 'Baffin'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Kaska
openness <- data.frame(subset(data, Bear == 'Kaska'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Nanuq
openness <- data.frame(subset(data, Bear == 'Nanuq'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Siku
openness <- data.frame(subset(data, Bear == 'Siku'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Star
openness <- data.frame(subset(data, Bear == 'Star'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Storm
openness <- data.frame(subset(data, Bear == 'Storm'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Willow
openness <- data.frame(subset(data, Bear == 'Willow'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# York
openness <- data.frame(subset(data, Bear == 'York'))
openness <- aggregate(openness, by = list(openness$Year), mean, na.rm = TRUE)
openness <- data.frame(subset(openness, select = c(O1, O2, O3, O4, O5, O6)))
openness <- t(openness)
colnames(openness) <- c("2018", "2019", "2020")
icc(openness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
o <- c(0.852, 0.818, 0.868, 0.964, 0.935, 0.719, 0.935, 0.911, 0.967)
mean(o)
sd(o)
## Conscientiousness
#Aurora
conscientiousness <- data.frame(subset(data, Bear == 'Aurora'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Baffin
conscientiousness <- data.frame(subset(data, Bear == 'Baffin'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Kaska
conscientiousness <- data.frame(subset(data, Bear == 'Kaska'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Nanuq
conscientiousness <- data.frame(subset(data, Bear == 'Nanuq'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Siku
conscientiousness <- data.frame(subset(data, Bear == 'Siku'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Star
conscientiousness <- data.frame(subset(data, Bear == 'Star'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Storm
conscientiousness <- data.frame(subset(data, Bear == 'Storm'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Willow
conscientiousness <- data.frame(subset(data, Bear == 'Willow'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# York
conscientiousness <- data.frame(subset(data, Bear == 'York'))
conscientiousness <- aggregate(conscientiousness, by = list(conscientiousness$Year), mean, na.rm = TRUE)
conscientiousness <- data.frame(subset(conscientiousness, select = c(C1, C2, C3, C4, C5, C6)))
conscientiousness <- t(conscientiousness)
colnames(conscientiousness) <- c("2018", "2019", "2020")
icc(conscientiousness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
c <- c(0.634, 0.684, 0.975, 0.869, 0.948, 0.519, 0.895, 0.866, 0.914)
mean(c)
sd(c)
## Extraversion
# Aurora
extraversion <- data.frame(subset(data, Bear == 'Aurora'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Baffin
extraversion <- data.frame(subset(data, Bear == 'Baffin'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Kaska
extraversion <- data.frame(subset(data, Bear == 'Kaska'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Nanuq
extraversion <- data.frame(subset(data, Bear == 'Nanuq'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Siku
extraversion <- data.frame(subset(data, Bear == 'Siku'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Star
extraversion <- data.frame(subset(data, Bear == 'Star'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Storm
extraversion <- data.frame(subset(data, Bear == 'Storm'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Willow
extraversion <- data.frame(subset(data, Bear == 'Willow'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# York
extraversion <- data.frame(subset(data, Bear == 'York'))
extraversion <- aggregate(extraversion, by = list(extraversion$Year), mean, na.rm = TRUE)
extraversion <- data.frame(subset(extraversion, select = c(E1, E2, E3, E4, E5, E6)))
extraversion <- t(extraversion)
colnames(extraversion) <- c("2018", "2019", "2020")
icc(extraversion, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
e <- c(0.957, 0.873, 0.97, 0.821, 0.973, 0.565, 0.92, 0.945, 0.947)
mean(e)
sd(e)
## Agreeableness
# Aurora
agreeableness <- data.frame(subset(data, Bear == 'Aurora'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Baffin
agreeableness <- data.frame(subset(data, Bear == 'Baffin'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Kaska
agreeableness <- data.frame(subset(data, Bear == 'Kaska'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Nanuq
agreeableness <- data.frame(subset(data, Bear == 'Nanuq'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Siku
agreeableness <- data.frame(subset(data, Bear == 'Siku'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Star
agreeableness <- data.frame(subset(data, Bear == 'Star'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Storm
agreeableness <- data.frame(subset(data, Bear == 'Storm'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Willow
agreeableness <- data.frame(subset(data, Bear == 'Willow'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# York
agreeableness <- data.frame(subset(data, Bear == 'York'))
agreeableness <- aggregate(agreeableness, by = list(agreeableness$Year), mean, na.rm = TRUE)
agreeableness <- data.frame(subset(agreeableness, select = c(A1, A2, A3, A4, A5, A6)))
agreeableness <- t(agreeableness)
colnames(agreeableness) <- c("2018", "2019", "2020")
icc(agreeableness, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
a <- c(0.825, 0.626, 0.911, 0.679, 0.932, -0.508, 0.838, 0.902, 0.947)
mean(a)
sd(a)
## Neuroticism
# Aurora
neuroticism <- data.frame(subset(data, Bear == 'Aurora'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Baffin
neuroticism <- data.frame(subset(data, Bear == 'Baffin'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Kaska
neuroticism <- data.frame(subset(data, Bear == 'Kaska'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Nanuq
neuroticism <- data.frame(subset(data, Bear == 'Nanuq'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Siku
neuroticism <- data.frame(subset(data, Bear == 'Siku'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Star
neuroticism <- data.frame(subset(data, Bear == 'Star'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Storm
neuroticism <- data.frame(subset(data, Bear == 'Storm'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# Willow
neuroticism <- data.frame(subset(data, Bear == 'Willow'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
# York
neuroticism <- data.frame(subset(data, Bear == 'York'))
neuroticism <- aggregate(neuroticism, by = list(neuroticism$Year), mean, na.rm = TRUE)
neuroticism <- data.frame(subset(neuroticism, select = c(N1, N2, N3, N4, N5, N6)))
neuroticism <- t(neuroticism)
colnames(neuroticism) <- c("2018", "2019", "2020")
icc(neuroticism, model = "twoway",
    type = "consistency",
    unit = "average", r0 = 0, conf.level = 0.95)
n <- c(0.963, 0.78, 0.938, 0.597, 0.205, 0.624, 0.615, 0.88, 0.923)
mean(n)
sd(n)
## Graph for visualization of Test Retest Correlations (not included in thesis, mean and standard deviation included instead)
ocean <- data.frame(o)
ocean <- add_column(ocean, c)
ocean <- add_column(ocean, e)
ocean <- add_column(ocean, a)
ocean <- add_column(ocean, n)
boxplot(ocean, ylim = c(0,1.1))
## Test-Retest Reliability Graphs (Most consistent and Least Consistent included in thesis)
AVG <- data %>%
  rowwise() %>%
  mutate(O = mean(c (O1, O2, O3, O4, O5, O6), na.rm = T)) %>%
  mutate(C = mean(c (C1, C2, C3, C4, C5, C6), na.rm = T)) %>%
  mutate(E = mean(c (E1, E2, E3, E4, E5, E6), na.rm = T)) %>%
  mutate(A = mean(c (A1, A2, A3, A4, A5, A6), na.rm = T)) %>%
  mutate(N = mean(c (N1, N2, N3, N4, N5, N6), na.rm = T))
AVG <- data.frame(AVG$Year, AVG$Bear, AVG$O, AVG$C, AVG$E, AVG$A, AVG$N)
colnames(AVG) <- c("Year", "Bear", "O", "C", "E", "A", "N")
# Kaska (Most Consistent)
kaska <- data.frame(subset(AVG, Bear == 'Kaska', select = -Bear))
kaska <- aggregate(kaska, by = list(kaska$Year), mean, na.rm = TRUE)
kaska <- data.frame(subset(kaska, select = -c(Year, Group.1)))
kaska <- data.frame(kaska,add_column(c("2018", "2019", "2020")))
colnames(kaska) <- c("O", "C", "E", "A", "N", "Year")
kaska <- melt(kaska, id.vars="Year")
kaska <- data.frame(add_column(kaska, "value" = 8 - kaska$value))
kaska <- data.frame(subset(kaska, select = -c(value)))
colnames(kaska) <- c("Year", "variable", "value")
kaska$variable <- factor(kaska$variable, levels = c("N", "A", "E", "C", "O"))
kaska$Year <- as.factor(kaska$Year)
ggplot(kaska) +
  aes(x = variable, y = value, fill = Year) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 7)) +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values = c(yarrr::transparent("lightcoral", trans.val = .3), yarrr::transparent("mediumseagreen", trans.val = .3), yarrr::transparent("dodgerblue", trans.val = .3))) +
  theme(legend.text = element_text(size = 20, face = "bold"), legend.title = element_text(size = 20)) +
  labs(title = "A. Kaska - Most Consistent", x = "Personality Factors", y = "Average of Ratings", cex = 1.3) +
  theme(axis.line = element_line(size = 0.5, colour = "grey90"))
# Star (Least ConsistenT)
star <- data.frame(subset(AVG, Bear == 'Star', select = -Bear))
star <- aggregate(star, by = list(star$Year), mean, na.rm = TRUE)
star <- data.frame(subset(star, select = -c(Year, Group.1)))
star <- data.frame(star,add_column(c("2018", "2019", "2020")))
colnames(star) <- c("O", "C", "E", "A", "N", "Year")
star <- melt(star, id.vars="Year")
star <- data.frame(add_column(star, "value" = 8 - star$value))
star <- data.frame(subset(star, select = -c(value)))
colnames(star) <- c("Year", "variable", "value")
star$variable <- factor(star$variable, levels = c("N", "A", "E", "C", "O"))
star$Year <- as.factor(star$Year)
ggplot(star) +
  aes(x = variable, y = value, fill = Year) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 7)) +
  theme_minimal() +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values = c(yarrr::transparent("lightcoral", trans.val = .3), yarrr::transparent("mediumseagreen", trans.val = .3), yarrr::transparent("dodgerblue", trans.val = .3))) +
  theme(legend.text = element_text(size = 20, face = "bold"), legend.title = element_text(size = 20)) +
  labs(title = "B. Star - Least Consistent", x = "Personality Factors", y = "Average of Ratings", cex = 1.3) +
  theme(axis.line = element_line(size = 0.5, colour = "grey90"))