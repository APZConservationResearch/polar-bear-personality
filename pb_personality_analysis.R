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

within.cor <- function (data, bear) {
  
  cor_data <- data %>%
    filter(Bear == bear) %>%
    group_by(Observer)  %>%
    summarise(across(O1:N6, mean, na.rm = TRUE)) %>%
    data.table::transpose(make.names = 'Observer', keep.names = 'Observer') %>%
    column_to_rownames(var="Observer")
  KendallW(cor_data, TRUE, test = TRUE)
  }

bears <- unique(survey_18_20$Bear)

within_df <- df
for (bear in bears) {
  
within.cor(survey_18_20, bears[bear]) }

within_df[bear] <-  single_cor
}

aurora <- within.cor(survey_18_20, "Aurora")
    
aurora <- aggregate(aurora, by = list(aurora$Observer), mean, na.rm = TRUE)
aurora <- data.frame(subset(aurora, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
aurora <- t(aurora)
colnames(aurora) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JS", "JZ", "SM", "SMG", "SS")
KendallW(aurora, TRUE, test = TRUE)

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
#### data import

### Interrater Correlation Matrix
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


##### Within Each Bear across all the factors
## Data Transformation
# Aurora
aurora2 <- data.frame(subset(survey_18_20, (Bear == 'Aurora')))
aurora2 <- aggregate(aurora2, by = list(aurora2$Observer), mean, na.rm = TRUE)
aurora2 <- data.frame(subset(aurora2, select = -c(Year, Exhibit, Month, Bear, Observer, Group.1)))
aurora2 <- t(aurora2)
colnames(aurora2) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JS", "JZ", "SM", "SMG", "SS")
KendallW(aurora2, TRUE, test = TRUE)
# Correlation plot for visualization purposes
aurora <- cor(aurora)
aurora
corrplot(aurora, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# Baffin
baffin <- data.frame(subset(survey_18_20, Bear == 'Baffin'))
baffin <- aggregate(baffin, by = list(baffin$Observer), mean, na.rm = TRUE)
baffin <- data.frame(subset(baffin, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
baffin <- t(baffin)
colnames(baffin) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(baffin, TRUE, test = TRUE)
# Kaska
kaska <- data.frame(subset(survey_18_20, Bear == 'Kaska'))
kaska <- aggregate(kaska, by = list(kaska$Observer), mean, na.rm = TRUE)
kaska <- data.frame(subset(kaska, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
kaska <- t(kaska)
colnames(kaska) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JS", "JZ", "SM", "SMG", "SS")
KendallW(kaska, TRUE, test = TRUE)
# Nanuq
nanuq <- data.frame(subset(survey_18_20, Bear == 'Nanuq'))
nanuq <- aggregate(nanuq, by = list(nanuq$Observer), mean, na.rm = TRUE)
nanuq <- data.frame(subset(nanuq, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
nanuq <- t(nanuq)
colnames(nanuq) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(nanuq, TRUE, test = TRUE)
# Siku
siku <- data.frame(subset(survey_18_20, Bear == 'Siku'))
siku <- aggregate(siku, by = list(siku$Observer), mean, na.rm = TRUE)
siku <- data.frame(subset(siku, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
siku <- t(siku)
colnames(siku) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(siku, TRUE, test = TRUE)
# Star
star <- data.frame(subset(survey_18_20, Bear == 'Star'))
star <- aggregate(star, by = list(star$Observer), mean, na.rm = TRUE)
star <- data.frame(subset(star, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
star <- t(star)
colnames(star) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(star, TRUE, test = TRUE)
# Storm
storm <- data.frame(subset(survey_18_20, Bear == 'Storm'))
storm <- aggregate(storm, by = list(storm$Observer), mean, na.rm = TRUE)
storm <- data.frame(subset(storm, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
storm <- t(storm)
colnames(storm) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JS", "JZ", "SM", "SMG", "SS")
KendallW(storm, TRUE, test = TRUE) #### Most Variance (w = 0.37, p = 7.9E-15)
# Willow
willow <- data.frame(subset(survey_18_20, Bear == 'Willow'))
willow <- aggregate(willow, by = list(willow$Observer), mean, na.rm = TRUE)
willow <- data.frame(subset(willow, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
willow <- t(willow)
colnames(willow) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(willow, TRUE, test = TRUE) #### Least Variance (w = 0.68, p = 2.2E-16)
# York
york <- data.frame(subset(survey_18_20, Bear == 'York'))
york <- aggregate(york, by = list(york$Observer), mean, na.rm = TRUE)
york <- data.frame(subset(york, select = -c(Year, Exhibit, Month, Observer, Bear, Group.1)))
york <- t(york)
colnames(york) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(york, TRUE, test = TRUE)

##### Within Each Bear and broken down within each Factor
##### Aurora
# Openness
aurora <- data.frame(subset(data, (Bear == 'Aurora')))
aurora <- data.frame(subset(aurora, select = c(Observer, O1, O2, O3, O4, O5, O6)))
aurora <- aggregate(aurora, by = list(aurora$Observer), mean, na.rm = TRUE)
aurora <- data.frame(subset(aurora, select = -c(Observer, Group.1)))
aurora <- t(aurora)
KendallW(aurora, TRUE, test = TRUE)
# Conscientiousness
aurora <- data.frame(subset(data, (Bear == 'Aurora')))
aurora <- data.frame(subset(aurora, select = c(Observer, C1, C2, C3, C4, C5, C6)))
aurora <- aggregate(aurora, by = list(aurora$Observer), mean, na.rm = TRUE)
aurora <- data.frame(subset(aurora, select = -c(Observer, Group.1)))
aurora <- t(aurora)
KendallW(aurora, TRUE, test = TRUE) #### Not Significant
# extraversion
aurora <- data.frame(subset(data, (Bear == 'Aurora')))
aurora <- data.frame(subset(aurora, select = c(Observer, E1, E2, E3, E4, E5, E6)))
aurora <- aggregate(aurora, by = list(aurora$Observer), mean, na.rm = TRUE)
aurora <- data.frame(subset(aurora, select = -c(Observer, Group.1)))
aurora <- t(aurora)
KendallW(aurora, TRUE, test = TRUE)
# Agreeableness
aurora <- data.frame(subset(data, (Bear == 'Aurora')))
aurora <- data.frame(subset(aurora, select = c(Observer, A1, A2, A3, A4, A5, A6)))
aurora <- aggregate(aurora, by = list(aurora$Observer), mean, na.rm = TRUE)
aurora <- data.frame(subset(aurora, select = -c(Observer, Group.1)))
aurora <- t(aurora)
KendallW(aurora, TRUE, test = TRUE)
# Neuroticism
aurora <- data.frame(subset(data, (Bear == 'Aurora')))
aurora <- data.frame(subset(aurora, select = c(Observer, N1, N2, N3, N4, N5, N6)))
aurora <- aggregate(aurora, by = list(aurora$Observer), mean, na.rm = TRUE)
aurora <- data.frame(subset(aurora, select = -c(Observer, Group.1)))
aurora <- t(aurora)
colnames(baffin) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(aurora, TRUE, test = TRUE)
##### Baffin
# Openness
baffin <- data.frame(subset(data, Bear == 'Baffin'))
baffin <- data.frame(subset(baffin, select = c(Observer, O1, O2, O3, O4, O5, O6)))
baffin <- aggregate(baffin, by = list(baffin$Observer), mean, na.rm = TRUE)
baffin <- data.frame(subset(baffin, select = -c(Observer, Group.1)))
baffin <- t(baffin)
colnames(baffin) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(baffin, TRUE, test = TRUE)
#Conscientiousness
baffin <- data.frame(subset(data, Bear == 'Baffin'))
baffin <- data.frame(subset(baffin, select = c(Observer, C1, C2, C3, C4, C5, C6)))
baffin <- aggregate(baffin, by = list(baffin$Observer), mean, na.rm = TRUE)
baffin <- data.frame(subset(baffin, select = -c(Observer)))
baffin <- t(baffin)
colnames(baffin) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(baffin, TRUE, test = TRUE)
kaska <- data.frame(subset(kaska, select = -c(Observer, Group.1)))
kaska <- t(kaska)
colnames(kaska) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(kaska, TRUE, test = TRUE)
# Agreeableness
kaska <- data.frame(subset(data, Bear == 'Kaska'))
kaska <- data.frame(subset(kaska, select = c(Observer, A1, A2, A3, A4, A5, A6)))
kaska <- aggregate(kaska, by = list(kaska$Observer), mean, na.rm = TRUE)
kaska <- data.frame(subset(kaska, select = -c(Observer, Group.1)))
kaska <- t(kaska)
colnames(kaska) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(kaska, TRUE, test = TRUE)
# Neuroticism
kaska <- data.frame(subset(data, Bear == 'Kaska'))
kaska <- data.frame(subset(kaska, select = c(Observer, N1, N2, N3, N4, N5, N6)))
kaska <- aggregate(kaska, by = list(kaska$Observer), mean, na.rm = TRUE)
kaska <- data.frame(subset(kaska, select = -c(Observer, Group.1)))
kaska <- t(kaska)
colnames(kaska) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(kaska, TRUE, test = TRUE)
##### Nanuq
# Openness
nanuq <- data.frame(subset(data, Bear == 'Nanuq'))
nanuq <- data.frame(subset(nanuq, select = c(Observer, O1, O2, O3, O4, O5, O6)))
nanuq <- aggregate(nanuq, by = list(nanuq$Observer), mean, na.rm = TRUE)
nanuq <- data.frame(subset(nanuq, select = -c(Observer, Group.1)))
nanuq <- t(nanuq)
colnames(nanuq) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(nanuq, TRUE, test = TRUE)
# Conscientiousness
nanuq <- data.frame(subset(data, Bear == 'Nanuq'))
nanuq <- data.frame(subset(nanuq, select = c(Observer, C1, C2, C3, C4, C5, C6)))
nanuq <- aggregate(nanuq, by = list(nanuq$Observer), mean, na.rm = TRUE)
nanuq <- data.frame(subset(nanuq, select = -c(Observer, Group.1)))
nanuq <- t(nanuq)
colnames(nanuq) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(nanuq, TRUE, test = TRUE)
# Extraversion
nanuq <- data.frame(subset(data, Bear == 'Nanuq'))
nanuq <- data.frame(subset(nanuq, select = c(Observer, E1, E2, E3, E4, E5, E6)))
nanuq <- aggregate(nanuq, by = list(nanuq$Observer), mean, na.rm = TRUE)
nanuq <- data.frame(subset(nanuq, select = -c(Observer, Group.1)))
nanuq <- t(nanuq)
colnames(nanuq) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(nanuq, TRUE, test = TRUE)
# Agreeableness
nanuq <- data.frame(subset(data, Bear == 'Nanuq'))
nanuq <- data.frame(subset(nanuq, select = c(Observer, A1, A2, A3, A4, A5, A6)))
nanuq <- aggregate(nanuq, by = list(nanuq$Observer), mean, na.rm = TRUE)
nanuq <- data.frame(subset(nanuq, select = -c(Observer, Group.1)))
nanuq <- t(nanuq)
colnames(nanuq) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(nanuq, TRUE, test = TRUE)
# Neuroticism
nanuq <- data.frame(subset(data, Bear == 'Nanuq'))
nanuq <- data.frame(subset(nanuq, select = c(Observer, N1, N2, N3, N4, N5, N6)))
nanuq <- aggregate(nanuq, by = list(nanuq$Observer), mean, na.rm = TRUE)
nanuq <- data.frame(subset(nanuq, select = -c(Observer, Group.1)))
nanuq <- t(nanuq)
colnames(nanuq) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(nanuq, TRUE, test = TRUE)
##### Siku
# Openness
siku <- data.frame(subset(data, Bear == 'Siku'))
siku <- data.frame(subset(siku, select = c(Observer, O1, O2, O3, O4, O5, O6)))
siku <- aggregate(siku, by = list(siku$Observer), mean, na.rm = TRUE)
siku <- data.frame(subset(siku, select = -c(Observer, Group.1)))
siku <- t(siku)
colnames(siku) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(siku, TRUE, test = TRUE)
# Consciousness
siku <- data.frame(subset(data, Bear == 'Siku'))
siku <- data.frame(subset(siku, select = c(Observer, C1, C2, C3, C4, C5, C6)))
siku <- aggregate(siku, by = list(siku$Observer), mean, na.rm = TRUE)
siku <- data.frame(subset(siku, select = -c(Observer, Group.1)))
siku <- t(siku)
colnames(siku) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(siku, TRUE, test = TRUE)
# Extraversion
siku <- data.frame(subset(data, Bear == 'Siku'))
siku <- data.frame(subset(siku, select = c(Observer, E1, E2, E3, E4, E5, E6)))
siku <- aggregate(siku, by = list(siku$Observer), mean, na.rm = TRUE)
siku <- data.frame(subset(siku, select = -c(Observer, Group.1)))
siku <- t(siku)
colnames(siku) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(siku, TRUE, test = TRUE)
# Agreeableness
siku <- data.frame(subset(data, Bear == 'Siku'))
siku <- data.frame(subset(siku, select = c(Observer, A1, A2, A3, A4, A5, A6)))
siku <- aggregate(siku, by = list(siku$Observer), mean, na.rm = TRUE)
siku <- data.frame(subset(siku, select = -c(Observer, Group.1)))
siku <- t(siku)
colnames(siku) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(siku, TRUE, test = TRUE)
# Neuroticism
siku <- data.frame(subset(data, Bear == 'Siku'))
siku <- data.frame(subset(siku, select = c(Observer, N1, N2, N3, N4, N5, N6)))
siku <- aggregate(siku, by = list(siku$Observer), mean, na.rm = TRUE)
siku <- data.frame(subset(siku, select = -c(Observer, Group.1)))
siku <- t(siku)
colnames(siku) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(siku, TRUE, test = TRUE)
##### Star
# Openness
star <- data.frame(subset(data, Bear == 'Star'))
star <- data.frame(subset(star, select = c(Observer, O1, O2, O3, O4, O5, O6)))
star <- aggregate(star, by = list(star$Observer), mean, na.rm = TRUE)
star <- data.frame(subset(star, select = -c(Observer, Group.1)))
star <- t(star)
colnames(star) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(star, TRUE, test = TRUE)
# Conscientiousness
star <- data.frame(subset(data, Bear == 'Star'))
star <- data.frame(subset(star, select = c(Observer, C1, C2, C3, C4, C5, C6)))
star <- aggregate(star, by = list(star$Observer), mean, na.rm = TRUE)
star <- data.frame(subset(star, select = -c(Observer, Group.1)))
star <- t(star)
colnames(star) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(star, TRUE, test = TRUE) ### Not Significant
# Extraversion
star <- data.frame(subset(data, Bear == 'Star'))
star <- data.frame(subset(star, select = c(Observer, E1, E2, E3, E4, E5, E6)))
star <- aggregate(star, by = list(star$Observer), mean, na.rm = TRUE)
star <- data.frame(subset(star, select = -c(Observer, Group.1)))
star <- t(star)
colnames(star) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(star, TRUE, test = TRUE)
# Agreeableness
star <- data.frame(subset(data, Bear == 'Star'))
star <- data.frame(subset(star, select = c(Observer, A1, A2, A3, A4, A5, A6)))
star <- aggregate(star, by = list(star$Observer), mean, na.rm = TRUE)
star <- data.frame(subset(star, select = -c(Observer, Group.1)))
star <- t(star)
colnames(star) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(star, TRUE, test = TRUE)
# Neuroticism
star <- data.frame(subset(data, Bear == 'Star'))
star <- data.frame(subset(star, select = c(Observer, N1, N2, N3, N4, N5, N6)))
star <- aggregate(star, by = list(star$Observer), mean, na.rm = TRUE)
star <- data.frame(subset(star, select = -c(Observer, Group.1)))
star <- t(star)
colnames(star) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(star, TRUE, test = TRUE)
##### Storm
# Openness
storm <- data.frame(subset(data, Bear == 'Storm'))
storm <- data.frame(subset(storm, select = c(Observer, O1, O2, O3, O4, O5, O6)))
storm <- aggregate(storm, by = list(storm$Observer), mean, na.rm = TRUE)
storm <- data.frame(subset(storm, select = -c(Observer, Group.1)))
storm <- t(storm)
colnames(storm) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(storm, TRUE, test = TRUE)
# Consciousness
storm <- data.frame(subset(data, Bear == 'Storm'))
storm <- data.frame(subset(storm, select = c(Observer, C1, C2, C3, C4, C5, C6)))
storm <- aggregate(storm, by = list(storm$Observer), mean, na.rm = TRUE)
storm <- data.frame(subset(storm, select = -c(Observer, Group.1)))
storm <- t(storm)
colnames(storm) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(storm, TRUE, test = TRUE)
# Extraversion
storm <- data.frame(subset(data, Bear == 'Storm'))
storm <- data.frame(subset(storm, select = c(Observer, E1, E2, E3, E4, E5, E6)))
storm <- aggregate(storm, by = list(storm$Observer), mean, na.rm = TRUE)
storm <- data.frame(subset(storm, select = -c(Observer, Group.1)))
storm <- t(storm)
colnames(storm) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(storm, TRUE, test = TRUE)
# Agreeableness
storm <- data.frame(subset(data, Bear == 'Storm'))
storm <- data.frame(subset(storm, select = c(Observer, A1, A2, A3, A4, A5, A6)))
storm <- aggregate(storm, by = list(storm$Observer), mean, na.rm = TRUE)
storm <- data.frame(subset(storm, select = -c(Observer, Group.1)))
storm <- t(storm)
colnames(storm) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(storm, TRUE, test = TRUE)
# Neuroticism
storm <- data.frame(subset(data, Bear == 'Storm'))
storm <- data.frame(subset(storm, select = c(Observer, N1, N2, N3, N4, N5, N6)))
storm <- aggregate(storm, by = list(storm$Observer), mean, na.rm = TRUE)
storm <- data.frame(subset(storm, select = -c(Observer, Group.1)))
storm <- t(storm)
colnames(storm) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(storm, TRUE, test = TRUE)
##### WILLOW
# Openness
willow <- data.frame(subset(data, Bear == 'Willow'))
willow <- data.frame(subset(willow, select = c(Observer, O1, O2, O3, O4, O5, O6)))
willow <- aggregate(willow, by = list(willow$Observer), mean, na.rm = TRUE)
willow <- data.frame(subset(willow, select = -c(Observer, Group.1)))
willow <- t(willow)
colnames(willow) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(willow, TRUE, test = TRUE)
# Conscientiousness
willow <- data.frame(subset(data, Bear == 'Willow'))
willow <- data.frame(subset(willow, select = c(Observer, C1, C2, C3, C4, C5, C6)))
willow <- aggregate(willow, by = list(willow$Observer), mean, na.rm = TRUE)
willow <- data.frame(subset(willow, select = -c(Observer, Group.1)))
willow <- t(willow)
colnames(willow) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(willow, TRUE, test = TRUE)
# Extraversion
willow <- data.frame(subset(data, Bear == 'Willow'))
willow <- data.frame(subset(willow, select = c(Observer, E1, E2, E3, E4, E5, E6)))
willow <- aggregate(willow, by = list(willow$Observer), mean, na.rm = TRUE)
willow <- data.frame(subset(willow, select = -c(Observer, Group.1)))
willow <- t(willow)
colnames(willow) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(willow, TRUE, test = TRUE)
# Agreeableness
willow <- data.frame(subset(data, Bear == 'Willow'))
willow <- data.frame(subset(willow, select = c(Observer, A1, A2, A3, A4, A5, A6)))
willow <- aggregate(willow, by = list(willow$Observer), mean, na.rm = TRUE)
willow <- data.frame(subset(willow, select = -c(Observer, Group.1)))
willow <- t(willow)
colnames(willow) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(willow, TRUE, test = TRUE)
# Neuroticism
willow <- data.frame(subset(data, Bear == 'Willow'))
willow <- data.frame(subset(willow, select = c(Observer, N1, N2, N3, N4, N5, N6)))
willow <- aggregate(willow, by = list(willow$Observer), mean, na.rm = TRUE)
willow <- data.frame(subset(willow, select = -c(Observer, Group.1)))
willow <- t(willow)
colnames(willow) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(willow, TRUE, test = TRUE)
##### YORK
# Openness
york <- data.frame(subset(data, Bear == 'York'))
york <- data.frame(subset(york, select = c(Observer, O1, O2, O3, O4, O5, O6)))
york <- aggregate(york, by = list(york$Observer), mean, na.rm = TRUE)
york <- data.frame(subset(york, select = -c(Observer, Group.1)))
york <- t(york)
colnames(york) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(york, TRUE, test = TRUE)
# Conscientiousness
york <- data.frame(subset(data, Bear == 'York'))
york <- data.frame(subset(york, select = c(Observer, C1, C2, C3, C4, C5, C6)))
york <- aggregate(york, by = list(york$Observer), mean, na.rm = TRUE)
york <- data.frame(subset(york, select = -c(Observer, Group.1)))
york <- t(york)
colnames(york) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(york, TRUE, test = TRUE)
# Extraversion
york <- data.frame(subset(data, Bear == 'York'))
york <- data.frame(subset(york, select = c(Observer, E1, E2, E3, E4, E5, E6)))
york <- aggregate(york, by = list(york$Observer), mean, na.rm = TRUE)
york <- data.frame(subset(york, select = -c(Observer, Group.1)))
york <- t(york)
colnames(york) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(york, TRUE, test = TRUE)
# Agreeableness
york <- data.frame(subset(data, Bear == 'York'))
york <- data.frame(subset(york, select = c(Observer, A1, A2, A3, A4, A5, A6)))
york <- aggregate(york, by = list(york$Observer), mean, na.rm = TRUE)
york <- data.frame(subset(york, select = -c(Observer, Group.1)))
york <- t(york)
colnames(york) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(york, TRUE, test = TRUE)
# Neuroticism
york <- data.frame(subset(data, Bear == 'York'))
york <- data.frame(subset(york, select = c(Observer, N1, N2, N3, N4, N5, N6)))
york <- aggregate(york, by = list(york$Observer), mean, na.rm = TRUE)
york <- data.frame(subset(york, select = -c(Observer, Group.1)))
york <- t(york)
colnames(york) <- c("BD", "BF", "DC", "HP", "JE", "JK", "JKE", "JZ", "SM", "SMG", "SS")
KendallW(york, TRUE, test = TRUE)
##### Overall Mean Within The Factors
# Openness
o <- c(0.5609589, 0.53745, 0.5496357, 0.5968725, 0.581592, 0.4534231, 0.383905, 0.5243402, 0.6386507)
mean(o)
sd(o)
# Conscientiousness
c <- c(0.1044974, 0.5984315, 0.3446872, 0.3777025, 0.469697, 0.1838131, 0.3726287, 0.7359525, 0.463034)
mean(c)
sd(c)
# Extraversion
e <- c(0.4937343, 0.428804, 0.6854491, 0.3740187, 0.6198585, 0.4029342, 0.5860621, 0.7842186, 0.5122995)
mean(e)
sd(e)
# Agreeableness
a <- c(0.3316017, 0.1190135, 0.3990426, 0.3806977, 0.7611054, 0.4034792, 0.2357668, 0.5468446, 0.7019374)
mean(a)
sd(a)
# Neuroticism
n <- c(0.7039141, 0.4171891, 0.4602865, 0.3500912, 0.2195641, 0.344697, 0.2393978, 0.5804813, 0.3349713)
mean(n)
sd(n)
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

## Internal Consistency: Cronbach's Alpha - how well the behavioural descriptions describe each factor (e.g how well being assertive" specifically describes Extraversion in polar bears)
#### data import
library(tidyverse)
library(readr)
library (irr)
library(dplyr)
library(DescTools)
library(corrplot)
library(ggplot2)
library(reshape2)
rm(list=ls(all=T)) ## Code To Clear
data <- read_csv("C:/Users/keria/Desktop/Code/Polar Bear Personality Data 2018-2020.csv")
data$O1 <- as.numeric(data$O1)
data$O2 <- as.numeric(data$O2)
data$O3 <- as.numeric(data$O3)
data$O4 <- as.numeric(data$O4)
data$O5 <- as.numeric(data$O5)
data$O6 <- as.numeric(data$O6)
#### Overall
#Openness
o <- data.frame(data$O1, data$O2, data$O3, data$O4, data$O5, data$O6)
colnames(o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
#Conscientiousness
c <- data.frame(data$C1, data$C2, data$C3, data$C4, data$C5, data$C6)
colnames(c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
#Extraversion
e <- data.frame(data$E1, data$E2, data$E3, data$E4, data$E5, data$E6)
colnames(e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
#Agreeableness
a <- data.frame(data$A1, data$A2, data$A3, data$A4, data$A5, data$A6)
colnames(a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
#Neuroticism
n <- data.frame(data$N1, data$N2, data$N3, data$N4, data$N5, data$N6)
colnames(n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
## data analysis
CronbachAlpha(o, na.rm = TRUE)
CronbachAlpha(c, na.rm = TRUE)
CronbachAlpha(e, na.rm = TRUE) #### Extraversion - pretty low (a = 0.47)
CronbachAlpha(a, na.rm = TRUE)
CronbachAlpha(n, na.rm = TRUE)
#### Between The Years
### 2018
data2018 <- data.frame(subset(data, (Year == '2018')))
## Aurora
a8 <- data.frame(subset(data2018, (Bear == 'Aurora')))
#Openness
a8o <- data.frame(a8$O1, a8$O2, a8$O3, a8$O4, a8$O5, a8$O6)
colnames(a8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(c("2018","Aurora", "O", CronbachAlpha(a8o, na.rm = TRUE)))
#Conscientiousness
a8c <- data.frame(a8$C1, a8$C2, a8$C3, a8$C4, a8$C5, a8$C6)
colnames(a8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "Aurora", "C", CronbachAlpha(a8c, na.rm = TRUE))))
#Extraversion
a8e <- data.frame(a8$E1, a8$E2, a8$E3, a8$E4, a8$E5, a8$E6)
colnames(a8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2018", "Aurora", "E", CronbachAlpha(a8e, na.rm = TRUE))))
#Agreeableness
a8a <- data.frame(a8$A1, a8$A2, a8$A3, a8$A4, a8$A5, a8$A6)
colnames(a8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "Aurora", "A", CronbachAlpha(a8a, na.rm = TRUE))))
#Neuroticism
a8n <- data.frame(a8$N1, a8$N2, a8$N3, a8$N4, a8$N5, a8$N6)
colnames(a8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "Aurora", "N", CronbachAlpha(a8n, na.rm = TRUE))))
## Baffin
b8 <- data.frame(subset(data2018, (Bear == 'Baffin')))
#Openness
b8o <- data.frame(b8$O1, b8$O2, b8$O3, b8$O4, b8$O5, b8$O6)
colnames(b8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2018", "Baffin", "O", CronbachAlpha(b8o, na.rm = TRUE))))
#Conscientiousness
b8c <- data.frame(b8$C1, b8$C2, b8$C3, b8$C4, b8$C5, b8$C6)
colnames(b8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "Baffin", "C", CronbachAlpha(b8c, na.rm = TRUE))))
#Extraversion
b8e <- data.frame(b8$E1, b8$E2, b8$E3, b8$E4, b8$E5, b8$E6)
colnames(b8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2018", "Baffin", "E", CronbachAlpha(b8e, na.rm = TRUE))))
#Agreeableness
b8a <- data.frame(b8$A1, b8$A2, b8$A3, b8$A4, b8$A5, b8$A6)
colnames(b8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "Baffin", "A", CronbachAlpha(b8a, na.rm = TRUE))))
#Neuroticism
b8n <- data.frame(b8$N1, b8$N2, b8$N3, b8$N4, b8$N5, b8$N6)
colnames(b8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "Baffin", "N", CronbachAlpha(b8n, na.rm = TRUE))))
## Kaska
k8 <- data.frame(subset(data2018, (Bear == 'Kaska')))
#Openness
k8o <- data.frame(k8$O1, k8$O2, k8$O3, k8$O4, k8$O5, k8$O6)
colnames(k8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2018", "Kaska", "O", CronbachAlpha(k8o, na.rm = TRUE))))
#Conscientiousness
k8c <- data.frame(k8$C1, k8$C2, k8$C3, k8$C4, k8$C5, k8$C6)
colnames(k8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "Kaska", "C", CronbachAlpha(k8c, na.rm = TRUE))))
#Extraversion
k8e <- data.frame(k8$E1, k8$E2, k8$E3, k8$E4, k8$E5, k8$E6)
colnames(k8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2018", "Kaska", "E", CronbachAlpha(k8e, na.rm = TRUE))))
#Agreeableness
k8a <- data.frame(k8$A1, k8$A2, k8$A3, k8$A4, k8$A5, k8$A6)
colnames(k8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "Kaska", "A", CronbachAlpha(k8a, na.rm = TRUE))))
#Neuroticism
k8n <- data.frame(k8$N1, k8$N2, k8$N3, k8$N4, k8$N5, k8$N6)
colnames(k8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "Kaska", "N", CronbachAlpha(k8n, na.rm = TRUE))))
## Nanuq
n8 <- data.frame(subset(data2018, (Bear == 'Nanuq')))
#Openness
n8o <- data.frame(n8$O1, n8$O2, n8$O3, n8$O4, n8$O5, n8$O6)
colnames(n8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2018", "Nanuq", "O", CronbachAlpha(n8o, na.rm = TRUE))))
#Conscientiousness
n8c <- data.frame(n8$C1, n8$C2, n8$C3, n8$C4, n8$C5, n8$C6)
colnames(n8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "Nanuq", "C", CronbachAlpha(n8c, na.rm = TRUE))))
#Extraversion
n8e <- data.frame(n8$E1, n8$E2, n8$E3, n8$E4, n8$E5, n8$E6)
colnames(n8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2018", "Nanuq", "E", CronbachAlpha(n8e, na.rm = TRUE))))
#Agreeableness
n8a <- data.frame(n8$A1, n8$A2, n8$A3, n8$A4, n8$A5, n8$A6)
colnames(n8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "Nanuq", "A", CronbachAlpha(n8a, na.rm = TRUE))))
#Neuroticism
n8n <- data.frame(n8$N1, n8$N2, n8$N3, n8$N4, n8$N5, n8$N6)
colnames(n8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "Nanuq", "N", CronbachAlpha(n8n, na.rm = TRUE))))
## Siku
si8 <- data.frame(subset(data2018, (Bear == 'Siku')))
#Openness
si8o <- data.frame(si8$O1, si8$O2, si8$O3, si8$O4, si8$O5, si8$O6)
colnames(si8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2018", "Siku", "O", CronbachAlpha(si8o, na.rm = TRUE))))
#Conscientiousness
si8c <- data.frame(si8$C1, si8$C2, si8$C3, si8$C4, si8$C5, si8$C6)
colnames(si8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "Siku", "C", CronbachAlpha(si8c, na.rm = TRUE))))
#Extraversion
si8e <- data.frame(si8$E1, si8$E2, si8$E3, si8$E4, si8$E5, si8$E6)
colnames(si8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2018", "Siku", "E", CronbachAlpha(si8e, na.rm = TRUE))))
#Agreeableness
si8a <- data.frame(si8$A1, si8$A2, si8$A3, si8$A4, si8$A5, si8$A6)
colnames(si8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "Siku", "A", CronbachAlpha(si8a, na.rm = TRUE))))
#Neuroticism
si8n <- data.frame(si8$N1, si8$N2, si8$N3, si8$N4, si8$N5, si8$N6)
colnames(si8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "Siku", "N", CronbachAlpha(si8n, na.rm = TRUE))))
## Star
s8 <- data.frame(subset(data2018, (Bear == 'Star')))
#Openness
s8o <- data.frame(s8$O1, s8$O2, s8$O3, s8$O4, s8$O5, s8$O6)
colnames(s8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2018", "Star", "O", CronbachAlpha(s8o, na.rm = TRUE))))
#Conscientiousness
s8c <- data.frame(s8$C1, s8$C2, s8$C3, s8$C4, s8$C5, s8$C6)
colnames(s8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "Star", "C", CronbachAlpha(s8c, na.rm = TRUE))))
#Extraversion
s8e <- data.frame(s8$E1, s8$E2, s8$E3, s8$E4, s8$E5, s8$E6)
colnames(s8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2018", "Star", "E", CronbachAlpha(s8e, na.rm = TRUE))))
#Agreeableness
s8a <- data.frame(s8$A1, s8$A2, s8$A3, s8$A4, s8$A5, s8$A6)
colnames(s8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "Star", "E", CronbachAlpha(s8a, na.rm = TRUE))))
#Neuroticism
s8n <- data.frame(s8$N1, s8$N2, s8$N3, s8$N4, s8$N5, s8$N6)
colnames(s8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "Star", "N", CronbachAlpha(s8n, na.rm = TRUE))))
##Storm
sm8 <- data.frame(subset(data2018, (Bear == 'Storm')))
#Openness
sm8o <- data.frame(sm8$O1, sm8$O2, sm8$O3, sm8$O4, sm8$O5, sm8$O6)
colnames(sm8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2018", "Storm", "O", CronbachAlpha(sm8o, na.rm = TRUE))))
#Conscientiousness
sm8c <- data.frame(sm8$C1, sm8$C2, sm8$C3, sm8$C4, sm8$C5, sm8$C6)
colnames(sm8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "Storm", "C", CronbachAlpha(sm8c, na.rm = TRUE))))
#Extraversion
sm8e <- data.frame(sm8$E1, sm8$E2, sm8$E3, sm8$E4, sm8$E5, sm8$E6)
colnames(sm8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2018", "Storm", "E", CronbachAlpha(sm8e, na.rm = TRUE))))
#Agreeableness
sm8a <- data.frame(sm8$A1, sm8$A2, sm8$A3, sm8$A4, sm8$A5, sm8$A6)
colnames(sm8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "Storm", "A", CronbachAlpha(sm8a, na.rm = TRUE))))
#Neuroticism
sm8n <- data.frame(sm8$N1, sm8$N2, sm8$N3, sm8$N4, sm8$N5, sm8$N6)
colnames(sm8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "Storm", "N", CronbachAlpha(sm8n, na.rm = TRUE))))
## Willow
w8 <- data.frame(subset(data2018, (Bear == 'Willow')))
#Openness
w8o <- data.frame(w8$O1, w8$O2, w8$O3, w8$O4, w8$O5, w8$O6)
colnames(w8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2018", "Willow", "O", CronbachAlpha(w8o, na.rm = TRUE))))
#Conscientiousness
w8c <- data.frame(w8$C1, w8$C2, w8$C3, w8$C4, w8$C5, w8$C6)
colnames(w8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "Willow", "C", CronbachAlpha(w8c, na.rm = TRUE))))
#Extraversion
w8e <- data.frame(w8$E1, w8$E2, w8$E3, w8$E4, w8$E5, w8$E6)
colnames(w8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
<- data.frame(IC, add_column(c("2018", "Willow", "E", CronbachAlpha(w8e, na.rm = TRUE))))
#Agreeableness
w8a <- data.frame(w8$A1, w8$A2, w8$A3, w8$A4, w8$A5, w8$A6)
colnames(w8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "Willow", "A", CronbachAlpha(w8a, na.rm = TRUE))))
#Neuroticism
w8n <- data.frame(w8$N1, w8$N2, w8$N3, w8$N4, w8$N5, w8$N6)
colnames(w8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "Willow", "N", CronbachAlpha(w8n, na.rm = TRUE))))
## York
y8 <- data.frame(subset(data2018, (Bear == 'York')))
#Openness
y8o <- data.frame(y8$O1, y8$O2, y8$O3, y8$O4, y8$O5, y8$O6)
colnames(y8o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2018", "York", "O", CronbachAlpha(y8o, na.rm = TRUE))))
#Conscientiousness
y8c <- data.frame(y8$C1, y8$C2, y8$C3, y8$C4, y8$C5, y8$C6)
colnames(y8c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2018", "York", "C", CronbachAlpha(y8c, na.rm = TRUE))))
#Extraversion
y8e <- data.frame(y8$E1, y8$E2, y8$E3, y8$E4, y8$E5, y8$E6)
colnames(y8e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2018", "York", "E", CronbachAlpha(y8e, na.rm = TRUE))))
#Agreeableness
y8a <- data.frame(y8$A1, y8$A2, y8$A3, y8$A4, y8$A5, y8$A6)
colnames(y8a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2018", "York", "A", CronbachAlpha(y8a, na.rm = TRUE))))
#Neuroticism
y8n <- data.frame(y8$N1, y8$N2, y8$N3, y8$N4, y8$N5, y8$N6)
colnames(y8n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2018", "York", "N", CronbachAlpha(y8n, na.rm = TRUE))))
### 2019
data2019 <- data.frame(subset(data, (Year == '2019')))
## Aurora
a9 <- data.frame(subset(data2019, (Bear == 'Aurora')))
#Openness
a9o <- data.frame(a9$O1, a9$O2, a9$O3, a9$O4, a9$O5, a9$O6)
colnames(a9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "Aurora", "O", CronbachAlpha(a9o, na.rm = TRUE))))
#Conscientiousness
a9c <- data.frame(a9$C1, a9$C2, a9$C3, a9$C4, a9$C5, a9$C6)
colnames(a9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "Aurora", "C", CronbachAlpha(a9c, na.rm = TRUE))))
#Extraversion
a9e <- data.frame(a9$E1, a9$E2, a9$E3, a9$E4, a9$E5, a9$E6)
colnames(a9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "Aurora", "E", CronbachAlpha(a9e, na.rm = TRUE))))
#Agreeableness
a9a <- data.frame(a9$A1, a9$A2, a9$A3, a9$A4, a9$A5, a9$A6)
colnames(a9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "Aurora", "A", CronbachAlpha(a9a, na.rm = TRUE))))
#Neuroticism
a9n <- data.frame(a9$N1, a9$N2, a9$N3, a9$N4, a9$N5, a9$N6)
colnames(a9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "Aurora", "N", CronbachAlpha(a9n, na.rm = TRUE))))
## Baffin
b9 <- data.frame(subset(data2019, (Bear == 'Baffin')))
#Openness
b9o <- data.frame(b9$O1, b9$O2, b9$O3, b9$O4, b9$O5, b9$O6)
colnames(b9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "Baffin", "O", CronbachAlpha(b9o, na.rm = TRUE))))
#Conscientiousness
b9c <- data.frame(b9$C1, b9$C2, b9$C3, b9$C4, b9$C5, b9$C6)
colnames(b9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "Baffin", "C", CronbachAlpha(b9c, na.rm = TRUE))))
#Extraversion
b9e <- data.frame(b9$E1, b9$E2, b9$E3, b9$E4, b9$E5, b9$E6)
colnames(b9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "Baffin", "E", CronbachAlpha(b9e, na.rm = TRUE))))
#Agreeableness
b9a <- data.frame(b9$A1, b9$A2, b9$A3, b9$A4, b9$A5, b9$A6)
colnames(b9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "Baffin", "A", CronbachAlpha(b9a, na.rm = TRUE))))
#Neuroticism
b9n <- data.frame(b9$N1, b9$N2, b9$N3, b9$N4, b9$N5, b9$N6)
colnames(b9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "Baffin", "N", CronbachAlpha(b9n, na.rm = TRUE))))
## Kaska
k9 <- data.frame(subset(data2019, (Bear == 'Kaska')))
#Openness
k9o <- data.frame(k9$O1, k9$O2, k9$O3, k9$O4, k9$O5, k9$O6)
colnames(k9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "Kaska", "O", CronbachAlpha(k9o, na.rm = TRUE))))
#Conscientiousness
k9c <- data.frame(k9$C1, k9$C2, k9$C3, k9$C4, k9$C5, k9$C6)
colnames(k9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "Kaska", "C", CronbachAlpha(k9c, na.rm = TRUE))))
#Extraversion
k9e <- data.frame(k9$E1, k9$E2, k9$E3, k9$E4, k9$E5, k9$E6)
colnames(k9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "Kaska", "E", CronbachAlpha(k9e, na.rm = TRUE))))
#Agreeableness
k9a <- data.frame(k9$A1, k9$A2, k9$A3, k9$A4, k9$A5, k9$A6)
colnames(k9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "Kaska", "A", CronbachAlpha(k9a, na.rm = TRUE))))
#Neuroticism
k9n <- data.frame(k9$N1, k9$N2, k9$N3, k9$N4, k9$N5, k9$N6)
colnames(k9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "Kaska", "N", CronbachAlpha(k9n, na.rm = TRUE))))
## Nanuq
n9 <- data.frame(subset(data2019, (Bear == 'Nanuq')))
#Openness
n9o <- data.frame(n9$O1, n9$O2, n9$O3, n9$O4, n9$O5, n9$O6)
colnames(n9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "Nanuq", "O", CronbachAlpha(n9o, na.rm = TRUE))))
#Conscientiousness
n9c <- data.frame(n9$C1, n9$C2, n9$C3, n9$C4, n9$C5, n9$C6)
colnames(n9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "Nanuq", "C", CronbachAlpha(n9c, na.rm = TRUE))))
#Extraversion
n9e <- data.frame(n9$E1, n9$E2, n9$E3, n9$E4, n9$E5, n9$E6)
colnames(n9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "Nanuq", "E", CronbachAlpha(n9e, na.rm = TRUE))))
#Agreeableness
n9a <- data.frame(n9$A1, n9$A2, n9$A3, n9$A4, n9$A5, n9$A6)
colnames(n9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "Nanuq", "A", CronbachAlpha(n9a, na.rm = TRUE))))
#Neuroticism
n9n <- data.frame(n9$N1, n9$N2, n9$N3, n9$N4, n9$N5, n9$N6)
colnames(n9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "Nanuq", "N", CronbachAlpha(n9n, na.rm = TRUE))))
## Siku
si9 <- data.frame(subset(data2019, (Bear == 'Siku')))
#Openness
si9o <- data.frame(si9$O1, si9$O2, si9$O3, si9$O4, si9$O5, si9$O6)
colnames(si9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "Siku", "O", CronbachAlpha(si9o, na.rm = TRUE))))
#Conscientiousness
si9c <- data.frame(si9$C1, si9$C2, si9$C3, si9$C4, si9$C5, si9$C6)
colnames(si9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "Siku", "C", CronbachAlpha(si9c, na.rm = TRUE))))
#Extraversion
si9e <- data.frame(si9$E1, si9$E2, si9$E3, si9$E4, si9$E5, si9$E6)
colnames(si9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "Siku", "E", CronbachAlpha(si9e, na.rm = TRUE))))
#Agreeableness
si9a <- data.frame(si9$A1, si9$A2, si9$A3, si9$A4, si9$A5, si9$A6)
colnames(si9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "Siku", "A", CronbachAlpha(si9a, na.rm = TRUE))))
#Neuroticism
si9n <- data.frame(si9$N1, si9$N2, si9$N3, si9$N4, si9$N5, si9$N6)
colnames(si9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "Siku", "N", CronbachAlpha(si9n, na.rm = TRUE))))
## Star
s9 <- data.frame(subset(data2019, (Bear == 'Star')))
#Openness
s9o <- data.frame(s9$O1, s9$O2, s9$O3, s9$O4, s9$O5, s9$O6)
colnames(s9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "Star", "O", CronbachAlpha(s9o, na.rm = TRUE))))
#Conscientiousness
s9c <- data.frame(s9$C1, s9$C2, s9$C3, s9$C4, s9$C5, s9$C6)
colnames(s9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "Star", "C", CronbachAlpha(s9c, na.rm = TRUE))))
#Extraversion
s9e <- data.frame(s9$E1, s9$E2, s9$E3, s9$E4, s9$E5, s9$E6)
colnames(s9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "Star", "E", CronbachAlpha(s9e, na.rm = TRUE))))
#Agreeableness
s9a <- data.frame(s9$A1, s9$A2, s9$A3, s9$A4, s9$A5, s9$A6)
colnames(s9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "Star", "E", CronbachAlpha(s9a, na.rm = TRUE))))
#Neuroticism
s9n <- data.frame(s9$N1, s9$N2, s9$N3, s9$N4, s9$N5, s9$N6)
colnames(s9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "Star", "N", CronbachAlpha(s9n, na.rm = TRUE))))
##Storm
sm9 <- data.frame(subset(data2019, (Bear == 'Storm')))
#Openness
sm9o <- data.frame(sm9$O1, sm9$O2, sm9$O3, sm9$O4, sm9$O5, sm9$O6)
colnames(sm9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "Storm", "O", CronbachAlpha(sm9o, na.rm = TRUE))))
#Conscientiousness
sm9c <- data.frame(sm9$C1, sm9$C2, sm9$C3, sm9$C4, sm9$C5, sm9$C6)
colnames(sm9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "Storm", "C", CronbachAlpha(sm9c, na.rm = TRUE))))
#Extraversion
sm9e <- data.frame(sm9$E1, sm9$E2, sm9$E3, sm9$E4, sm9$E5, sm9$E6)
colnames(sm9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "Storm", "E", CronbachAlpha(sm9e, na.rm = TRUE))))
#Agreeableness
sm9a <- data.frame(sm9$A1, sm9$A2, sm9$A3, sm9$A4, sm9$A5, sm9$A6)
colnames(sm9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "Storm", "A", CronbachAlpha(sm9a, na.rm = TRUE))))
#Neuroticism
sm9n <- data.frame(sm9$N1, sm9$N2, sm9$N3, sm9$N4, sm9$N5, sm9$N6)
colnames(sm9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "Storm", "N", CronbachAlpha(sm9n, na.rm = TRUE))))
## Willow
w9 <- data.frame(subset(data2019, (Bear == 'Willow')))
#Openness
w9o <- data.frame(w9$O1, w9$O2, w9$O3, w9$O4, w9$O5, w9$O6)
colnames(w9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "Willow", "O", CronbachAlpha(w9o, na.rm = TRUE))))
#Conscientiousness
w9c <- data.frame(w9$C1, w9$C2, w9$C3, w9$C4, w9$C5, w9$C6)
colnames(w9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "Willow", "C", CronbachAlpha(w9c, na.rm = TRUE))))
#Extraversion
w9e <- data.frame(w9$E1, w9$E2, w9$E3, w9$E4, w9$E5, w9$E6)
colnames(w9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "Willow", "E", CronbachAlpha(w9e, na.rm = TRUE))))
#Agreeableness
w9a <- data.frame(w9$A1, w9$A2, w9$A3, w9$A4, w9$A5, w9$A6)
colnames(w9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "Willow", "A", CronbachAlpha(w9a, na.rm = TRUE))))
#Neuroticism
w9n <- data.frame(w9$N1, w9$N2, w9$N3, w9$N4, w9$N5, w9$N6)
colnames(w9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "Willow", "N", CronbachAlpha(w9n, na.rm = TRUE))))
## York
y9 <- data.frame(subset(data2019, (Bear == 'York')))
#Openness
y9o <- data.frame(y9$O1, y9$O2, y9$O3, y9$O4, y9$O5, y9$O6)
colnames(y9o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2019", "York", "O", CronbachAlpha(y9o, na.rm = TRUE))))
#Conscientiousness
y9c <- data.frame(y9$C1, y9$C2, y9$C3, y9$C4, y9$C5, y9$C6)
colnames(y9c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2019", "York", "C", CronbachAlpha(y9c, na.rm = TRUE))))
#Extraversion
y9e <- data.frame(y9$E1, y9$E2, y9$E3, y9$E4, y9$E5, y9$E6)
colnames(y9e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2019", "York", "E", CronbachAlpha(y9e, na.rm = TRUE))))
#Agreeableness
y9a <- data.frame(y9$A1, y9$A2, y9$A3, y9$A4, y9$A5, y9$A6)
colnames(y9a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2019", "York", "A", CronbachAlpha(y9a, na.rm = TRUE))))
#Neuroticism
y9n <- data.frame(y9$N1, y9$N2, y9$N3, y9$N4, y9$N5, y9$N6)
colnames(y9n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2019", "York", "N", CronbachAlpha(y9n, na.rm = TRUE))))
### 2020
data2020 <- data.frame(subset(data, (Year == '2020')))
## Aurora
a2 <- data.frame(subset(data2020, (Bear == 'Aurora')))
#Openness
a2o <- data.frame(a2$O1, a2$O2, a2$O3, a2$O4, a2$O5, a2$O6)
colnames(a2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "Aurora", "O", CronbachAlpha(a2o, na.rm = TRUE))))
#Conscientiousness
a2c <- data.frame(a2$C1, a2$C2, a2$C3, a2$C4, a2$C5, a2$C6)
colnames(a2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "Aurora", "C", CronbachAlpha(a2c, na.rm = TRUE))))
#Extraversion
a2e <- data.frame(a2$E1, a2$E2, a2$E3, a2$E4, a2$E5, a2$E6)
colnames(a2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "Aurora", "E", CronbachAlpha(a2e, na.rm = TRUE))))
#Agreeableness
a2a <- data.frame(a2$A1, a2$A2, a2$A3, a2$A4, a2$A5, a2$A6)
colnames(a2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "Aurora", "A", CronbachAlpha(a2a, na.rm = TRUE))))
#Neuroticism
a2n <- data.frame(a2$N1, a2$N2, a2$N3, a2$N4, a2$N5, a2$N6)
colnames(a2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "Aurora", "N", CronbachAlpha(a2n, na.rm = TRUE))))
## Baffin
b2 <- data.frame(subset(data2020, (Bear == 'Baffin')))
#Openness
b2o <- data.frame(b2$O1, b2$O2, b2$O3, b2$O4, b2$O5, b2$O6)
colnames(b2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "Baffin", "O", CronbachAlpha(b2o, na.rm = TRUE))))
#Conscientiousness
b2c <- data.frame(b2$C1, b2$C2, b2$C3, b2$C4, b2$C5, b2$C6)
colnames(b2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "Baffin", "C", CronbachAlpha(b2c, na.rm = TRUE))))
#Extraversion
b2e <- data.frame(b2$E1, b2$E2, b2$E3, b2$E4, b2$E5, b2$E6)
colnames(b2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "Baffin", "E", CronbachAlpha(b2e, na.rm = TRUE))))
#Agreeableness
b2a <- data.frame(b2$A1, b2$A2, b2$A3, b2$A4, b2$A5, b2$A6)
colnames(b2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "Baffin", "A", CronbachAlpha(b2a, na.rm = TRUE))))
#Neuroticism
b2n <- data.frame(b2$N1, b2$N2, b2$N3, b2$N4, b2$N5, b2$N6)
colnames(b2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "Baffin", "N", CronbachAlpha(b2n, na.rm = TRUE))))
## Kaska
k2 <- data.frame(subset(data2020, (Bear == 'Kaska')))
#Openness
k2o <- data.frame(k2$O1, k2$O2, k2$O3, k2$O4, k2$O5, k2$O6)
colnames(k2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "Kaska", "O", CronbachAlpha(k2o, na.rm = TRUE))))
#Conscientiousness
k2c <- data.frame(k2$C1, k2$C2, k2$C3, k2$C4, k2$C5, k2$C6)
colnames(k2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "Kaska", "C", CronbachAlpha(k2c, na.rm = TRUE))))
#Extraversion
k2e <- data.frame(k2$E1, k2$E2, k2$E3, k2$E4, k2$E5, k2$E6)
colnames(k2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "Kaska", "E", CronbachAlpha(k2e, na.rm = TRUE))))
#Agreeableness
k2a <- data.frame(k2$A1, k2$A2, k2$A3, k2$A4, k2$A5, k2$A6)
colnames(k2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "Kaska", "A", CronbachAlpha(k2a, na.rm = TRUE))))
#Neuroticism
k2n <- data.frame(k2$N1, k2$N2, k2$N3, k2$N4, k2$N5, k2$N6)
colnames(k2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "Kaska", "N", CronbachAlpha(k2n, na.rm = TRUE))))
## Nanuq
n2 <- data.frame(subset(data2020, (Bear == 'Nanuq')))
#Openness
n2o <- data.frame(n2$O1, n2$O2, n2$O3, n2$O4, n2$O5, n2$O6)
colnames(n2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "Nanuq", "O", CronbachAlpha(n2o, na.rm = TRUE))))
#Conscientiousness
n2c <- data.frame(n2$C1, n2$C2, n2$C3, n2$C4, n2$C5, n2$C6)
colnames(n2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "Nanuq", "C", CronbachAlpha(n2c, na.rm = TRUE))))
#Extraversion
n2e <- data.frame(n2$E1, n2$E2, n2$E3, n2$E4, n2$E5, n2$E6)
colnames(n2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "Nanuq", "E", CronbachAlpha(n2e, na.rm = TRUE))))
#Agreeableness
n2a <- data.frame(n2$A1, n2$A2, n2$A3, n2$A4, n2$A5, n2$A6)
colnames(n2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "Nanuq", "A", CronbachAlpha(n2a, na.rm = TRUE))))
#Neuroticism
n2n <- data.frame(n2$N1, n2$N2, n2$N3, n2$N4, n2$N5, n2$N6)
colnames(n2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "Nanuq", "N", CronbachAlpha(n2n, na.rm = TRUE))))
## Siku
si2 <- data.frame(subset(data2020, (Bear == 'Siku')))
#Openness
si2o <- data.frame(si2$O1, si2$O2, si2$O3, si2$O4, si2$O5, si2$O6)
colnames(si2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "Siku", "O", CronbachAlpha(si2o, na.rm = TRUE))))
#Conscientiousness
si2c <- data.frame(si2$C1, si2$C2, si2$C3, si2$C4, si2$C5, si2$C6)
colnames(si2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "Siku", "C", CronbachAlpha(si2c, na.rm = TRUE))))
#Extraversion
si2e <- data.frame(si2$E1, si2$E2, si2$E3, si2$E4, si2$E5, si2$E6)
colnames(si2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "Siku", "E", CronbachAlpha(si2e, na.rm = TRUE))))
#Agreeableness
si2a <- data.frame(si2$A1, si2$A2, si2$A3, si2$A4, si2$A5, si2$A6)
colnames(si2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "Siku", "A", CronbachAlpha(si2a, na.rm = TRUE))))
#Neuroticism
si2n <- data.frame(si2$N1, si2$N2, si2$N3, si2$N4, si2$N5, si2$N6)
colnames(si2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "Siku", "N", CronbachAlpha(si2n, na.rm = TRUE))))
## Star
s2 <- data.frame(subset(data2020, (Bear == 'Star')))
#Openness
s2o <- data.frame(s2$O1, s2$O2, s2$O3, s2$O4, s2$O5, s2$O6)
colnames(s2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "Star", "O", CronbachAlpha(s2o, na.rm = TRUE))))
#Conscientiousness
s2c <- data.frame(s2$C1, s2$C2, s2$C3, s2$C4, s2$C5, s2$C6)
colnames(s2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "Star", "C", CronbachAlpha(s2c, na.rm = TRUE))))
#Extraversion
s2e <- data.frame(s2$E1, s2$E2, s2$E3, s2$E4, s2$E5, s2$E6)
colnames(s2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "Star", "E", CronbachAlpha(s2e, na.rm = TRUE))))
#Agreeableness
s2a <- data.frame(s2$A1, s2$A2, s2$A3, s2$A4, s2$A5, s2$A6)
colnames(s2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "Star", "E", CronbachAlpha(s2a, na.rm = TRUE))))
#Neuroticism
s2n <- data.frame(s2$N1, s2$N2, s2$N3, s2$N4, s2$N5, s2$N6)
colnames(s2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "Star", "N", CronbachAlpha(s2n, na.rm = TRUE))))
##Storm
sm2 <- data.frame(subset(data2020, (Bear == 'Storm')))
#Openness
sm2o <- data.frame(sm2$O1, sm2$O2, sm2$O3, sm2$O4, sm2$O5, sm2$O6)
colnames(sm2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "Storm", "O", CronbachAlpha(sm2o, na.rm = TRUE))))
#Conscientiousness
sm2c <- data.frame(sm2$C1, sm2$C2, sm2$C3, sm2$C4, sm2$C5, sm2$C6)
colnames(sm2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "Storm", "C", CronbachAlpha(sm2c, na.rm = TRUE))))
#Extraversion
sm2e <- data.frame(sm2$E1, sm2$E2, sm2$E3, sm2$E4, sm2$E5, sm2$E6)
colnames(sm2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "Storm", "E", CronbachAlpha(sm2e, na.rm = TRUE))))
#Agreeableness
sm2a <- data.frame(sm2$A1, sm2$A2, sm2$A3, sm2$A4, sm2$A5, sm2$A6)
colnames(sm2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "Storm", "A", CronbachAlpha(sm2a, na.rm = TRUE))))
#Neuroticism
sm2n <- data.frame(sm2$N1, sm2$N2, sm2$N3, sm2$N4, sm2$N5, sm2$N6)
colnames(sm2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "Storm", "N", CronbachAlpha(sm2n, na.rm = TRUE))))
## Willow
w2 <- data.frame(subset(data2020, (Bear == 'Willow')))
#Openness
w2o <- data.frame(w2$O1, w2$O2, w2$O3, w2$O4, w2$O5, w2$O6)
colnames(w2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "Willow", "O", CronbachAlpha(w2o, na.rm = TRUE))))
#Conscientiousness
w2c <- data.frame(w2$C1, w2$C2, w2$C3, w2$C4, w2$C5, w2$C6)
colnames(w2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "Willow", "C", CronbachAlpha(w2c, na.rm = TRUE))))
#Extraversion
w2e <- data.frame(w2$E1, w2$E2, w2$E3, w2$E4, w2$E5, w2$E6)
colnames(w2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "Willow", "E", CronbachAlpha(w2e, na.rm = TRUE))))
#Agreeableness
w2a <- data.frame(w2$A1, w2$A2, w2$A3, w2$A4, w2$A5, w2$A6)
colnames(w2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "Willow", "A", CronbachAlpha(w2a, na.rm = TRUE))))
#Neuroticism
w2n <- data.frame(w2$N1, w2$N2, w2$N3, w2$N4, w2$N5, w2$N6)
colnames(w2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "Willow", "N", CronbachAlpha(w2n, na.rm = TRUE))))
## York
y2 <- data.frame(subset(data2020, (Bear == 'York')))
#Openness
y2o <- data.frame(y2$O1, y2$O2, y2$O3, y2$O4, y2$O5, y2$O6)
colnames(y2o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
IC <- data.frame(IC, add_column(c("2020", "York", "O", CronbachAlpha(y2o, na.rm = TRUE))))
#Conscientiousness
y2c <- data.frame(y2$C1, y2$C2, y2$C3, y2$C4, y2$C5, y2$C6)
colnames(y2c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
IC <- data.frame(IC, add_column(c("2020", "York", "C", CronbachAlpha(y2c, na.rm = TRUE))))
#Extraversion
y2e <- data.frame(y2$E1, y2$E2, y2$E3, y2$E4, y2$E5, y2$E6)
colnames(y2e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
IC <- data.frame(IC, add_column(c("2020", "York", "E", CronbachAlpha(y2e, na.rm = TRUE))))
#Agreeableness
y2a <- data.frame(y2$A1, y2$A2, y2$A3, y2$A4, y2$A5, y2$A6)
colnames(y2a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
IC <- data.frame(IC, add_column(c("2020", "York", "A", CronbachAlpha(y2a, na.rm = TRUE))))
#Neuroticism
y2n <- data.frame(y2$N1, y2$N2, y2$N3, y2$N4, y2$N5, y2$N6)
colnames(y2n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
IC <- data.frame(IC, add_column(c("2020", "York", "N", CronbachAlpha(y2n, na.rm = TRUE))))
colnames(IC) <- c(1:135)
IC <- data.frame(t(IC))
colnames(IC) <- c("Year", "Bear", "Factor", "Alpha")
means <- IC
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
rm(list=ls(all=T)) ## Code To Clear
data <- read_csv("C:/Users/keria/Desktop/Code/Polar Bear Personality Data 2018-2020.csv")
# o
o <- data.frame(data$O1, data$O2, data$O3, data$O4, data$O5, data$O6)
colnames(o) <- c("O1", "O2", "O3", "O4", "O5", "O6")
o <- cor(o, method = "pearson", use = "complete.obs")
# C
c <- data.frame(data$C1, data$C2, data$C3, data$C4, data$C5, data$C6)
colnames(c) <- c("C1", "C2", "C3", "C4", "C5", "C6")
c <- cor(c, method = "pearson", use = "complete.obs")
mean(c)
sd(c)
# E
e <- data.frame(data$E1, data$E2, data$E3, data$E4, data$E5, data$E6)
colnames(e) <- c("E1", "E2", "E3", "E4", "E5", "E6")
e <- cor(e, method = "pearson", use = "complete.obs")
mean(e)
sd(e)
# A
a <- data.frame(data$A1, data$A2, data$A3, data$A4, data$A5, data$A6)
colnames(a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
a <- cor(a, method = "pearson", use = "complete.obs")
mean(a)
sd(a)
# N
n <- data.frame(data$N1, data$N2, data$N3, data$N4, data$N5, data$N6)
colnames(n) <- c("N1", "N2", "N3", "N4", "N5", "N6")
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
library(tidyverse)
library(readr)
library(irr)
library(DescTools)
library(car)
library(ggridges)
library(dplyr)
library(lubridate)
rm(list=ls(all=T)) ## code to clear
data <- read_csv("C:/Users/keria/Desktop/Code/Polar Bear Personality Data 2018-2020.csv")
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