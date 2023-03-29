setwd("/Users/lukeandreesen/Desktop/SOSC Final")

library(rio)
library(here)
library(tidyverse)
library(countrycode)
library(ggplot2)
library(sjmisc)
library(sjlabelled)
library(sandwich)
library(lmtest)
library(modelr)
library(broom)
library(sjPlot)


here::i_am("data/GDP.csv")
gdp <- import( here("data/GDP.csv") )
dim(gdp)

colnames(gdp)[1] <- "Country"
colnames(gdp)[2] <- "Country_Code"
colnames(gdp)[3] <- "GDP"
gdp <- drop_na(gdp, any_of("GDP"))
gdp <- gdp %>% select(-Country)

here::i_am("data/GNI.csv")
gni <- import( here("data/GNI.csv") )
dim(gni)

colnames(gni)[1] <- "Country"
colnames(gni)[2] <- "Country_Code"
colnames(gni)[3] <- "GNI"
gni <- drop_na(gni, any_of("GNI"))
gni <- gni %>% select(-Country)

gdp_gni <- merge(gdp, gni, by = "Country_Code")

here::i_am("data/Female HPV.csv")
female_hpv <- import( here("data/Female HPV.csv") )
dim(female_hpv)

colnames(female_hpv)[1] <- "Country"
colnames(female_hpv)[4] <- "Uncertainty"
colnames(female_hpv)[5] <- "Rate"
colnames(female_hpv)[6] <- "ASR"
colnames(female_hpv)[7] <- "Risk"

female_hpv <- female_hpv %>% select(-Population)
female_hpv <- female_hpv %>% select(-Number)
female_hpv <- female_hpv %>% select(-Uncertainty)
female_hpv <- female_hpv %>% select(-Risk)
female_hpv <- female_hpv %>% select(-Rate)
female_hpv$Country <- countrycode(female_hpv$Country, "country.name", "wb")
female_hpv <- drop_na(female_hpv, any_of("Country"))
colnames(female_hpv)[1] <- "Country_Code"


here::i_am("data/Male HPV.csv")
male_hpv <- import( here("data/Male HPV.csv") )
dim(male_hpv)

colnames(male_hpv)[1] <- "Country"
colnames(male_hpv)[4] <- "Uncertainty"
colnames(male_hpv)[5] <- "Rate"
colnames(male_hpv)[6] <- "ASR"
colnames(male_hpv)[7] <- "Risk"

male_hpv <- male_hpv %>% select(-Population)
male_hpv <- male_hpv %>% select(-Number)
male_hpv <- male_hpv %>% select(-Uncertainty)
male_hpv <- male_hpv %>% select(-Risk)
male_hpv <- male_hpv %>% select(-Rate)
male_hpv$Country <- countrycode(male_hpv$Country, "country.name", "wb")
male_hpv <- drop_na(male_hpv, any_of("Country"))
colnames(male_hpv)[1] <- "Country_Code"

hpv <- merge(female_hpv, male_hpv, by = "Country_Code")
hpv$row_mean <- rowMeans(hpv[ , c(2,3)], na.rm=TRUE)
colnames(hpv)[2] <- "ASR_Female"
colnames(hpv)[3] <- "ASR_Male"
colnames(hpv)[4] <- "ASR_Combined"


here::i_am("data/Testis.csv")
testis <- import( here("data/Testis.csv") )
dim(testis)

colnames(testis)[1] <- "Country"
colnames(testis)[4] <- "Uncertainty"
colnames(testis)[5] <- "Rate"
colnames(testis)[6] <- "ASR"
colnames(testis)[7] <- "Risk"

testis <- testis %>% select(-Population)
testis <- testis %>% select(-Number)
testis <- testis %>% select(-Uncertainty)
testis <- testis %>% select(-Risk)
testis <- testis %>% select(-Rate)

testis$Country <- countrycode(testis$Country, "country.name", "wb")
testis <- drop_na(testis, any_of("Country"))
colnames(testis)[1] <- "Country_Code"


here::i_am("data/Melanoma.csv")
melanoma <- import( here("data/Melanoma.csv") )
dim(melanoma)

colnames(melanoma)[1] <- "Country"
colnames(melanoma)[4] <- "Uncertainty"
colnames(melanoma)[5] <- "Rate"
colnames(melanoma)[6] <- "ASR"
colnames(melanoma)[7] <- "Risk"

melanoma <- melanoma %>% select(-Population)
melanoma <- melanoma %>% select(-Number)
melanoma <- melanoma %>% select(-Uncertainty)
melanoma <- melanoma %>% select(-Risk)
melanoma <- melanoma %>% select(-Rate)

melanoma$Country <- countrycode(melanoma$Country, "country.name", "wb")
melanoma <- drop_na(melanoma, any_of("Country"))
colnames(melanoma)[1] <- "Country_Code"


here::i_am("data/Prostate.csv")
prostate <- import( here("data/Prostate.csv") )
dim(prostate)

colnames(prostate)[1] <- "Country"
colnames(prostate)[4] <- "Uncertainty"
colnames(prostate)[5] <- "Rate"
colnames(prostate)[6] <- "ASR"
colnames(prostate)[7] <- "Risk"

prostate <- prostate %>% select(-Population)
prostate <- prostate %>% select(-Number)
prostate <- prostate %>% select(-Uncertainty)
prostate <- prostate %>% select(-Risk)
prostate <- prostate %>% select(-Rate)

prostate$Country <- countrycode(prostate$Country, "country.name", "wb")
prostate <- drop_na(prostate, any_of("Country"))
colnames(prostate)[1] <- "Country_Code"


here::i_am("data/Thyroid.csv")
thyroid <- import( here("data/Thyroid.csv") )
dim(thyroid)

colnames(thyroid)[1] <- "Country"
colnames(thyroid)[4] <- "Uncertainty"
colnames(thyroid)[5] <- "Rate"
colnames(thyroid)[6] <- "ASR"
colnames(thyroid)[7] <- "Risk"

thyroid <- thyroid %>% select(-Population)
thyroid <- thyroid %>% select(-Number)
thyroid <- thyroid %>% select(-Uncertainty)
thyroid <- thyroid %>% select(-Risk)
thyroid <- thyroid %>% select(-Rate)

thyroid$Country <- countrycode(thyroid$Country, "country.name", "wb")
thyroid <- drop_na(thyroid, any_of("Country"))
colnames(thyroid)[1] <- "Country_Code"


here::i_am("data/Tobacco.csv")
tobacco <- import( here("data/Tobacco.csv") )
dim(tobacco)

colnames(tobacco)[1] <- "Country"
colnames(tobacco)[4] <- "Uncertainty"
colnames(tobacco)[5] <- "Rate"
colnames(tobacco)[6] <- "ASR"
colnames(tobacco)[7] <- "Risk"

tobacco <- tobacco %>% select(-Population)
tobacco <- tobacco %>% select(-Number)
tobacco <- tobacco %>% select(-Uncertainty)
tobacco <- tobacco %>% select(-Risk)
tobacco <- tobacco %>% select(-Rate)

tobacco$Country <- countrycode(tobacco$Country, "country.name", "wb")
tobacco <- drop_na(tobacco, any_of("Country"))
colnames(tobacco)[1] <- "Country_Code"

hpv_wealth <- merge(gdp_gni, hpv, by = "Country_Code")
testis_wealth <- merge(gdp_gni, testis, by = "Country_Code")
melanoma_wealth <- merge(gdp_gni, melanoma, by = "Country_Code")
prostate_wealth <- merge(gdp_gni, prostate, by = "Country_Code")
thyroid_wealth <- merge(gdp_gni, thyroid, by = "Country_Code")
tobacco_wealth <- merge(gdp_gni, tobacco, by = "Country_Code")

hpv_wealth <- cbind(hpv_wealth, hpv_wealth[,c(2)]) ##duplicating GDP
hpv_wealth <- cbind(hpv_wealth, hpv_wealth[,c(3)]) ##duplicating GNI
colnames(hpv_wealth)[7] <- "Log_GDP"
colnames(hpv_wealth)[8] <- "Log_GNI"
hpv_wealth$Log_GDP=log(hpv_wealth$Log_GDP)
hpv_wealth$Log_GNI=log(hpv_wealth$Log_GNI)



testis_wealth <- cbind(testis_wealth, testis_wealth[,c(2)]) ##duplicating GDP
testis_wealth <- cbind(testis_wealth, testis_wealth[,c(3)]) ##duplicating GNI
colnames(testis_wealth)[5] <- "Log_GDP"
colnames(testis_wealth)[6] <- "Log_GNI"
testis_wealth$Log_GDP=log(testis_wealth$Log_GDP)
testis_wealth$Log_GNI=log(testis_wealth$Log_GNI)

prostate_wealth <- cbind(prostate_wealth, prostate_wealth[,c(2)]) ##duplicating GDP
prostate_wealth <- cbind(prostate_wealth, prostate_wealth[,c(3)]) ##duplicating GNI
colnames(prostate_wealth)[5] <- "Log_GDP"
colnames(prostate_wealth)[6] <- "Log_GNI"
prostate_wealth$Log_GDP=log(prostate_wealth$Log_GDP)
prostate_wealth$Log_GNI=log(prostate_wealth$Log_GNI)


tobacco_wealth <- cbind(tobacco_wealth, tobacco_wealth[,c(2)]) ##duplicating GDP
tobacco_wealth <- cbind(tobacco_wealth, tobacco_wealth[,c(3)]) ##duplicating GNI
colnames(tobacco_wealth)[5] <- "Log_GDP"
colnames(tobacco_wealth)[6] <- "Log_GNI"
tobacco_wealth$Log_GDP=log(tobacco_wealth$Log_GDP)
tobacco_wealth$Log_GNI=log(tobacco_wealth$Log_GNI)


melanoma_wealth <- cbind(melanoma_wealth, melanoma_wealth[,c(2)]) ##duplicating GDP
melanoma_wealth <- cbind(melanoma_wealth, melanoma_wealth[,c(3)]) ##duplicating GNI
colnames(melanoma_wealth)[5] <- "Log_GDP"
colnames(melanoma_wealth)[6] <- "Log_GNI"
melanoma_wealth$Log_GDP=log(melanoma_wealth$Log_GDP)
melanoma_wealth$Log_GNI=log(melanoma_wealth$Log_GNI)


thyroid_wealth <- cbind(thyroid_wealth, thyroid_wealth[,c(2)]) ##duplicating GDP
thyroid_wealth <- cbind(thyroid_wealth, thyroid_wealth[,c(3)]) ##duplicating GNI
colnames(thyroid_wealth)[5] <- "Log_GDP"
colnames(thyroid_wealth)[6] <- "Log_GNI"
thyroid_wealth$Log_GDP=log(thyroid_wealth$Log_GDP)
thyroid_wealth$Log_GNI=log(thyroid_wealth$Log_GNI)


gdp_plot <- ggplot(gdp,aes(x=GDP))+ geom_histogram() + xlab("GDP per Capita") + 
  ylab("Number of Countries")+
  xlim(0,200000)+
  ylim(0,120)+
  scale_y_continuous(breaks = seq(0, 150, by = 10))+
  scale_x_continuous(breaks = seq(0, 200000, by = 25000))+
  ggtitle("GDP per Capita") +
  theme(
    plot.title = element_text(hjust = 0.5))
gdp_plot 

gni_plot <- ggplot(gni,aes(x=GNI))+ geom_histogram() + xlab("GNI per Capita") + 
  ylab("Number of Countries")+
  xlim(0,200000)+
  ylim(0,120)+
  scale_y_continuous(breaks = seq(0, 150, by = 10))+
  scale_x_continuous(breaks = seq(0,150000, by = 25000))+
  ggtitle("GNI per Capita") +
  theme(
    plot.title = element_text(hjust = 0.5))
gni_plot 

hpv_plot <- ggplot(hpv,aes(x= ASR_Combined))+ geom_boxplot() +
  xlab("Age Standardized Incidence Rate (per 100,000)") + 
  ggtitle("HPV-Related Cancer Incidence Rate") +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(hjust = 0.5))
hpv_plot

testis_plot <- ggplot(testis,aes(x= ASR))+ geom_boxplot() +
  xlab("Age Standardized Incidence Rate (per 100,000)") + 
  ggtitle("Testicular Cancer Incidence Rate") +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(hjust = 0.5))
testis_plot

melanoma_plot <- ggplot(melanoma,aes(x= ASR))+ geom_boxplot() +
  xlab("Age Standardized Incidence Rate (per 100,000)") + 
  ggtitle("Melanoma of Skin Incidence Rate") +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(hjust = 0.5))
melanoma_plot

prostate_plot <- ggplot(prostate,aes(x= ASR))+ geom_boxplot() +
  xlab("Age Standardized Incidence Rate (per 100,000)") + 
  ggtitle("Prostate Cancer Incidence Rate") +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(hjust = 0.5))
prostate_plot

thyroid_plot <- ggplot(thyroid,aes(x= ASR))+ geom_boxplot() +
  xlab("Age Standardized Incidence Rate (per 100,000)") + 
  ggtitle("Thyroid Cancer Incidence Rate") +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(hjust = 0.5))
thyroid_plot


tobacco_plot <- ggplot(tobacco,aes(x= ASR))+ geom_boxplot() +
  xlab("Age Standardized Incidence Rate (per 100,000)") + 
  ggtitle("Tobacco-Related Cancer Incidence Rate") +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(hjust = 0.5))
tobacco_plot


hpv_gdp <- ggplot(hpv_wealth, aes(x = Log_GDP, y = ASR_Combined)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GDP") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GDP: HPV-Related Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
hpv_gdp

hpv_gni <- ggplot(hpv_wealth, aes(x = Log_GNI, y = ASR_Combined)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GNI") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GNI: HPV-Related Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
hpv_gni

testis_gdp <- ggplot(testis_wealth, aes(x = Log_GDP, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GDP") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GDP: Testicular Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
testis_gdp

testis_gni <- ggplot(testis_wealth, aes(x = Log_GNI, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GNI") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GNI: Testicular Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
testis_gni

melanoma_gdp <- ggplot(melanoma_wealth, aes(x = Log_GDP, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GDP") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GDP: Melanoma of Skin") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
melanoma_gdp

melanoma_gni <- ggplot(melanoma_wealth, aes(x = Log_GNI, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GNI") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GNI: Melanoma of Skin") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
melanoma_gni

prostate_gdp <- ggplot(prostate_wealth, aes(x = Log_GDP, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GDP") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GDP: Prostate Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
prostate_gdp

prostate_gni <- ggplot(prostate_wealth, aes(x = Log_GNI, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GNI") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GNI: Prostate Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
prostate_gni

tobacco_gdp <- ggplot(tobacco_wealth, aes(x = Log_GDP, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GDP") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GDP: Tobacco-Related Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
tobacco_gdp

tobacco_gni <- ggplot(tobacco_wealth, aes(x = Log_GNI, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GNI") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GNI: Tobacco-Related Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
tobacco_gni

thyroid_gdp <- ggplot(thyroid_wealth, aes(x = Log_GDP, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GDP") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GDP: Thyroid Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
thyroid_gdp

thyroid_gni <- ggplot(thyroid_wealth, aes(x = Log_GNI, y = ASR)) + # Create ggplot2 plot
  geom_point() +
  xlab("Logged GNI") + 
  ylab("ASR") +
  ggtitle("ASR vs Logged GNI: Thyroid Cancer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")
thyroid_gni

hpv_regression <- lm(ASR_Combined ~ Log_GDP, data=hpv_wealth)
summary(hpv_regression)

hpv_regression2 <- lm(ASR_Combined ~ Log_GNI, data=hpv_wealth)
summary(hpv_regression2)


testis_regression <- lm(ASR ~ Log_GDP, data=testis_wealth)
summary(testis_regression)

testis_regression2 <- lm(ASR ~ Log_GNI, data=testis_wealth)
summary(testis_regression2)

melanoma_regression <- lm(ASR ~ Log_GDP, data=melanoma_wealth)
summary(melanoma_regression)

melanoma_regression2 <- lm(ASR ~ Log_GNI, data=melanoma_wealth)
summary(melanoma_regression2)

prostate_regression <- lm(ASR ~ Log_GDP, data=prostate_wealth)
summary(prostate_regression)

prostate_regression2 <- lm(ASR ~ Log_GNI, data=prostate_wealth)
summary(prostate_regression2)

tobacco_regression <- lm(ASR ~ Log_GDP, data=tobacco_wealth)
summary(tobacco_regression)

tobacco_regression2 <- lm(ASR ~ Log_GNI, data=tobacco_wealth)
summary(tobacco_regression2)

thyroid_regression <- lm(ASR ~ Log_GDP, data=thyroid_wealth)
summary(thyroid_regression)

thyroid_regression2 <- lm(ASR ~ Log_GNI, data=thyroid_wealth)
summary(thyroid_regression2)

##model1 = tab_model(hpv_regression, testis_regression, prostate_regression, 
##          melanoma_regression, tobacco_regression, thyroid_regression, show.std = TRUE,
 ##         dv.labels = c("HPV", "Testis", "Prostate",
  ##                      "Melanoma", "Tobacco", "Thyroid"))

model2 = tab_model(hpv_regression2, testis_regression2, prostate_regression2, 
          melanoma_regression2, tobacco_regression2, thyroid_regression2, show.std = TRUE,
          dv.labels = c("HPV", "Testis", "Prostate",
                        "Melanoma", "Tobacco", "Thyroid"))


print(fivenum(gdp$GDP))
print(fivenum(gni$GNI))
print(fivenum(hpv$ASR_Combined))
print(fivenum(testis$ASR))
print(fivenum(prostate$ASR))
print(fivenum(tobacco$ASR))
print(fivenum(melanoma$ASR))
print(fivenum(thyroid$ASR))


##Creating one large combined dataset
hpv_combine <- hpv_wealth %>% select(-ASR_Female,-ASR_Male,-GDP,-GNI) ##leaving only logged
hpv_combine <- hpv_combine[, c(1,3,4,2)]
testis_combine <- testis_wealth %>% select(-GDP,-GNI,-Log_GDP,-Log_GNI)
prostate_combine <- prostate_wealth %>% select(-GDP,-GNI,-Log_GDP,-Log_GNI)
melanoma_combine <- melanoma_wealth %>% select(-GDP,-GNI,-Log_GDP,-Log_GNI)
tobacco_combine <- tobacco_wealth %>% select(-GDP,-GNI,-Log_GDP,-Log_GNI)
thyroid_combine <- thyroid_wealth %>% select(-GDP,-GNI,-Log_GDP,-Log_GNI)


combined_gdp <- merge(hpv_combine, testis_combine, by = "Country_Code")
colnames(combined_gdp)[4] <- "HPV_ASR"
colnames(combined_gdp)[5] <- "Testis_ASR"

combined_gdp <- merge(combined_gdp, prostate_combine, by = "Country_Code")
colnames(combined_gdp)[6] <- "Prostate_ASR"

combined_gdp <- merge(combined_gdp, melanoma_combine, by = "Country_Code")
colnames(combined_gdp)[7] <- "Melanoma_ASR"

combined_gdp <- merge(combined_gdp, tobacco_combine, by = "Country_Code")
colnames(combined_gdp)[8] <- "Tobacco_ASR"

combined_gdp <- merge(combined_gdp, thyroid_combine, by = "Country_Code")
colnames(combined_gdp)[9] <- "Thyroid_ASR"

plot_gdp <- data.frame(x = combined_gdp$Log_GDP,           # Reshape data frame
                       y = c(combined_gdp$HPV_ASR, 
                             combined_gdp$Testis_ASR, 
                             combined_gdp$Prostate_ASR, 
                             combined_gdp$Melanoma_ASR, 
                             combined_gdp$Tobacco_ASR, 
                             combined_gdp$Thyroid_ASR),
                       group = c(rep("HPV_ASR", nrow(combined_gdp)),
                                 rep("Testis_ASR", nrow(combined_gdp)),
                                 rep("Prostate_ASR", nrow(combined_gdp)),
                                 rep("Melanoma_ASR", nrow(combined_gdp)),
                                 rep("Tobacco_ASR", nrow(combined_gdp)),
                                 rep("Thyroid_ASR", nrow(combined_gdp))))

plot_gdp$y=log(plot_gdp$y)

gdp_combined_plot <- ggplot(plot_gdp, aes(x, y, col = group)) +      # Create ggplot2 plot
  geom_point(size = .75) +
  xlab("Logged GDP") + 
  ylab("Logged ASR") +
  ggtitle("Logged ASR vs Logged GDP for all Cancer Types") +
  ## ylim(0,25)+
  theme(plot.title = element_text(hjust = 0.5)) 
  
gdp_combined_plot + scale_fill_discrete(name = "Cancer Site", 
                                        labels = c ("HPV", "Melanoma", 
                                                    "Prostate", "Testis",
                                                    "Thyroid", "Tobacco" ))


gdp_combined_plot


coeftest(hpv_regression, vcov = vcovHC(hpv_regression))