install.packages("lattice")
install.packages("xts")
install.packages("XLS")
install.packages("mFilter")
install.packages("readxl")
install.packages("zoo")      
install.packages("chron")
install.packages("mFilter")
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("WDI")
install.packages("nortest")  # pour ad.test
install.packages("moments")  # pour jarque.bera.test
library(nortest)
library(moments)
library(lattice)
library(tsDyn)
library(readxl)
library(xts)
library(readxl)
library(astsa)
library(ggfortify)
library(forecast)
library(fpp2)
library(tsDyn)
library(vars)
library(seasonal)
library(urca)
library(ggplot2)
library(devtools)
library(taceconomics)
library(reshape2)
library(tseries)
library(caret)
library(randomForest)
library(xgboost)
library(mFilter)
library(ggplot2)
library(vars)
library(VAR.etp)
library(stargazer)
library(ggplot2)
library(ggfortify)
library(tseries)
library(xts)
library(zoo)
library(chron)
library(tsDyn)
library(readxl)
library(tidyverse)
library(xts)
library(readxl)
library(astsa)
library(ggfortify)
library(forecast)
library(fpp2)
library(tsDyn)
library(ggfortify)
library(vars)
library(vars)
library(seasonal)
library(urca)
library(ggplot2)
library(devtools)
library(taceconomics)
library(reshape2)
library(tseries)
library(caret)
library(randomForest)
library(xgboost)
library(taceconomics)
taceconomics.apikey("sk_cTXLlGweAfEINW9AOz3-swpITf8I1R_b07F-Yjg81xY")
library(mFilter)
library(ggplot2)
library(vars)
library(VAR.etp)
library(stargazer)
library(ggplot2)
library(ggfortify)
library(tseries)

# Charger le package
library(httr)
library(jsonlite)
library(readxl)

library(WDI)

# 1. Importation Excel
data <- read_xlsx("base.xlsx")
data <- data[ , -(3:4)]
data <- data[-(25:26), ]      #)
data$date <- as.Date(data$date)




#Taux de croissance de l’allemagne : 

# PIB Allemagne (code : NY.GDP.MKTP.CD = PIB nominal en USD)
pib_de <- WDI(country = "DE", indicator = "NY.GDP.MKTP.CD", start = 1999, end = 2023)

# Afficher les premières lignes
head(pib_de)

# Optionnel : transformer en xts
library(xts)
pib_de_xts <- xts(pib_de$NY.GDP.MKTP.CD, order.by = as.Date(paste0(pib_de$year, "-01-01")))
head(pib_de_xts)

#  Calcul du taux de croissance (variation en % d'une année sur l'autre)
taux_croissance <- 100 * (pib_de_xts / lag(pib_de_xts, 1) - 1)


#  Supprimer la première ligne (NA)
taux_croissance <- taux_croissance[-1, ]
colnames(taux_croissance)= "tx_al"

plot(taux_croissance, main = " Tx de Croissance de l'allemagne", col = "red")


# ----- APIs -----
# 2. Ratio dette/PIB (xts déjà, ou à convertir)
ratio_d <- getdata("FSI/FSHG_PT_A/FRA")["2000-01-01/2023-01-01"]
ratio_d <- xts(as.numeric(ratio_d), order.by = seq(as.Date("2000-01-01"), as.Date("2023-01-01"), by = "year"))
colnames(ratio_d) <- "ratio_d"

# 3. Taux de croissance PIB
gdp <- getdata("UNCTAD/GDPCON_0/FRA")
tc_pibf <- diff(gdp) / lag(gdp, 1) * 100
tc_pibf <- tc_pibf["2000-01-01/2023-01-01"]
# Refaire l'index car diff() décale (vérifie les longueurs)
tc_pibf <- xts(as.numeric(tc_pibf), order.by = seq(as.Date("2000-01-01"), as.Date("2023-01-01"), by = "year"))
colnames(tc_pibf) <- "tc_pib"


#taux de croissance france-allemagne
tc_pib= tc_pibf-taux_croissance

plot(tc_pib)


# 4. Transformation Excel en xts aligné sur même index
data_xts <- xts(data[ , -which(names(data) == "date")], order.by = data$date)





# ----- MERGE SÉRIES -----
df <- merge(data_xts, tc_pib, ratio_d)
# ----- PLOTS -----
plot(df$spread, main = "Spread", col = "blue")

plot(df$tc_pib, main = "Taux de croissance du PIB france-allemagne", col = "red")
plot(df$ratio_d, main = "Ratio dette/PIB", col = "green")

if("vstoxx" %in% colnames(df)) plot(df$vstoxx, main = "VSTOXX", col = "purple")



# Supposons que df est ton objet xts ou zoo
df <- data.frame(date = index(df), coredata(df))
head(df)


spread = xts(df$spread, order.by = df$date)
tc_pib = xts(df$tc_pib, order.by = df$date)
ratio_d = xts(df$ratio_d, order.by = df$date)
vstoxx = xts(df$vstoxx, order.by = df$date)


#SPREAD

acf2(spread)

#On voit que le corrélogramme diminue lentement donc on peut supooser qu il y a une tendance. 
#Mais on ne peut dire s il s agit d une tendance stochastique ou d une tendance déterministe.
#Pour cela faisons le test augmenté de Dickey Fuller pour voir s il y a une racine unité. 

#Test de Dickey Fuller

#modele 3


summary(ur.df(spread, type = "trend", selectlags = "AIC"))


#Puisque la statistique de test (-1.135) est supérieure au valeurs critiques à 5% (-4.38) et 10% (-3.13),
#on ne peut pas rejeter l hypothèse nulle de non-stationnarité.donc la serie n'est pas stationnaire

#Modèle 2


summary(ur.df(spread, type = "drift", selectlags = "AIC"))


#Puisque la statistique de test (-1.6) est supérieure au valeurs critiques à 5% (-3.00) et 10% (-2.57),
#on ne peut pas rejeter l hypothèse nulle de non-stationnarité.


#Modèle 1


summary(ur.df(spread, type = "none", selectlags = "AIC"))

#Puisque la staspreadPuisque la statistique de test (-0.9565) est supérieure au valeurs critiques à 5% (-1.95) et 10% (-1.62),
#on ne peut pas rejeter l hypothèse nulle de non-stationnarité.

#On va donc différencier notre série.


#Stationnarisation de la série à faire après les tests économétriques

#Différenciation en moyenne pour faire disparaître la tendance

spread_diff = diff(spread)

plot(spread_diff)


#Test ADF sur les données stationnaires

spread_diff2 = spread_diff[-1,]

plot(spread_diff2,
     main = "Série stationnarisée du spread entre 2000 et 2024")

#Refaisons le test ADF sur la série différenciée pour s assurer
#de sa stationnarité après la différenciation première


#test kpss

kpss.test(spread_diff2)

#H₀ (hypothèse nulle) : La série est stationnaire

#H₁ : La série est non stationnaire (contient une racine unitaire)

#verification avec le Corrélogramme de la serie stationnarisé

# on a p-value >0.5 donc la série est stationnaire

acf2(spread_diff2)


#ACF  MA(1)
#PACF → AR(1)
#d=1



#tc_pib( taux de croissance du pib)

plot(tc_pib, main="Taux de croissance du PIB", col="red")

#la serie est stationnaire d'apres le graphique 

acf2(tc_pib)

#D'apres le corré la serie est stationnaire

#Test de Dickey Fuller

#modele 3


summary(ur.df(tc_pib, type = "trend", selectlags = "AIC"))


#Puisque la statistique de test (-4.9) est inférieur aux valeurs critiques à 5% (-3.60) et 10% (-3.24),
#on rejete l hypothèse nulle de non-stationnarité.donc la serie est stationnaire

#Modèle 2


summary(ur.df(tc_pib, type = "drift", selectlags = "AIC"))


#la serie stationnaire


#Modèle 1


summary(ur.df(tc_pib, type = "none", selectlags = "AIC"))

#la série est stationnaire


#ratio_d (ratio dette/pib)

plot(ratio_d, main="Ratio dette/PIB", col="green")

acf2(ratio_d)

#On voit que le corrélogramme diminue lentement donc on peut supooser qu il y a une tendance. 
#Mais on ne peut dire s il s agit d une tendance stochastique ou d une tendance déterministe.
#Pour cela faisons le test augmenté de Dickey Fuller pour voir s il y a une racine unité. 

#Test de Dickey Fuller

#modele 3


summary(ur.df(ratio_d, type = "trend", selectlags = "AIC"))


#Puisque la statistique de test (-0.8) est supérieure au valeurs critiques à 5% (-3.60)
#on ne peut pas rejeter l hypothèse nulle de non-stationnarité.donc la serie n'est pas stationnaire

#Modèle 2


summary(ur.df(ratio_d, type = "drift", selectlags = "AIC"))


#Puisque la statistique de test (-1.88) est supérieure au valeurs critiques à 5% (-3.00) 
#on ne peut pas rejeter l hypothèse nulle de non-stationnarité.


#Modèle 1


summary(ur.df(ratio_d, type = "none", selectlags = "AIC"))

#Puisque la staspreadPuisque la statistique de test (1.2) est supérieure au valeurs critiques à 5% (-1.95) et 10% (-1.6)
#on ne peut pas rejeter l hypothèse nulle de non-stationnarité.

#On va donc différencier notre série.


#Stationnarisation de la série à faire après les tests économétriques

#Différenciation en moyenne pour faire disparaître la tendance

ratio_diff = diff(ratio_d)

plot(ratio_diff)


#Test ADF sur les données stationnaires

ratio_diff2 = ratio_diff[-1,]

plot(ratio_diff2,
     main = "Série stationnarisée du spread entre 2000 et 2024")

#Refaisons le test ADF sur la série différenciée pour s assurer
#de sa stationnarité après la différenciation première

kpss.test(ratio_diff2)


#pvalue>0.05 donc la serie est stationnaire

acf2(ratio_diff2)


#Vstoxxx
plot(vstoxx, main="VSTOXX", col="purple")


#la serie est stationnaire d'apres le graphique 

acf2(vstoxx)

#D'apres le corré la serie est stationnaire

#Test de Dickey Fuller

#modele 3


summary(ur.df(vstoxx, type = "trend", selectlags = "AIC"))


#Puisque la statistique de test (-3.16) est sup aux valeurs critiques à 5% (-3.60) et 10% (-3.24),
#on ne peut rejeter l hypothèse nulle de non-stationnarité.donc la serie est non stationnaire

#Modèle 2


summary(ur.df(vstoxx, type = "drift", selectlags = "AIC"))


#la serie non stationnaire


#Modèle 1


summary(ur.df(vstoxx, type = "none", selectlags = "AIC"))

#la série est non stationnaire


#adftest
adf.test(vstoxx)

#Puisque p-value (0.02) < 0.05, on rejette H₀ La série VSTOXX est STATIONNAIRE


#kpss 
kpss.test(vstoxx)

#D'apres le kpss la serie est stationnaire

# test de selection des retars 

# Sélection automatique du retard optimal
#ici ca sera 1



# Préparation données stationnaires
df$spread_diff <- c(NA, diff(df$spread))
df$ratio_diff <- c(NA, diff(df$ratio_d))
df_svar <- df[-1, ]

# Variables ordonnées des plus exogènes aux moins exogene
df_ts <- ts(df_svar[, c("vstoxx", "ratio_diff", "tc_pib", "spread_diff")], start = 2001, frequency = 1)

# Estimation VAR avec 1 retard
var_model <- VAR(df_ts, p = 1, type = "const")

# test du portemanteau H0 modèle bien spécifié
ser11 <- serial.test(var_model, lags.pt = 1, type = "PT.asymptotic")
ser11$serial
#p-value < 2.2e-16 → On rejette H₀
 #donc rejet de Ho et modèle mal spécifié 

# test Breusch-Godfrey H0 modèle bien spécifié
ser12 <- serial.test(var_model, lags.bg = 1, type = "BG" )
ser12
#p-value = 0.1395



#JB test H0 loi normale
library(tseries)
# Extraire les résidus du modèle VAR
residuals_var <- residuals(var_model)

# Tester la normalité sur chaque série de résidus
jb_test_results <- apply(residuals_var, 2, jarque.bera.test)

# Afficher les résultats
jb_test_results





# Matrice A pour SVAR (ordre: vstoxx -> ratio_diff -> tc_pib -> spread_diff)
Amat <- diag(4)
Amat[2,1] <- NA   # vstoxx -> ratio_dette
Amat[3,1] <- NA   # vstoxx -> tc_pib
Amat[3,2] <- NA   # ratio_dette -> tc_pib
Amat[4,1] <- NA   # vstoxx -> spread
Amat[4,2] <- NA   # ratio_dette -> spread
Amat[4,3] <- NA   # tc_pib -> spread

# Estimation SVAR 
svar_model <- SVAR(var_model, Amat = Amat)

# Fonctions de réponse impulsionnelle
irf_svar <- irf(svar_model, n.ahead = 8, ci = 0.95)
plot(irf_svar)


# Prévisions 2024-2026
var_forecast <- predict(var_model, n.ahead = 3, ci = 0.95)
par(mar = c(2, 2, 2, 2))  # marges plus petites : bas, gauche, haut, droite







# Graphique des prévisions
plot(var_forecast)

# Prévisions en niveaux
last_spread <- tail(df$spread, 1)
last_ratio_d <- tail(df$ratio_d, 1)

spread_diff_forecasts <- var_forecast$fcst$spread_diff[, "fcst"]
spread_level_forecasts <- as.numeric(last_spread) + cumsum(spread_diff_forecasts)

ratio_diff_forecasts <- var_forecast$fcst$ratio_diff[, "fcst"]
ratio_level_forecasts <- as.numeric(last_ratio_d) + cumsum(ratio_diff_forecasts)

# Résultats prévisions
forecasts_df <- data.frame(
  Année = 2024:2026,
  Spread = spread_level_forecasts,
  Ratio_Dette = ratio_level_forecasts,
  TC_PIB = var_forecast$fcst$tc_pib[, "fcst"],
  VSTOXX = var_forecast$fcst$vstoxx[, "fcst"]
)

print(forecasts_df)
library(ggplot2)


# Prévisions pour le spread 2024-2026
forecast_spread <- data.frame(
  Year = 2024:2026,
  Spread = spread_level_forecasts,
  Lower = spread_level_forecasts - 0.15,
  Upper = spread_level_forecasts + 0.15
)




# BACKTESTING 2023 ET PRÉVISIONS 2024-2026 AVEC VRAIES VALEURS


# Suppose que df_ts commence en 2000 et va jusqu'à 2023
df_ts_train <- window(df_ts, end = c(2022, 1))
var_train <- VAR(df_ts_train, p = 1, type = "const")
forecast_train <- predict(var_train, n.ahead = 1, ci = 0.95)
spread_diff_pred_2023 <- forecast_train$fcst$spread_diff[,"fcst"]
spread_2022 <- as.numeric(df$spread[df$date == as.Date("2022-01-01")])
spread_pred_2023 <- spread_2022 + spread_diff_pred_2023
spread_real_2023 <- as.numeric(df$spread[df$date == as.Date("2023-01-01")])
print(paste("Prévision VAR pour 2023:", round(spread_pred_2023, 2)))
print(paste("Valeur réelle du spread en 2023:", round(spread_real_2023, 2)))

library(ggplot2)

df_bt <- data.frame(
  Year = 2022:2023,
  Spread = c(spread_2022, spread_pred_2023),
  Type = c("Observé", "Prévision")
)
df_bt$Spread_Reel <- c(NA, spread_real_2023)

ggplot(df_bt, aes(x = Year, y = Spread)) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_point(size = 3, color = "darkred") +
  geom_point(aes(y = Spread_Reel), color = "black", size = 4, shape = 18, na.rm = TRUE) +
  geom_text(aes(label = sprintf("%.2f%%", Spread)), 
            vjust = -1, color = "darkred", size = 4, fontface = "bold") +
  geom_text(aes(y = Spread_Reel, label = sprintf("%.2f%%", Spread_Reel)), 
            color = "black", size = 4, fontface = "bold", na.rm = TRUE, vjust = 1.2) +
  labs(
    title = "Backtest du Spread OAT 10 ans pour 2023",
    subtitle = "Prévision SVAR sur la base 2022 et comparaison avec la valeur réelle IC[0.8;3.30]",
    x = "Année",
    y = "Spread (%)"
  ) +
  scale_x_continuous(breaks = 2022:2023) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12)
  )

# 1. Récupère les bornes de l'IC pour spread_diff prédit en 2023
ic_lower_2023 <- forecast_train$fcst$spread_diff[, "lower"]
ic_upper_2023 <- forecast_train$fcst$spread_diff[, "upper"]


# La prévision en niveau pour chaque borne
spread_pred_2023_lower <- spread_2022 + ic_lower_2023
spread_pred_2023_upper <- spread_2022 + ic_upper_2023

print(sprintf("IC 95%% du spread 2023 prédit : [%.2f ; %.2f]", spread_pred_2023_lower, spread_pred_2023_upper))
print(sprintf("Observation réelle : %.2f", spread_real_2023))




# Observée 2023
spread_2023 <- as.numeric(last_spread)

# Prévisions en niveau avec vrai IC du VAR
spread_pred_2024 <- spread_2023 + var_forecast$fcst$spread_diff[1, "fcst"]
spread_pred_2025 <- spread_pred_2024 + var_forecast$fcst$spread_diff[2, "fcst"]
spread_pred_2026 <- spread_pred_2025 + var_forecast$fcst$spread_diff[3, "fcst"]

spread_2024_lower <- spread_2023 + var_forecast$fcst$spread_diff[1, "lower"]
spread_2024_upper <- spread_2023 + var_forecast$fcst$spread_diff[1, "upper"]
spread_2025_lower <- spread_2024_lower + var_forecast$fcst$spread_diff[2, "lower"]
spread_2025_upper <- spread_2024_upper + var_forecast$fcst$spread_diff[2, "upper"]
spread_2026_lower <- spread_2025_lower + var_forecast$fcst$spread_diff[3, "lower"]
spread_2026_upper <- spread_2025_upper + var_forecast$fcst$spread_diff[3, "upper"]

# Valeur réelle observée pour 2024
spread_2024_reel <- 3.25

# Data frame principal
df_graph <- data.frame(
  Year = c(2023, 2024, 2025, 2026),
  Spread = c(spread_2023, spread_pred_2024, spread_pred_2025, spread_pred_2026),
  Lower = c(NA, spread_2024_lower, spread_2025_lower, spread_2026_lower),
  Upper = c(NA, spread_2024_upper, spread_2025_upper, spread_2026_upper),
  Type = c("Historique", "Prévision", "Prévision", "Prévision")
)

# Data frame pour la valeur réelle
df_reel <- data.frame(
  Year = 2024,
  Spread = spread_2024_reel,
  Type = "Observation réelle"
)

library(ggplot2)
ggplot(df_graph, aes(x = Year, y = Spread)) +
  geom_ribbon(
    data = subset(df_graph, !is.na(Lower)),
    aes(ymin = Lower, ymax = Upper),
    fill = "blue", alpha = 0.3
  ) +
  geom_line(data = subset(df_graph, Type == "Prévision"), color = "red", linewidth = 1.2) +
  geom_point(data = subset(df_graph, Type == "Historique"), color = "black", size = 4, shape = 18) +
  geom_point(data = subset(df_graph, Type == "Prévision"), color = "darkred", size = 3) +
  geom_point(data = df_reel, color = "darkgreen", size = 4, shape = 17) +
  geom_text(
    aes(label = sprintf("%.2f%%", Spread)),
    vjust = -1, color = "darkred", size = 4, fontface = "bold",
    data = subset(df_graph, Type == "Prévision")
  ) +
  geom_text(
    aes(label = sprintf("%.2f%%", Spread)),
    vjust = -1, color = "black", size = 4, fontface = "bold",
    data = subset(df_graph, Type == "Historique")
  ) +
  geom_text(
    aes(label = sprintf("%.2f%%", Spread)),
    vjust = -1, color = "darkgreen", size = 4, fontface = "bold",
    data = df_reel
  ) +
  labs(
    title = "Prévision et observation du Spread OAT 10 ans français",
    subtitle = "Historique 2023, prévision et observation réelle 2024, prévisions 2025-2026 (IC 95%)",
    x = "Année",
    y = "Spread (%)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 2023:2026) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12)
  )


#les bandes bleues sur le graphique correspondent exactement à l’incertitude 
#calculée par le modèle, et non à une valeur arbitraire.
