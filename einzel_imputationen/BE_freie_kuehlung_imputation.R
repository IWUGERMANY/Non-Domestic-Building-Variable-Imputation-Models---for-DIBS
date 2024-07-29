# BE_freie_kuehlung_imputation.R
# Julian Bischof
# 29.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# freie_kuehlung: night_flushing_flow
#-----------------------------------------

# freie_kuehlung ist unabhängig von qh1==3

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, freie_kuehlung!=-7) # wenn freie_kuehlung = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

freie_kuehlung <- DB_BE_clean$freie_kuehlung
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
f_ant_gekuehlt <- DB_BE_clean$f_ant_gekuehlt

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


    # Counting number of imputations in this variable and marking them for later analysis
    freie_kuehlung_imputated <- DB_BE$freie_kuehlung == -7
    table <- table(freie_kuehlung_imputated)
    freie_kuehlung_n_imputated <- table[2]
    
    freie_kuehlung_imputated[freie_kuehlung_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt



#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 freie_kuehlung auf Basis multinominal logistc regression multinom() of the nnet package
# Alle weitern Zuweisungen zu Variablen, welche auf zur RLT gehören werden nur im Fall von freie_kuehlung==3 imputiert! Damit konsistent.

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$freie_kuehlung <- relevel(as.factor(DB_BE_clean$freie_kuehlung), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

# Reg <- multinom(freie_kuehlung ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 4681.792 # NAs
Reg <- multinom(freie_kuehlung ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 5027.404  #
# Reg <- multinom(freie_kuehlung ~ as.factor(hk_geb) : as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 4681.791 # NAs
# Reg <- multinom(freie_kuehlung ~ as.factor(hk_geb) : as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 5027.389 # NAs
# Reg <- multinom(freie_kuehlung ~ as.factor(hk_geb) : as.factor(bak) : f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 5594.713 # NAs
# Reg <- multinom(freie_kuehlung ~ as.factor(hk_geb) : f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 5288.847 # NAs
# Reg <- multinom(freie_kuehlung ~ as.factor(bak) : f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 5278.535 # NAs
# Reg <- multinom(freie_kuehlung ~ f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 5265.136 # NAs

# Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/freie_kuehlung.txt")
print(summary(Reg))
sink()  # returns output to the console
# summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/freie_kuehlung.rds")

# mlogit() Multinomial logistic regression
# multinom function of the package nnet https://stats.stackexchange.com/questions/175782/how-to-perform-a-logistic-regression-for-more-than-2-response-classes-in-r


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of freie_kuehlung by a dice the has the 
#       levels_freie_kuehlung (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_freie_kuehlung <- levels(DB_BE_clean$freie_kuehlung)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$freie_kuehlung[i]==-7){
    DB_BE$freie_kuehlung[i] <- sample(levels_freie_kuehlung, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4]))
  }
}



#
#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

freie_kuehlung <- DB_BE$freie_kuehlung
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, freie_kuehlung, freie_kuehlung_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_freie_kuehlung_imputiert.csv')

