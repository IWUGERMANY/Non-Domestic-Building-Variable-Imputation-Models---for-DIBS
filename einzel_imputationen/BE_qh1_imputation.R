# BE_qh1_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# qh1: Gibt es zentrale raumlufttechnische Anlagen zur mechanischen Lüftung von Nutzungsflächen im Gebäude?
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, qh1!=-7) # wenn qh1 = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

qh1 <- DB_BE_clean$qh1
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# # TESTEN
# DB_BE_clean <- subset(DB_BE, qh1=="3") 
# DB_BE_clean <- DB_BE
# sum(DB_BE_clean$HRF) 


    # Counting number of imputations in this variable and marking them for later analysis
    qh1_imputated <- DB_BE$qh1 == -7
    table <- table(qh1_imputated)
    qh1_n_imputated <- table[2]
    
    qh1_imputated[qh1_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 qh1 auf Basis multinominal logistc regression multinom() of the nnet package
# Alle weitern Zuweisungen zu Variablen, welche auf zur RLT gehören werden nur im Fall von qh1==3 imputiert! Damit konsistent.

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$qh1 <- relevel(as.factor(DB_BE_clean$qh1), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

# Reg <- multinom(qh1 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 5517.446 # NAs
# Reg <- multinom(qh1 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 6195.049 #NAs
Reg <- multinom(qh1 ~ as.factor(hk_geb) : as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 5517.071 # NAs not relevant for predict of missing values

# Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qh1__existance_of_central_ventilation_system.txt")
print(summary(Reg))
sink()  # returns output to the console
summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qh1__existance_of_central_ventilation_system.rds")

# mlogit() Multinomial logistic regression
# multinom function of the package nnet https://stats.stackexchange.com/questions/175782/how-to-perform-a-logistic-regression-for-more-than-2-response-classes-in-r


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs")

predicted.probs <- predict(Reg,newdata=DB_BE, type="probs")


# THIRD for each row overwrite the -7 of qh1 by a dice the has the 
#       levels_qh1 (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_qh1 <- levels(DB_BE_clean$qh1)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$qh1[i]==-7){
    DB_BE$qh1[i] <- sample(levels_qh1, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3]))
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

qh1 <- DB_BE$qh1
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, qh1, qh1_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_qh1_imputiert.csv')

