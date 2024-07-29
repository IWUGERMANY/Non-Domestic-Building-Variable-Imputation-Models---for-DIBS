# BE_qf1_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# qf1: Bezogen auf die Nutzungsfläche des Gebäudes - die überwiegende Beleuchtungsart
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & fen_fl>=0 & q66a_1>=0 & qf1>=0)
####

hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
fen_fl <- DB_BE_clean$fen_fl
qf1 <- DB_BE_clean$qf1
q66a_1 <- DB_BE_clean$q66a_1


N_Clean <- nrow(DB_BE_clean)
Sum_HRF_BE_clean <- sum(HRF)
HRF <- DB_BE_clean$HRF

# min(fen_fl)
# min(hk_geb)
# min(qf1)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem qf1 = -7
DB_BE_unclean_qf1 <- subset(DB_BE, hk_geb>=0 & fen_fl>=0 & q66a_1>=0 & qf1==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    qf1_imputated <- DB_BE$qf1 == -7
    table <- table(qf1_imputated)
    qf1_n_imputated <- table[2]
    
    qf1_imputated[qf1_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt



#............................................................................
#
# 2
#Auswertung####
#
#............................................................................


#----------------------------------------
# 2.3 qf1 auf Basis multinominal logistc regression multinom() of the nnet package

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$qf1 <- relevel(as.factor(DB_BE_clean$qf1), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model
#Reg <- multinom(qf1 ~ as.factor(hk_geb) + q66a_1, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 3067.534
#Reg <- multinom(qf1 ~ as.factor(uk_geb) + q66a_1, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 3046.299 
Reg <- multinom(qf1 ~ as.factor(hk_geb) * q66a_1, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 3027.571

# Checking the model

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qf1_average_type_of_lighting.txt")
print(summary(Reg))
sink()  # returns output to the console
#summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qf1_average_type_of_lighting.rds")

# mlogit() Multinomial logistic regression
# multinom function of the package nnet https://stats.stackexchange.com/questions/175782/how-to-perform-a-logistic-regression-for-more-than-2-response-classes-in-r


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs")

predicted.probs <- predict(Reg,newdata=DB_BE, type="probs")

# THIRD for each row overwrite the -7 of qf1 by a dice the has the 
#       levels_qf1 (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_qf1 <- levels(DB_BE_clean$qf1)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$qf1[i]==-7){
    DB_BE$qf1[i] <- sample(levels_qf1, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3]))
  }
}

#table(DB_BE$qf1)


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

qf1 <- DB_BE$qf1
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, qf1, qf1_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_qf1_imputiert.csv')
