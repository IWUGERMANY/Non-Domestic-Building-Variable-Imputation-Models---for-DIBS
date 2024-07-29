# BE_qh3_6_imputation.R
# Julian Bischof
# 26.07.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# qh3_6: überwiegende Funktionen der RLTs Abluft
#-----------------------------------------

#-----------------
# qh3_1 bis qh3_9
# -7 nur aus Teilmenge RLT vorhanden qH1 = 3 (Ja, zentrale Anlage(n) vorhanden) imputieren! Auf basis von qh1 (siehe oben). 
# Nur Anlagen welche qh1 = 3 sind bekommen auch hier Eigenschaften zugewiesen!
#
# Fälle identifizieren, welche qh1==3 und qh3_1 bis qh3_9==-8. Diese Anlagen müssen zu qh3_1 bis qh3_9==-7 abgeändert werden, 
# da es sich bei diesen Gebäude um diejenigen handelt, welche unter qh1 eine 3 zugewürfelt bekommen haben. In qh3_1 bis qh3_9==-8
# waren diese, da vorher qh1==-7 war konsequent als -8 eingetragen. In der qh3_1 bis qh3_9==-7 Imputation müssen diese nun aber
# Berücksichtig werden, um Konsistenz inerhalb eines Gebäudedatensatzes zu gewährleisten.
#-----------------


#............................................................................
#
# 0
#qh3_1 bis qh3_9: -8 zu -7 für Fälle, welche nun über qh1 eine RLT zugewiesen bekommen haben####
#
#............................................................................

DB_BE$qh3_6[DB_BE$qh3_6==-8 & DB_BE$qh1==3] <- -7




#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, qh3_6!=-7 & qh3_6!=-8) # wenn qh3_6 = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

qh3_6 <- DB_BE_clean$qh3_6
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem XXX = -7
DB_BE_unclean_qh3_6 <- subset(DB_BE, hk_geb>=0 & bak_grob>=0 & qh3_6==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    qh3_6_imputated <- DB_BE$qh3_6 == -7
    table <- table(qh3_6_imputated)
    qh3_6_n_imputated <- table[2]
    
    qh3_6_imputated[qh3_6_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$qh3_6 <- relevel(as.factor(DB_BE_clean$qh3_6), ref = "1")


#----------------------------------------
# 2.1 Regressionsmodel bak_clean und hk_geb_clean zu qh3_6_clean

# Reg <- glm(qh3_6 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, family=binomial) # Algorithmus konvergierte nicht  # NAs
# Reg <- glm(qh3_6 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial) # AIC: 1166 #NAs
# Reg <- glm(qh3_6 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # AIC: 1166 #NAs
Reg <- glm(qh3_6 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=quasibinomial, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # AIC:  #NAs

# Reg <- glm(qh3_6 ~ as.factor(hk_geb) : as.factor(bak), data = DB_BE_clean, family=binomial) # Algorithmus konvergierte nicht # NAs

#glm() binomial model without weights for testing best option of predictors, than with weights (binomial) for AIC and than (quasibinomial) for prediction model, as weights are non-integer values!!!! 
#https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qh3_6.txt")
print("Also the regression equation contains NAs, it has been used for the imputation, as these combinations do not exist in the DataNWG BRE data and therfore do not affect the prediction")
print("-7 nur aus Teilmenge RLT vorhanden qH1 = 3 (Ja, zentrale Anlage(n) vorhanden) imputieren! Auf basis von qh1 (siehe oben). Nur Anlagen welche qh1 = 3 sind bekommen auch hier Eigenschaften zugewiesen! Fälle identifizieren, welche qh1==3 und qh3_1 bis qh3_9==-8. Diese Anlagen müssen zu qh3_1 bis qh3_9==-7 abgeändert werden, da es sich bei diesen Gebäude um diejenigen handelt, welche unter qh1 eine 3 zugewürfelt bekommen haben. In qh3_1 bis qh3_9==-8 waren diese, da vorher qh1==-7 war konsequent als -8 eingetragen. In der qh3_1 bis qh3_9==-7 Imputation müssen diese nun aber Berücksichtig werden, um Konsistenz inerhalb eines Gebäudedatensatzes zu gewährleisten.")
print(summary(Reg))
sink()  # returns output to the console

#summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qh3_6.rds")

# model_equation(Reg)


# SECOND 13.2.2 Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = response)! P321 
#####################################################
#Add a probabilit of beeing 1 (TRUE) to DB_BE -->

#DB_BE$prob_qh3_6_1 <- predict(Reg, newdata=DB_BE, type="response") 
prob_qh3_6_1 <- predict(Reg, newdata=DB_BE, type="response") 

# THIRD get subset of missing qh3_6 (or do above with unclean data set and use here onwards)
#####################################################
#Generieren von Un-Clean Subset d.h. das Subset in welchem XXX = -7
DB_BE_unclean_qh3_6 <- subset(DB_BE, hk_geb>=0 & bak_grob>=0 & qh3_6==-7)

# FORUTH for each row overwrite the -7 of dachform by a dice the has the 
#        Ausprägungen (1 und 0) mit der 
#       Frequency zu 1 = prob (siehe oben) und zu 2 = 1 - prob
#####################################################
#Damit wird für jede einzelen Zeile mit einem individuellen Würfel gewürfelt
levels_qh3_6 <- levels(DB_BE_clean$qh3_6)
rows_DB_BE <- nrow(DB_BE)  

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$qh3_6[i]==-7){
    DB_BE$qh3_6[i] <- sample(levels_qh3_6, size = 1, replace = TRUE, prob = c(prob_qh3_6_1[i],1-prob_qh3_6_1[i]))
    #DB_BE$qh3_6[i] <- sample(levels_qh3_6, size = 1, replace = TRUE, prob = c(DB_BE$prob_qh3_6_1[i],1-DB_BE$prob_qh3_6_1[i]))
  }
}

# table(DB_BE$qh3_6)

#
#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

qh3_6 <- DB_BE$qh3_6
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, qh3_6, qh3_6_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_qh3_6_imputiert.csv')

