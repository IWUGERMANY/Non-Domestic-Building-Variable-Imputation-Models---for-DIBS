# BE_qi1_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# qi1: Wird im Gebäude mit zentralen mechanischen Kälteanlagen gekühlt? 
#-----------------------------------------

# qi1 ist unabhängig von qh1==3
# Wenn qh3_3 = 1 dann muss auch qi1 = 3 sein!? Annahme, dass in diesem Fall (Zentrale RLT mit Kühlung) auch eine Zentrale Kälteanlage existiert
# Anzahl(N) qi1 = 3 in Teilmenge mit qi1 = -7 würfeln. qh3_3=1 fälle(O) in qi1=-7 mit 3 überschreiben. Verbleibene Anzahl(N-O) noch einmal zufällig in unter verbleibenden qi1=-7 auswürfeln?

# table(DB_BE$qh3_3==1 & DB_BE$qi1==3) 
# 
# table(DB_BE$qh3_3==1 & DB_BE$qi1==2) 
# 
# table(DB_BE$qh3_3==1 & (DB_BE$qi1==2 | DB_BE$qi1==3))

# 175(+-X) Fälle haben eine zentrale RLT (qh1==3) mit überwiegender Funktion der RLTs gleich Kühlen (qh3==3 bzw. qh3_3==1), 
# JEDOCH keine zentrale mechanische Kälteanlagen (qi1!=3)?!
  
# Ich hätte erwartet, dass alle Fälle mit zentraler RLT und überwiegende Funktion Kühlen auch zentrale mechanische Kälteanlagen habe, 
# oder zumindest dezentrale Anlagen die in Summe einer solchen zentralen Anlage ebenbürdig sind.

# Da es zusätzlich in der Energiebilanz problematisch wird wenn wir eine RLT mit Kühlung, 
# aber keine Angaben zur Kälteanlage haben, schlage ich vor diesen Fällen allen eine zentrale mechanische Kälteanlage "zu geben".




#............................................................................
#
# 0 Korrektur von qi1 auf Basis von anderen Angaben 
# qi1 bekommt noch die Fälle zugeschrieben, welche ine zentrale RLT (qh1==3) 
# mit überwiegender Funktion der RLTs gleich Kühlen (qh3==3 bzw. qh3_3==1) haben####
#
#............................................................................

DB_BE$qi1[DB_BE$qh3_3==1 & DB_BE$qi1!=3 & DB_BE$qi1!=2] <- 3 #  Zuweisung einer Zentralen Kältererzeugung wenn Zentrale RLT mit Kühlung und keine zentrale Kälte oder dezentrale Kälte

DB_BE$qi1[DB_BE$qh3_3==1 & DB_BE$qi1==2] <- 3 #  Zuweisung einer Zentralen Kältererzeugung wenn Zentrale RLT mit Kühlung dezentrale Kälte, da Modelle für RLT mit Kälte auch eine Kälteanlage brauchen



#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, qi1!=-7) # wenn qi1 = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

qi1 <- DB_BE_clean$qi1
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# # TESTEN
# DB_BE_clean <- subset(DB_BE, qi1=="3") 
# DB_BE_clean <- DB_BE
# sum(DB_BE_clean$HRF) 


    # Counting number of imputations in this variable and marking them for later analysis
    qi1_imputated <- DB_BE$qi1 == -7
    table <- table(qi1_imputated)
    qi1_n_imputated <- table[2]
    
    qi1_imputated[qi1_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 qi1 auf Basis multinominal logistc regression multinom() of the nnet package
# Alle weitern Zuweisungen zu Variablen, welche auf zur RLT gehören werden nur im Fall von qi1==3 imputiert! Damit konsistent.

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$qi1 <- relevel(as.factor(DB_BE_clean$qi1), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

# Reg <- multinom(qi1 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 4420.66 # NAs
# Reg <- multinom(qi1 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 4714.153  #
# Reg <- multinom(qi1 ~ as.factor(hk_geb) : as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 4420.471 # NAs
Reg <- multinom(qi1 ~ as.factor(hk_geb) : as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 4673.072

# Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qi1__existance_central_Cooler.txt")
print("qi1 ist unabhängig von qh1==3. Wenn qh3_3 = 1 dann muss auch qi1 = 3 sein!? Annahme, dass in diesem Fall (Zentrale RLT mit Kühlung) auch eine Zentrale Kälteanlage existiert (siehe detailiete Beschreibung im Script)")
print(summary(Reg))
sink()  # returns output to the console
#summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qi1__existance_central_Cooler.rds")

# mlogit() Multinomial logistic regression
# multinom function of the package nnet https://stats.stackexchange.com/questions/175782/how-to-perform-a-logistic-regression-for-more-than-2-response-classes-in-r


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of qi1 by a dice the has the 
#       levels_qi1 (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_qi1 <- levels(DB_BE_clean$qi1)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$qi1[i]==-7){
    DB_BE$qi1[i] <- sample(levels_qi1, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3]))
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

qi1 <- DB_BE$qi1
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, qi1, qi1_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_qi1_imputiert.csv')

