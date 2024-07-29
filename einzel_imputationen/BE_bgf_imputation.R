# BE_bgf_imputation.R
# Julian Bischof
# 15.03.2022
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# bgf: Brutto-Grundfläche (BGF) bezeichnet diejenige Fläche, 
# welche sich aus der Summe aller Grundflächen aller Grundrissebenen eines Gebäudes errechnet. 
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

    #----------------------------------------
    # 1.1 Sauberer Datensatz für Regressionsmodel
    
    DB_BE_clean <- subset(DB_BE, hk_geb>=0 & geb_flaeche>=0 & geb_f_hoehe_mittel_iwu>=0 & bgf>=0)
    ####
    
    hk_geb <- DB_BE_clean$hk_geb
    bgf <- DB_BE_clean$bgf
    geb_flaeche <- DB_BE_clean$geb_flaeche
    geb_f_hoehe_mittel_iwu <- DB_BE_clean$geb_f_hoehe_mittel_iwu
    HRF <- DB_BE_clean$HRF
    Sum_HRF_BE_clean <- sum(HRF)
    N_Clean <- nrow(DB_BE_clean)
    
    uk_geb <- DB_BE_clean$uk_geb
    
    
    # min(bgf)
    # min(hk_geb)
    # min(geb_flaeche)
    
    
    #----------------------------------------
    # 1.2 Datensatz aus zu imputierenden Zeilen
    
    #Generieren von Un-Clean Subset d.h. das Subset in welchem gbf = -7
    DB_BE_unclean_bgf <- subset(DB_BE, hk_geb>=0 & geb_flaeche>=0 & geb_f_hoehe_mittel_iwu>=0 & bgf==-7)

      # Counting number of imputations in this variable and marking them for later analysis
      bgf_imputated <- DB_BE$bgf == -7
      table <- table(bgf_imputated)
      bgf_n_imputated <- table[2]
      
      bgf_imputated[bgf_imputated == TRUE] <- 1
      # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt
    


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................
    
    #----------------------------------------
    # 2.1 Regressionsmodel bgf_clean und hk_geb_clean zu bgf_clean
    
    #Regression linear, Faktoren werden einzeln berücksichtigt
    #hk_geb und geb_f_hoehe_mittel_iwu -> ~ Geschossanzahl; Geschossanzahl * geb_flaeche -> ~ bgf
    Reg <- lm(bgf ~ as.factor(hk_geb):geb_f_hoehe_mittel_iwu:geb_flaeche, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.8065 #Sum_HRF_BE_clean =  Anzahl aller BE NWG
    #Reg <- lm(bgf ~ as.factor(hk_geb) + geb_f_hoehe_mittel_iwu + geb_flaeche, weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.706
    
    # Reg <- lm(bgf ~ as.factor(hk_geb):geb_f_hoehe_mittel_iwu:geb_flaeche) # : indicates the product!!!; *asterisk means "include the marginals and the interaction". So lm(y ~ x1*x2) is the same thing as lm(y ~ x1 + x2 + x1:x2)
    
    #TEST Reg vs. manuelle Mittelwerte nach hk_geb_clean
    #Reg <- lm(bgf_clean ~ as.factor(hk_geb_clean)) # Alternative zur Mittelwertbestimmung in Abhänigigkeit von hk_geb_clean und Zuweisung unten!!! Verfahren einfach über Regressionsimputationsweg!
    
    sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/bgf.txt")
    print("Due to full final dataset of DataNWG-BRE not used for imputation. However, the Regression might be of interest anyway.")
    print(summary(Reg))
    print(model_equation(Reg))
    sink()  # returns output to the console
    
    # Save Model for later R use
    saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/bgf.rds")

    # summary(Reg)
    # 
    # model_equation(Reg)
    
    #----------------------------------------
    # 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und bgf = -7
    #     Berechnung der bgf in der bisher -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
    data_for_predict <- DB_BE_unclean_bgf
    
    rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 4) #berechne und runde fehlender Werte
    rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
    DB_BE$bgf[DB_BE$bgf==(-7) & DB_BE$geb_flaeche>=0 & DB_BE$geb_f_hoehe_mittel_iwu>=0 & DB_BE$hk_geb>=0] <- rounded_predict #überschreibt bgf in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden
    

#............................................................................
#
# 3
#Speichern####
#
#............................................................................

    # Imputierte Variable zum Speichern vorbereiten
    
    bgf <- DB_BE$bgf
    Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, bgf, bgf_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar
    
    # write it as a csv file
    write.csv(Imputierte_Variable, 'DB_BE_bgf_imputiert.csv')

