# BE_unter_hoehe_imputation.R
# Julian Bischof
# 11.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# unter_hoehe: Unterierdische lichte Raumhöhe (anlaog zu q66a_1)
#-----------------------------------------


    # Counting number of imputations in this variable and marking them for later analysis
    unter_hoehe_imputated <- DB_BE$unter_hoehe == -7
    table <- table(unter_hoehe_imputated)
    unter_hoehe_n_imputated <- table[2]
    
    unter_hoehe_imputated[unter_hoehe_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#----------------------------------------
# Zuweisung auf Basis bereits imputierter Werte von q66a_1
# Die Imputation der oberirdischen lichten Raumhöhe war bereits nur mit einem großen Fehler möglich. (siehe Imputation q66a_1)
# Da der vorhandene vollständige Datensatz für eine mögliche Regression erheblich kleiner ist, kann angenommen wederden, dass die Schätzung noch schlecher wird.
# die unter_hoehe wird der q66a_1 gleichgesetzt!
DB_BE$unter_hoehe[DB_BE$unter_hoehe==(-7)] <- DB_BE$q66a_1[DB_BE$unter_hoehe==(-7)]/100
table(DB_BE$unter_hoehe)


#----------------------------------------
# Alle -8 zu 0; da diese nicht vorhanden sind...

DB_BE$unter_hoehe[DB_BE$unter_hoehe==(-8)] <- 0



#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

unter_hoehe <- DB_BE$unter_hoehe
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, unter_hoehe, unter_hoehe_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_unter_hoehe_imputiert.csv')
