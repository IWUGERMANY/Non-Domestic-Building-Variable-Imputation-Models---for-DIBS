# BE_glasart_1_imputation.R
# Julian Bischof
# 03.08.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# glasart_1: überwiegende Art der Verglasung -> Anzahl der Scheiben
#-----------------------------------------

#Da keine -7 vorkommen wird nur -8 zu 0 geändert.



#----------------------------------------
# Alle -8 zu 0 

DB_BE$glasart_1[DB_BE$glasart_1==(-8)] <- 0


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

glasart_1 <- DB_BE$glasart_1
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, glasart_1)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_glasart_1_imputiert.csv')

