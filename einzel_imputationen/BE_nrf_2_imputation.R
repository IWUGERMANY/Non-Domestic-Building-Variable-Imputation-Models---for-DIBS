# BE_nrf_2_imputation.R
# Julian Bischof
# 25.02.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# nrf_2: Maßzahl der NRF aus der BGF, die über die Geodaten sowie Geschossangaben 
# und -höhen aus SCR und BE bestimmt wurde, mit dem  Flächenfaktor f_flaeche für 
# die UK_Geb-spezifische Umrechnung der BGF in die NRF.
#-----------------------------------------

# f_flaeche ist der BGF zu NGF Umrechnungsfaktor abhänig von Nutzungsart
min(DB_BE$f_flaeche) #keine Missings

# Hier keine Reg. bzw. Mittwelwertschätzer, sonder umrechnung der imputierten bgf für alle nrf_2 == -7
DB_BE$nrf_2[DB_BE$nrf_2==(-7)] <- DB_BE$bgf[DB_BE$nrf_2==(-7)]*DB_BE$f_flaeche[DB_BE$nrf_2==(-7)]
# table(DB_BE$nrf_2)
# min(DB_BE$nrf_2)


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

nrf_2 <- DB_BE$nrf_2
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, nrf_2)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_nrf_2_imputiert.csv')

