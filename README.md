# Non-Domestic-Building-Variable-Imputation-Models---for-DIBS
This repository provides regression models to 'estimate' the most likely values for imputation purposes. The models are statistically representative of the German non-domestic building stock and cover DIBS input variables with a larger probability of missing values. For a better understanding, the models' summaries are provided in the .txt files, while the R-based models are provided via .rds files.

Further, also the scripts used for the development of these estimation models are provided. BE_imputation.R is the main script, testing the individual variables vor missing values and describing the general approch of tackeling the imputation. This script utilises the scripts available in the einzel_imputationen folder. Those each include the deveolopment of the variabel esimators and the application for the imputation. Last the Determination-of-full-datasets folder holds the script used to analyse the overall extend of missing variables.

These models were developed using the interview data set of the ENOB:DataNWG Project https://datanwg.de. The development was undertaken within the PhD project of Julian Bischof. Details are published with the Ph.D. thesis (coming soon). 

Due to data protection reasons, the full data set on which the model development is based can not be provided. However, the data set can be partly accessed and analysed to a certain extent via https://datanwg.de/forschungsdatenbank/. More information on the Dynamic ISO Building Simulator (DIBS) is available here: https://iwugermany.github.io/dibs/overview


The tables below summarise the applied imputation approaches, dependent on the type of variable, and a list of DIBS input and auxiliary variables with their imputation approach taken.

# Variables Types and best-fitted/applied imputation approaches

| **null** | **Imputation Type**                                                                   | **Use Case**                                                                                                                                            | **Scale**          | **Pseudo-Code**                                         | **Example of estimation algorithm in R**                                                                                                                    |
|----------|---------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------|---------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **1**    | Calculation based on physical or known statistical relations                          | Metric variables, directly quantifiable via physical relationships of available variables.                                                              | metrical           | a = b + c                                               | aw_fl = Fassendfläche(f_fl_geo_iwu) - Fensterfläche(fen_fl)                                                                                                 |
| **2**    | Linear (singular and multiple) weighted regression estimation                         | Metric variables, not directly quantifiable via physical relationships of available variables.                                                          | metrical           | lm(x~a:b, weights = Y)                                  | lm(n_og ~ as.factor(uk_geb) : geb_f_hoehe_mittel_iwu : as.factor(dachform_be), weights = (HRF/Sum_HRF_BE)*N_Clean)                                          |
| **3**    | Individual - quasi-binominal (quasi-logistic) - regression-based - dice - weighted    | Nominal or ordinal scaled variables with only two characteristics.                                                                                      | nominal or ordinal | gml(x~a*b, weights = Y, family = quasibinomial) -> dice | glm(qh3_1 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=quasibinomial, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)                    |
| **4**    | Individual - multinominal - binominal (logistic) - regression-based - dice - weighted | Nominal or ordinal scaled variables with more than two characteristics.                                                                                 | nominal or ordinal | multinom(y~a*b, weights = Y) -> dice                    | multinom (lampenart ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean) * N_Clean, maxit = 1000, MaxNWts = 1000) |
| **5**    | Cleaning up -8 if non-existent                                                        | Metric variables, where "Does not apply" equals a zero value.                                                                                           | metrical           | x[x==-8] <- 0                                           | DB_BE$glasart_1[DB_BE$glasart_1==(-8)] <- 0                                                                                                                 |
| **6**    | Assumption of values based on related variable                                        | Variables with available closely related imputed variables, that can only be estimated based on a small training data set, causing great uncertainties.  | all                | x[x==-7] <- y[x==-7]                                    | DB_BE$unter_hoehe[DB_BE$unter_hoehe==(-7)] <- DB_BE$q66a_1[DB_BE$unter_hoehe==(-7)]/100                                                                     |


# Required variables for DIBS simulation and applied imputation types

| Interview Phase Variable Name | DIBS Variable Name        | Description                                                             | Applied Imputation Type |
|-------------------------------|---------------------------|-------------------------------------------------------------------------|-------------------------|
| scr_gebaeude_id               | scr_gebaeude_id           | Building-ID                                                             | None - No Missings      |
| plz                           | plz                       | Postal Code                                                             | None - No Missings      |
| hk_geb                        | hk_geb                    | Main Building-Usage-Category                                            | None - No Missings      |
| uk_geb                        | uk_geb                    | Sub Building-Usage-Category                                             | None - No Missings      |
| bak_grob                      | bak_grob                  | Time periode of building construction (rough)                           | None - No Missings      |
| geb_f_flaeche_n_iwu           | geb_f_flaeche_n_iwu       | Northern building facade area                                           | None - No Missings      |
| geb_f_flaeche_o_iwu           | geb_f_flaeche_o_iwu       | Eastern building facade area                                            | None - No Missings      |
| geb_f_flaeche_s_iwu           | geb_f_flaeche_s_iwu       | Southern building facade area                                           | None - No Missings      |
| geb_f_flaeche_w_iwu           | geb_f_flaeche_w_iwu       | Western building facade area                                            | None - No Missings      |
| geb_f_hoehe_mittel_iwu        | building_height           | Average building height                                                 | None - No Missings      |
| nrf_2                         | net_room_area             | Netto room area                                                         | None - No Missings      |
| ebf                           | energy_ref_area           | Energy reference area                                                   | None - No Missings      |
| qd1                           | Fen_ant                   | Window area share of building facade                                    | None - No Missings      |
| d_fl_be                       | roof_area                 | Roof area                                                               | None - No Missings      |
| n_ug                          | n_UG                      | Average number of floors below ground                                   | None - No Missings      |
| lampenart                     | lampenart_be              | Predominant lamp type                                                   | 4                       |
| q25_1                         | max_occupancy             | Maximal number of building occupants                                    | 2                       |
| aw_fl                         | wall_area_og              | Building wall area above ground                                         | 1                       |
| n_og                          | n_OG                      | Average number of floors above ground                                   | 2                       |
| unteraw_fl                    | wall_area_ug              | Building wall area below ground                                         | 1                       |
| qf1                           | qF1                       | Predominant lighting type                                               | 4                       |
| glasart_1                     | glass_solar_transmittance | Energy transmittance rate of the glassing                               | 5                       |
| aw_konstr_1                   | thermal_capacitance       | Construction type of facade (thermal capacity)                          | 4                       |
| u_aw                          | u_walls                   | U-Value of facade (opak)                                                | 2                       |
| u_fen                         | u_windows                 | U-Value of windows                                                      | 2                       |
| qh1                           | qH1                       | Type of ventilation                                                     | 4                       |
| qh3_1                         | qH3                       | Heat recovery efficiency of the ventilation system                      | 3                       |
| freie_kuehlung                | night_flushing_flow       | Usage of natural ventilation cooling during the night                   | 4                       |
| w_erz_art_et                  | heating_supply_system     | Type of heating supply system                                           | 4                       |
| qg13                          | heating_emission_system   | Type of heating emission system                                         | 4                       |
| k_erz_art_rk                  | cooling_supply_system     | Type of cooling supply system                                           | 4                       |
| qi11                          | cooling_emission_system   | Type of cooling emission system                                         | 4                       |
| u_ug                          | u_base                    | U-Value of base plate                                                   | 2                       |
| qd8                           | qD8                       | Energy transmittance rate of the glassing in case of active sun shading | 4                       |
| d_u_ges                       | u_roof                    | U-Value of roof (transparent + opak)                                    | 2                       |
| qg21                          | dhw_system                | Type of hot water supply                                                | 4                       |



# Auxiliary variables for DIBS input imputation and applied imputation types

| **Interview Phase Variable Name** | **Used for Imputation of Variable**                                                                                      | **Description**                                              | **Applied Imputation Type** |
|-----------------------------------|--------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------|-----------------------------|
| **geb_flaeche**                   | bfg; d_fl_wueoa                                                                                                          | Net area of the ground projection of the building            | None - No Missings          |
| **f_ant_beheizt**                 | ebf                                                                                                                      | Share of conditioned (heated) building area                  | None - No Missings          |
| **bak**                           | aw_daemm_staerke_1; aw_flantgedges; u_fen; qh1; f_ant_belueftet; f_ant_gekuehlt; qg13; k_erz_art_rk; qi11; u_ug; d_u_ges | Time periode of building construction                        | None - No Missings          |
| **geb_f_umfang**                  | unteraw_fl                                                                                                               | Circumference of building                                    | None - No Missings          |
| **f_fl_geo_iwu**                  | aw_fl                                                                                                                    | Ground area covert by building                               | None - No Missings          |
| **bgf**                           | q25_1                                                                                                                    | Gross room base area                                         | None - No Missings          |
| **fen_fl**                        | aw_fl                                                                                                                    | Window area                                                  | 2                           |
| **dachform_be**                   | n_og; d_fl_wueoa                                                                                                         | Roof type                                                    | 3                           |
| **q66a_1**                        | unter_hoehe; qf1;                                                                                                        | Clear room height ob above ground levels                     | 2                           |
| **unter_hoehe**                   | unteraw_fl                                                                                                               | Clear room height of underground levels                      | 6                           |
| **aw_daemm_staerke_1**            | aw_flantgedges; u_aw                                                                                                     | Primary wall construction insulation thickness               | 2                           |
| **aw_flantgedges**                | u_aw                                                                                                                     | Share of envelope wall insulated                             | 2                           |
| **qi1**                           | f_ant_gekuehlt                                                                                                           | Possesses the building a central cooling system              | 4                           |
| **f_ant_gekuehlt**                | qi11                                                                                                                     | Share of building usage cooled by central cooling system | 2                           |
| **w_erz_art**                     | w_erz_art_et                                                                                                             | Main heat generator                                          | 4                           |
| **energietraeger**                | w_erz_art_et                                                                                                             | Main energy carrier                                          | 4                           |


 <a href="https://trackgit.com">
<img src="https://us-central1-trackgit-analytics.cloudfunctions.net/token/ping/lx90704xsbrp1l2wilgl" alt="trackgit-views" />
</a>
