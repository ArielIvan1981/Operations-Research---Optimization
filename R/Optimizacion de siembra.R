# Ud ha adquirido 150 Ha de terreno adecuado para la siembra de hortalizas
# En la siguiente tabla se muestran los datos asociados a cada una de las dos opciones de siembra
# La produccion de cada hortaliza debe ser superior a 1000 Tn
# Con la ayuda de la programacion lineal, determine:
#   a) las hectareas que se deben sembrar de cada tipo de hortaliza para obtener max. ben.
#   b) el beneficio obtenido

# Hortalizas    Produccion    Beneficio/Vta   Agua    Abono   Mano de Obra    Equipo
#                 Tn/Ha           $/Kg        L/Ha    Kg/Ha      HH/Ha        HE/Ha
# Cebolla         50.00           0.50      50000.00 900.00      20.00        12.00
# Papa            25.00           0.20      75000.00 450.00      25.00        10.00
# Disponibilidad  -----           -----   9000000.00 120000.00  3200.00     1600.00

# Variables: Ce: Hectareas para siembra de cebolla
#            Pa: Hectareas para siembra de papa

# Funcion objetivo (Maximizar)
# 0.50 $/Kg * 1000 Kg/Tn * 50 Tn/Ha * Ce Ha + 0.20 $/Kg * 1000 Kg/Tn * 25 Tn/Ha * Pa Ha
# 25000.00 Ce + 5000 Pa (Maximizar)

# Restricciones
# Hectareas: Ce Ha + Pa Ha                                                <= 150 Ha
# Agua:      50000 L/Ha * Ce Ha + 75000 L/Ha * Pa Ha                      <= 9000000 L
# Abono:     900 Kg/Ha * Ce Ha + 450 Kg/Ha * Pa Ha                        <= 120000 Kg
# Mano Obra: 20 HH/Ha * Ce Ha + 25 HH/Ha * Pa Ha                          <= 3200 HH
# Equipo:    12 HE/Ha * Ce Ha + 10 HE/Ha * Pa Ha                          <= 1600 HE
# Cebolla:   50 Tn/Ha * Ce Ha                                             >= 1000 Tn
# Papa:      25 Tn/Ha * Pa Ha                                             >= 1000 Tn
# Cebolla:   Ce Ha                                                        >= 0 Ha     lpsolve asume variables no negativas
# Papa:      Pa Ha                                                        >= 0 Ha

# USAREMOS LA LIBRERIA LPSOLVE PARA RESOLVER EL PROBLEMA

install.packages("lpSolve") # paquete para programacion lineal en R
library(lpSolve)
CFO <- matrix(c(0.5*1000*50,0.2*1000*25)) # Coeficientes Funcion Objetivo

Restricc <- matrix(c(1, 1, 50000, 75000, 900, 450, 20, 25, 12, 10, 50, 0, 0, 25), nrow = 7, byrow = T) # Coef Restricc
Restricc_LD <- matrix(c(150, 9000000, 120000, 3200, 1600, 1000, 1000)) # Lado derecho de restricc
Restricc_Des <- c("<=", "<=", "<=", "<=", "<=", ">=", ">=") # Direccion desigualdad

CalPL <- lp("max", CFO, Restricc, Restricc_Des, Restricc_LD) # Calculo Program Lineal
View(CalPL)
FO <- CalPL$objval # visualizar Calculo FO
FO # Funcion Objetivo
MFO <- matrix(c(FO)) # Transformar a matriz
MFO
MFO <- format(MFO, scientific = F, big.mark = ",", digits = "0") # cambio formato
MFO
row.names(MFO) <- c("Beneficio") # nombrar fila
MFO
colnames(MFO) <- c("     $") # nombrar columna
MFO

var <- CalPL$solution # visualizar variables
var
MVar <- matrix(c(var)) # transformar a matriz
MVar
row.names(MVar) <- c("Ha Cebolla", "Ha Papa") # nombrar fila
MVar
colnames(MVar) <- c("Variables") # nombrar columnas
MVar

Restricc_Calc <- Restricc%*%MVar # restricciones * variables
Restricc_Calc
row.names(Restricc_Calc) <- c('Ha', 'Agua', "Abono", "MO", "HE", "Ce", "Pa") # nombrar filas
Restricc_Calc
colnames(Restricc_Calc) <- c("Utilizadas") # nombrar columna
Restricc_Calc
row.names(Restricc_LD) <- c('Ha', 'Agua', "Abono", "MO", "HE", "Ce", "Pa") # nombrar filas
colnames(Restricc_LD) <- c("Limitaciones") # nombrar columna
Restricc_LD
Dif_Restricc <- Restricc_LD-Restricc_Calc
b <- format(Dif_Restricc, scientific = F, big.mark = ",", digits = "0") # cambio formato
row.names(b) <- c('Ha', 'Agua', "Abono", "MO", "HE", "Ce", "Pa") # nombrar filas
b
colnames(b) <- c("Sobrante") # nombrar columna
b

# rm(list=ls()) #borrar todos los datos
# rm(a) #borrar objeto a