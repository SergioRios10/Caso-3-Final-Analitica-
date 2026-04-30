####### 1

## Cargar librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(psych)

## Cargar base de datos
data <- read_excel("Data_Predicting_Customer_Churn.xlsx", sheet = 2)

## Estructura de los datos
str(data)

## Vista general
head(data)

## Nombres de variables
colnames(data)

## Resumen estadístico
summary(data)
