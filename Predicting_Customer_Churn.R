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


## Conteo de valores faltantes
colSums(is.na(data))

## Estandarizar nombres de variables
colnames(data) <- make.names(colnames(data))

## Ver nombres ajustados
colnames(data)

## Convertir variable dependiente a factor
data$Churn..1...Yes..0...No. <- as.factor(data$Churn..1...Yes..0...No.)

## Distribución de la variable objetivo
table(data$Churn..1...Yes..0...No.)

## Proporción de churn
prop.table(table(data$Churn..1...Yes..0...No.))

## Estadísticos descriptivos generales
describe(data)

## Estadísticos por grupo (Churn vs No Churn)
data %>%
  group_by(Churn..1...Yes..0...No.) %>%
  summarise(across(where(is.numeric), 
                   list(mean = mean, sd = sd), 
                   na.rm = TRUE))



###### 2

## Modelo Logit
modelo_logit <- glm(Churn..1...Yes..0...No. ~ 
                      Customer.Age..in.months. +
                      CHI.Score.Month.0 +
                      CHI.Score.0.1 +
                      Support.Cases.Month.0 +
                      Support.Cases.0.1 +
                      SP.Month.0 +
                      SP.0.1 +
                      Logins.0.1 +
                      Blog.Articles.0.1 +
                      Views.0.1 +
                      Days.Since.Last.Login.0.1,
                    data = data,
                    family = binomial(link = "logit"))


## Resumen del modelo
summary(modelo_logit)


## Pseudo R2

library(pscl)
pR2(modelo_logit)



###### 3

## Probabilidades predichas
data$prob_pred <- predict(modelo_logit, type = "response")

## Clasificación (umbral 0.1)
data$pred <- ifelse(data$prob_pred > 0.1, 1, 0)

## Convertir a factor
data$pred <- as.factor(data$pred)
data$Churn..1...Yes..0...No. <- as.factor(data$Churn..1...Yes..0...No.)

## Matriz de confusión
table(Predicho = data$pred, Real = data$Churn..1...Yes..0...No.)


mean(data$pred == data$Churn..1...Yes..0...No.)


## Error = Real - Predicción
data$error <- as.numeric(as.character(data$Churn..1...Yes..0...No.)) - data$prob_pred

## Histograma de errores
ggplot(data, aes(x = error)) +
  geom_histogram(bins = 30)



