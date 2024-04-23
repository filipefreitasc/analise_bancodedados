install.packages("MASS")
install.packages("jmv")
install.packages("janitor")
install.packages("dplyr")
library("jmv")
library(ggplot2)
library(dplyr)
library(janitor)
tabyl1(dados$race, sort = TRUE)
tabyl
data(birthwt)
dados = birthwt
dados <- birthwt %>%
  mutate(race = case_when(
    race == '1' ~ "Branca",
    race == '2' ~ "Preto",
    race == '3' ~ "Outros"),
  (smoke = case_when(
    smoke == '0' ~ "Não",
    smoke == '1' ~ "Sim")
   ),(low = case_when(
    low == '0' ~ "Peso acima de 2.5 kg",
    low == '1' ~ "Peso abaixo de 2.5 kg")
  ))
intervalos <- cut(dados$age, 
                  breaks = c(0, 18, 25, 30, 35, 40, 45), 
                  labels = c("0-18", "18-25", "25-30", "30-35", "35-40", "40-45"))
dados$intervalos = intervalos
tabyl(dados$race, sort = TRUE)
t1 = descriptives(dados, vars = vars(race,low), freq = TRUE)
t1
tabyl(dados$low, sort = TRUE)
t3 = table(dados[, c("race", "low")], dnn = c("raça", "Crianças com baixo peso"))
t3
t4 = table(dados[, c("smoke", "low")], dnn = c("Mães Fumantes", "Crianças  com baixo peso"))
t4
t5 = table(dados[, c("intervalos", "low")], dnn = c("Idade das mães", "Crianças  com baixo peso"))
t5
ggplot(dados, aes(x = factor(low)))+
  geom_bar(fill="blue") +
  labs(title = "Gráfico de Barras para Variável ",
       x = "Crianças que nasceram com menos de 2.5kg", y = "Frequência")

ggplot(dados, aes(x = factor(race)))+
  geom_bar(fill="red") +
  labs(title = "Gráfico de Barras para Variável ",
       x = "Raça das mães", y = "Frequência")

ggplot(dados, aes(x = factor(smoke)))+
  geom_bar(fill="lightblue") +
  labs(title = "Gráfico de Barras para Variável ",
       x = "Raça das mães", y = "Frequência")

ggplot(data = dados, aes(x = smoke, y = low, fill = smoke)) +
  geom_bar(stat = "identity") +
  labs(x = "Mãe fumante", y = "Nascimento com Baixo Peso", title = "Gráfico de Barras")

ggplot(data = dados, aes(x = intervalos, y = low, fill = race)) +
  geom_bar(stat = "identity") +
  labs(x = "Idade", y = "Nascimento com Baixo Peso", title = "Gráfico de Barras") +
  scale_fill_manual(values = c("orange", "darkblue", "lightgreen" )) +
  theme_minimal()

ggplot(data = dados, aes(x = race, y = low, fill = race)) +
  geom_bar(stat = "identity") +
  labs(x = "Raça", y = "Nascimento com Baixo Peso", title = "Gráfico de Barras") +
  scale_fill_manual(values = c("red", "blue", "green", "orange")) +
  theme_minimal()

