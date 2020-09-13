rm(list = ls())

# Leitura do banco de dados
library(readxl)

dados <- read_excel(path = "Distocia.xlsx", skip = 1, na = "NA")

summary(dados)
# Variaveis com grandes quantidades de NA

summary(as.factor(dados$`Situação do parto`))
# Quantidade de observacoes por classe

missing_values <- sapply(dados, function(i) sum(is.na(i))); missing_values
# Quantidade de NA por varivavel

missing_values_classe <- sapply(dados[-2], function(i) tapply(i, dados$`Situação do parto`, function(x) sum(is.na(x)))); missing_values_classe
# Quantidade de NA das variaveis por classe em relacao a situacao do parto

percentual_missing_values_class <- sapply(dados[-2], function(i) tapply(i, dados$`Situação do parto`, 
                                          function(x) sum(is.na(x))/length(i))); percentual_missing_values_class
# Percentual de NA das variaveis por classe em relacao a situacao do parto

