rm(list = ls())

# Leitura do banco de dados
library(readxl)

dados <- read_excel(path = "Distocia.xlsx", skip = 1, na = "NA", guess_max = min(1000, Inf))

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

install.packages("mice")
library(mice)

names(dados)
dim(dados)

dados <- dados[,!(names(dados) %in% c('NRO', 'Data do parto'))]
dim(dados)

str(dados)

dados$`Situação do parto` <- as.factor(dados$`Situação do parto`)
dados$`Bezerro deste parto` <- as.factor(dados$`Bezerro deste parto`)
dados$`Tipo de prenhez` <- as.factor(dados$`Tipo de prenhez`)
dados$Multiparidade <- as.factor(dados$Multiparidade)
dados$`GORD (%)` <- as.numeric(dados$`GORD (%)`)
dados$`PROT (%)` <- as.numeric(dados$`PROT (%)`)
dados$`LACT (%)` <- as.numeric(dados$`LACT (%)`)
dados$`ST (%)` <- as.numeric(dados$`ST (%)`)
dados$`ES (%)` <- as.numeric(dados$`ES (%)`)
dados$`Estações do ano` <- as.factor(dados$`Estações do ano`)

str(dados)

names(dados) <- gsub("\\((\\S+)", "", names(dados))
names(dados)[22] <- "CCS"
names(dados)[9] <- "NCS"
names(dados)
names(dados) <- abbreviate(tolower(names(dados)))

y <- dados$stdp
X <- dados
X$stdp <- NULL
dim(X)

md.pattern(X)
# Retorna uma tabela da qtde de missing values presente no banco de dados

X_imputed <- mice(X, m = 5, maxit = 100, seed = 0)

summary(X_imputed)

X_imputed$imp$ipc

dados_1 <- complete(X_imputed, 1)
dados_2 <- complete(X_imputed, 2)
dados_3 <- complete(X_imputed, 3)
dados_4 <- complete(X_imputed, 4)
dados_5 <- complete(X_imputed, 5)

dim(dados_1)

dados_1 <- cbind(y, dados_1)
names(dados_1)[1] <- "stdp"
dados_2 <- cbind(y, dados_2)
names(dados_2)[1] <- "stdp"
dados_3 <- cbind(y, dados_3)
names(dados_3)[1] <- "stdp"
dados_4 <- cbind(y, dados_4)
names(dados_4)[1] <- "stdp"
dados_5 <- cbind(y, dados_5)
names(dados_5)[1] <- "stdp"


sapply(dados_1, function(i) sum(is.null(i)))

install.packages("writexl")
library(writexl)

write_xlsx(dados_1, "Distocia_full_1.xlsx")
write_xlsx(dados_2, "Distocia_full_2.xlsx")
write_xlsx(dados_3, "Distocia_full_3.xlsx")
write_xlsx(dados_4, "Distocia_full_4.xlsx")
write_xlsx(dados_5, "Distocia_full_5.xlsx")


# Trabalhando com Distocia Reduzida

dados <- read_excel(path = "Distocia_reduzida.xlsx", na = " ", guess_max = min(1000, Inf))

dados <- dados[,!(names(dados) %in% c('...1', 'NRO', 'Data do parto'))]

dados$`Situação do parto` <- as.factor(dados$`Situação do parto`)
dados$`Bezerro deste parto` <- as.factor(dados$`Bezerro deste parto`)
dados$`Tipo de prenhez` <- as.factor(dados$`Tipo de prenhez`)
dados$Multiparidade <- as.factor(dados$Multiparidade)
dados$`GORD (%)` <- as.numeric(dados$`GORD (%)`)
dados$`PROT (%)` <- as.numeric(dados$`PROT (%)`)
dados$`LACT (%)` <- as.numeric(dados$`LACT (%)`)
dados$`ST (%)` <- as.numeric(dados$`ST (%)`)
dados$`ES (%)` <- as.numeric(dados$`ES (%)`)
dados$`Estações do ano` <- as.factor(dados$`Estações do ano`)

names(dados) <- gsub("\\((\\S+)", "", names(dados))
names(dados)[22] <- "CCS"
names(dados)[9] <- "NCS"
names(dados)
names(dados) <- abbreviate(tolower(names(dados)))

y <- dados$stdp
X <- dados
X$stdp <- NULL

library(mice)
X_imputed <- mice(X, m = 1, maxit = 50, seed = 0)

dados_reduzidos <- complete(X_imputed, 1)
dados_reduzidos <- cbind(y, dados_reduzidos)
names(dados_reduzidos)[1] <- "stdp"

library(writexl)

write_xlsx(dados_reduzidos, "Distocia_reduzidoR.xlsx")
