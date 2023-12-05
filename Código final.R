#Bibliotecas

# Verificando se o pacote "Dplyr" está instalado
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
}

# Verificando se o pacote "Viridis" está instalado
if (!require(viridis, quietly = TRUE)) {
  install.packages("viridis")
}

# Verificando se o pacote "pwr" está instalado
if (!require(pwr, quietly = TRUE)) {
  install.packages("pwr")
}

# Verificando se o pacote "xtable" está instalado
if (!require(xtable, quietly = TRUE)) {
  install.packages("ggplott2")
}

# Verificando se o pacote "ggplot2" está instalado
if (!require(ggplot2, quietly = TRUE)) {
  install.packages("xtable")
}

# Verificando se o pacote "gridExtra" está instalado
if (!require(gridExtra, quietly = TRUE)) {
  install.packages("gridExtra")
}

library(pwr)
library(dplyr)
library(xtable)
library(ggplot2)
library(viridis)
library(gridExtra)

#Definindo diretorio para base de dados

setwd("C:\\Users\\Diogo\\Downloads")

#Para o R ler numeros grandes
options(digits = 21)
options(scipen = 999)

#Lendo base unica

Candidatos_recandidatura <- read.csv("Candidatos_recandidatura.csv")

#Base de candidatos eleitos em 2016 que se candidataram em 2012  ----

Candidatos_recandidatura_eleitos <-
  subset(
    Candidatos_recandidatura,
    DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP")
  )

#Criando 2 populações, uma incluindo os candidatos que mantiveram o mesmo partido nas duas eleições e uma com os que mudaram de partido

Candidatos_partidos_iguais <-
  Candidatos_recandidatura[Candidatos_recandidatura$NM_PARTIDO.x == Candidatos_recandidatura$NM_PARTIDO.y,]
Candidatos_partidos_alterados <-
  Candidatos_recandidatura[Candidatos_recandidatura$NM_PARTIDO.x != Candidatos_recandidatura$NM_PARTIDO.y,]


# Criando 2 populações: uma incluindo os candidatos eleitos em 2016 com o mesmo partido nas duas eleições e outra com os candidatos eleitos em 2016 que mudaram de partido.

Candidatos_eleitos_partidos_iguais <-
  Candidatos_recandidatura_eleitos[Candidatos_recandidatura_eleitos$NM_PARTIDO.x == Candidatos_recandidatura_eleitos$NM_PARTIDO.y,]
Candidatos_eleitos_partidos_alterados <-
  Candidatos_recandidatura_eleitos[Candidatos_recandidatura_eleitos$NM_PARTIDO.x != Candidatos_recandidatura_eleitos$NM_PARTIDO.y,]

# Início do teste de hipótese para a população completa (Será feita uma vez a conta por meio da fórmula para mostrar que a função de teste de proporção no R nos fornece o mesmo resultado). -----

# Definindo o teste: H0 - A proporção de eleitos após recandidatura com o mesmo partido é maior ou igual à proporção de eleitos após recandidatura com outro partido.
# Logo, Ha - A proporção de eleitos após recandidatura com o mesmo partido é menor do que a proporção de eleitos após recandidatura com outro partido.
# Por conveniência, abreviamos "proporção de eleitos após recandidatura com o mesmo partido" como Pm e "proporção de eleitos após recandidatura com outro partido" como Pd. Adicionarei também o 'a' após as abreviações quando estivermos falando de valores amostrais. Por exemplo, Pma seria a proporção amostral daqueles que não mudaram de partido. No código, Pma = X e Pda = Y.
# Assim, sob H0, temos que Pm - Pd = 0.
# Como estamos sob H0, a probabilidade de alguém ser eleito segue uma distribuição única, sendo esta uma distribuição de Bernoulli com probabilidade p de ser eleito e variância = p(1-p).

# Primeiro, devemos criar nossa estatística de teste:
# Para construir nossa estatística de teste, devemos considerar primeiramente a distribuição dos nossos estimadores. Como estamos sob H0, sabemos que devido ao Teorema do Limite Central, a distribuição desses se aproximará de uma normal com média p e variância p(1-p)/n. A outra distribuição terá média p e variância p(1-p)/m, onde n e m são a quantidade de observações das respectivas amostras.
# Logo, nosso Tn = [(Pda - Pma) - E(Pd - Pm)] terá uma distribuição normal sob H0 com média 0 e variância p(1-p)(1/n + 1/m).
# Para aproximarmos de uma normal com média 0 e variância 1, nossa estatística TN deve ser [(Pda - Pma) - E(Pm - Pd)]/sqrt(p(1-p)(1/n + 1/m)).

#Para proporções

X <-
  nrow(Candidatos_eleitos_partidos_iguais) / nrow(Candidatos_partidos_iguais)
Y <-
  nrow(Candidatos_eleitos_partidos_alterados) / nrow(Candidatos_partidos_alterados)

#Para variância

# Primeiro, calculamos nosso P. Isso ocorre porque temos a população completa da eleição e, assim, podemos calcular a variância populacional, dado que temos 68.835 candidatos eleitos em 2016 e 498.391 candidatos concorrendo à eleição:

Prob_sucesso <- 68835 / 498391

# Agora, calculando a variância de Tn sob H0:

var_tn <-
  Prob_sucesso * (1 - Prob_sucesso) * (1 / nrow(Candidatos_partidos_iguais) + 1 /
                                         nrow(Candidatos_partidos_alterados))

# Calculando agora o valor de Tn (Lembrando que estamos sob H0, logo E(Pma - Pda) = 0):

Tn <- (Y - X) / sqrt(var_tn)

#Calculando o P valor

p_valor_pcompleta <- pnorm(Tn, lower.tail = FALSE)

# Demonstração utilizando um comando original do R.

Teste_hipotese_Completo <-
  prop.test(
    x = c(21827, 23728),
    n = c(90351, 77237),
    alternative = "greater"
  )

# Teste de hipótese apenas para prefeitos ----

# Criando bases com apenas prefeitos

Candidatos_recandidatura_prefeitos <-
  subset(Candidatos_recandidatura, DS_CARGO.y %in% c("PREFEITO"))
Candidatos_recandidatura_prefeitos_eleitos <-
  subset(
    Candidatos_recandidatura_prefeitos,
    DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP")
  )

# Criando 2 populações: uma incluindo os candidatos a prefeito que mantiveram o mesmo partido nas duas eleições e outra com os que mudaram de partido

Prefeitos_partidos_iguais <-
  Candidatos_recandidatura_prefeitos[Candidatos_recandidatura_prefeitos$NM_PARTIDO.x == Candidatos_recandidatura_prefeitos$NM_PARTIDO.y,]
Prefeitos_partidos_alterados <-
  Candidatos_recandidatura_prefeitos[Candidatos_recandidatura_prefeitos$NM_PARTIDO.x != Candidatos_recandidatura_prefeitos$NM_PARTIDO.y,]

# Criando 2 populações: uma incluindo os candidatos a prefeito eleitos em 2016 com o mesmo partido nas duas eleições e outra com os candidatos a prefeito eleitos em 2016 que mudaram de partido

Prefeitos_partidos_iguais_eleitos <-
  Candidatos_recandidatura_prefeitos_eleitos[Candidatos_recandidatura_prefeitos_eleitos$NM_PARTIDO.x == Candidatos_recandidatura_prefeitos_eleitos$NM_PARTIDO.y,]
Prefeitos_partidos_alterados_eleitos <-
  Candidatos_recandidatura_prefeitos_eleitos[Candidatos_recandidatura_prefeitos_eleitos$NM_PARTIDO.x != Candidatos_recandidatura_prefeitos_eleitos$NM_PARTIDO.y,]

# Teste de hipótese de proporção seguindo a mesma H0 e Ha do primeiro

Teste_hipotese_prefeitos <-
  prop.test(
    x = c(
      nrow(Prefeitos_partidos_alterados_eleitos),
      nrow(Prefeitos_partidos_iguais_eleitos)
    ),
    n = c(
      nrow(Prefeitos_partidos_alterados),
      nrow(Prefeitos_partidos_iguais)
    ),
    alternative = "greater"
  )

# Teste apenas para vereadores ----

# Criando bases com apenas vereadores

Candidatos_recandidatura_vereadores <-
  subset(Candidatos_recandidatura, DS_CARGO.y %in% c("VEREADOR"))
Candidatos_recandidatura_vereadores_eleitos <-
  subset(
    Candidatos_recandidatura_vereadores,
    DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP")
  )

# Criando 2 populações: uma incluindo os candidatos a vereadores que mantiveram o mesmo partido nas duas eleições e outra com os que mudaram de partido

vereadores_partidos_iguais <-
  Candidatos_recandidatura_vereadores[Candidatos_recandidatura_vereadores$NM_PARTIDO.x == Candidatos_recandidatura_vereadores$NM_PARTIDO.y,]
vereadores_partidos_alterados <-
  Candidatos_recandidatura_vereadores[Candidatos_recandidatura_vereadores$NM_PARTIDO.x != Candidatos_recandidatura_vereadores$NM_PARTIDO.y,]

# Criando 2 populações: uma incluindo os candidatos a vereadores eleitos em 2016 com o mesmo partido nas duas eleições e outra com os candidatos a vereadores eleitos em 2016 que mudaram de partido

vereadores_partidos_iguais_eleitos <-
  Candidatos_recandidatura_vereadores_eleitos[Candidatos_recandidatura_vereadores_eleitos$NM_PARTIDO.x == Candidatos_recandidatura_vereadores_eleitos$NM_PARTIDO.y,]
vereadores_partidos_alterados_eleitos <-
  Candidatos_recandidatura_vereadores_eleitos[Candidatos_recandidatura_vereadores_eleitos$NM_PARTIDO.x != Candidatos_recandidatura_vereadores_eleitos$NM_PARTIDO.y,]

# Teste de hipótese de proporção seguindo a mesma H0 e Ha

Teste_hipotese_vereadores <-
  prop.test(
    x = c(
      nrow(vereadores_partidos_alterados_eleitos),
      nrow(vereadores_partidos_iguais_eleitos)
    ),
    n = c(
      nrow(vereadores_partidos_alterados),
      nrow(vereadores_partidos_iguais)
    ),
    alternative = "greater"
  )

# Teste de hipótese baseado na separação de partidos em direita, esquerda e centro (Espectro político tomado de Faganelo)

# Mudando o nome dos partidos para siglas ----

Candidatos_recandidatura_dec <- Candidatos_recandidatura

Siglas <- c(
  "PARTIDO SOCIALISTA BRASILEIRO" = "PSB",
  "PARTIDO DEMOCRÁTICO TRABALHISTA" = "PDT",
  "PARTIDO POPULAR SOCIALISTA" = "PPS",
  "PARTIDO PROGRESSISTA" = "PP",
  "PARTIDO VERDE" = "PV",
  "DEMOCRATAS" = "DEM",
  "PARTIDO TRABALHISTA CRISTÃO" = "PTC",
  "PARTIDO DO MOVIMENTO DEMOCRÁTICO BRASILEIRO" = "PMDB",
  "PARTIDO DA REPÚBLICA" = "PR",
  "PARTIDO SOCIAL CRISTÃO" = "PSC",
  "PARTIDO TRABALHISTA BRASILEIRO" = "PTB",
  "PARTIDO REPUBLICANO BRASILEIRO" = "PRB",
  "PARTIDO DA SOCIAL DEMOCRACIA BRASILEIRA" = "PSDB",
  "PARTIDO SOCIAL DEMOCRATA CRISTÃO" = "PSDC",
  "PARTIDO SOCIAL LIBERAL" = "PSL",
  "PARTIDO DA MOBILIZAÇÃO NACIONAL" = "PMN",
  "PARTIDO DOS TRABALHADORES" = "PT",
  "PARTIDO REPUBLICANO PROGRESSISTA" = "PRP",
  "PARTIDO COMUNISTA DO BRASIL" = "PC do B",
  "PARTIDO TRABALHISTA DO BRASIL" = "PT do B",
  "PARTIDO HUMANISTA DA SOLIDARIEDADE" = "PHS",
  "PARTIDO SOCIALISMO E LIBERDADE" = "PSOL",
  "PARTIDO TRABALHISTA NACIONAL" = "PTN",
  "PARTIDO RENOVADOR TRABALHISTA BRASILEIRO" = "PRTB",
  "PARTIDO SOCIAL DEMOCRÁTICO" = "PSD",
  "SOLIDARIEDADE" = "SD",
  "PARTIDO REPUBLICANO DA ORDEM SOCIAL" = "PROS",
  "PARTIDO ECOLÓGICO NACIONAL" = "PEN",
  "PARTIDO PÁTRIA LIVRE" = "PPL",
  "REDE SUSTENTABILIDADE" = "REDE",
  "PARTIDO DA MULHER BRASILEIRA" = "PMB",
  "PARTIDO SOCIALISTA DOS TRABALHADORES UNIFICADO" = "PSTU",
  "PARTIDO COMUNISTA BRASILEIRO" = "PCB",
  "PARTIDO LIBERAL" = "PL",
  "PARTIDO DA CAUSA OPERÁRIA" = "PCO",
  "MOVIMENTO DEMOCRÁTICO BRASILEIRO" = "MDB",
  "PATRIOTA" = "PATRIOTA",
  "PARTIDO NOVO" = "NOVO",
  "DEMOCRACIA CRISTÃ" = "DC",
  "PROGRESSISTAS" = "PPB",
  "AVANTE" = "AVANTE",
  "PODEMOS" = "PODE"
)

#Substituindo

Candidatos_recandidatura_dec$NM_PARTIDO.y <-
  Siglas[Candidatos_recandidatura_dec$NM_PARTIDO.y]

#Classificando agora em direita, esquerda e centro ----

Classificacoes <- c(
  "PT" = "ESQUERDA",
  "PSTU" = "ESQUERDA",
  "PSOL" = "ESQUERDA",
  "PC do B" = "ESQUERDA",
  "PCB" = "ESQUERDA",
  "PCO" = "ESQUERDA",
  "PDT" = "ESQUERDA",
  "PSB" = "ESQUERDA",
  "REDE" = "ESQUERDA",
  "PPL" = "ESQUERDA",
  "SD" = "CENTRO",
  "PMDB" = "CENTRO",
  "PMN" = "CENTRO",
  "PSDB" = "CENTRO",
  "PV" = "CENTRO",
  "AVANTE" = "CENTRO",
  "PPB" = "CENTRO",
  "MDB" = "CENTRO",
  "PMB" = "CENTRO",
  "PROS" = "CENTRO",
  "PSD" = "CENTRO",
  "PTC" = "CENTRO",
  "DC" = "DIREITA",
  "PATRIOTA" = "DIREITA",
  "NOVO" = "DIREITA",
  "PAN" = "DIREITA",
  "PRN" = "DIREITA",
  "PSL" = "DIREITA",
  "PFL" = "DIREITA",
  "DEM" = "DIREITA",
  "PGT" = "DIREITA",
  "PHS" = "DIREITA",
  "PEN" = "DIREITA",
  "PL" = "DIREITA",
  "PR" = "DIREITA",
  "PPB" = "DIREITA",
  "PP" = "DIREITA",
  "PPS" = "DIREITA",
  "PRONA" = "DIREITA",
  "PRP" = "DIREITA",
  "PRTB" = "DIREITA",
  "PSC" = "DIREITA",
  "PSD" = "DIREITA",
  "PSDC" = "DIREITA",
  "PST" = "DIREITA",
  "PT do B" = "DIREITA",
  "PTB" = "DIREITA",
  "PTN" = "DIREITA",
  "PRB" = "DIREITA",
  "PODE"  = "DIREITA"
)

#Substituindo -----

Candidatos_recandidatura_dec$NM_PARTIDO.y <-
  Classificacoes[Candidatos_recandidatura_dec$NM_PARTIDO.y]
Candidatos_recandidatura_dec$SG_PARTIDO <-
  Classificacoes[Candidatos_recandidatura_dec$SG_PARTIDO]

# Candidatos que permaneceram no seu espectro

#Direita ----

Candidatos_dd <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "DIREITA" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "DIREITA",]
Candidatos_dd_eleitos <-
  subset(Candidatos_dd,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))

#Esquerda ----

Candidatos_ee <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "ESQUERDA" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "ESQUERDA",]
Candidatos_ee_eleitos <-
  subset(Candidatos_ee,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))

#Centro ----

Candidatos_cc <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "CENTRO" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "CENTRO",]
Candidatos_cc_eleitos <-
  subset(Candidatos_cc,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))

#Mudança de direita para esquerda ----

Candidatos_de <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "DIREITA" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "ESQUERDA",]
Candidatos_de_eleitos <-
  subset(Candidatos_de,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))


Teste_hipotese_de_dd <-
  prop.test(
    x = c(nrow(Candidatos_de_eleitos), nrow(Candidatos_dd_eleitos)),
    n = c(nrow(Candidatos_de), nrow(Candidatos_dd)),
    alternative = "greater"
  )

#Mudança de esquerda para direita ----

Candidatos_ed <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "ESQUERDA" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "DIREITA",]
Candidatos_ed_eleitos <-
  subset(Candidatos_ed,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))

Teste_hipotese_ed_ee <-
  prop.test(
    x = c(nrow(Candidatos_ed_eleitos), nrow(Candidatos_ee_eleitos)),
    n = c(nrow(Candidatos_ed), nrow(Candidatos_ee)),
    alternative = "greater"
  )

#Mudança de direita para centro ----

Candidatos_dc <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "DIREITA" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "CENTRO",]
Candidatos_dc_eleitos <-
  subset(Candidatos_dc,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))


Teste_hipotese_dc_dd <-
  prop.test(
    x = c(nrow(Candidatos_dc_eleitos), nrow(Candidatos_dd_eleitos)),
    n = c(nrow(Candidatos_dc), nrow(Candidatos_dd)),
    alternative = "greater"
  )

#Mudança de centro para direita ----

Candidatos_cd <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "CENTRO" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "DIREITA",]
Candidatos_cd_eleitos <-
  subset(Candidatos_cd,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))


Teste_hipotese_cd_cc <-
  prop.test(
    x = c(nrow(Candidatos_cd_eleitos), nrow(Candidatos_cc_eleitos)),
    n = c(nrow(Candidatos_cd), nrow(Candidatos_cc)),
    alternative = "greater"
  )

#Mudança de esquerda para centro ----

Candidatos_ec <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "ESQUERDA" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "CENTRO",]
Candidatos_ec_eleitos <-
  subset(Candidatos_ec,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))


Teste_hipotese_ec_ee <-
  prop.test(
    x = c(nrow(Candidatos_ec_eleitos), nrow(Candidatos_ee_eleitos)),
    n = c(nrow(Candidatos_ec), nrow(Candidatos_ee)),
    alternative = "greater"
  )

#Mudança de centro para esquerda ----

Candidatos_ce <-
  Candidatos_recandidatura_dec[Candidatos_recandidatura_dec$SG_PARTIDO == "CENTRO" &
                                 Candidatos_recandidatura_dec$NM_PARTIDO.y == "ESQUERDA",]
Candidatos_ce_eleitos <-
  subset(Candidatos_ce,
         DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))


Teste_hipotese_ce_cc <-
  prop.test(
    x = c(nrow(Candidatos_ce_eleitos), nrow(Candidatos_cc_eleitos)),
    n = c(nrow(Candidatos_ce), nrow(Candidatos_cc)),
    alternative = "greater"
  )


# Criando um data frame para armazenar os resultados dos testes de hipóteses
resultados <- data.frame(
  Teste = character(),
  "P-valor" = numeric(),
  "Intervalo de Confiança Inferior" = numeric(),
  "Intervalo de Confiança Superior" = numeric(),
  stringsAsFactors = FALSE
)

# Função para adicionar resultados ao data frame
adicionar_resultado <- function(teste, p_valor, ic) {
  resultados[nrow(resultados) + 1, ] <<- c(teste, p_valor, ic[1], ic[2])
}

# Adicionando os resultados ao data frame
adicionar_resultado("Completo", Teste_hipotese_Completo$p.value, Teste_hipotese_Completo$conf.int)
adicionar_resultado("Prefeitos", Teste_hipotese_prefeitos$p.value, Teste_hipotese_prefeitos$conf.int)
adicionar_resultado("Vereadores", Teste_hipotese_vereadores$p.value, Teste_hipotese_vereadores$conf.int)
adicionar_resultado("De para DD", Teste_hipotese_de_dd$p.value, Teste_hipotese_de_dd$conf.int)
adicionar_resultado("DC para DD", Teste_hipotese_dc_dd$p.value, Teste_hipotese_dc_dd$conf.int)
adicionar_resultado("EC para EE", Teste_hipotese_ec_ee$p.value, Teste_hipotese_ec_ee$conf.int)
adicionar_resultado("ED para EE", Teste_hipotese_ed_ee$p.value, Teste_hipotese_ed_ee$conf.int)
adicionar_resultado("CE para CC", Teste_hipotese_ce_cc$p.value, Teste_hipotese_ce_cc$conf.int)
adicionar_resultado("CD para CC", Teste_hipotese_cd_cc$p.value, Teste_hipotese_cd_cc$conf.int)

# Renomeando as colunas
colnames(resultados) <- c("Teste", "P-valor", "Intervalo de Confiança Inferior", "Intervalo de Confiança Superior")


# Exibindo a tabela de resultados
print(resultados)

# Convertendo o data frame em uma tabela LaTeX - Usar caso queira baixar a tabela de resultados em um .tex
#tabela_latex <- xtable(resultados, caption = "Resultados dos Testes de Hipótese")
# Escrevendo a tabela LaTeX em um arquivo .tex
#print(tabela_latex, file = "resultados.tex", caption.placement = "top")

# Análise descritiva
# criando dataframe com tamanho e taxa de sucesso dos grupos: Total, Recandidaturas, Migrantes e não migrantes
descritiva1 <- data.frame()
descritiva1 <- rbind.data.frame(descritiva1, c("Total", 496416, 68835))
descritiva1 <- rbind.data.frame(descritiva1, c("Recandidaturas", nrow(Candidatos_recandidatura), nrow(Candidatos_recandidatura_eleitos)))
descritiva1 <- rbind.data.frame(descritiva1, c("Migrantes partidários", nrow(Candidatos_partidos_alterados), nrow(Candidatos_eleitos_partidos_alterados)))

# Renomear colunas
colnames(descritiva1) <- c("Grupo", "Observações", "Porcentagem eleita")
descritiva1$Observações <- as.numeric(descritiva1$Observações)
descritiva1$`Porcentagem eleita` <- as.numeric(descritiva1$`Porcentagem eleita`)
descritiva1$proporções <- descritiva1$`Porcentagem eleita`/descritiva1$Observações
descritiva1$proporções <- as.numeric(descritiva1$proporções)

# Criar gráfico
eleitos <- c('13,8%', sprintf("%.1f%%", (nrow(Candidatos_recandidatura_eleitos) / nrow(Candidatos_recandidatura)) * 100), sprintf("%.1f%%", (nrow(Candidatos_eleitos_partidos_alterados) / nrow(Candidatos_partidos_alterados)) * 100))

ggplot(descritiva1, aes(x = Grupo)) +
  geom_bar(stat = "identity", aes(y = 1, fill = "Não eleitos"), position = "stack") +
  geom_bar(stat = "identity", aes(y = proporções, fill = 'Eleitos'), position = "stack") +
  geom_text(aes(label = eleitos, y = proporções), vjust = 1, color = "black", size = 4, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Eleitos" = "skyblue", "Não eleitos" = "green")) +
  labs(y = "Proporção de candidatos") +
  theme_minimal()

# Criando vetores para análises comparando os cargos (prefeito e vereador)
prefeitos <- c(nrow(Prefeitos_partidos_alterados), nrow(Prefeitos_partidos_iguais))
porcentagensp <- sprintf("%.1f%%", (prefeitos / sum(prefeitos)) * 100)
df1 <- data.frame(prefeitos, porcentagensp)
vereadores <- c(nrow(vereadores_partidos_alterados), nrow(vereadores_partidos_iguais))
porcentagensv <- sprintf("%.1f%%", (vereadores / sum(vereadores)) * 100)
df2 <- data.frame(vereadores, porcentagensv)
Legenda <- c('Mudaram', 'Não mudaram')
vereadoresmudaram <- c(nrow(vereadores_partidos_alterados_eleitos), (nrow(vereadores_partidos_alterados)-nrow(vereadores_partidos_alterados_eleitos)))
porcentagensvm <- sprintf("%.1f%%", (vereadoresmudaram / sum(vereadoresmudaram)) * 100)
df3 <- data.frame(vereadoresmudaram, porcentagensvm)
vereadoresnmudaram <- c(nrow(vereadores_partidos_iguais_eleitos), (nrow(vereadores_partidos_iguais)-nrow(vereadores_partidos_iguais_eleitos)))
porcentagensvnm <- sprintf("%.1f%%", (vereadoresnmudaram / sum(vereadoresnmudaram)) * 100)
df4 <- data.frame(vereadoresnmudaram, porcentagensvnm)
prefeitosmudaram <- c(nrow(Prefeitos_partidos_alterados_eleitos), (nrow(Prefeitos_partidos_alterados)-nrow(Prefeitos_partidos_alterados_eleitos)))
porcentagenspm <- sprintf("%.1f%%", (prefeitosmudaram / sum(prefeitosmudaram)) * 100)
df5 <- data.frame(prefeitosmudaram, porcentagenspm)
prefeitosnmudaram <- c(nrow(Prefeitos_partidos_iguais_eleitos), (nrow(Prefeitos_partidos_iguais)-nrow(Prefeitos_partidos_iguais_eleitos)))
porcentagenspnm <- sprintf("%.1f%%", (prefeitosnmudaram / sum(prefeitosnmudaram)) * 100)
df6 <- data.frame(prefeitosnmudaram, porcentagenspnm)
Sucesso_eleitoral <- c("Eleito", "Não eleito")

# Criando gráficos
plot1 <- ggplot(df1, aes(x = "", y = prefeitos, fill = Legenda)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("Mudança de partido entre prefeitos") +
  scale_fill_manual(values = c("Mudaram" = "skyblue", "Não mudaram" = "green")) +
  geom_text(aes(label = porcentagensp), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme(legend.position = "right") 

plot2 <- ggplot(df2, aes(x = "", y = vereadores, fill = Legenda)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle('Mudança de partido entre vereadores') +
  scale_fill_manual(values = c("Mudaram" = "skyblue", "Não mudaram" = "green")) +
  geom_text(aes(label = porcentagensv), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme(legend.position = "right") 

plot3 <- ggplot(df3, aes(x = "", y = vereadoresmudaram, fill = Sucesso_eleitoral)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle('Vereadores que mudaram de partido') +
  scale_fill_manual(values = c("Eleito" = "red", "Não eleito" = "blue")) +
  geom_text(aes(label = porcentagensvm), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme(legend.position = "right") 

plot4 <- ggplot(df4, aes(x = "", y = vereadoresnmudaram, fill = Sucesso_eleitoral)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle('Vereadores que não mudaram de partido') +
  scale_fill_manual(values = c("Eleito" = "red", "Não eleito" = "blue")) +
  geom_text(aes(label = porcentagensvnm), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme(legend.position = "right") 

plot5 <- ggplot(df5, aes(x = "", y = prefeitosmudaram, fill = Sucesso_eleitoral)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle('Prefeitos que mudaram de partido') +
  scale_fill_manual(values = c("Eleito" = "red", "Não eleito" = "blue")) +
  geom_text(aes(label = porcentagenspm), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme(legend.position = "right") 

plot6 <- ggplot(df6, aes(x = "", y = prefeitosnmudaram, fill = Sucesso_eleitoral)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle('Vereadores que não mudaram de partido') +
  scale_fill_manual(values = c("Eleito" = "red", "Não eleito" = "blue")) +
  geom_text(aes(label = porcentagenspnm), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme(legend.position = "right") 

# Criando grid com todos os gráficos
grid.arrange(plot1, plot2, plot3, plot5, plot4, plot6, ncol = 2)

# Criando gráficos para análises conforme espectro político

plot_1 <- ggplot(Candidatos_partidos_alterados, aes(NM_PARTIDO.y)) +
  geom_bar(orientation = 'horizontal') +
  labs(title = "Filiações depois da troca de partido", x = 'Partido', y = "Número de filiados") +
  coord_flip() +
  theme(plot.title = element_text(size = 16),  # Font size for the main title
        axis.title = element_text(size = 10),  # Font size for axis titles
        axis.text = element_text(size = 5),   # Font size for axis labels
        plot.subtitle = element_text(size = 10) # Font size for subtitle
  )

plot_2 <- ggplot(Candidatos_partidos_alterados, aes(NM_PARTIDO.x)) +
  geom_bar(orientation = 'horizontal') +
  labs(title = "Filiações antes da troca de partido", x = 'Partido', y = "Número de filiados") +
  coord_flip() +
  theme(plot.title = element_text(size = 16),  # Font size for the main title
        axis.title = element_text(size = 10),  # Font size for axis titles
        axis.text = element_text(size = 5),   # Font size for axis labels
        plot.subtitle = element_text(size = 10) # Font size for subtitle
  )

grid.arrange(plot_1, plot_2, ncol = 1)

# cálculo de proporções de migração

pesquerda <- sprintf("%.1f%%", (nrow(Candidatos_ee)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'ESQUERDA') * 100))
pdireita <- sprintf("%.1f%%", (nrow(Candidatos_dd)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'DIREITA')) * 100)
pcentro <- sprintf("%.1f%%", (nrow(Candidatos_cc)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'CENTRO')) * 100)
pec <- sprintf("%.1f%%", (nrow(Candidatos_ec)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'ESQUERDA') * 100))
pdc <- sprintf("%.1f%%", (nrow(Candidatos_dc)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'DIREITA')) * 100)
ped <- sprintf("%.1f%%", (nrow(Candidatos_ed)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'ESQUERDA') * 100))
pde <- sprintf("%.1f%%", (nrow(Candidatos_de)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'DIREITA')) * 100)
pce <- sprintf("%.1f%%", (nrow(Candidatos_ce)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'CENTRO')) * 100)
pcd <- sprintf("%.1f%%", (nrow(Candidatos_cd)/sum(Candidatos_recandidatura_dec$SG_PARTIDO == 'CENTRO')) * 100)

Candidatos_recandidatura_dec$`Espectro em 2016` <- Candidatos_recandidatura_dec$NM_PARTIDO.y
ggplot(Candidatos_recandidatura_dec, aes(x = SG_PARTIDO, fill = `Espectro em 2016`)) +
  geom_bar(position = 'stack') +
  labs(title = "Filiações em 2012 vs. 2016", x = 'Espectro em 2012', y = "Número de filiados") +
  coord_flip()

print(pesquerda) 
print(pdireita)
print(pcentro)
print(pce)
print(pcd)
print(pdc)
print(pde)
print(pec)
print(ped)

# Criação da tabela de estatísticas descritivas

calcular_estatisticas_lista <- function(lista_bases, nomes_bases, Tabela_descritiva) {
  for (i in seq_along(lista_bases)) {
    # Extrair as bases
    bases <- lista_bases[[i]]
    
    # Obter o nome da base
    nome_base <- nomes_bases[i]
    
    # Número total de observações em cada base
    nrow_base1 <- nrow(bases[[1]])
    nrow_base2 <- nrow(bases[[2]])
    
    # Adicionar uma linha ao dataframe
    nova_linha <- data.frame(
      Base = nome_base,
      Quantidade_total = nrow_base1,
      Quantidade_eleitos = nrow_base2,
      Porcentagem_eleitos = nrow_base2 / nrow_base1
    )
    
    # Adicionar a nova linha ao dataframe existente
    Tabela_descritiva <- rbind(Tabela_descritiva, nova_linha)
  }
  
  rownames(Tabela_descritiva) <- Tabela_descritiva$Base  # Usar a coluna Base como nomes de linha
  Tabela_descritiva$Base <- NULL  # Remover a coluna Base
  
  return(Tabela_descritiva)
}

Tabela_descritiva <- data.frame(Base = character(), Quantidade_total = numeric(),
                                Quantidade_eleitos = numeric(), Porcentagem_eleitos = numeric())

# Criando os pares de bases 
lista_bases <- list(list(Candidatos_recandidatura, Candidatos_recandidatura_eleitos),
                    list(Candidatos_partidos_alterados, Candidatos_eleitos_partidos_alterados),
                    list(Candidatos_partidos_iguais, Candidatos_eleitos_partidos_iguais),
                    list(Candidatos_recandidatura_prefeitos, Candidatos_recandidatura_prefeitos_eleitos),
                    list(Prefeitos_partidos_alterados, Prefeitos_partidos_alterados_eleitos),
                    list(Prefeitos_partidos_iguais, Prefeitos_partidos_iguais_eleitos),
                    list(Candidatos_recandidatura_vereadores, Candidatos_recandidatura_vereadores_eleitos),
                    list(vereadores_partidos_alterados, vereadores_partidos_alterados_eleitos),
                    list(vereadores_partidos_iguais, vereadores_partidos_iguais_eleitos),
                    list(Candidatos_dd, Candidatos_dd_eleitos),
                    list(Candidatos_ee, Candidatos_ee_eleitos),
                    list(Candidatos_cc, Candidatos_cc_eleitos),
                    list(Candidatos_dc, Candidatos_dc_eleitos),
                    list(Candidatos_de, Candidatos_de_eleitos),
                    list(Candidatos_cd, Candidatos_cd_eleitos),
                    list(Candidatos_ce, Candidatos_ce_eleitos),
                    list(Candidatos_ed, Candidatos_ed_eleitos),
                    list(Candidatos_ec, Candidatos_ec_eleitos))

nomes_bases <- c("Candidatos_recandidatura", "Candidatos_partidos_alterados", "Candidatos_partidos_iguais",
                 "Candidatos_recandidatura_prefeitos", "Prefeitos_partidos_alterados", "Prefeitos_partidos_iguais",
                 "Candidatos_recandidatura_vereadores", "vereadores_partidos_alterados", "vereadores_partidos_iguais",
                 "Candidatos_dd", "Candidatos_ee", "Candidatos_cc", "Candidatos_dc", "Candidatos_de",
                 "Candidatos_cd", "Candidatos_ce", "Candidatos_ed", "Candidatos_ec")

# Calcular estatísticas para cada par de bases
Tabela_descritiva <- calcular_estatisticas_lista(lista_bases, nomes_bases, Tabela_descritiva)

# Mostrar o resultado
print(Tabela_descritiva)

#Baixar tabela em .tex
# latex_tabela_descritiva <- xtable(Tabela_descritiva, caption = "Sua Tabela Descritiva", label = "tab:TabelaDescritiva")

print(latex_tabela_descritiva, include.rownames = TRUE, booktabs = TRUE, floating = FALSE)
