# Bibliotecas -----------------------------------
library(dplyr)
library(tidyr)
library(plm)
library(stringr)
library(stargazer)
library(lmtest)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(tidyverse)
library(tidyr)
library(corrplot)
library(Hmisc)


# Importando base final -----------------------------------
base_completa <- read.csv("C:\\Users\\Diogo\\Documents\\base_final.csv") # <- substitua aqui

# Testando relevância do atrito gerado pela variável despesa ----------------------------
NAs <- subset(base_completa, is.na(base_completa$VR_DESPESA_CONTRATADA))
nao_NA <- subset(base_completa, !is.na(base_completa$VR_DESPESA_CONTRATADA))

t.test(NAs$aa_eleicao, nao_NA$aa_eleicao)
t.test(NAs$trocou_partido, nao_NA$trocou_partido)
t.test(NAs$incumbente, nao_NA$incumbente)
t.test(NAs$pc_votos_nominais, nao_NA$pc_votos_nominais)
t.test(NAs$pc_votos_totais, nao_NA$pc_votos_totais)
t.test(NAs$reincidente, nao_NA$reincidente)

test <- subset(NAs, NAs$ds_sit_totalizacao =="Eleito")

cpfs_comuns <- intersect(NAs$nr_cpf_candidato, nao_NA$nr_cpf_candidato)
length(cpfs_comuns)

diferencas_anos <- nao_NA %>%
  inner_join(NAs, by = "nr_cpf_candidato", suffix = c("_na", "_nao_na")) %>%
  mutate(dif_ano = aa_eleicao_na - aa_eleicao_nao_na)

## Análise descritiva -------------------------------

# Momentos amostrais relevantes
stargazer(base_completa)

mean(base_completa$reincidente)
sd(base_completa$reincidente)

mean(base_completa$incumbente)
sd(base_completa$incumbente)

mean(base_completa$trocou_partido)
sd(base_completa$trocou_partido)

mean(base_completa$ordem_eleicao)
sd(base_completa$ordem_eleicao)

#Matriz de correlação
dados_cor <- base_completa %>% 
  select(reincidente, incumbente, trocou_partido, VR_DESPESA_CONTRATADA, governo, espectro, pc_votos_nominais, pc_votos_totais) 

res <- rcorr(as.matrix(dados_cor), type = "pearson")
correlacoes <- res$r
pvalores <- res$P

corrplot(
  correlacoes,
  method = "color",            # tipo do gráfico: cor
  type = "upper",              # mostra só metade superior da matriz
  order = "hclust",            # ordenação dos eixos por agrupamento hierárquico
  addCoef.col = "black",
  number.cex = 0.7,     # <- tamanho dos valores de correlação
  tl.cex = 0.7, # adiciona os valores de correlação
  tl.col = "black",            # cor dos nomes das variáveis
  tl.srt = 45,                 # rotação dos nomes
  p.mat = pvalores,            # matriz de p-valores
  sig.level = 0.05,            # nível de significância
  insig = "blank",             # esconde correlações não significantes
  col = colorRampPalette(c("red", "white", "blue"))(200)  # paleta de cores
)

# Calcular média e desvio padrão por ano
resumo <- base_completa %>%
  group_by(aa_eleicao) %>%
  summarise(
    sd_reincidente = sd(reincidente, na.rm = TRUE),
    sd_incumbente = sd(incumbente, na.rm = TRUE),
    sd_trocou_partido = sd(trocou_partido, na.rm = TRUE),
    sd_VR_DESPESA_CONTRATADA = sd(VR_DESPESA_CONTRATADA, na.rm = TRUE),
    sd_governo = sd(governo, na.rm = TRUE),
    sd_espectro = sd(espectro, na.rm = TRUE),
    candidato_reincidente = mean(reincidente, na.rm = TRUE),
    incumbente = mean(incumbente, na.rm = TRUE),
    trocou_partido = mean(trocou_partido, na.rm = TRUE),
    VR_DESPESA_CONTRATADA = mean(VR_DESPESA_CONTRATADA, na.rm = TRUE),
    governo = mean(governo, na.rm = TRUE),
    espectro = mean(espectro, na.rm = TRUE),
    total = n()
  )

## Gráficos
#Total de candidatos
p1 <- ggplot(resumo, aes(x = aa_eleicao, y = total)) +
  geom_line(color = "red") +                      # linha da média
  geom_point(size = 1, color = "red") +       # pontos nas médias
  labs(
    x = "Ano",
    y = "Número de candidatos",
    title = "Total de observações"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8)
  ) +
  scale_y_continuous(limits = c(0, 10000)) +
  scale_x_continuous(breaks = seq(2010, 2022, 4))

# Reincidentes
p2 <- ggplot(resumo, aes(x = aa_eleicao, y = candidato_reincidente)) +
  geom_line(color = "blue") +                      # linha da média
  geom_point(size = 1, color = "darkblue") +       # pontos nas médias
  geom_errorbar(aes(ymin = candidato_reincidente - sd_reincidente, ymax = candidato_reincidente + sd_reincidente), 
                width = 0.2, color = "gray40") +   # barras de erro
  labs(
    x = "Ano",
    y = "Proporção",
    title = "Proporção de reincidentes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8)
  ) +
  scale_x_continuous(breaks = seq(2010, 2022, 4))

# Incumbente
p33 <- ggplot(resumo, aes(x = aa_eleicao, y = incumbente)) +
  geom_line(color = "blue") +                      # linha da média
  geom_point(size = 1, color = "darkblue") +       # pontos nas médias
  geom_errorbar(aes(ymin = incumbente - sd_incumbente, ymax = incumbente + sd_incumbente), 
                width = 0.2, color = "gray40") +   # barras de erro
  labs(
    x = "Ano",
    y = "Proporção",
    title = "Proporção de candidatos incumbentes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8)
  ) +
  scale_y_continuous(limits = c(0, 1.2)) +
  scale_x_continuous(breaks = seq(2010, 2022, 4))

# Trocou partido
p3 <- ggplot(resumo, aes(x = aa_eleicao, y = trocou_partido)) +
  geom_line(color = "blue") +                      # linha da média
  geom_point(size = 1, color = "darkblue") +       # pontos nas médias
  geom_errorbar(aes(ymin = trocou_partido - sd_trocou_partido, ymax = trocou_partido + sd_trocou_partido), 
                width = 0.2, color = "gray40") +   # barras de erro
  labs(
    x = "Ano",
    y = "Proporção",
    title = "Proporção de trocas de partido"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8)
  ) +
  scale_y_continuous(limits = c(-0.25, 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2022, 4))

# Despesa
p4 <- ggplot(resumo, aes(x = aa_eleicao, y = VR_DESPESA_CONTRATADA)) +
  geom_line(color = "blue") +                      # linha da média
  geom_point(size = 1, color = "darkblue") +       # pontos nas médias
  geom_errorbar(aes(ymin = VR_DESPESA_CONTRATADA - sd_VR_DESPESA_CONTRATADA, ymax = VR_DESPESA_CONTRATADA + sd_VR_DESPESA_CONTRATADA), 
                width = 0.2, color = "gray40") +   # barras de erro
  labs(
    x = "Ano",
    y = "Despesa (em R$)",
    title = "Despesa contratada média"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8)
  ) +
  scale_x_continuous(breaks = seq(2010, 2022, 4))


# Espectro
p5 <- ggplot(resumo, aes(x = aa_eleicao, y = espectro)) +
  geom_line(color = "blue") +                      # linha da média
  geom_point(size = 1, color = "darkblue") +       # pontos nas médias
  geom_errorbar(aes(ymin = espectro - sd_espectro, ymax = espectro + sd_espectro), 
                width = 0.2, color = "gray40") +   # barras de erro
  labs(
    x = "Ano",
    y = "Posição média",
    title = "Posição no espectro ideológico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8)
  ) +
  scale_y_continuous(limits = c(-4,4)) +
  scale_x_continuous(breaks = seq(2010, 2022, 4))

grid.arrange(p1, p2, p3, p33, p4, p5, ncol = 3)

#Histogramas

#Reincidentes
p1 <- ggplot(base_completa, aes(x = reincidente, fill = factor(trocou_partido), color = factor(trocou_partido))) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.3, bins = 30) +
  geom_density(size = 1) +
  labs(
    title = "Reincidentes",
    x = "Valor da dummy de reincidência",
    y = "Densidade",
    fill = "Troca de partido",
    color = "Troca de partido"
  ) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5),
    legend.text = element_text(size = 5),       # tamanho dos itens da legenda
    legend.title = element_text(size = 8)
  ) +
  scale_x_continuous(breaks = seq(0, 1, 2))

#Ordem da eleição
p2 <- ggplot(base_completa, aes(x = ordem_eleicao, fill = factor(trocou_partido), color = factor(trocou_partido))) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.3, bins = 30) +
  geom_density(size = 1) +
  labs(
    title = "Ordem da eleição",
    x = "Total de eleições anteriores",
    y = "Densidade",
    fill = "Troca de partido",
    color = "Troca de partido"
  ) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5)
  ) +
  scale_x_continuous(breaks = seq(0, 1, 2))

#Incumbentes
p3 <- ggplot(base_completa, aes(x = incumbente, fill = factor(trocou_partido), color = factor(trocou_partido))) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.3, bins = 30) +
  geom_density(size = 1) +
  labs(
    title = "Incumbentes",
    x = "Valor da dummy de incumbente",
    y = "Densidade",
    fill = "Troca de partido",
    color = "Troca de partido"
  ) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5)
  ) +
  scale_x_continuous(breaks = seq(0, 1, 2))

#Despesa
p4 <- ggplot(base_completa, aes(x = VR_DESPESA_CONTRATADA, color = factor(trocou_partido))) +
  geom_histogram(aes(y = ..density..), position = "identity", bins = NA, fill = NA, size = 0.8) +
  geom_density(size = 1) +
  labs(
    title = "Despesa",
    x = "Despesa contratada (em R$)",
    y = "Densidade",
    color = "Troca de partido"
  ) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5)
  )

#Governo
p5 <- ggplot(base_completa, aes(x = governo, fill = factor(trocou_partido), color = factor(trocou_partido))) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.3, bins = 30) +
  geom_density(size = 1) +
  labs(
    title = "Governo",
    x = "Valor da dummy de governo",
    y = "Densidade",
    fill = "Troca de partido",
    color = "Troca de partido"
  ) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5)
  ) +
  scale_x_continuous(breaks = seq(0, 1, 2))

#Espectro
p6 <- ggplot(base_completa, aes(x = espectro, fill = factor(trocou_partido), color = factor(trocou_partido))) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.3, bins = 30) +
  geom_density(size = 1) +
  labs(
    title = "Espectro político",
    x = "Posição no espectro político",
    y = "Densidade",
    fill = "Troca de partido",
    color = "Troca de partido"
  ) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5)
  ) +
  scale_x_continuous(breaks = seq(0, 1, 2))

#Porcentagem de votos
p7 <- ggplot(base_completa, aes(x = pc_votos_nominais, color = factor(trocou_partido))) +
  geom_histogram(aes(y = ..density..), position = "identity", bins = NA, fill = NA, size = 0.8) +
  geom_density(size = 1) +
  labs(
    title = "Votos nominais",
    x = "Porcentagem de votos nominais",
    y = "Densidade",
    color = "Troca de partido"
  ) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5)
  )

p8 <- ggplot(base_completa, aes(x = pc_votos_totais, color = factor(trocou_partido))) +
  geom_histogram(aes(y = ..density..), position = "identity", bins = NA, fill = NA, size = 0.8) +
  geom_density(size = 1) +
  labs(
    title = "Votos totais",
    x = "Porcentagem dos votos totais",
    y = "Densidade",
    color = "Troca de partido"
  ) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5)
  )

# Extraindo legenda
legenda <- get_legend(p1)

# Criando gráficos sem legenda
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
p4 <- p4 + theme(legend.position = "none")
p5 <- p5 + theme(legend.position = "none")
p6 <- p6 + theme(legend.position = "none")
p7 <- p7 + theme(legend.position = "none")
p8 <- p8 + theme(legend.position = "none")

# Arranjando os gráficos com a legenda única
grid.arrange(
  arrangeGrob(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4),
  legenda,
  ncol = 2,
  widths = c(6, 1)  
)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)

#Gráficos complementares

# Identificar ano em que a dummy virou 1
base_classificada <- base_completa %>%
  group_by(nr_cpf_candidato) %>%
  mutate(ano_dummy_1 = ifelse(any(trocou_partido == 1),
                              min(aa_eleicao[trocou_partido == 1], na.rm = TRUE),
                              NA)) %>%
  ungroup() %>%
  mutate(periodo = case_when(
    is.na(ano_dummy_1) ~ NA_character_,
    aa_eleicao < ano_dummy_1 ~ "Antes",
    aa_eleicao >= ano_dummy_1 ~ "Depois"
  ))

medias_detalhadas <- base_classificada %>%
 summarise(
    despesa_antes = mean(VR_DESPESA_CONTRATADA[periodo == "Antes"], na.rm = TRUE),
    despesa_depois = mean(VR_DESPESA_CONTRATADA[periodo == "Depois"], na.rm = TRUE),
    governo_antes = mean(governo[periodo == "Antes"], na.rm = TRUE),
    governo_depois = mean(governo[periodo == "Depois"], na.rm = TRUE),
    espectro_antes = mean(espectro[periodo == "Antes"], na.rm = TRUE),
    espectro_depois = mean(espectro[periodo == "Depois"], na.rm = TRUE),
    voto_antes = mean(pc_votos_nominais[periodo == "Antes"], na.rm = TRUE),
    voto_depois = mean(pc_votos_nominais[periodo == "Depois"], na.rm = TRUE),
    min_despesa = min(VR_DESPESA_CONTRATADA, na.rm = TRUE),
    max_despesa = max(VR_DESPESA_CONTRATADA, na.rm = TRUE),
    .groups = "drop"
  )

t.test(VR_DESPESA_CONTRATADA ~ periodo, data = base_classificada)
t.test(governo ~ periodo, data = base_classificada)
t.test(espectro ~ periodo, data = base_classificada)
t.test(pc_votos_nominais ~ periodo, data = base_classificada)

medias_long <- rbind(
  data.frame(
    variavel = c("Despesa", "Governo", "Espectro", "Voto"),
    media = c(
      medias_detalhadas$despesa_antes,
      medias_detalhadas$governo_antes,
      medias_detalhadas$espectro_antes,
      medias_detalhadas$voto_antes
    ),
    min = c(
      medias_detalhadas$min_despesa,
      0, -4, 0
    ),
    max = c(
      medias_detalhadas$max_despesa,
      1, 4, 0.1
    ),
    periodo = "Antes"
  ),
  data.frame(
    variavel = c("Despesa", "Governo", "Espectro", "Voto"),
    media = c(
      medias_detalhadas$despesa_depois,
      medias_detalhadas$governo_depois,
      medias_detalhadas$espectro_depois,
      medias_detalhadas$voto_depois
    ),
    min = c(
      medias_detalhadas$min_despesa,
      0, -4, 0
    ),
    max = c(
      medias_detalhadas$max_despesa,
      1, 4, 0.1
    ),
    periodo = "Depois"
  )
)

ggplot(medias_long, aes(y = periodo)) +
  geom_linerange(aes(xmin = min, xmax = max), size = 4, color = "gray80") +
  geom_point(aes(x = media, color = periodo), size = 3) +
  facet_wrap(~ variavel, scales = "free_x") +
  scale_color_manual(values = c("Antes" = "steelblue", "Depois" = "firebrick")) +
  labs(
    x = "Valor médio",
    y = "Período",
    color = "Período",
    title = "Médias antes e depois da troca de partido"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top"
  )


# Regressões com dados em painel -----------------------------------

# Com votos nominais
r1 = plm(pc_votos_nominais ~ reincidente + governo + trocou_partido +
           incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "pooling")

r2 = plm(pc_votos_nominais ~ reincidente + governo + trocou_partido + 
           incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "within")

r3 = plm(pc_votos_nominais ~ reincidente + governo + trocou_partido + 
           incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "fd")

r4 = plm(pc_votos_nominais ~ reincidente + governo + trocou_partido + 
           incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "random")

# Com votos totais
r5 = plm(pc_votos_totais ~ reincidente + governo + trocou_partido + 
           incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "pooling")

r6 = plm(pc_votos_totais ~ reincidente + governo + trocou_partido + 
           incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "within")

r7 = plm(pc_votos_totais ~ reincidente + governo + trocou_partido + 
           incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "fd")

r8 = plm(pc_votos_totais ~ reincidente + governo + trocou_partido + 
           incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "random")

# Resultados e Testes Robustez -----------------------------------

summary(r1)
summary(r2)
summary(r3)
summary(r3)
summary(r4)
summary(r5)
summary(r6)
summary(r7)
summary(r8)

# Erros robustos heterocedasticidade
erros_robustos1 <- vcovHC(r1, type = "HC0")
erros_robustos2 <- vcovHC(r2, type = "HC0")
erros_robustos3 <- vcovHC(r3, type = "HC0", method = "arellano", cluster = "time")
erros_robustos4 <- vcovHC(r4, type = "HC0")
erros_robustos5 <- vcovHC(r5, type = "HC0")
erros_robustos6 <- vcovHC(r6, type = "HC0")
erros_robustos7 <- vcovHC(r7, type = "HC0", method = "arellano", cluster = "time")
erros_robustos8 <- vcovHC(r8, type = "HC0")

# Teste dos coeficientes com erros robustos
coeftest(r1, vcov = erros_robustos1)
coeftest(r2, vcov = erros_robustos2)
coeftest(r3, vcov = erros_robustos3)
coeftest(r4, vcov = erros_robustos4)
coeftest(r5, vcov = erros_robustos5)
coeftest(r6, vcov = erros_robustos6)
coeftest(r7, vcov = erros_robustos7)
coeftest(r8, vcov = erros_robustos8)

# Teste de Autocorrelação Serial (Wooldridge)
pwartest(r2)
pwartest(r6)

# Teste de Hausman
phtest(r2, r4)
phtest(r6, r8)

# Probit
probit1 <- glm(pc_votos_nominais ~ reincidente + governo + trocou_partido +
                 incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA,
               family = binomial(link = "probit"),
               data = base_completa
)

probit2 <- glm(
  pc_votos_totais ~ reincidente + governo + trocou_partido +
    incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro,
  family = binomial(link = "probit"),
  data = base_completa
)

# Logit
logit1 <- glm(pc_votos_nominais ~ reincidente + governo + trocou_partido +
                incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA,
              family = binomial(link = "logit"),
              data = base_completa
)

logit2 <- glm(
  pc_votos_totais ~ reincidente + governo + trocou_partido +
    incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro,
  family = binomial(link = "logit"),
  data = base_completa
)

# Teste de Robustez Probit e Logit
# Probit models with robust standard errors
erros_robustos_probit1 <- vcovHC(probit1, type = "HC0")
tp1 <- coeftest(probit1, vcov = erros_robustos_probit1)

erros_robustos_probit2 <- vcovHC(probit2, type = "HC0")
coeftest(probit2, vcov = erros_robustos_probit2)

# Logit models with robust standard errors
erros_robustos_logit1 <- vcovHC(logit1, type = "HC0")
tl1 <- coeftest(logit1, vcov = erros_robustos_logit1)

erros_robustos_logit2 <- vcovHC(logit2, type = "HC0")
coeftest(logit2, vcov = erros_robustos_logit2)

#teste variável despesa
r_sem = plm(pc_votos_totais ~ reincidente + governo + trocou_partido + 
           incumbente + as.factor(aa_eleicao) + as.factor(aa_eleicao)*espectro, 
         data = base_completa, index = c("nr_cpf_candidato", "aa_eleicao"), model = "within")
erros_robustos_sem <- vcovHC(r_sem, type = "HC0")
coeftest(r_sem, vcov = erros_robustos_sem)

teste <- base_completa
teste$VR_DESPESA_CONTRATADA <- ifelse(is.na(teste$VR_DESPESA_CONTRATADA), 0, teste$VR_DESPESA_CONTRATADA)
r0 = plm(pc_votos_totais ~ reincidente + governo + trocou_partido + 
              incumbente + as.factor(aa_eleicao) + VR_DESPESA_CONTRATADA + as.factor(aa_eleicao)*espectro, 
            data = teste, index = c("nr_cpf_candidato", "aa_eleicao"), model = "within")
erros_robustos_0 <- vcovHC(r0, type = "HC0")
coeftest(r0, vcov = erros_robustos_0)

# Tabelas -----------------------------------
# Em texto
stargazer(r1, r2, r3, r4, type = "text",
          se=list(erros_robustos1, erros_robustos2, erros_robustos3, erros_robustos4))
stargazer(r5, r6, r7, r8, type = "text",
          se=list(erros_robustos5, erros_robustos6, erros_robustos7, erros_robustos8))
stargazer(r2, r6, type = "text",
          se=list(erros_robustos2, erros_robustos6))
stargazer(t2, tp1, tl1, type = "text")