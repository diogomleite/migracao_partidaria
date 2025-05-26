library(dplyr)
library(readr)

## Compilar dados para 2010 (divulgado em arquivos separados por UF)
# Caminho da pasta principal
pasta_principal <- "C:\\Users\\Diogo\\Downloads\\prestacao_contas_2010\\candidato"  # <- substitua aqui

# Nome do arquivo que está presente em todas as subpastas
nome_arquivo <- "DespesasCandidatos.txt"

# Lista todos os arquivos com esse nome nas subpastas
arquivos <- list.files(path = pasta_principal, pattern = nome_arquivo,
                       recursive = TRUE, full.names = TRUE)

# Lista para armazenar os data frames
lista_bases <- list()

# Loop para ler e armazenar cada arquivo
for (i in seq_along(arquivos)) {
  caminho <- arquivos[i]
  estado <- basename(dirname(caminho))  # nome da subpasta como identificador
  
  # Lê o arquivo (ajuste o separador se necessário)
  df <- read_delim(caminho, delim = ";", locale = locale(encoding = "latin1", decimal_mark = ","), show_col_types = FALSE)
  
  # Adiciona a coluna "estado"
  df$estado <- estado
  
  # Adiciona à lista
  lista_bases[[i]] <- df
}

# Junta todos os data frames
base_unificada <- bind_rows(lista_bases)

summary(base_unificada)
base_unificada$aa_eleicao = 2010
e2010 <- base_unificada %>%
  select(`Número candidato`, UF, Cargo, `Nome candidato`, `CPF do candidato`, `Valor despesa`, `Sequencial Candidato`, aa_eleicao)

e2010 <- e2010 %>%
  rename(
    "DS_CARGO" = Cargo, 
    "NM_CANDIDATO" = `Nome candidato`, 
    "NR_CPF_CANDIDATO" = `CPF do candidato`, 
    "VR_DESPESA_CONTRATADA" = `Valor despesa`, 
    "SQ_CANDIDATO" = `Sequencial Candidato`,
    "NR_CANDIDATO" = `Número candidato`,
    "SG_UF" = UF
  )

write.csv(e2010, "base_unificada.txt", row.names = FALSE)

# #abrindo bases # <- substitua aqui

e2022 = read.csv2("C:/Users/Diogo/Downloads/despesas_contratadas_candidatos_2022_BRASIL.csv", fileEncoding = "latin1")
e2018 = read.csv2("C:/Users/Diogo/Downloads/despesas_contratadas_candidatos_2018_BRASIL.csv", fileEncoding = "latin1")
e2014 <- read_delim("C:/Users/Diogo/Downloads/despesas_candidatos_2014_brasil.txt", delim = ";", locale = locale(encoding = "latin1", decimal_mark = ","), show_col_types = FALSE)
e2010 <- read.csv("C:\\Users\\Diogo\\Documents\\base_unificada.txt", header=TRUE, sep=",")


e2022$aa_eleicao = 2022
e2018$aa_eleicao = 2018
e2014$aa_eleicao = 2014

e2022 <- e2022 %>%
  select(SG_UF, NR_CANDIDATO, DS_CARGO, NM_CANDIDATO, NR_CPF_CANDIDATO, SQ_CANDIDATO, VR_DESPESA_CONTRATADA, aa_eleicao)

e2018 <- e2018 %>%
  select(SG_UF, DS_CARGO, NR_CANDIDATO, NM_CANDIDATO, NR_CPF_CANDIDATO, SQ_CANDIDATO, VR_DESPESA_CONTRATADA, aa_eleicao)

e2014 <- e2014 %>%
  select(`Número candidato`, UF, `CPF do candidato`, `Sequencial Candidato`, `Nome candidato`, Cargo, `Valor despesa`, aa_eleicao)

e2014 <- e2014 %>%
  rename(
    "DS_CARGO" = Cargo, 
    "NM_CANDIDATO" = `Nome candidato`, 
    "NR_CPF_CANDIDATO" = `CPF do candidato`, 
    "SQ_CANDIDATO" = `Sequencial Candidato`, 
    "VR_DESPESA_CONTRATADA" = `Valor despesa`,
    "NR_CANDIDATO" = `Número candidato`,
    "SG_UF" = UF
  )

e2010$NR_CPF_CANDIDATO <- as.numeric(e2010$NR_CPF_CANDIDATO)
e2014$NR_CPF_CANDIDATO <- as.numeric(e2014$NR_CPF_CANDIDATO)
e2010$VR_DESPESA_CONTRATADA <- as.numeric(e2010$VR_DESPESA_CONTRATADA)
e2014$VR_DESPESA_CONTRATADA <- as.numeric(e2014$VR_DESPESA_CONTRATADA)
e2010$SQ_CANDIDATO <- as.numeric(e2010$SQ_CANDIDATO)
e2014$SQ_CANDIDATO <- as.numeric(e2014$SQ_CANDIDATO)

juntada = bind_rows(e2022, e2018, e2014, e2010)

#agregar por candidato

juntada %>%
  group_by(SQ_CANDIDATO, aa_eleicao) %>%
  summarise(
    n_cargos = n_distinct(DS_CARGO),
    n_numeros = n_distinct(NR_CANDIDATO),
    .groups = "drop"
  ) %>% filter(n_cargos > 1 | n_numeros > 1)

dados_resumidos <- juntada %>%
  group_by(SQ_CANDIDATO, aa_eleicao) %>%
  summarise(
    VR_DESPESA_CONTRATADA = sum(VR_DESPESA_CONTRATADA, na.rm = TRUE),
    NR_CPF_CANDIDATO = first(NR_CPF_CANDIDATO),
    DS_CARGO = first(DS_CARGO),
    SG_UF = first(SG_UF),
    NR_CANDIDATO = first(NR_CANDIDATO),
    NM_CANDIDATO = first(NM_CANDIDATO),  # se ainda quiser o nome do candidato
    .groups = "drop"
  )

summary(dados_resumidos)

#fazer correção inflacionária (calculada com base no IPCA na calculadora do BCB)

dados_resumidos$VR_DESPESA_CONTRATADA <- ifelse(dados_resumidos$aa_eleicao == 2010, 2.02*dados_resumidos$VR_DESPESA_CONTRATADA, dados_resumidos$VR_DESPESA_CONTRATADA)
dados_resumidos$VR_DESPESA_CONTRATADA <- ifelse(dados_resumidos$aa_eleicao == 2014, 1.61*dados_resumidos$VR_DESPESA_CONTRATADA, dados_resumidos$VR_DESPESA_CONTRATADA)
dados_resumidos$VR_DESPESA_CONTRATADA <- ifelse(dados_resumidos$aa_eleicao == 2018, 1.24*dados_resumidos$VR_DESPESA_CONTRATADA, dados_resumidos$VR_DESPESA_CONTRATADA)

#juntar com a outra base

dados_resumidos <- dados_resumidos %>%
  rename(
    "nr_cpf_candidato" = NR_CPF_CANDIDATO,
    "nm_candidato" = NM_CANDIDATO,
    "sq_candidato" = SQ_CANDIDATO,
    "nr_candidato" = NR_CANDIDATO,
    "sg_uf" = SG_UF
  )

## Criando base com candidatos de todas as eleições marcando migração partidária

#abrindo bases
  e2022 = read.csv2("votacao_candidato-uf_deputado_federal_2022.csv", fileEncoding = "latin1")
  e2018 = read.csv2("votacao_candidato-uf_deputado_federal_2018.csv", fileEncoding = "latin1")
  e2014 = read.csv2("votacao_candidato-uf_deputado_federal_2014.csv", fileEncoding = "latin1")
  e2010 = read.csv2("votacao_candidato-uf_deputado_federal_2010.csv", fileEncoding = "latin1")
 
  e2022 = e2022[, c("sg_uf", "nm_candidato", "sq_candidato", "nm_urna_candidato", "sg_partido", "ds_sit_totalizacao", "pc_votos_validos","qt_votos_nom_validos","aa_eleicao")]
  e2018 = e2018[, c("sg_uf", "nm_candidato", "sq_candidato", "nm_urna_candidato", "sg_partido", "ds_sit_totalizacao", "pc_votos_validos","qt_votos_nom_validos","aa_eleicao")]
  e2014 = e2014[, c("sg_uf", "nm_candidato", "sq_candidato", "nm_urna_candidato", "sg_partido", "ds_sit_totalizacao", "pc_votos_validos","qt_votos_nom_validos","aa_eleicao")]
  e2010 = e2010[, c("sg_uf", "nm_candidato", "sq_candidato", "nm_urna_candidato", "sg_partido", "ds_sit_totalizacao", "pc_votos_validos","qt_votos_nom_validos","aa_eleicao")]
 
  e2022$aa_eleicao = 2022
  e2018$aa_eleicao = 2018
  e2014$aa_eleicao = 2014
  e2010$aa_eleicao = 2010
 
  cpf2010 = read.csv2("consulta_cand_2010_BRASIL.csv", fileEncoding = "latin1")
  cpf2010 = cpf2010 %>% filter(CD_CARGO==6)
  cpf2010 = cpf2010 %>% select(SQ_CANDIDATO, NR_CPF_CANDIDATO)
  names(cpf2010)
  cpf2010 = cpf2010 %>% rename(sq_candidato = SQ_CANDIDATO, nr_cpf_candidato = NR_CPF_CANDIDATO)
  e2010 = e2010 %>%
    left_join(cpf2010, by = "sq_candidato")
 
  cpf2014 = read.csv2("consulta_cand_2014_BRASIL.csv", fileEncoding = "latin1")
  cpf2014 = cpf2014 %>% filter(CD_CARGO==6)
  cpf2014 = cpf2014 %>% select(SQ_CANDIDATO, NR_CPF_CANDIDATO)
  names(cpf2014)
  cpf2014 = cpf2014 %>% rename(sq_candidato = SQ_CANDIDATO, nr_cpf_candidato = NR_CPF_CANDIDATO)
  e2014 = e2014 %>%
    left_join(cpf2014, by = "sq_candidato")
 
  cpf2018 = read.csv2("consulta_cand_2018_BRASIL.csv", fileEncoding = "latin1")
  cpf2018 = cpf2018 %>% filter(CD_CARGO==6)
  cpf2018 = cpf2018 %>% select(SQ_CANDIDATO, NR_CPF_CANDIDATO)
  names(cpf2018)
  cpf2018 = cpf2018 %>% rename(sq_candidato = SQ_CANDIDATO, nr_cpf_candidato = NR_CPF_CANDIDATO)
  e2018 = e2018 %>%
    left_join(cpf2018, by = "sq_candidato")
 
  cpf2022 = read.csv2("consulta_cand_2022_BRASIL.csv", fileEncoding = "latin1")
  cpf2022 = cpf2022 %>% filter(CD_CARGO==6)
  cpf2022 = cpf2022 %>% select(SQ_CANDIDATO, NR_CPF_CANDIDATO)
  names(cpf2022)
  cpf2022 = cpf2022 %>% rename(sq_candidato = SQ_CANDIDATO, nr_cpf_candidato = NR_CPF_CANDIDATO)
  e2022 = e2022 %>%
    left_join(cpf2022, by = "sq_candidato")

 base_completa = bind_rows(e2022, e2018, e2014, e2010)
 write.csv(base_completa, "base_final.csv")

 base_completa = read.csv("base_final.csv")

 base_completa$nr_cpf_candidato[1604] = -2

  candidatos_freq = base_completa %>%
    arrange(nr_cpf_candidato, aa_eleicao) %>%
    group_by(nr_cpf_candidato) %>%
    mutate(
      ordem_eleicao = row_number(),
      candidato_reincidente = if_else(ordem_eleicao > 1, 1L, 0L)
    ) %>%
    ungroup()
 
  base_completa = candidatos_freq
 
# criando a dummy
  partido_candidato = base_completa %>%
   select(nr_cpf_candidato, aa_eleicao, sg_partido) %>%
   distinct() %>%
   arrange(nr_cpf_candidato, aa_eleicao) %>%
   group_by(nr_cpf_candidato) %>%
   mutate(
     partido_anterior = dplyr::lag(sg_partido),
     trocou_partido = if_else(sg_partido != partido_anterior & !is.na(partido_anterior), 1L, 0L)
   ) %>%
   ungroup()

  base_completa = base_completa %>%
   left_join(
     partido_candidato %>%
       select(nr_cpf_candidato, aa_eleicao, trocou_partido),
     by = c("nr_cpf_candidato", "aa_eleicao")
   ) %>%
   mutate(trocou_partido = ifelse(is.na(trocou_partido), 0L, trocou_partido))
 
  incumbencia_df = base_completa %>%
   select(nr_cpf_candidato, aa_eleicao, ds_sit_totalizacao) %>%
   distinct() %>%
   arrange(nr_cpf_candidato, aa_eleicao) %>%
   group_by(nr_cpf_candidato) %>%
   mutate(
     eleito_ano_passado = dplyr::lag(str_detect(ds_sit_totalizacao, "Eleito")),
     incumbente = if_else(eleito_ano_passado, 1L, 0L, missing = 0L)
   ) %>%
   ungroup()
 

  base_completa = base_completa %>%
    left_join(
      incumbencia_df %>%
        select(nr_cpf_candidato, aa_eleicao, incumbente),
      by = c("nr_cpf_candidato", "aa_eleicao")
    ) %>%
    mutate(incumbente = if_else(is.na(incumbente), 0L, incumbente))
 
  write.csv(base_completa, "base_final3.csv")

base_completa = read.csv("C:\\Users\\Diogo\\Downloads\\agregados.csv") # <- substitua aqui

base_final <- base_completa %>%
  left_join(
    dados_resumidos %>%
      select(sq_candidato, aa_eleicao, VR_DESPESA_CONTRATADA, DS_CARGO, nr_candidato),
    by = c("sq_candidato", "aa_eleicao")
  )

teste <- base_final %>%
  left_join(
    dados_resumidos %>%
      select(nr_cpf_candidato, aa_eleicao, VR_DESPESA_CONTRATADA, DS_CARGO, nr_candidato),
    by = c("nr_cpf_candidato", "aa_eleicao")
  )

teste <- teste %>%
  left_join(
    dados_resumidos %>%
      select(nm_candidato, aa_eleicao, VR_DESPESA_CONTRATADA, DS_CARGO, nr_candidato),
    by = c("nm_candidato", "aa_eleicao")
  )

NAs <- subset(base_final, is.na(base_final$VR_DESPESA_CONTRATADA))

base_final <- teste %>%
  mutate(VR_DESPESA_CONTRATADA = coalesce(VR_DESPESA_CONTRATADA.x, VR_DESPESA_CONTRATADA.y, VR_DESPESA_CONTRATADA))


base_final$VR_DESPESA_CONTRATADA <- as.numeric(base_final$VR_DESPESA_CONTRATADA)

write.csv(base_final, "base_final2.csv", row.names = FALSE)

base_completa = read.csv("C:\\Users\\Diogo\\Documents\\base_final.csv")

#Criando dummy de governo/oposição

base_completa <- base_completa %>%
  mutate(governo = case_when(
    aa_eleicao == 2010 & sg_partido %in% c('PT', 'PC do B', 'PMDB', 'PDT', 'PRB', 'PR', 'PSB', 'PSC', 'PTC', 'PTN', 'PP') ~ 1,
    aa_eleicao == 2014 & sg_partido %in% c('PT', 'PC do B', 'PMDB', 'PDT', 'PRB', 'PR', 'PROS', 'PSD', 'PP') ~ 1,
    aa_eleicao == 2018 & sg_partido %in% c('PSL', 'PRTB') ~ 1,
    aa_eleicao == 2022 & sg_partido %in% c('PT', 'PC do B', 'PV', 'PSB', 'PSOL', 'REDE', 'SOLIDARIEDADE', 'PROS', 'AVANTE', 'AGIR') ~ 1,
    TRUE ~ 0
  ))

#Criando variável de espectro político (baseado no GPS Folha)
#-4 esquerda até 4 direita

base_completa <- base_completa %>%
  mutate(espectro = case_when(
    sg_partido=='PSTU' ~ -4,
    sg_partido=='UP' ~ -4,
    sg_partido=='PSOL' ~ -4,
    sg_partido=='PCO' ~ -4,
    sg_partido=='PCB' ~ -3,
    sg_partido=='PT' ~ -3,
    sg_partido=='PC do B' ~ -3,
    sg_partido=='PPL' ~ -3,
    sg_partido=='REDE' ~ -3,
    sg_partido=='PV' ~ -2,
    sg_partido=='PSB' ~ -2,
    sg_partido=='PDT' ~ -2,
    sg_partido=='SOLIDARIEDADE' ~ -1,
    sg_partido=='PROS' ~ -1,
    sg_partido=='SD' ~ -1,
    sg_partido=='AVANTE' ~ -1,
    sg_partido=='PT do B' ~ -1,
    sg_partido=='PSD' ~ 0,
    sg_partido=='MDB' ~ 0,
    sg_partido=='PMDB' ~ 0,
    sg_partido=='MOBILIZA' ~ 0,
    sg_partido=='PMN' ~ 0,
    sg_partido=='PMB' ~ 1,
    sg_partido=='PODE' ~ 1,
    sg_partido=='PSC' ~ 1,
    sg_partido=='PHS' ~ 1,
    sg_partido=='PTN' ~ 1,
    sg_partido=='PP' ~ 1,
    sg_partido=='AGIR' ~ 1,
    sg_partido=='PTC' ~ 1,
    sg_partido=='REPUBLICANOS' ~ 2,
    sg_partido=='PRB' ~ 2,
    sg_partido=='CIDADANIA' ~ 2,
    sg_partido=='PPS' ~ 2,
    sg_partido=='PSDB' ~ 2,
    sg_partido=='UNIÃO' ~ 3,
    sg_partido=='PSL' ~ 3,
    sg_partido=='DEM' ~ 3,
    sg_partido=='DC' ~ 3,
    sg_partido=='PSDC' ~ 3,
    sg_partido=='PRD' ~ 3,
    sg_partido=='PEN' ~ 3,
    sg_partido=='PATRIOTA' ~ 3,
    sg_partido=='PRP' ~ 3,
    sg_partido=='PTB' ~ 3,
    sg_partido=='PRD' ~ 3,
    sg_partido=='PATRI' ~ 3,
    sg_partido=='PRTB' ~ 4,
    sg_partido=='PL' ~ 4,
    sg_partido=='PR' ~ 4,
    sg_partido=='NOVO' ~ 4
  ))

unique(base_completa$sg_partido)

#Ajustando pc_validos
e2010 <- read.csv("C:\\Users\\Diogo\\Downloads\\detalhe_votacao.csv\\2010.csv", sep=';', encoding = 'latin1')
e2014 <- read.csv("C:\\Users\\Diogo\\Downloads\\detalhe_votacao.csv\\2014.csv", sep=';', encoding = 'latin1')
e2018 <- read.csv("C:\\Users\\Diogo\\Downloads\\detalhe_votacao.csv\\2018.csv", sep=';', encoding = 'latin1')
e2022 <- read.csv("C:\\Users\\Diogo\\Downloads\\detalhe_votacao.csv\\2022.csv", sep=';', encoding = 'latin1')

e2010$aa_eleicao <-2010
e2014$aa_eleicao <-2014
e2018$aa_eleicao <-2018
e2022$aa_eleicao <-2022

qt_votos <- bind_rows(e2010, e2014, e2018, e2022)
qt_votos$DS_CARGO <- qt_votos$Cargo
qt_votos$sg_uf <- qt_votos$UF

qt_votos <- subset(qt_votos, qt_votos$Cargo == "Deputado Federal")

library(dplyr)

base_completa <- base_completa %>%
  left_join(
    qt_votos %>% select(aa_eleicao, sg_uf, Quantidade.de.votos.nominais.válidos, Quantidade.de.votos.totais),
    by = c("aa_eleicao", "sg_uf")
  )

base_completa$pc_votos_nominais <- base_completa$qt_votos_nom_validos/base_completa$Quantidade.de.votos.nominais.válidos
base_completa$pc_votos_totais <- base_completa$qt_votos_nom_validos/base_completa$Quantidade.de.votos.totais

base_completa <- base_completa %>%
  select(-pc_votos_validos)

#Removendo valores duplicados
base_completa <- base_completa %>%
  distinct(aa_eleicao, nr_cpf_candidato, .keep_all = TRUE)



#Resolvendo questão dos incumbentes, reincidentes e troca de partido
e2006 <- read.csv("C:\\Users\\Diogo\\Downloads\\consulta_cand_2006\\consulta_cand_2006_BRASIL.csv", sep=';', encoding='latin1')

cpfs_restritos <- unique(e2006$NR_CPF_CANDIDATO)

# Atualiza a dummy na base principal
base_completa <- base_completa %>%
  arrange(nr_cpf_candidato, aa_eleicao) %>%
  group_by(nr_cpf_candidato) %>%
  mutate(reincidente = if_else(row_number() == 1, 0, 1)) %>%
  ungroup()

base_completa <- base_completa %>%
  mutate(
    reincidente = case_when(
      aa_eleicao == 2010 & nr_cpf_candidato %in% e2006$NR_CPF_CANDIDATO ~ 1,
      aa_eleicao == 2010 & !(nr_cpf_candidato %in% e2006$NR_CPF_CANDIDATO) ~ 0,
      TRUE ~ reincidente
    )
  )

base_completa <- base_completa %>%
  arrange(nr_cpf_candidato, aa_eleicao) %>%
  group_by(nr_cpf_candidato) %>%
  mutate(ordem_eleicao = cumsum(reincidente)) %>%
  ungroup()

# 1. Base auxiliar da própria base (exceto 2010)
eleitos_anteriores <- base_completa %>%
  select(nr_cpf_candidato, aa_eleicao, ds_sit_totalizacao) %>%
  mutate(aa_eleicao = aa_eleicao + 4) %>%  # Avança o ano para alinhar com o ano seguinte
  mutate(eleito_ano_anterior = ifelse(ds_sit_totalizacao == "Eleito", 1, 0)) %>%
  select(nr_cpf_candidato, aa_eleicao, eleito_ano_anterior)

# 2. Base auxiliar da base especial de 2010 (assumindo df_2010 tem os mesmos nomes de colunas)
eleitos_2010 <- e2006 %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  mutate(aa_eleicao = 2010) %>%  # Para ser checado em 2011
  mutate(eleito_ano_anterior = 1) %>%
  select(NR_CPF_CANDIDATO, aa_eleicao, eleito_ano_anterior)
eleitos_2010$nr_cpf_candidato <- eleitos_2010$NR_CPF_CANDIDATO

# 3. Junta tudo em uma única base auxiliar

eleitos_ano_anterior <- bind_rows(eleitos_anteriores, eleitos_2010)

# 4. Junta com a base principal
base_completa <- base_completa %>%
  left_join(eleitos_ano_anterior, by = c("nr_cpf_candidato", "aa_eleicao")) %>%
  mutate(eleito_ano_anterior = ifelse(is.na(eleito_ano_anterior), 0, eleito_ano_anterior),
         incumbente = ifelse(eleito_ano_anterior == 1, 0, 1))


# 1. Base com partidos do ano anterior (exceto 2010)
base_anterior <- base_completa %>%
  select(nr_cpf_candidato, aa_eleicao, sg_partido) %>%
  mutate(aa_eleicao = aa_eleicao + 4) %>%
  rename(SG_PARTIDO_ano_anterior = sg_partido)

# 2. Base com dados de 2006 para comparar com 2010
base_2006_prepared <- e2006 %>%
  select(NR_CPF_CANDIDATO, SG_PARTIDO) %>%
  mutate(aa_eleicao = 2010) %>%
  rename(SG_PARTIDO_ano_anterior = SG_PARTIDO)
base_2006_prepared$nr_cpf_candidato <- base_2006_prepared$NR_CPF_CANDIDATO

juntada <- bind_rows(base_anterior, base_2006_prepared)
# 3. Junta tudo na base principal
base_completa <- base_completa %>%
  left_join(juntada,
            by = c("nr_cpf_candidato", "aa_eleicao")) %>%
  mutate(trocou_partido = ifelse(sg_partido != SG_PARTIDO_ano_anterior, 1, 0)
  )

base_completa$trocou_partido <- ifelse(is.na(base_completa$trocou_partido), 0, base_completa$trocou_partido)

base_completa <- base_completa %>% select(-NR_CPF_CANDIDATO.x, -NR_CPF_CANDIDATO.y, -eleito_ano_anterior, -SG_PARTIDO_ano_anterior, -candidato_reincidente)

#escrevendo base final

readr::write_csv(base_completa, "base_final.csv")
