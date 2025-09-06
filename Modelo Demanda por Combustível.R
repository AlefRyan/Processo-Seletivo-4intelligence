# =========================================================
# PACOTES
# =========================================================
library(tidyverse)
library(sidrar)
library(plm)
library(lmtest)
library(ipeadatar)

# =========================================================
# DADOS ANP: VENDAS (diesel, gasolina, etanol)
# =========================================================
diesel   = readxl::read_excel("dados/vendas_distribuidoras_anp 1 (2).xlsx", sheet = "diesel")
gasolina = readxl::read_excel("dados/vendas_distribuidoras_anp 1 (2).xlsx", sheet = "gasolina")
etanol   = readxl::read_excel("dados/vendas_distribuidoras_anp 1 (2).xlsx", sheet = "etanol")

# Identificador do tipo de combustível
diesel$tipo   = "diesel"
gasolina$tipo = "gasolina"
etanol$tipo   = "etanol"

# Consolidando em um único data.frame
dados <- rbind(diesel, gasolina, etanol)

# =========================================================
# ANP: ORGANIZAÇÃO (wide -> long; renomeios)
# =========================================================
dados <- dados |>
  pivot_longer(
    cols      = starts_with("20"),
    names_to  = "ano",
    values_to = "Volume"
  )

dados <- dados |>
  rename(uf_sigla = regiao, Ano = ano)

# =========================================================
# SIDRA: POPULAÇÃO (6579)
# =========================================================
info_sidra(6579)

pop_uf <- get_sidra(
  6579,
  geo    = "State",
  period = "all"
) |>
  select(
    uf = `Unidade da Federação`,
    Ano,
    pop = Valor
  )

# =========================================================
# SIDRA: SETOR DE SERVIÇOS (5938, variável 6575)
# =========================================================
info_sidra(5938)

setor_servicos <- get_sidra(
  5938,
  variable = 6575,
  geo      = "State",
  period   = "all"
) |>
  select(
    uf = `Unidade da Federação`,
    Ano,
    vab_servicos = Valor
  )

# ========================================================
# SIDRA : SETOR DA AGROPECUÁRIA
# =======================================================

setor_agro <- get_sidra(
  5938,
  variable = 516,
  geo      = "State",
  period   = "all"
) |>
  select(
    uf = `Unidade da Federação`,
    Ano,
    share_agro = Valor
  )

#=========================================================

# ========================================================
# SIDRA : SETOR DA INDÚSTRIA


setor_ind <- get_sidra(
  5938,
  variable = 520,
  geo      = "State",
  period   = "all"
) |>
  select(
    uf = `Unidade da Federação`,
    Ano,
    share_ind = Valor
  )

# =========================================================
# SIDRA: VAB TOTAL (5938, variável 498) E PIB PER CAPITA
# =========================================================
vab_total_uf <- get_sidra(
  5938,
  variable = 498,
  geo      = "State",
  period   = "all"
) |>
  select(
    uf = `Unidade da Federação`,
    Ano,
    vab_total = Valor
  )

# PIB per capita a partir do VAB total e População
vab_percapita <- vab_total_uf |>
  inner_join(pop_uf, by = c("uf", "Ano"))

vab_percapita <- vab_percapita |>
  mutate(pib_percapita = (vab_total * 1000) / pop)


# importando o IPCA
ipca <- search_series(terms = "IPCA")
ipca_mensal_br <- ipeadata("PRECOS12_IPCA12")


# Transformando em anual e fazendo o deflatos
ipca_anual_br <- ipca_mensal_br |> 
  group_by(Ano = year(date)) |> 
  summarise(defl_media = mean(value, na.rm = TRUE), .groups = "drop") |> 
  mutate(defl_100 = 100 * (defl_media / 5827.77), Ano = as.character(Ano) )

vab_percapita <- vab_percapita |> 
  left_join(ipca_anual_br, by = "Ano")

vab_percapita <- vab_percapita |> 
  mutate(
    pib_pc_real = pib_percapita / (defl_100/100)
  )
         

# Participação do setor de serviços no VAB total
particip_serv <- setor_servicos |>
  inner_join(vab_total_uf, by = c("uf", "Ano")) |>
  mutate(share_serv = 100 * (vab_servicos / vab_total))

# =========================================================
# SENATRAN/BD+: FROTA DE VEÍCULOS (arquivo local)
# =========================================================
frota <- read.csv("dados/bq-results-20250904-185615-1757012199044.csv")

# Tipos únicos (inspeção)
unicos <- unique(frota$tipo_veiculo)

# Filtrando veículos não motorizados/sem consumo direto
frota_combustivel <- frota |>
  rename(Ano = ano, uf_sigla = sigla_uf) |>
  mutate(Ano = as.character(Ano)) |>
  filter(
    !tipo_veiculo %in% c(
      "reboque",
      "semi-reboque",
      "chassi plataforma",
      "side-car"
    )
  )

# Agregação por tipo de veículo, ano e UF
frota_combustivel_tratada <- frota_combustivel |>
  group_by(tipo_veiculo, Ano, uf_sigla) |>
  summarise(quantidade_total = sum(quantidade, na.rm = TRUE))

# Limpando linhas vazias de tipo
frota_combustivel_tratada <- frota_combustivel_tratada |>
  filter(!tipo_veiculo == "")

# Long -> Wide: colunas por tipo de veículo
frota_combustivel_tratada <- frota_combustivel_tratada |>
  pivot_wider(
    names_from  = tipo_veiculo,
    values_from = quantidade_total
  )

# =========================================================
# DICIONÁRIO DE UFs (cod, sigla, nome)
# =========================================================
dicionario_uf <- tibble::tibble(
  uf_cod   = c(12, 27, 13, 16, 29, 23, 53, 32, 52, 21,
               31, 50, 51, 15, 25, 26, 22, 41, 33, 24,
               43, 11, 14, 42, 28, 35, 17),
  uf_sigla = c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
               "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
               "RS","RO","RR","SC","SE","SP","TO"),
  uf_nome  = c("Acre","Alagoas","Amazonas","Amapá","Bahia","Ceará",
               "Distrito Federal","Espírito Santo","Goiás","Maranhão",
               "Minas Gerais","Mato Grosso do Sul","Mato Grosso",
               "Pará","Paraíba","Pernambuco","Piauí","Paraná",
               "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul",
               "Rondônia","Roraima","Santa Catarina","Sergipe",
               "São Paulo","Tocantins")
)

# =========================================================
# CHAVES & PADRONIZAÇÃO (nome UF -> sigla)
# =========================================================
# PIB per capita
vab_percapita <- vab_percapita |>
  left_join(dicionario_uf, by = c("uf" = "uf_nome"))

vab_percapita <- vab_percapita |>
  select(uf_sigla, Ano, pib_percapita)

# População
pop_uf <- pop_uf |>
  left_join(dicionario_uf, by = c("uf" = "uf_nome"))

pop_uf <- pop_uf |>
  select(Ano, uf_sigla, pop)

# Participação de serviços
particip_serv <- particip_serv |>
  left_join(dicionario_uf, by = c("uf" = "uf_nome"))

particip_serv <- particip_serv |>
  select(share_serv, Ano, uf_sigla)

# Participação de Agropecuária
setor_agro <- setor_agro |>
  left_join(dicionario_uf, by = c("uf" = "uf_nome"))

setor_agro <- setor_agro |>
  select(share_agro, Ano, uf_sigla)

# participação da Indústria

setor_ind  <- setor_ind |>
  left_join(dicionario_uf, by = c("uf" = "uf_nome"))

setor_ind <- setor_ind |>
  select(share_ind, Ano, uf_sigla)


## ========================================================



# =========================================================
# ANP: AGREGAÇÃO POR UF×ANO×TIPO (volume anual)
# =========================================================
df <- dados |>
  group_by(tipo, Ano, uf_sigla) |>
  summarise(volume_anual = sum(Volume, na.rm = TRUE))

df$uf_sigla <- toupper(df$uf_sigla)


#==========================================================
#ANP: Preço combustíveis
#===========================================================

precos <- read.csv("dados/preco_venda3.csv")

precos <- precos |> 
  group_by(ano, sigla_uf,produto) |> 
  summarise(
    preco_ano = mean(preco_venda)
  ) |> 
  rename(
    uf_sigla = sigla_uf, 
    tipo = produto,
    Ano = ano
  ) |> 
  filter(
    tipo %in% c("Gasolina","Diesel","Etanol")
  ) |> 
  mutate(
    Ano = as.character(Ano)
  )

# deflacionando

precos <- precos |> 
  left_join(ipca_anual_br, by = "Ano")

precos <- precos |> 
  mutate(
    preco_real = preco_ano / (defl_100/100)
  ) |> 
  select(
    Ano, uf_sigla, preco_real,tipo
  ) |> 
  mutate(
    tipo = tolower(tipo)
  )

# competitividade etanol-gasolina

precos_wide <- precos |> 
  pivot_wider(names_from = tipo, values_from = preco_real, values_fill = NA_real_)

preco_et_gas <- precos_wide |> 
  mutate(
    et_gas = log(etanol/gasolina)
  ) |> 
  select(
    Ano, uf_sigla,et_gas
  )


# =========================================================
# MERGE GERAL: POP + FROTA + SERVIÇOS + ANP + PIB_PC
# =========================================================
base_final <- pop_uf |>
  left_join(frota_combustivel_tratada, by = c("uf_sigla", "Ano")) |>
  left_join(particip_serv,             by = c("uf_sigla", "Ano")) |>
  inner_join(df,                        by = c("uf_sigla", "Ano")) |>
  left_join(vab_percapita,             by = c("uf_sigla", "Ano")) |> 
  left_join(setor_agro,                by = c("uf_sigla", "Ano")) |> 
  left_join(setor_ind,                 by = c("uf_sigla", "Ano")) |> 
  left_join(precos,                   by = c("uf_sigla", "Ano", "tipo")) |> 
  left_join(preco_et_gas,              by = c("uf_sigla", "Ano"))

# =========================================================
# FILTRO FINAL: SUBCONJUNTO DE UFs E JANELA DE ANOS
# =========================================================
ufs <- c("df", "go", "ma", "mt", "mg", "pa", "sp", "to", "br")
ufs_upper <- toupper(ufs)

base_final <- base_final |>
  filter(
    uf_sigla %in% c(ufs_upper),
    Ano < 2022 & Ano > 2003
  )

# ============================================================
# TRANSFORMANDO FROTA PARA FROTA PER CAPITA
# ===========================================================

base_final <- base_final |> 
  mutate(
    frota_leve   = coalesce(automovel,0), #+ coalesce(`caminhonete`,0) + coalesce(utilitario,0) + coalesce(camioneta,0),
    frota_moto   = coalesce(motocicleta,0) ,#+ coalesce(motoneta,0) + coalesce(ciclomotor,0) + coalesce(triciclo,0) + coalesce(quadriciclo,0),
    frota_pesado = coalesce(caminhao,0) + coalesce(`caminhao trator`,0),
    frota_onibus = coalesce(onibus,0) + coalesce(`micro-onibus`,0),
    frota_trator = coalesce(`trator rodas`,0),
    
    frota_total_sel = frota_leve + frota_moto + frota_pesado + frota_onibus + frota_trator
  ) |> 
  # padronizar por população  para comparabilidade 
  mutate(
    auto_pc      = 1000 * frota_leve   / pop,
    moto_pc      = 1000 * frota_moto   / pop,
    caminhao_pc  = 1000 * frota_pesado / pop,
    onibus_pc    = 1000 * frota_onibus / pop,
    trator_pc    = 1000 * frota_trator / pop
  ) |> 
  #variável dependente: consumo per capita 
  mutate(
    vol_pc  = volume_anual / pop,
    ln_vol_pc = log(pmax(vol_pc, 1e-9)),      # evita log(0)
    ln_pib_pc = log(pmax(pib_percapita, 1e-9)),
    ln_pib_pc_real = log(pmax(pib_pc_real, 1e-9)),
    ln_preco_real = log(pmax(preco_real, 1e-9))
    
  )




#==========================================================
# ESTIMANDO MODELOS POR MQO USANDO EFEITOS FIXOS
#===========================================================

#----------------------------------------
# VARIÁVEL DEPENDENTE : CONSUMO PER CAPITA
#----------------------------------------

pdata <- pdata.frame(base_final, index = c("uf_sigla","Ano"))


#

# Supondo colunas: share_serv, share_ind, share_agro em 0–100
pdata <- pdata |> 
  mutate(
    # 1) de % para proporção
    s_serv = share_serv / 100,
    s_ind  = share_ind  / 100,
    s_agro = share_agro / 100
  ) |> 
  # 2) zero-replacement (evita log(0))
  mutate(
    s_serv = pmax(s_serv, 1e-6),
    s_ind  = pmax(s_ind,  1e-6),
    s_agro = pmax(s_agro, 1e-6)
  ) |> 
  mutate(
    s_sum  = s_serv + s_ind + s_agro,
    s_serv = s_serv / s_sum,
    s_ind  = s_ind  / s_sum,
    s_agro = s_agro / s_sum
  ) |> 
  # 4) ALR com baseline = serviços
  mutate(
    alr_ind  = log(s_ind  / s_serv),
    alr_agro = log(s_agro / s_serv)
  )


options(scipen = 999)
# Gasolina (leve)
# --------- GASOLINA ---------
m_gas <- plm(
  ln_vol_pc ~ ln_pib_pc_real +  alr_ind+ alr_agro+ log(auto_pc) + log(moto_pc) ,
  data   = pdata,
  model  = "within",
  effect = "twoways",
  subset = tipo == "gasolina"
)

vcov_gas <- vcovSCC(m_gas, type = "HC1")      # Driscoll–Kraay
coeftest(m_gas, vcov = vcov_gas)

# --------- ETANOL ---------

m_etn <- plm(
  ln_vol_pc ~ ln_pib_pc_real + alr_ind + alr_agro+ log(auto_pc)  ,
  data   = pdata,
  model  = "within",
  effect = "twoways",
  subset = tipo == "etanol"
)
vcov_etn <- vcovSCC(m_etn, type = "HC1")
coeftest(m_etn, vcov = vcov_etn)

# --------- DIESEL ---------
m_dsl <- plm(
  ln_vol_pc ~ ln_pib_pc_real +  log(trator_pc) +  alr_ind + alr_agro+ log(caminhao_pc) + log(onibus_pc),
  data   = pdata,
  model  = "within",
  effect = "twoways",
  subset = tipo == "diesel"
)
vcov_dsl <- vcovSCC(m_dsl, type = "HC1")
coeftest(m_dsl, vcov = vcov_dsl)



# ----------------------------------
# VARIVÁVEL DEPENDENTE EM NÍVEL
#-----------------------------------


base_final <- distinct(base_final)
pdata <- pdata.frame(base_final, index = c("uf_sigla","Ano"))


options(scipen = 999)
# Gasolina (leve)
# --------- GASOLINA ---------
m_gas <- plm(
  log(volume_anual)  ~ ln_pib_pc_real +   log(auto_pc) + log(moto_pc)  + log(share_ind) ,
  data   = pdata,
  model  = "within",
  effect = "twoways",
  subset = tipo == "gasolina"
)

vcov_gas <- vcovSCC(m_gas, type = "HC1")      # Driscoll–Kraay
coeftest(m_gas, vcov = vcov_gas)

# --------- ETANOL ---------

m_etn <- plm(
  log(volume_anual) ~ ln_pib_pc_real + log(share_ind)+  moto_pc +  auto_pc,
  data   = pdata,
  model  = "within",
  effect = "twoways",
  subset = tipo == "etanol"
)
vcov_etn <- vcovSCC(m_etn, type = "HC1")
coeftest(m_etn, vcov = vcov_etn)

# --------- DIESEL ---------
m_dsl <- plm(
  log(volume_anual) ~ ln_pib_pc_real + log(share_serv)+ log(share_ind) + log(frota_pesado) + log(frota_trator)+log( frota_onibus) ,
  data   = pdata,
  model  = "within",
  effect = "twoways",
  subset = tipo == "diesel"
)
vcov_dsl <- vcovSCC(m_dsl, type = "HC1")
coeftest(m_dsl, vcov = vcov_dsl)

