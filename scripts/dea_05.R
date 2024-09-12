# PACOTES: ----------------------------------------------------------------
library(magrittr)
library(ggplot2)


# IMPORTAÇÃO: -------------------------------------------------------------
dados <- readxl::read_xlsx(path = "./dados/consolidado_01.xlsx")

dados <- dados %>%
  dplyr::transmute(
    id_municipio = id_municipio,
    cidade = cidade,
    nome_hierarquia = nome_hierarquia,
    outros = outros + seguranca_publica + desporto_e_lazer,
    saude = saude,
    assistencia_social = assistencia_social,
    educacao_e_cultura = educacao_e_cultura,
    urbanismo_saneamento_transporte = urbanismo_saneamento_transporte,
    ods_3 = ods_3,
    ods_4 = ods_4,
    ods_6 = ods_6)

insumos <- dados %>%
  dplyr::select(
    outros,
    saude,
    assistencia_social,
    educacao_e_cultura,
    urbanismo_saneamento_transporte)

produtos <- dados %>%
  dplyr::select(
    ods_3,
    ods_4,
    ods_6)

referencia <- dados %>%
  dplyr::select(id_municipio,
                nome_hierarquia,
                cidade)


# MODELAGEM: --------------------------------------------------------------
resultado <- rDEA::dea(
  XREF = insumos,
  YREF = produtos,
  X = insumos,
  Y = produtos,
  model = 'output',
  RTS = 'constant')

eficiencia <- referencia %>%
  dplyr::mutate(eficiencia = resultado$thetaOpt) %>%
  dplyr::left_join(dados, by = c('id_municipio' = 'id_municipio')) %>%
  dplyr::arrange(desc(eficiencia)) %>%
  dplyr::select(-cidade.y,-nome_hierarquia.y) %>%
  dplyr::rename('cidade' = cidade.x,
                'nome_hierarquia' = nome_hierarquia.x)


# VISUALIZAÇÃO: -----------------------------------------------------------
eficiencia %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(nome_hierarquia), y = eficiencia)) +
  labs(title = 'Eficiência dos municípios',
       x = 'Hierarquia',
       y = 'Eficiência') +
  theme_bw()

eficiencia %>%
  dplyr::select(dplyr::starts_with('ods')) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = 'ods',
                      values_to = 'valor') %>%
  tidyr::separate(col = 'ods',
                  into = c('ods_pre', 'ods_num'),
                  sep = '_') %>%
  dplyr::select(-ods_pre) %>%
  dplyr::mutate(ods_num = as.numeric(ods_num)) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(ods_num), y = valor)) +
  labs(title = 'Dispersão das ODS (INPUTS)',
       subtitle = 'Municípios de Tamanho Populacional entre 75.000 e 150.000',
       x = 'Objetivo de Desenvolvimento Sustentável (ODS)',
       y = 'Índice') +
  theme_bw()

eficiencia %>%
  dplyr::select(!c(id_municipio,
                   cidade,
                   nome_hierarquia,
                   eficiencia,
                   dplyr::starts_with('ods'))) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = 'orcamento',
                      values_to = 'valor') %>%
  dplyr::mutate(orcamento = dplyr::case_when(
    orcamento == 'urbanismo_saneamento_transporte' ~ 'Urbanismo,\n Saneamento\n e Transporte',
    orcamento == 'saude' ~ 'Saúde',
    orcamento == 'educacao_e_cultura' ~ 'Educação\n e Cultura',
    orcamento == 'assistencia_social' ~ 'Assistência\n Social',
    orcamento == 'outros' ~ 'Outros')) %>%
  ggplot() +
  geom_boxplot(aes(y = orcamento, x = valor)) +
  labs(title = 'Dispersão dos Orçamentos (OUTPUTS)',
       subtitle = 'Municípios de Tamanho Populacional entre 75.000 e 150.000',
       x = 'Orçamento Per Capta (R$/Habitante)',
       y = '') +
  theme_bw()

# Quero quebrar o rótulo de y Urbanismo, Saneamento e Transporte no meio, como faz?
eficiencia %>%




