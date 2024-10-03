# PACOTES: ----------------------------------------------------------------
library(magrittr)
library(ggplot2)


# IMPORTAÇÃO E ARRUMAÇÃO: -------------------------------------------------
base <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                          sheet = 2,
                          range = 'A1:AE5395') %>%
  janitor::clean_names() %>%
  tidyr::separate(col = 'hierarquia',
                  into = c('sig_hierarquia', 'nome_hierarquia'),
                  sep = '-') %>%
  dplyr::mutate(nome_hierarquia = dplyr::case_when(
    nome_hierarquia == ' Metrópole' ~ 'Metrópole',
    nome_hierarquia == ' Capital Regional' ~ 'Capital Regional',
    nome_hierarquia == ' Centro Sub' ~ 'Centro Subregional',
    nome_hierarquia == ' Centro de Zona' ~ 'Centro de Zona',
    nome_hierarquia == ' Centro Local' ~ 'Centro Local'
  )) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(id_municipio = as.character(id_municipio))


populacao <- sidrar::get_sidra(
  api = '/t/4714/n6/all/v/93/p/all') %>%
  janitor::clean_names() %>%
  dplyr::select(municipio_codigo, valor)


consolidado_01 <-  base %>%
  dplyr::full_join(populacao,
                   by = c('id_municipio' = 'municipio_codigo')) %>%
  dplyr::filter(valor >= 75000 & valor <= 150000) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(outros = previdencia_social +
                  legislativa +
                  gestao_ambiental_agricultura +
                  comercio_e_servicos +
                  administracao +
                  outros) %>%
  dplyr::select(!c(previdencia_social,
                     legislativa,
                     gestao_ambiental_agricultura,
                     comercio_e_servicos,
                     administracao))


consolidado <-  base %>%
  dplyr::full_join(populacao,
                   by = c('id_municipio' = 'municipio_codigo')) %>%
  dplyr::filter(valor >= 75000 & valor <= 150000) %>%
  dplyr::select(id_municipio,
                cidade,
                nome_hierarquia,
                assistencia_social,
                urbanismo_saneamento_transporte,
                desporto_e_lazer,
                educacao_e_cultura,

                ods_3,
                ods_4,
                ods_6) %>%
  tidyr::drop_na()


writexl::write_xlsx(consolidado_01, './dados/consolidado_01.xlsx')
writexl::write_xlsx(consolidado_02, './dados/consolidado_02.xlsx')


# TRATAMENTO E MODELAGEM --------------------------------------------------
referencia <- consolidado %>%
  dplyr::select(id_municipio,
                cidade)

outputs <- consolidado %>%
  dplyr::select(dplyr::starts_with('ods'))

inputs <- consolidado %>%
  dplyr::select(!c(id_municipio,
                   cidade,
                   nome_hierarquia,
                   dplyr::starts_with('ods')))

resultado <- rDEA::dea(
  XREF = inputs,
  YREF = outputs,
  X = inputs,
  Y = outputs,
  model = 'input',
  RTS = 'constant')

eficiencia <- referencia %>%
  dplyr::mutate(eficiencia = resultado$thetaOpt) %>%
  dplyr::arrange(desc(eficiencia))


# VISUALIZAÇÃO: -----------------------------------------------------------
consolidado %>%
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


consolidado %>%
  dplyr::select(!c(id_municipio,
                   cidade,
                   sig_hierarquia,
                   nome_hierarquia,
                   valor,
                   dplyr::starts_with('ods'))) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = 'orcamento',
                      values_to = 'valor') %>%
  dplyr::mutate(orcamento = dplyr::case_when(
    orcamento == 'urbanismo_saneamento_transporte' ~ 'Urbanismo, Saneamento e Transporte',
    orcamento == 'saude' ~ 'Saúde',
    orcamento == 'educacao_e_cultura' ~ 'Educação e Cultura',
    orcamento == 'previdencia_social' ~ 'Previdência Social',
    orcamento == 'assistencia_social' ~ 'Assistência Social',
    orcamento == 'seguranca_publica' ~ 'Segurança Pública',
    orcamento == 'outros' ~ 'Outros',
    orcamento == 'legislativa' ~ 'Legislativa',
    orcamento == 'gestao_ambiental_agricultura' ~ 'Gestão Ambiental e Agricultura',
    orcamento == 'desporto_e_lazer' ~ 'Desporto e Lazer',
    orcamento == 'comercio_e_servicos' ~ 'Comércio e Serviços',
    orcamento == 'administracao' ~ 'Administração')) %>%
  ggplot() +
  geom_boxplot(aes(y = orcamento, x = valor)) +
  labs(title = 'Dispersão dos Orçamentos (OUTPUTS)',
       subtitle = 'Municípios de Tamanho Populacional entre 75.000 e 150.000',
       x = 'Orçamento Per Capta (R$/Habitante)',
       y = '') +
  theme_bw()


