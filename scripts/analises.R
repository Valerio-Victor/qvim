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
    nome_hierarquia == ' Centro Sub' ~ 'Centro Sub',
    nome_hierarquia == ' Centro de Zona' ~ 'Centro de Zona',
    nome_hierarquia == ' Centro Local' ~ 'Centro Local'
  )) %>%
  tidyr::drop_na()

nomes_municipios <- base %>%
  dplyr::transmute(as.character(id_municipio), cidade) %>%
  dplyr::rename('dmu' = `as.character(id_municipio)`)

# -------------------------------------------------------------------------
inputs <- base[,1:16]

outputs <- base[,-c(5:16)]

# -------------------------------------------------------------------------
eficiencia_metropole <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                          sheet = 8) %>%
  janitor::clean_names() %>%
  dplyr::select(dmu, score) %>%
  dplyr::mutate(nome_hierarquia = 'Metrópole')

eficiencia_capital_regional <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                                 sheet = 11) %>%
  janitor::clean_names() %>%
  dplyr::select(dmu, score) %>%
  dplyr::mutate(nome_hierarquia = 'Capital Regional')

eficiencia_centro_sub <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                           sheet = 14) %>%
  janitor::clean_names() %>%
  dplyr::select(dmu, score) %>%
  dplyr::mutate(nome_hierarquia = 'Centro Sub')

eficiencia_centro_zona <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                            sheet = 17) %>%
  janitor::clean_names() %>%
  dplyr::select(dmu, score) %>%
  dplyr::mutate(nome_hierarquia = 'Centro de Zona')

eficiencia_centro_local <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                             sheet = 20) %>%
  janitor::clean_names() %>%
  dplyr::select(dmu, score) %>%
  dplyr::mutate(nome_hierarquia = 'Centro Local')

eficiencia <- dplyr::bind_rows(eficiencia_metropole,
                               eficiencia_capital_regional,
                               eficiencia_centro_sub,
                               eficiencia_centro_zona,
                               eficiencia_centro_local) %>%
  dplyr::full_join(nomes_municipios, by = c('dmu' = 'dmu')) %>%
  tidyr::drop_na()

# -------------------------------------------------------------------------
pesos_metropole <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                     sheet = 9,
                                     range = 'B1:O212') %>%
  janitor::clean_names() %>%
  dplyr::mutate(nome_hierarquia = 'Metrópole')

pesos_capital_regional <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                            sheet = 12,
                                            range = 'B1:O298') %>%
  janitor::clean_names() %>%
  dplyr::mutate(nome_hierarquia = 'Capital Regional')

pesos_centro_sub <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                      sheet = 15,
                                      range = 'B1:O488') %>%
  janitor::clean_names() %>%
  dplyr::mutate(nome_hierarquia = 'Centro Sub')

pesos_centro_zona <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                       sheet = 18,
                                       range = 'B1:O428') %>%
  janitor::clean_names() %>%
  dplyr::mutate(nome_hierarquia = 'Centro de Zona')

pesos_centro_local <- readxl::read_xlsx(path = './dados/240828_dea.xlsx',
                                        sheet = 21,
                                        range = 'B1:O3971') %>%
  janitor::clean_names() %>%
  dplyr::mutate(nome_hierarquia = 'Centro Local')

pesos <- dplyr::bind_rows(pesos_metropole,
                          pesos_capital_regional,
                          pesos_centro_sub,
                          pesos_centro_zona,
                          pesos_centro_local) %>%
  dplyr::full_join(nomes_municipios, by = c('dmu' = 'dmu')) %>%
  tidyr::drop_na()

# -------------------------------------------------------------------------
populacao <- sidrar::get_sidra(
  api = '/t/4714/n6/all/v/93/p/all') %>%
  janitor::clean_names() %>%
  dplyr::select(municipio_codigo, valor)

# -------------------------------------------------------------------------
rm(eficiencia_metropole,
   eficiencia_capital_regional,
   eficiencia_centro_sub,
   eficiencia_centro_zona,
   eficiencia_centro_local,
   pesos_metropole,
   pesos_capital_regional,
   pesos_centro_sub,
   pesos_centro_zona,
   pesos_centro_local,
   nomes_municipios)


# TRATAMENTO: -------------------------------------------------------------
ref_pesos <- pesos %>%
  dplyr::filter(nome_hierarquia == 'Centro Sub') %>%
  dplyr::filter(score == 1) %>%
  dplyr::select(dmu,
                cidade,
                weight_seguranca_publica,
                weight_desporto_e_lazer,
                weight_educacao_e_cultura,
                weight_assistencia_social,
                weight_saude)


dplyr::slice_max()

# VISUALIZAÇÃO: -----------------------------------------------------------
eficiencia %>%
  dplyr::filter(score < 1) %>%
  ggplot() +
  geom_boxplot(aes(x = nome_hierarquia, y = score))


outputs[,5:20] %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'output',
                      values_to = 'valor') %>%
  ggplot() +
  geom_boxplot(aes(x = output, y = valor))


inputs[,5:16] %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'input',
                      values_to = 'valor') %>%
  ggplot() +
  geom_boxplot(aes(y = input, x = valor))

































































