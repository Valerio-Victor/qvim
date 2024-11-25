library(magrittr)


# ANÁLISE DE EFICIÊNCIA: --------------------------------------------------
eficiencia <- readxl::read_xlsx(path = './dados/resultados_dea_output.xlsx')

municipios <- geobr::read_municipality(code_muni = 'all', year = 2018)

ranking_eficiencia <- eficiencia %>%
  dplyr::transmute(dmu = as.numeric(DMU),
                   score = Score) %>%
  dplyr::left_join(municipios,
                   by = c('dmu' = 'code_muni')) %>%
  dplyr::arrange(desc(score))


summary(ranking_eficiencia$score)
sd(ranking_eficiencia$score)


# ANÁLISE DOS PRODUTOS: ---------------------------------------------------
produtos <- eficiencia %>%
  dplyr::filter(DMU == '3132404') %>%
  dplyr::select(ODS3, ODS4, ODS6, ODS11) %>%
  dplyr::mutate(ODS3_ef = ODS3 * 1.052,
                ODS4_ef = ODS4 * 1.052,
                ODS6_ef = ODS6 * 1.052,
                ODS11_ef = ODS11 * 1.052) %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'ref',
                      values_to = 'value')


# ANÁLISE DOS INSUMOS: ----------------------------------------------------
insumos <- eficiencia %>%
  dplyr::filter(DMU == '3132404') %>%
  dplyr::select(educacao,
                outros,
                saneamento,
                saude,
                urbanismo) %>%
  dplyr::mutate(educacao_ef = educacao * 0.9505706,
                outros_ef = outros * 0.9505706,
                saneamento_ef = saneamento * 0.9505706,
                saude_ef = saude * 0.9505706,
                urbanismo_ef = urbanismo * 0.9505706) %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'ref',
                      values_to = 'value')








