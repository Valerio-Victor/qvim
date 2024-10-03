# PACOTES NECESSÁRIOS: ----------------------------------------------------
library(basedosdados)
library(magrittr, include.only = '%>%')
library(ggplot2)


# IMPORTAÇÃO DOS DADOS: ---------------------------------------------------
# Utilizar o seu ID (entre as aspas) Google Cloud no código abaixo:
basedosdados::set_billing_id(billing_project_id = '234510366223')

# Para identificar o tabela no site base dos dados basta "juntar"
# o nome do conjunto com o nome da tabela, no nosso caso.
# Nome do Conjunto: br_me_siconfi
# Nome da Tabela: municipio_despesas_orcamentarias
dados <- basedosdados::bdplyr(
  table = 'br_me_siconfi.municipio_despesas_funcao')

# Ao imprimir os dados no console pode-se ter uma ideia das primeiras linhas da
# tabela.
dados
tibble::glimpse(dados)

# Neste caso, vou filtrar a base para o ano de 2022.
despesas_2018_2022 <- dados %>%
  dplyr::filter(ano >= 2018)

# Caso você queira traduzir o comando acima em linguagem SQL basta utilizar o
# comando abaixo, mas isso é opcional.
dplyr::show_query(despesas_2018_2022)

# Para coletar os dados já filtrados para o R, deve-se executar o comando
# bd_collect(). Neste caso, vou atribuir o nome final para o objeto.
despesas_municipais_2018_2022 <- basedosdados::bd_collect(despesas_2018_2022)

# Para evitar ficar baixando os dados da núvem, vou exportar em formato csv
write.csv2(x = despesas_municipais_2018_2022,
           file = './dados/despesas_municipais_2018_2022_funcao.csv')


# TRANFORMANDO AS DESPESAS EM INSUMOS DO MODELO DEA: ----------------------
# INFORMAÇÕES - ESPELHO INFORMAÇÕES: --------------------------------------
# PORTARIA  No  42,  DE  14 DE  ABRIL  DE 1999 (ATUALIZADA) (*)
# (Publicada no D.O.U. de 15.04.99)

despesas_municipais <- readr::read_csv2(
  file = './dados/despesas_municipais_2018_2022_funcao.csv')

insumos <- despesas_municipais %>%
  dplyr::select(ano,
                id_municipio,
                estagio_bd,
                conta_bd,
                valor) %>%
  dplyr::filter(estagio_bd == 'Despesas Empenhadas') %>%
  dplyr::rename('subfuncao' = conta_bd) %>%
  dplyr::mutate(funcao = dplyr::case_when(
    subfuncao == 'Ação Legislativa' ~ 'Legislativa',
    subfuncao == 'Controle Externo' ~ 'Legislativa',
    subfuncao == 'Ação Judiciária' ~ 'Judiciária',
    subfuncao == 'Defesa do Interesse Público no Processo Judiciário' ~ 'Judiciária',
    subfuncao == 'Defesa da Ordem Jurídica' ~ 'Essencial à Justiça',
    subfuncao == 'Representação Judicial e Extrajudicial' ~ 'Essencial à Justiça',
    subfuncao == 'Planejamento e Orçamento' ~ 'Administração',
    subfuncao == 'Administração Geral' ~ 'Administração',
    subfuncao == 'Administração Financeira' ~ 'Administração',
    subfuncao == 'Controle Interno' ~ 'Administração',
    subfuncao == 'Normatização e Fiscalização' ~ 'Administração',
    subfuncao == 'Tecnologia da Informação' ~ 'Administração',
    subfuncao == 'Ordenamento Territorial' ~ 'Administração',
    subfuncao == 'Formação de Recursos Humanos' ~ 'Administração',
    subfuncao == 'Administração de Receitas' ~ 'Administração',
    subfuncao == 'Administração de Concessões' ~ 'Administração',
    subfuncao == 'Comunicação Social' ~ 'Administração',
    subfuncao == 'Defesa Aérea' ~ 'Defesa Nacional',
    subfuncao == 'Defesa Áerea' ~ 'Defesa Nacional',
    subfuncao == 'Defesa Naval' ~ 'Defesa Nacional',
    subfuncao == 'Defesa Terrestre' ~ 'Defesa Nacional',
    subfuncao == 'Policiamento' ~ 'Segurança Pública',
    subfuncao == 'Defesa Civil' ~ 'Segurança Pública',
    subfuncao == 'Informação e Inteligência' ~ 'Segurança Pública',
    subfuncao == 'Relações Diplomáticas' ~ 'Relações Exteriores',
    subfuncao == 'Cooperação Internacional' ~ 'Relações Exteriores',
    subfuncao == 'Assistência ao Idoso' ~ 'Assistência Social',
    subfuncao == 'Assistência ao Portador de Deficiência' ~ 'Assistência Social',
    subfuncao == 'Assistência à Criança e ao Adolescente' ~ 'Assistência Social',
    subfuncao == 'Assistência Comunitária' ~ 'Assistência Social',
    subfuncao == 'Previdência Básica' ~ 'Previdência Social',
    subfuncao == 'Previdência do Regime Estatutário' ~ 'Previdência Social',
    subfuncao == 'Previdência Complementar' ~ 'Previdência Social',
    subfuncao == 'Previdência Especial' ~ 'Previdência Social',
    subfuncao == 'Atenção Básica' ~ 'Saúde',
    subfuncao == 'Assistência Hospitalar e Ambulatorial' ~ 'Saúde',
    subfuncao == 'Suporte Profilático e Terapêutico' ~ 'Saúde',
    subfuncao == 'Vigilância Sanitária' ~ 'Saúde',
    subfuncao == 'Vigilância Epidemiológica' ~ 'Saúde',
    subfuncao == 'Alimentação e Nutrição' ~ 'Saúde',
    subfuncao == 'Proteção e Benefícios ao Trabalhador' ~ 'Trabalho',
    subfuncao == 'Relações de Trabalho' ~ 'Trabalho',
    subfuncao == 'Empregabilidade' ~ 'Trabalho',
    subfuncao == 'Fomento ao Trabalho' ~ 'Trabalho',
    subfuncao == 'Ensino Fundamental' ~ 'Educação',
    subfuncao == 'Ensino Médio' ~ 'Educação',
    subfuncao == 'Ensino Profissional' ~ 'Educação',
    subfuncao == 'Ensino Superior' ~ 'Educação',
    subfuncao == 'Educação Infantil' ~ 'Educação',
    subfuncao == 'Educação de Jovens e Adultos' ~ 'Educação',
    subfuncao == 'Educação Especial' ~ 'Educação',
    subfuncao == 'Educação Básica' ~ 'Educação',
    subfuncao == 'Patrimônio Histórico, Artístico e Arqueológico' ~ 'Cultura',
    subfuncao == 'Difusão Cultural' ~ 'Cultura',
    subfuncao == 'Custódia e Reintegração Social' ~ 'Direitos da Cidadania',
    subfuncao == 'Direitos Individuais, Coletivos e Difusos' ~ 'Direitos da Cidadania',
    subfuncao == 'Assistência aos Povos Indígenas' ~ 'Direitos da Cidadania',
    subfuncao == 'Infra-Estrutura Urbana' ~ 'Urbanismo',
    subfuncao == 'Infraestrutura Urbana' ~ 'Urbanismo',
    subfuncao == 'Serviços Urbanos' ~ 'Urbanismo',
    subfuncao == 'Transportes Coletivos Urbanos' ~ 'Urbanismo',
    subfuncao == 'Habitação Rural' ~ 'Habitação',
    subfuncao == 'Habitação Urbana' ~ 'Habitação',
    subfuncao == 'Saneamento Básico Rural' ~ 'Saneamento',
    subfuncao == 'Saneamento Básico Urbano' ~ 'Saneamento',
    subfuncao == 'Preservação e Conservação Ambiental' ~ 'Gestão Ambiental',
    subfuncao == 'Controle Ambiental' ~ 'Gestão Ambiental',
    subfuncao == 'Recuperação de Áreas Degradadas' ~ 'Gestão Ambiental',
    subfuncao == 'Recursos Hídricos' ~ 'Gestão Ambiental',
    subfuncao == 'Meteorologia' ~ 'Gestão Ambiental',
    subfuncao == 'Desenvolvimento Científico' ~ 'Ciência e Tecnologia',
    subfuncao == 'Desenvolvimento Tecnológico e Engenharia' ~ 'Ciência e Tecnologia',
    subfuncao == 'Difusão do Conhecimento Científico e Tecnológico' ~ 'Ciência e Tecnologia',
    subfuncao == 'Promoção da Produção Vegetal' ~ 'Agricultura',
    subfuncao == 'Promoção da Produção Animal' ~ 'Agricultura',
    subfuncao == 'Defesa Sanitária Vegetal' ~ 'Agricultura',
    subfuncao == 'Defesa Sanitária Animal' ~ 'Agricultura',
    subfuncao == 'Abastecimento' ~ 'Agricultura',
    subfuncao == 'Extensão Rural' ~ 'Agricultura',
    subfuncao == 'Irrigação' ~ 'Agricultura',
    subfuncao == 'Promoção da Produção Agropecuária' ~ 'Agricultura',
    subfuncao == 'Defesa Agropecuária' ~ 'Agricultura',
    subfuncao == 'Reforma Agrária' ~ 'Organização Agrária',
    subfuncao == 'Colonização' ~ 'Organização Agrária',
    subfuncao == 'Promoção Industrial' ~ 'Indústria',
    subfuncao == 'Produção Industrial' ~ 'Indústria',
    subfuncao == 'Mineração' ~ 'Indústria',
    subfuncao == 'Propriedade Industrial' ~ 'Indústria',
    subfuncao == 'Normalização e Qualidade' ~ 'Indústria',
    subfuncao == 'Promoção Comercial' ~ 'Comércio e Serviços',
    subfuncao == 'Comercialização' ~ 'Comércio e Serviços',
    subfuncao == 'Comércio Exterior' ~ 'Comércio e Serviços',
    subfuncao == 'Serviços Financeiros' ~ 'Comércio e Serviços',
    subfuncao == 'Turismo' ~ 'Comércio e Serviços',
    subfuncao == 'Comunicações Postais' ~ 'Comunicações',
    subfuncao == 'Telecomunicações' ~ 'Comunicações',
    subfuncao == 'Conservação de Energia' ~ 'Energia',
    subfuncao == 'Energia Elétrica' ~ 'Energia',
    subfuncao == 'Combustíveis Minerais' ~ 'Energia',
    subfuncao == 'Biocombustíveis' ~ 'Energia',
    subfuncao == 'Transporte Aéreo' ~ 'Transporte',
    subfuncao == 'Transporte Rodoviário' ~ 'Transporte',
    subfuncao == 'Transporte Ferroviário' ~ 'Transporte',
    subfuncao == 'Transporte Hidroviário' ~ 'Transporte',
    subfuncao == 'Transportes Especiais' ~ 'Transporte',
    subfuncao == 'Desporto de Rendimento' ~ 'Desporto e Lazer',
    subfuncao == 'Desporto Comunitário' ~ 'Desporto e Lazer',
    subfuncao == 'Lazer' ~ 'Desporto e Lazer',
    subfuncao == 'Refinanciamento da Dívida Interna' ~ 'Encargos Especiais',
    subfuncao == 'Refinanciamento da Dívida Externa' ~ 'Encargos Especiais',
    subfuncao == 'Serviço da Dívida Interna' ~ 'Encargos Especiais',
    subfuncao == 'Serviço da Dívida Externa' ~ 'Encargos Especiais',
    subfuncao == 'Outras Transferências' ~ 'Encargos Especiais',
    subfuncao == 'Outros Encargos Especiais' ~ 'Encargos Especiais',
    subfuncao == 'Transferências para a Educação Básica' ~ 'Encargos Especiais',
    subfuncao == 'Legislativa' ~ 'Legislativa',
    subfuncao == 'Judiciária' ~ 'Judiciária',
    subfuncao == 'Essencial à Justiça' ~ 'Essencial à Justiça',
    subfuncao == 'Administração' ~ 'Administração',
    subfuncao == 'Defesa Nacional' ~ 'Defesa Nacional',
    subfuncao == 'Segurança Pública' ~ 'Segurança Pública',
    subfuncao == 'Relações Exteriores' ~ 'Relações Exteriores',
    subfuncao == 'Assistência Social' ~ 'Assistência Social',
    subfuncao == 'Previdência Social' ~ 'Previdência Social',
    subfuncao == 'Saúde' ~ 'Saúde',
    subfuncao == 'Trabalho' ~ 'Trabalho',
    subfuncao == 'Educação' ~ 'Educação',
    subfuncao == 'Cultura' ~ 'Cultura',
    subfuncao == 'Direitos da Cidadania' ~ 'Direitos da Cidadania',
    subfuncao == 'Urbanismo' ~ 'Urbanismo',
    subfuncao == 'Habitação' ~ 'Habitação',
    subfuncao == 'Saneamento' ~ 'Saneamento',
    subfuncao == 'Gestão Ambiental' ~ 'Gestão Ambiental',
    subfuncao == 'Ciência e Tecnologia' ~ 'Ciência e Tecnologia',
    subfuncao == 'Agricultura' ~ 'Agricultura',
    subfuncao == 'Organização Agrária' ~ 'Organização Agrária',
    subfuncao == 'Indústria' ~ 'Indústria',
    subfuncao == 'Comércio e Serviços' ~ 'Comércio e Serviços',
    subfuncao == 'Comunicações' ~ 'Comunicações',
    subfuncao == 'Energia' ~ 'Energia',
    subfuncao == 'Transporte' ~ 'Transporte',
    subfuncao == 'Desporto e Lazer' ~ 'Desporto e Lazer',
    subfuncao == 'Encargos Especiais' ~ 'Encargos Especiais',
    subfuncao == 'Demais Subfunções Legislativa' ~ 'Legislativa',
    subfuncao == 'Demais Subfunções Judiciária' ~ 'Judiciária',
    subfuncao == 'Demais Subfunções Essencial à Justiça' ~ 'Essencial à Justiça',
    subfuncao == 'Demais Subfunções Administração' ~ 'Administração',
    subfuncao == 'Demais Subfunções Defesa Nacional' ~ 'Defesa Nacional',
    subfuncao == 'Demais Subfunções Segurança Pública' ~ 'Segurança Pública',
    subfuncao == 'Demais Subfunções Relações Exteriores' ~ 'Relações Exteriores',
    subfuncao == 'Demais Subfunções Assistência Social' ~ 'Assistência Social',
    subfuncao == 'Demais Subfunções Previdência Social' ~ 'Previdência Social',
    subfuncao == 'Demais Subfunções Saúde' ~ 'Saúde',
    subfuncao == 'Demais Subfunções Trabalho' ~ 'Trabalho',
    subfuncao == 'Demais Subfunções Educação' ~ 'Educação',
    subfuncao == 'Demais Subfunções Cultura' ~ 'Cultura',
    subfuncao == 'Demais Subfunções Direitos da Cidadania' ~ 'Direitos da Cidadania',
    subfuncao == 'Demais Subfunções Urbanismo' ~ 'Urbanismo',
    subfuncao == 'Demais Subfunções Habitação' ~ 'Habitação',
    subfuncao == 'Demais Subfunções Saneamento' ~ 'Saneamento',
    subfuncao == 'Demais Subfunções Gestão Ambiental' ~ 'Gestão Ambiental',
    subfuncao == 'Demais Subfunções Ciência e Tecnologia' ~ 'Ciência e Tecnologia',
    subfuncao == 'Demais Subfunções Agricultura' ~ 'Agricultura',
    subfuncao == 'Demais Subfunções Organização Agrária' ~ 'Organização Agrária',
    subfuncao == 'Demais Subfunções Indústria' ~ 'Indústria',
    subfuncao == 'Demais Subfunções Comércio e Serviços' ~ 'Comércio e Serviços',
    subfuncao == 'Demais Subfunções Comunicações' ~ 'Comunicações',
    subfuncao == 'Demais Subfunções Energia' ~ 'Energia',
    subfuncao == 'Demais Subfunções Transporte' ~ 'Transporte',
    subfuncao == 'Demais Subfunções Desporto e Lazer' ~ 'Desporto e Lazer',
    subfuncao == 'Demais Subfunções Encargos Especiais' ~ 'Encargos Especiais',
    subfuncao == 'Despesas Exceto Intraorçamentárias' ~ 'Exceto Intraorçamentárias',
    subfuncao == 'Despesas Intraorçamentárias' ~ 'Intraorçamentárias'
  )) %>%
  dplyr::group_by(ano,
                  id_municipio,
                  funcao) %>%
  dplyr::summarise(despesa_empenhada = sum(valor)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(despesa_empenhada_p22 = dplyr::case_when(
    ano == 2018 ~ despesa_empenhada * 1.265803,
    ano == 2019 ~ despesa_empenhada * 1.213501,
    ano == 2020 ~ despesa_empenhada * 1.161023,
    ano == 2021 ~ despesa_empenhada * 1.0549,
    ano == 2022 ~ despesa_empenhada * 1
  ))

unique(insumos$funcao)

populacao <- sidrar::get_sidra(
  api = '/t/4714/n6/all/v/93/p/all') %>%
  janitor::clean_names() %>%
  dplyr::select(municipio_codigo,valor)

insumo_ver01 <- insumos %>%
  dplyr::select(-despesa_empenhada) %>%
  dplyr::filter(!is.na(funcao)) %>%
  tidyr::pivot_wider(values_from = 'despesa_empenhada_p22',
                     names_from = 'funcao') %>%
  dplyr::mutate_all(tidyr::replace_na, 0) %>%
  janitor::clean_names() %>%
  dplyr::transmute(ano = ano,
                   id_municipio = id_municipio,
                   saude_desporto_lazer = saude + desporto_e_lazer,
                   educacao_cultura = educacao + cultura,
                   saneamento_gestao_ambiental = saneamento + gestao_ambiental,
                   urbanismo_habitacao_transporte_seguranca_agricultura =
                     urbanismo + habitacao + seguranca_publica + transporte + agricultura,
                   outros = administracao + assistencia_social + comercio_e_servicos +
                     trabalho + ciencia_e_tecnologia + defesa_nacional +
                     direitos_da_cidadania + industria + relacoes_exteriores) %>%
  dplyr::mutate(id_municipio = as.character(id_municipio)) %>%
  dplyr::left_join(populacao, by = c('id_municipio' = 'municipio_codigo')) %>%
  dplyr::rename('populacao' = valor) %>%
  dplyr::mutate(
    saude_desporto_lazer_per_capta = saude_desporto_lazer/populacao,
    educacao_cultura_per_capta = educacao_cultura/populacao,
    saneamento_gestao_ambiental_per_capta = saneamento_gestao_ambiental/populacao,
    urbanismo_habitacao_transporte_seguranca_agricultura_per_capta = urbanismo_habitacao_transporte_seguranca_agricultura/populacao,
    outros_per_capta = outros/populacao)

grafico_anual <- insumo_ver01 %>%
  dplyr::filter(populacao %in% c(75000:150000)) %>%
  dplyr::select(-c(saude_desporto_lazer,
                   educacao_cultura,
                   saneamento_gestao_ambiental,
                   urbanismo_habitacao_transporte_seguranca_agricultura,
                   outros,
                   populacao)) %>%
  tidyr::pivot_longer(cols = -c(ano, id_municipio),
                      names_to = 'input',
                      values_to = 'valor') %>%
  dplyr::filter(ano < 2023) %>%
  ggplot() +
  geom_boxplot(aes(y = input, x = valor, fill = as.factor(ano))) +
  scale_x_continuous(limits = c(0,10000))

grafico_anual

grafico_total <- insumo_ver01 %>%
  dplyr::filter(populacao %in% c(75000:150000)) %>%
  dplyr::select(-c(saude_desporto_lazer,
                   educacao_cultura,
                   saneamento_gestao_ambiental,
                   urbanismo_habitacao_transporte_seguranca_agricultura,
                   outros,
                   populacao)) %>%
  tidyr::pivot_longer(cols = -c(ano, id_municipio),
                      names_to = 'input',
                      values_to = 'valor') %>%
  dplyr::filter(ano < 2023) %>%
  dplyr::group_by(id_municipio, input) %>%
  dplyr::summarise(valor = sum(valor)) %>%
  dplyr::ungroup() %>%
  ggplot() +
  geom_boxplot(aes(y = input, x = valor))

grafico_total


# EXPORTAÇÃO: -------------------------------------------------------------
idsc <- readxl::read_xlsx(path = './dados/idsc_2022.xlsx',
                          sheet = 2,
                          range = 'A1:X5571') %>%
  janitor::clean_names()

insumo_final <- insumo_ver01 %>%
  dplyr::filter(populacao %in% c(75000:150000)) %>%
  dplyr::select(-c(saude_desporto_lazer,
                   educacao_cultura,
                   saneamento_gestao_ambiental,
                   urbanismo_habitacao_transporte_seguranca_agricultura,
                   outros,
                   populacao)) %>%
  tidyr::pivot_longer(cols = -c(ano, id_municipio),
                      names_to = 'input',
                      values_to = 'valor') %>%
  dplyr::filter(ano < 2023) %>%
  dplyr::group_by(id_municipio, input) %>%
  dplyr::summarise(valor = sum(valor)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = 'input',
                     values_from = 'valor')

produto_final <- idsc %>%
  dplyr::select(id,
                goal_3_score,
                goal_4_score,
                goal_6_score,
                goal_11_score) %>%
  dplyr::mutate(id = as.character(id)) %>%
  dplyr::full_join(populacao, by = c('id' = 'municipio_codigo')) %>%
  dplyr::rename('populacao' = valor,
                'ODS3' = goal_3_score,
                'ODS4' = goal_4_score,
                'ODS6' = goal_6_score,
                'ODS11' = goal_11_score) %>%
  dplyr::filter(populacao %in% c(75000:150000)) %>%
  dplyr::select(-populacao)

dea_final <- insumo_final %>%
  dplyr::left_join(produto_final, by = c('id_municipio' = 'id'))

writexl::write_xlsx(x = dea_final,
                    path = './dados/20240926dea_final.xlsx')






































































