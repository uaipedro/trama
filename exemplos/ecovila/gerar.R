# Gerador reproduzivel dos dados simulados da Ecovila Serra das Aguas.
set.seed(20260921)

base_dir <- "exemplos/ecovila"
out_dir <- file.path(base_dir, "dados")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

lim <- function(x, low, high) pmax(low, pmin(high, x))
save_csv <- function(x, name) utils::write.csv(x, file.path(out_dir, name), row.names = FALSE, fileEncoding = "UTF-8")

# Cadastro territorial. A distribuicao historica dos sistemas cria confundimento.
n <- 36L
talhao_id <- sprintf("T%02d", seq_len(n))
faixa <- rep(c("alta_seca", "intermediaria", "baixa_umida"), each = 12)
faixa_i <- match(faixa, c("alta_seca", "intermediaria", "baixa_umida"))
sistema <- c(
  sample(c(rep("cafe_solteiro", 5), rep("cafe_banana", 4), rep("cafe_inga", 2), "cafe_banana_inga")),
  sample(c(rep("cafe_solteiro", 3), rep("cafe_banana", 4), rep("cafe_inga", 4), "cafe_banana_inga")),
  sample(c("cafe_solteiro", rep("cafe_banana", 2), rep("cafe_inga", 3), rep("cafe_banana_inga", 6)))
)
talhoes <- data.frame(
  talhao_id, nome = paste("Talhao", talhao_id), area_ha = round(runif(n, .38, .92), 2),
  longitude = round(-44.123 + rep(c(-.006, 0, .006), each = 12) + runif(n, -.0015, .0015), 6),
  latitude = round(-21.123 - rep(seq(0, .011, length.out = 12), 3) + runif(n, -.001, .001), 6),
  altitude_m = round(rnorm(n, c(970, 925, 885)[faixa_i], 10)),
  declividade_pct = round(lim(rnorm(n, c(19, 12, 5)[faixa_i], 2.8), 1, 32), 1),
  orientacao_encosta = sample(c("nordeste", "leste", "sudeste", "sul"), n, TRUE),
  distancia_rio_m = round(lim(rnorm(n, c(550, 290, 95)[faixa_i], 45), 35, 720)),
  distancia_sede_m = round(runif(n, 110, 860)), faixa_paisagem = faixa,
  solo_base = c("faixa_1_mais_arenosa", "faixa_2_argilosa", "faixa_3_organica")[faixa_i],
  historico_uso = sample(c("pastagem ate 2008", "cafe convencional ate 2012", "pousio e capoeira", "pomar antigo"), n, TRUE),
  area_app = FALSE
)
save_csv(talhoes, "talhoes.csv")

produtos <- data.frame(
  produto_id = c("CAF_SACA", "CAF_TORRADO", "HORTA_CESTA", "HORTA_AVULSO", "BANANA_CAIXA", "MEL_POTE", "MUDA", "OFICINA"),
  produto = c("Cafe verde especial", "Cafe torrado e moido", "Cesta de hortalicas", "Hortalicas avulsas", "Caixa de banana", "Pote de mel", "Muda agroflorestal", "Oficina ou visita"),
  grupo_produto = c("Cafe", "Cafe", "Horta", "Horta", "Banana", "Mel", "Mudas", "Servicos"),
  atividade = c("cafeicultura", "beneficiamento", "horta", "horta", "agrofloresta", "meliponicultura", "viveiro", "formacao"),
  unidade_padrao = c("saca_60kg", "pacote_250g", "cesta", "kg", "caixa", "pote_300g", "unidade", "participacao"),
  principal = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)
save_csv(produtos, "produtos.csv")

eventos <- data.frame(
  evento_id = c("E2019_PREMIO", "E2021_SECA", "E2024_CHUVA"),
  inicio = c("2019-08-01", "2021-05-01", "2024-01-10"), fim = c("2020-03-31", "2021-09-30", "2024-02-10"),
  tipo = c("reconhecimento_comercial", "seca_sazonal", "chuva_intensa"),
  descricao = c("Microlote vence concurso regional", "Estiagem prolongada", "Chuvas intensas elevam turbidez"),
  efeito_esperado = c("aumento de preco, sem aumento automatico de produtividade", "reducao de umidade e produtividade", "aumento transitorio de turbidez")
)
save_csv(eventos, "eventos_contexto.csv")

manejo <- do.call(rbind, lapply(2016:2025, function(ano) {
  sombra <- ifelse(sistema == "cafe_solteiro", 8, ifelse(sistema == "cafe_banana", 28, ifelse(sistema == "cafe_inga", 34, 48)))
  data.frame(talhao_id, ano, sistema, idade_cafezal = sample(4:14, n, TRUE) + ano - 2016,
    cultivar = sample(c("Catuai Vermelho", "Mundo Novo", "Arara"), n, TRUE, c(.55, .3, .15)),
    densidade_cafe_ha = round(rnorm(n, 4200, 180)),
    densidade_banana_ha = ifelse(grepl("banana", sistema), round(rnorm(n, 310, 35)), 0),
    densidade_inga_ha = ifelse(grepl("inga", sistema), round(rnorm(n, 80, 12)), 0),
    cobertura_solo_pct = round(lim(rnorm(n, sombra + 28, 8), 25, 96)),
    sombreamento_pct = round(lim(rnorm(n, sombra, 6), 2, 75)),
    dose_composto_kg_ha = round(lim(rnorm(n, 1600 + 10 * sombra, 220), 800, 2800)),
    poda_realizada = sample(c(TRUE, FALSE), n, TRUE, c(.78, .22)), irrigacao = FALSE)
}))
save_csv(manejo, "manejo_talhao_ano.csv")

# Clima regional diario, comum a toda propriedade.
dias <- seq(as.Date("2016-01-01"), as.Date("2025-12-31"), by = "day")
doy <- as.integer(format(dias, "%j")); ano_dia <- as.integer(format(dias, "%Y")); mes_dia <- as.integer(format(dias, "%m"))
seca <- as.integer(ano_dia == 2021 & mes_dia %in% 5:9)
clima <- data.frame(
  data = dias,
  chuva_mm = round(pmax(0, 4 + 6 * sin((doy - 25) * 2 * pi / 365) + rnorm(length(dias), 0, 6) - 3 * seca), 1),
  temperatura_min_c = round(15 + 3 * sin((doy - 190) * 2 * pi / 365) + rnorm(length(dias), 0, 1.4), 1),
  temperatura_max_c = round(27 + 4.8 * sin((doy - 190) * 2 * pi / 365) + 1.2 * seca + rnorm(length(dias), 0, 1.8), 1),
  umidade_relativa_pct = round(lim(74 + 10 * sin((doy - 20) * 2 * pi / 365) - 6 * seca + rnorm(length(dias), 0, 6), 28, 100)),
  radiacao_mj_m2 = round(lim(16 + 5 * sin((doy - 185) * 2 * pi / 365) + rnorm(length(dias), 0, 1.5), 5, 30), 1),
  vento_m_s = round(lim(rnorm(length(dias), 2.1, .8), .1, 6), 1)
)
save_csv(clima, "clima_regional_diario.csv")

# Solo: grupos por faixa da paisagem, com efeito menor do consorcio.
campanhas <- as.Date(c("2021-09-15", "2023-09-15", "2025-09-15"))
solo <- do.call(rbind, lapply(campanhas, function(data) {
  organ <- c(-6, 0, 7)[faixa_i] + ifelse(grepl("inga", sistema), 3, 0) + ifelse(grepl("banana", sistema), 1.5, 0) + rnorm(n, 0, 2)
  argila <- c(30, 48, 39)[faixa_i] + rnorm(n, 0, 3)
  areia <- lim(73 - argila + rnorm(n, 0, 2), 18, 68)
  data.frame(
    amostra_solo_id = sprintf("S%s_%s_01", format(data, "%Y"), talhao_id), talhao_id, data_coleta = data, profundidade_cm = "0_20",
    pH = round(lim(c(5.1, 5.5, 5.7)[faixa_i] + .015 * organ + rnorm(n, 0, .18), 4.4, 6.8), 2),
    materia_organica_g_kg = round(lim(31 + organ + rnorm(n, 0, 2), 15, 55), 1),
    carbono_g_kg = round(lim(18 + .55 * organ + rnorm(n, 0, 1.5), 8, 35), 1),
    nitrogenio_g_kg = round(lim(1.25 + .045 * organ + rnorm(n, 0, .12), .6, 3), 2),
    fosforo_mg_dm3 = round(lim(13 + ifelse(grepl("inga", sistema), 2, 0) + rnorm(n, 0, 4), 3, 40), 1),
    potassio_mg_dm3 = round(lim(120 + ifelse(grepl("banana", sistema), 14, 0) + rnorm(n, 0, 22), 45, 260)),
    calcio_cmolc_dm3 = round(lim(2.5 + .07 * organ + rnorm(n, 0, .35), .8, 6), 2),
    magnesio_cmolc_dm3 = round(lim(.8 + .035 * organ + rnorm(n, 0, .15), .25, 2.2), 2),
    ctc_cmolc_dm3 = round(lim(6.2 + .13 * organ + .025 * argila + rnorm(n, 0, .5), 3, 15), 2),
    areia_pct = round(areia, 1), silte_pct = round(lim(100 - argila - areia, 8, 35), 1), argila_pct = round(argila, 1),
    densidade_g_cm3 = round(lim(1.37 - .009 * organ - .002 * argila + rnorm(n, 0, .035), .9, 1.55), 2),
    infiltracao_mm_h = round(lim(18 + 1.2 * organ + ifelse(grepl("banana", sistema), 3, 0) + rnorm(n, 0, 4), 7, 75), 1),
    umidade_gravimetrica_pct = round(lim(17 + .42 * organ + c(-3, 0, 4)[faixa_i] + rnorm(n, 0, 2), 8, 42), 1)
  )
}))
save_csv(solo, "solo_talhao.csv")

# Sensores: minima/maxima do ar; o drone medira temperatura de superficie.
meses <- seq(as.Date("2021-01-01"), as.Date("2025-12-01"), by = "month")
micro <- do.call(rbind, lapply(meses, function(data) {
  ano <- as.integer(format(data, "%Y")); mes <- as.integer(format(data, "%m"))
  cmes <- clima[format(clima$data, "%Y-%m") == format(data, "%Y-%m"), ]
  m <- manejo[manejo$ano == ano, ]; sombra <- m$sombreamento_pct
  tmin <- mean(cmes$temperatura_min_c) + .02 * sombra - .006 * (talhoes$altitude_m - 920) + rnorm(n, 0, .45)
  tmax <- mean(cmes$temperatura_max_c) - .075 * sombra - .004 * (talhoes$altitude_m - 920) + rnorm(n, 0, .7)
  seca_mes <- as.integer(ano == 2021 & mes %in% 5:9)
  data.frame(
    talhao_id, ano_mes = format(data, "%Y-%m"), temperatura_min_c = round(tmin, 1), temperatura_max_c = round(tmax, 1),
    amplitude_termica_c = round(tmax - tmin, 1),
    umidade_relativa_pct = round(lim(64 + .22 * sombra + c(-4, 0, 5)[faixa_i] + 7 * sin((mes - 1) * 2 * pi / 12) - 5 * seca_mes + rnorm(n, 0, 3), 35, 98)),
    umidade_solo_pct = round(lim(16 + .13 * sum(cmes$chuva_mm) + .16 * sombra + c(-4, 0, 5)[faixa_i] - 5 * seca_mes + rnorm(n, 0, 2), 7, 46), 1),
    chuva_mm = round(sum(cmes$chuva_mm), 1), sombreamento_pct = sombra
  )
}))
save_csv(micro, "microclima_talhao_mes.csv")

voos <- as.Date(unlist(lapply(2021:2025, function(ano) paste0(ano, c("-02-15", "-05-15", "-08-15", "-11-15")))))
drone <- do.call(rbind, lapply(voos, function(data) {
  m <- micro[micro$ano_mes == format(data, "%Y-%m"), ]
  data.frame(
    talhao_id, data_campanha = data, horario_voo = "11:30",
    temperatura_superficie_media_c = round(m$temperatura_max_c + 3.8 - .045 * m$sombreamento_pct + rnorm(n, 0, .8), 1),
    temperatura_superficie_min_c = round(m$temperatura_min_c + 1.5 + rnorm(n, 0, .5), 1),
    temperatura_superficie_max_c = round(m$temperatura_max_c + 6 - .08 * m$sombreamento_pct + rnorm(n, 0, 1), 1),
    indice_cobertura_vegetal = round(lim(.43 + .006 * m$sombreamento_pct + rnorm(n, 0, .03), .3, .92), 2), qualidade_imagem = "boa"
  )
}))
save_csv(drone, "drone_termico.csv")

pontos <- data.frame(ponto_agua_id = c("AG01", "AG02", "AG03"), nome = c("Montante", "Apos a area produtiva", "Jusante"), posicao = c("montante", "intermediario", "jusante"))
save_csv(pontos, "pontos_agua.csv")
coletas <- seq(as.Date("2021-01-20"), as.Date("2025-12-20"), by = "month")
agua <- do.call(rbind, lapply(coletas, function(data) {
  chuva72 <- sum(clima$chuva_mm[clima$data >= data - 3 & clima$data <= data]); pico <- as.integer(format(data, "%Y-%m") == "2024-01"); p <- 0:2
  data.frame(
    ponto_agua_id = pontos$ponto_agua_id, data_coleta = data, chuva_72h_mm = round(chuva72, 1),
    vazao_l_s = round(lim(3.5 + .045 * chuva72 + rnorm(3, 0, .6), 1, 14), 1),
    turbidez_ntu = round(lim(2.5 + .19 * chuva72 + 1.8 * p + 15 * pico + rnorm(3, 0, 2), .4, 80), 1),
    pH = round(lim(6.6 - .05 * p + rnorm(3, 0, .12), 5.8, 7.4), 2), condutividade_us_cm = round(lim(50 + 7 * p + rnorm(3, 0, 4), 25, 100)),
    nitrato_mg_l = round(lim(.35 + .13 * p + .003 * chuva72 + rnorm(3, 0, .07), .03, 2), 2),
    fosforo_mg_l = round(lim(.025 + .012 * p + .0015 * chuva72 + rnorm(3, 0, .01), .005, .4), 3),
    observacao_campo = ifelse(pico == 1, "chuva intensa na semana", "coleta de rotina")
  )
}))
save_csv(agua, "qualidade_agua.csv")

# Producao e qualidade do cafe: efeito de consorcio existe, mas nao explica tudo.
producao <- do.call(rbind, lapply(2021:2025, function(ano) {
  ma <- micro[substr(micro$ano_mes, 1, 4) == ano, ]
  umidade <- tapply(ma$umidade_solo_pct, ma$talhao_id, mean)[talhao_id]
  efeito_sistema <- ifelse(sistema == "cafe_solteiro", 0, ifelse(sistema == "cafe_banana", 85, ifelse(sistema == "cafe_inga", 105, 145)))
  produtividade <- lim(1880 + efeito_sistema + c(-150, 10, 155)[faixa_i] + 22 * (umidade - mean(umidade)) + ifelse(ano == 2021, -280, 0) + rnorm(n, 0, 155), 850, 3500)
  nota <- lim(80 + .0045 * (produtividade - 1800) + .7 * grepl("inga", sistema) + rnorm(n, 0, 1.5), 76, 88)
  preco <- 820 + 52 * (nota - 80)
  data.frame(
    talhao_id, ano_safra = ano, area_colhida_ha = talhoes$area_ha,
    producao_cereja_kg = round(produtividade * talhoes$area_ha * 3.45),
    producao_beneficiada_kg = round(produtividade * talhoes$area_ha), produtividade_kg_ha = round(produtividade),
    rendimento_beneficiamento_pct = round(lim(28 + .04 * (nota - 80) + rnorm(n, 0, 1), 23, 35), 1),
    peneira_alta_pct = round(lim(55 + 1.1 * (nota - 80) + rnorm(n, 0, 4), 35, 82)),
    defeitos_pct = round(lim(13 - .8 * (nota - 80) + rnorm(n, 0, 1.2), 2, 22), 1),
    nota_sensorial = round(nota, 1), preco_medio_saca = round(preco),
    receita_estimada_cafe = round(produtividade * talhoes$area_ha / 60 * preco),
    custo_direto_estimado = round(talhoes$area_ha * (4100 + 120 * grepl("inga", sistema) + rnorm(n, 0, 250)))
  )
}))
save_csv(producao, "producao_talhao_safra.csv")

# Livro-caixa atomico: horta e frequente, cafe concentra a maior receita anual.
lancamentos <- list()
contador <- 0L
adiciona <- function(data, tipo, categoria, subcategoria, produto = NA_character_, quantidade = NA_real_, unidade = NA_character_, preco = NA_real_, canal = NA_character_, atividade, talhao = NA_character_, alocacao = "direto", evento = NA_character_, observacao = "") {
  contador <<- contador + 1L
  total <- if (is.na(quantidade)) preco else quantidade * preco
  lancamentos[[contador]] <<- data.frame(
    lancamento_id = sprintf("L%04d_%06d", as.integer(format(data, "%Y")), contador),
    data, tipo, categoria, subcategoria, produto_id = produto, quantidade, unidade,
    valor_unitario = preco, valor_total = round(total, 2), canal_venda = canal,
    atividade, talhao_id = talhao, natureza_alocacao = alocacao, evento_id = evento, observacao
  )
}

for (data_semana_bruta in seq(as.Date("2016-01-04"), as.Date("2025-12-28"), by = "week")) {
  # `for` retira a classe Date; recuperamo-la antes de agregar data ou ano.
  data_semana <- as.Date(data_semana_bruta, origin = "1970-01-01")
  ano <- as.integer(format(data_semana, "%Y"))
  mes <- as.integer(format(data_semana, "%m"))
  for (j in seq_len(sample(2:4, 1))) {
    cesta <- runif(1) < .68
    quantidade <- if (cesta) sample(5:15, 1) else round(runif(1, 7, 28), 1)
    preco <- if (cesta) round(rnorm(1, 42, 4), 2) else round(rnorm(1, 10.5, 1.1), 2)
    adiciona(data_semana + sample(0:4, 1), "receita", "venda_produto", "feira_local", if (cesta) "HORTA_CESTA" else "HORTA_AVULSO", quantidade, if (cesta) "cesta" else "kg", preco, "feira_e_csa", "horta")
  }
  if (runif(1) < ifelse(mes %in% c(1:4, 10:12), .6, .28)) {
    adiciona(data_semana, "receita", "venda_produto", "banana_agroflorestal", "BANANA_CAIXA", sample(3:10, 1), "caixa", round(rnorm(1, 48, 5), 2), "feira_e_csa", "agrofloresta", sample(talhao_id[grepl("banana", sistema)], 1))
  }
  if (mes %in% 6:11 && runif(1) < .38) {
    premio <- ano == 2019 && runif(1) < .9
    preco <- if (premio) rnorm(1, 3100, 180) else rnorm(1, 790 + 14 * (ano - 2016), 65)
    adiciona(data_semana, "receita", "venda_produto", if (premio) "microlote_premiado" else "cafe_verde", "CAF_SACA", sample(8:24, 1), "saca_60kg", round(preco, 2), if (premio) "cafeteria_especializada" else "cooperativa", "cafeicultura", sample(talhao_id, 1), "direto", if (premio) "E2019_PREMIO" else NA_character_, if (premio) "lote reconhecido em concurso regional" else "venda de safra")
  }
  if (mes %in% 6:12 && runif(1) < .2) {
    adiciona(data_semana, "receita", "venda_produto", "cafe_torrado", "CAF_TORRADO", sample(35:110, 1), "pacote_250g", round(rnorm(1, 20, 1.8), 2), "venda_direta", "beneficiamento")
  }
  if (runif(1) < .035) adiciona(data_semana, "receita", "venda_produto", "mel", "MEL_POTE", sample(25:90, 1), "pote_300g", round(rnorm(1, 24, 2), 2), "feira_local", "meliponicultura")
  if (runif(1) < .055) adiciona(data_semana, "receita", "venda_produto", "mudas", "MUDA", sample(15:70, 1), "unidade", round(rnorm(1, 12, 1.2), 2), "viveiro", "viveiro")
}

for (ano in 2016:2025) for (mes in 1:12) {
  data_mes <- as.Date(sprintf("%d-%02d-08", ano, mes))
  adiciona(data_mes, "despesa", "manejo", "mao_de_obra", preco = round(rnorm(1, 6200, 650), 2), atividade = "cafeicultura", alocacao = "compartilhado")
  adiciona(data_mes + 3, "despesa", "operacao", "transporte_e_embalagem", preco = round(rnorm(1, 1300, 180), 2), atividade = "beneficiamento", alocacao = "compartilhado")
  if (mes %in% c(2, 3, 9, 10)) adiciona(data_mes + 8, "despesa", "insumo", "composto_e_cobertura", preco = round(rnorm(1, 8400, 850), 2), atividade = "cafeicultura", alocacao = "compartilhado")
  if (mes %in% c(5, 6, 7)) adiciona(data_mes + 12, "despesa", "manejo", "colheita_e_beneficiamento", preco = round(rnorm(1, 4600, 600), 2), atividade = "cafeicultura", alocacao = "compartilhado")
  adiciona(data_mes + 18, "despesa", "infraestrutura", "energia_e_manutencao", preco = round(rnorm(1, 980, 120), 2), atividade = "geral", alocacao = "compartilhado")
}
lancamentos <- do.call(rbind, lancamentos)
lancamentos <- lancamentos[order(lancamentos$data, lancamentos$lancamento_id), ]
save_csv(lancamentos, "lancamentos_financeiros.csv")

dicionario <- data.frame(
  arquivo = c("talhoes.csv", "lancamentos_financeiros.csv", "producao_talhao_safra.csv", "solo_talhao.csv", "microclima_talhao_mes.csv", "drone_termico.csv", "qualidade_agua.csv"),
  chave = c("talhao_id", "lancamento_id", "talhao_id + ano_safra", "amostra_solo_id", "talhao_id + ano_mes", "talhao_id + data_campanha", "ponto_agua_id + data_coleta"),
  granularidade = c("um talhao", "uma movimentacao", "talhao por safra", "amostra de solo", "talhao por mes", "talhao por voo", "ponto por coleta"),
  descricao = c("Cadastro espacial e covariaveis", "Entradas e saidas do livro-caixa", "Producao observacional de cafe", "Analises de solo a 0-20 cm", "Sensores consolidados", "Termografia de superficie", "Monitoramento do corrego")
)
save_csv(dicionario, "dicionario_arquivos.csv")
message("Dados gerados em: ", out_dir)
