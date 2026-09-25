# Pressupostos e referências: decomposição e os F da regressão.

.tr_series_docs_decompor <- function() {
  P <- .tr_series_P; I <- .tr_series_impl; R <- trama::tr_ref
  L <- .tr_series_livros()
  ciclo <- P("O **ciclo declarado** (a frequência da série) é o período sazonal verdadeiro, e a série tem **pelo menos dois ciclos** completos.",
             verificar = c("series/seasonal_plot", "series/subseries", "series/fisher"),
             se_falhar = "Declare a frequência certa no nó que cria a série (`series/from_table`).")
  erro_indep <- P(
    "Com **Erro = independente** (padrão, MQO), os erros da regressão são **independentes** (sem autocorrelação). Série temporal quase nunca obedece, e o efeito é conhecido: erros-padrão pequenos demais e p-valores **otimistas**. Com **Erro = arma**, o erro segue o ARMA(p, q) declarado.",
    verificar = c("series/component", "series/ljung_box", "series/acf"),
    se_falhar = "Tire o `resto` com `series/component` e passe no `series/ljung_box`. Com autocorrelação, ligue **Erro = arma** no `series/regression` (GLS com erro ARMA; comece por AR(1) e escolha a ordem pela `series/acf` e `series/pacf` do resto); os três F viram testes de Wald do GLS. Perto da raiz unitária nem o GLS segura o nível: diferencie. Os testes não paramétricos (`series/mann_kendall`, `series/kruskal_wallis`) também supõem independência, então não resolvem.")
  pinheiro_bates <- R(autores = c("Pinheiro, J. C.", "Bates, D. M."), ano = 2000,
                      titulo = "Mixed-Effects Models in S and S-PLUS",
                      fonte = "New York: Springer", doi = "10.1007/b98882", papel = "livro-texto")
  f_wald <- I("trama.series", "tr_series_f_global",
              "Com regressão de erro ARMA (GLS): F de Wald b'V⁻¹b/q do bloco, com a covariância do `gls`, q e n - p graus de liberdade; conferido refazendo a conta à mão (1e-8).")
  erro_normal <- P(
    "Os erros são **normais e de variância constante**: o F é exato só assim.",
    verificar = c("series/component", "view/qq", "models/shapiro"),
    se_falhar = "O `resto` do `series/component` vira tabela no fio (coluna `valor`). Variância que cresce com o nível: ajuste no log (`series/transform`). Sem normalidade: `series/mann_kendall` (tendência) ou `series/kruskal_wallis` (sazonalidade).")
  forma <- P(
    "A tendência é um **polinômio** do grau escolhido e a sazonalidade é **fixa** (o mesmo efeito para cada estação em todos os ciclos). Tendência que muda de forma, ou sazonalidade que evolui, sobra no resto.",
    verificar = c("series/plot_decomposition", "series/seasonal_plot"),
    se_falhar = "Sazonalidade que muda com os anos: `series/stl` (sem p-valor) ou diferença sazonal (`series/diff`).")
  regressao_ref <- list(L$morettin, L$fpp3)
  list(
    "series/decompose" = list(
      pressupostos = list(
        P("A série é **soma** (aditiva) ou **produto** (multiplicativa) de tendência, sazonal e resto. A multiplicativa é a certa quando a oscilação cresce com o nível, e pede série positiva.",
          verificar = "series/plot",
          se_falhar = "Na dúvida, decomponha o log (`series/transform`) na aditiva."),
        P("O **padrão sazonal é o mesmo em todos os ciclos**: a clássica estima UM efeito por estação.",
          verificar = c("series/seasonal_plot", "series/subseries"),
          se_falhar = "Use `series/stl` com janela sazonal finita, que deixa o padrão mudar."),
        P("A tendência é **suave o bastante** para uma média móvel de um ciclo acompanhá-la; um outlier ou uma quebra vazam para sazonal e tendência.",
          verificar = "series/plot",
          se_falhar = "`series/stl` com **Robusta** ligada."),
        ciclo),
      referencias = list(L$morettin, L$fpp3,
        I("stats", "decompose", "`type = \"additive\"` ou `\"multiplicative\"`; média móvel centrada de ordem igual à frequência (2×m quando par)."))),

    "series/stl" = list(
      pressupostos = list(
        P("A série é **soma** de tendência, sazonal e resto: a STL é só aditiva.",
          verificar = "series/plot",
          se_falhar = "Para sazonalidade multiplicativa, decomponha o log (`series/transform`)."),
        P("Tendência e sazonalidade **mudam devagar** (são suavizadas por loess); a janela sazonal escolhida diz o quão devagar.",
          verificar = "series/plot_decomposition",
          se_falhar = "Se o resto ainda mostra o ciclo, diminua a janela sazonal; se a sazonal fica ruidosa, aumente."),
        P("Outliers, sem **Robusta**, entram na estimação e puxam tendência e sazonal.",
          se_falhar = "Ligue **Robusta**."),
        ciclo),
      referencias = list(
        R(autores = c("Cleveland, R. B.", "Cleveland, W. S.", "McRae, J. E.", "Terpenning, I."), ano = 1990,
          titulo = "STL: a seasonal-trend decomposition procedure based on loess",
          fonte = "Journal of Official Statistics, 6(1), 3-73"),
        L$fpp3,
        I("stats", "stl", "`s.window = \"periodic\"` quando a janela é 0, senão o número dado; `robust` = **Robusta**."))),

    "series/regression" = list(
      pressupostos = list(forma, erro_indep, erro_normal,
        P("Do grau 2 em diante, as potências cruas do tempo são **quase colineares**: os coeficientes de tendência não se leem um a um.",
          se_falhar = "Leia a tendência pelo `series/f_tendencia`, que testa o bloco inteiro.")),
      referencias = c(regressao_ref, list(
        I("stats", "lm", "Mínimos quadrados ordinários de `y ~ t + ... + t^grau + estacao`; o contraste da estação é `contr.sum` (soma_zero) ou `contr.treatment` (categoria_base), posto no fator."),
        pinheiro_bates,
        I("nlme", "gls", "Com `erro = \"arma\"`: `gls(y ~ ..., correlation = corARMA(p = ar, q = ma), method = \"ML\")`. Conferido contra `stats::arima(xreg = , method = \"ML\")` (coeficientes a 1e-3, log-verossimilhança a 1e-4) e, em AR(1), contra o MQO de Prais-Winsten com o phi estimado (1e-8).")))),

    "series/f_global" = list(
      pressupostos = list(erro_indep, erro_normal, forma),
      referencias = c(regressao_ref, list(
        I("stats", "summary.lm", "O F do `summary()` do ajuste do `series/regression`; p-valor por `stats::pf`."),
        f_wald))),

    "series/f_sazonal" = list(
      pressupostos = list(erro_indep, erro_normal,
        P("A sazonalidade é **determinística**: o mesmo efeito por estação em todo ciclo. Sazonalidade estocástica (que muda de ano para ano) não é o que as dummies medem.",
          verificar = c("series/seasonal_plot", "series/subseries"),
          se_falhar = "Diferença sazonal (`series/diff`) e um modelo com parte sazonal (`series/arima`)."),
        P("A tendência está **bem modelada** pelo polinômio: tendência sobrando no erro infla a variância residual e esconde a sazonalidade.",
          verificar = "series/plot_decomposition")),
      referencias = c(regressao_ref, list(
        I("stats", "anova", "F parcial: `anova(lm(sem as dummies), ajuste)`, reajustado a partir de `fit$model`."),
        f_wald))),

    "series/f_tendencia" = list(
      pressupostos = list(erro_indep, erro_normal,
        P("A tendência é **determinística** (um polinômio no tempo). Uma série com raiz unitária — passeio aleatório — produz tendência aparente e F significativo sem tendência nenhuma (regressão espúria).",
          verificar = c("series/adf", "series/kpss"),
          se_falhar = "Com raiz unitária, diferencie (`series/diff`) e modele a série diferenciada."),
        P("A sazonalidade, se existe, está **no modelo**: sazonalidade sobrando no erro infla a variância residual.",
          verificar = "series/f_sazonal")),
      referencias = c(regressao_ref, list(
        I("stats", "anova", "F parcial: `anova(lm(sem os termos t...t^grau), ajuste)`, reajustado a partir de `fit$model`."),
        f_wald)))
  )
}
