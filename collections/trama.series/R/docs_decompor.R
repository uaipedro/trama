# Pressupostos e referências: decomposição e regressão da série.

.tr_series_docs_decompor <- function() {
  P <- .tr_series_P; I <- .tr_series_impl; R <- trama::tr_ref
  L <- .tr_series_livros()
  ciclo <- P("O **ciclo declarado** (a frequência da série) é o período sazonal verdadeiro, e a série tem **pelo menos dois ciclos** completos.",
             verificar = c("series/seasonal_plot", "series/subseries", "series/periodicity_fisher"),
             se_falhar = "Declare a frequência certa no nó que cria a série (`series/from_table`).")
  erro_indep <- P(
    "Com **Erro = independente** (padrão, MQO), os erros da regressão são **independentes** (sem autocorrelação). Série temporal quase nunca obedece, e o efeito é conhecido: erros-padrão pequenos demais e p-valores **otimistas**. Com **Erro = arma**, o erro segue o ARMA(p, q) declarado.",
    verificar = c("series/component", "series/ljung_box", "series/acf"),
    se_falhar = "Tire o `resto` com `series/component` e passe no `series/ljung_box`. Com autocorrelação, ligue **Erro = arma** no `series/regression` (GLS com erro ARMA; comece por AR(1) e escolha a ordem pela `series/acf` e `series/pacf` do resto); os F do `models/anova_table` viram testes de Wald do GLS. Perto da raiz unitária nem o GLS segura o nível: diferencie. Os testes não paramétricos (`series/mann_kendall`, `series/seasonality_kw`) também supõem independência, então não resolvem.")
  pinheiro_bates <- R(autores = c("Pinheiro, J. C.", "Bates, D. M."), ano = 2000,
                      titulo = "Mixed-Effects Models in S and S-PLUS",
                      fonte = "New York: Springer", doi = "10.1007/b98882", papel = "livro-texto")
  erro_normal <- P(
    "Os erros são **normais e de variância constante**: o F é exato só assim.",
    verificar = c("series/component", "view/qq", "models/shapiro"),
    se_falhar = "O `resto` do `series/component` vira tabela no fio (coluna `valor`). Variância que cresce com o nível: ajuste no log (`series/transform`). Sem normalidade: `series/mann_kendall` (tendência) ou `series/seasonality_kw` (sazonalidade).")
  forma <- P(
    "A **fórmula** descreve a série: com `periodo` como efeito principal, a sazonalidade é **fixa** (o mesmo efeito por período em todos os ciclos, determinística), e a tendência tem a forma escrita em `t`. Tendência que muda de forma, ou sazonalidade que evolui, sobra no resto.",
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

    "series/deseasonalize" = list(
      pressupostos = list(
        P("A sazonalidade é **fixa e aditiva**: o mesmo efeito por período em todo ciclo, somado ao nível. Sazonalidade que muda com os anos, ou que cresce com o nível, sobra no resto.",
          verificar = c("series/seasonal_plot", "series/subseries", "series/range_mean"),
          se_falhar = "Oscilação que cresce com o nível: `series/transform` com `log` antes. Padrão que muda: `series/stl`."),
        P("Com **Controlar a tendência** desligado, a série **não tem tendência**: senão os períodos do fim do ano parecem maiores só por virem depois.",
          verificar = c("series/plot", "series/mann_kendall"),
          se_falhar = "Ligue **Controlar a tendência**, ou use `series/regression`."),
        erro_indep, erro_normal, ciclo),
      referencias = c(regressao_ref, list(
        I("stats", "lm", "`valor ~ t + periodo` (ou `valor ~ periodo`), com `periodo` fator do ciclo e contraste `contr.sum` ou `contr.treatment` posto no fator; o componente sazonal é a parte do preditor que vem de `periodo`, centrada pela média do ciclo.")))),

    "series/regression" = list(
      pressupostos = list(forma, erro_indep, erro_normal,
        P("Potências cruas do tempo (`t`, `I(t^2)`, `I(t^3)`) como termos separados são **quase colineares**: os coeficientes de tendência não se leem um a um.",
          se_falhar = "Escreva a tendência num termo só, `poly(t, 2, raw = TRUE)`, e leia o F da linha dela no `models/anova_table` (tipo III), que testa o bloco inteiro."),
        P("A tendência é **determinística**. Uma série com raiz unitária — passeio aleatório — produz tendência aparente e F significativo sem tendência nenhuma (regressão espúria).",
          verificar = c("series/adf", "series/kpss"),
          se_falhar = "Com raiz unitária, diferencie (`series/diff`) e modele a série diferenciada."),
        P("Com **regressor** ligado: o efeito dele é **contemporâneo** (x no mesmo instante, sem defasagem) e **exógeno** (y não volta a mexer em x). Duas séries que só compartilham tendência produzem regressão espúria: o p do β sai pequeno sem relação nenhuma, sobretudo com erro autocorrelacionado.",
          verificar = c("series/ljung_box", "series/adf"),
          se_falhar = "Deixe a tendência na fórmula (`t`), ligue **Erro = arma** se o resto for autocorrelacionado, e com séries de raiz unitária diferencie as duas antes (`series/diff`).")),
      referencias = c(regressao_ref, list(
        R(autores = c("Granger, C. W. J.", "Newbold, P."), ano = 1974, titulo = "Spurious regressions in econometrics",
          fonte = "Journal of Econometrics, 2(2), 111-120", doi = "10.1016/0304-4076(74)90034-7", papel = "complementar"),
        I("stats", "lm", "Mínimos quadrados ordinários da fórmula sobre `valor`, `t`, `periodo`, `ano` e `regressor`; o contraste de `periodo` é `contr.sum` (soma_zero) ou `contr.treatment` (categoria_base), posto no fator."),
        pinheiro_bates,
        I("nlme", "gls", "Com `erro = \"arma\"`: `gls(y ~ ..., correlation = corARMA(p = ar, q = ma), method = \"ML\")`. Conferido contra `stats::arima(xreg = , method = \"ML\")` (coeficientes a 1e-3, log-verossimilhança a 1e-4) e, em AR(1), contra o MQO de Prais-Winsten com o phi estimado (1e-8)."))))
  )
}
