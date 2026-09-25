# Pressupostos e referências: escolha de hiperparâmetros e avaliação.

.tr_ml_docs_avaliacao <- function() {
  P <- .tr_ml_P; I <- .tr_ml_impl; R <- trama::tr_ref
  L <- .tr_ml_livros(); C <- .tr_ml_comuns()
  teste_fora <- P("As linhas avaliadas são do **teste**, que não participou do ajuste nem da escolha de hiperparâmetros. O bloco mede o que receber: no treino, ou num teste já usado para decidir, a medida sai otimista.",
    se_falhar = "Avalie a saída teste do `ml/split` passada pelo `models/predict`; se o teste já foi usado para escolher, separe um novo teste ou reporte a estimativa como otimista.")
  desequilibrio <- P("Com **classes desequilibradas**, a acurácia engana: prever sempre a classe maioritária já a deixa alta. A acurácia balanceada e o macro F1 dão peso igual a cada classe.",
    verificar = "data/group_summarise",
    se_falhar = "Conte as linhas por classe no `data/group_summarise` e leia a `balanced_accuracy` ou o `macro_f1` e a `models/confusion`, não só a `accuracy`.")
  fawcett <- R(autores = "Fawcett, T.", ano = 2006, titulo = "An introduction to ROC analysis",
               fonte = "Pattern Recognition Letters, 27(8), 861-874", doi = "10.1016/j.patrec.2005.10.010")
  brodersen <- R(autores = c("Brodersen, K. H.", "Ong, C. S.", "Stephan, K. E.", "Buhmann, J. M."), ano = 2010,
                 titulo = "The Balanced Accuracy and Its Posterior Distribution",
                 fonte = "20th International Conference on Pattern Recognition (ICPR), 3121-3124",
                 doi = "10.1109/ICPR.2010.764", papel = "complementar")
  list(
    "ml/tune" = list(
      pressupostos = list(
        P("Entra **só o treino**: os folds saem das linhas recebidas e o vencedor é reajustado nelas todas. Se o teste entrar aqui, ele deixa de ser teste.",
          se_falhar = "Ligue a saída treino do `ml/split`; a teste vai só ao `models/predict`."),
        C$sem_vazamento,
        P("Os folds refletem a **dependência** dos dados: `aleatoria` (estratificada pela classe) supõe linhas independentes; `grupo` põe cada indivíduo, lote ou área num só fold; `temporal` usa origem móvel com janela crescente — cada fold treina no passado e valida no bloco seguinte, nunca no futuro do treino. Folds aleatórios com dados dependentes dão erro otimista (Roberts et al. 2017).",
          se_falhar = "Escolha `estrategia = \"grupo\"` com `grupo`, ou `\"temporal\"` com `ordem`, e use a mesma estratégia no `ml/split`."),
        P("A **média dos folds do vencedor é otimista**: foi a melhor entre muitas tentativas, e parte da vantagem é sorte. Ela serve para escolher, não para reportar o desempenho.",
          verificar = "ml/tuning_plot",
          se_falhar = "Reporte o desempenho medido no teste com `models/predict` e `models/evaluate`. Sem teste separado (n pequeno), use o `ml/nested_cv`, que estima o procedimento inteiro sem reaproveitar as linhas da escolha."),
        P("A **métrica** escolhida é a que importa no problema; `auto` usa RMSE (regressão) e macro F1 (classificação).",
          verificar = "data/group_summarise",
          se_falhar = "Com classes desequilibradas, prefira `balanced_accuracy` ou `macro_f1` a `accuracy`."),
        P("As **tentativas bastam** para o espaço de busca (busca aleatória): se o melhor ainda melhora perto do fim, o orçamento foi curto.",
          verificar = "ml/tuning_plot",
          se_falhar = "Aumente `tentativas` ou mude a `amplitude`."),
        C$semente),
      referencias = list(
        R(autores = "Stone, M.", ano = 1974, titulo = "Cross-Validatory Choice and Assessment of Statistical Predictions",
          fonte = "Journal of the Royal Statistical Society. Series B, 36(2), 111-133",
          doi = "10.1111/j.2517-6161.1974.tb00994.x"),
        R(autores = c("Bergstra, J.", "Bengio, Y."), ano = 2012, titulo = "Random Search for Hyper-Parameter Optimization",
          fonte = "Journal of Machine Learning Research, 13, 281-305",
          url = "https://www.jmlr.org/papers/v13/bergstra12a.html"),
        L$islr, L$kuhn, L$roberts, L$tashman, L$fpp3, L$bergmeir18,
        R(autores = c("Varma, S.", "Simon, R."), ano = 2006,
          titulo = "Bias in error estimation when using cross-validation for model selection",
          fonte = "BMC Bioinformatics, 7, 91", doi = "10.1186/1471-2105-7-91", papel = "complementar"),
        I("trama.ml", "tr_ml_tune", "Implementação própria: busca aleatória (log-uniforme para `cost`, `gamma` e `eta`) avaliada nos mesmos k folds (aleatórios estratificados, por grupo ou de origem móvel no tempo); o vencedor, pela média, é reajustado com `tr_ml_fit` em todas as linhas."))),

    "ml/nested_cv" = list(
      pressupostos = list(
        P("A estimativa é do **procedimento de ajuste inteiro** (busca + reajuste), não de um modelo final específico: cada fold externo escolhe hiperparâmetros próprios, e o vencedor pode mudar de fold para fold.",
          se_falhar = "Para o modelo a usar, rode o `ml/tune` em todas as linhas; reporte a média `externa` do `ml/nested_cv` como o desempenho esperado desse procedimento."),
        P("Só a coluna **`externa`** é honesta: mede linhas que a busca nunca viu. A `interna` é a média dos folds do vencedor dentro da busca e sai otimista (Varma & Simon 2006); a diferença entre elas mostra o otimismo da seleção.",
          se_falhar = "Não reporte a `interna`; se ela ficar muito acima da `externa`, a busca está escolhendo ruído — reduza o espaço ou as tentativas."),
        P("Os folds externos e internos seguem a mesma **estratégia** (`aleatoria`, `grupo` ou `temporal`) e supõem a mesma estrutura de dependência do `ml/tune`.",
          se_falhar = "Com tempo ou grupos, escolha `estrategia = \"temporal\"` (com `ordem`) ou `\"grupo\"` (com `grupo`)."),
        P("Com poucas linhas, cada busca interna vê só uma parte do treino: a estimativa externa é **pessimista e variável** (poucas linhas por fold), e o custo é folds externos × tentativas × folds internos ajustes.",
          se_falhar = "Leia a variação entre folds; aumente `folds_externos` só se houver linhas para isso."),
        C$semente),
      referencias = list(
        R(autores = c("Varma, S.", "Simon, R."), ano = 2006,
          titulo = "Bias in error estimation when using cross-validation for model selection",
          fonte = "BMC Bioinformatics, 7, 91", doi = "10.1186/1471-2105-7-91"),
        R(autores = c("Cawley, G. C.", "Talbot, N. L. C."), ano = 2010,
          titulo = "On over-fitting in model selection and subsequent selection bias in performance evaluation",
          fonte = "Journal of Machine Learning Research, 11, 2079-2107",
          url = "https://www.jmlr.org/papers/v11/cawley10a.html", papel = "complementar"),
        L$islr, L$kuhn,
        I("trama.ml", "tr_ml_nested_cv", "Implementação própria: folds externos formados como no `ml/tune`; em cada um, `tr_ml_tune` completo só no treino externo e a métrica do vencedor na validação externa."))),

    "ml/split" = list(
      pressupostos = list(
        P("A **estratégia de divisão** respeita a dependência entre linhas: `aleatoria` supõe linhas independentes; com medidas no tempo o teste deve ser todo posterior ao treino (`temporal`), e com várias linhas do mesmo indivíduo, lote ou área nenhum grupo pode cair dos dois lados (`grupo`). Senão o teste mede memorização, não generalização (Roberts et al. 2017).",
          se_falhar = "Troque `estrategia` para `temporal` (com `ordem`) ou `grupo` (com `grupo`) e use a mesma no `ml/tune`."),
        P("Na divisão `temporal`, o teste representa o **futuro em que o modelo será usado**: o corte no tempo cai onde a proporção pede, com instantes empatados todos do mesmo lado.",
          se_falhar = "Se houver mudança de regime depois do corte, o teste mede também essa mudança; leia o desempenho junto do período."),
        P("Na divisão `grupo`, `proporcao` é a **fração dos grupos**; com grupos de tamanhos muito diferentes, a fração das linhas pode se afastar dela, e a estratificação por classe não é feita.",
          verificar = "data/group_summarise",
          se_falhar = "Conte linhas e classes por lado no `data/group_summarise`; se uma classe faltar no teste, mude a semente ou a proporção."),
        C$semente),
      referencias = list(L$roberts, L$tashman, L$fpp3, L$islr, L$kuhn,
        I("trama.ml", "tr_ml_split", "Implementação própria: sorteio estratificado pela classe (`aleatoria`); corte no instante da linha floor(n·proporção) na ordem do tempo, todas as linhas até ele no treino (`temporal`); sorteio de floor(G·proporção) grupos inteiros (`grupo`).")))

  )
}
