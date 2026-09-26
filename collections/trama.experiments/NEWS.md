# trama.experiments 0.2.0

* Os nove blocos passam a declarar pressupostos e referências ESTRUTURADOS
  (`tr_pressuposto`, `tr_ref` com papel teoria/livro-texto/implementacao), em
  `R/docs_blocos.R`; as seções "Pressupostos" e "Referências" saem da prosa da
  ajuda, e a página as monta do nó. As referências são as já conferidas.

* `experiments/design` (versão 4): o param `alfa` do composto central virou
  `distancia_axial` (o glossário reserva `alfa` para significância) e aceita,
  além de `rotacional` e `face`, um número positivo. Fluxos salvos com `alfa`
  abrem migrados (`migrations`), e o re-sorteio lê planos antigos.

* `experiments/design` (versão 3): no crossover, a coluna `residual` não tem
  mais o nível `nenhum` (que coincidia com o 1º período e fazia o `lme4`
  descartar uma coluna). No 1º período ela leva o primeiro tratamento, a
  referência: mesmo espaço ajustado e mesmo F de t − 1 gl, matriz de posto
  completo, coeficientes = diferenças para o residual do 1º tratamento (Jones
  & Kenward, 2014). `residual` passa a nome reservado e papel do contrato.
* Ajuda: como ler de volta a magnitude de um produto de contrastes com
  `dentro`; aviso da gama com aleatório diz que o teste da parcela é liberal;
  referências conferidas em catálogo (notas "a conferir" removidas).
* `experiments/power` (novo, categoria Avaliar): poder por simulação de Monte
  Carlo. Refaz a cadeia `design → effect → error` guardada no plano com
  sementes derivadas, roda a análise do plano (ou outro nó de models, ou uma
  linha do `experiments/contrasts`) e conta as rejeições a p < `significancia`;
  taxa com IC de Clopper-Pearson (`confianca`), a hipótese dita verdadeira ou
  falsa no modelo declarado (erro tipo I × poder) e a curva por uma grade de
  `repeticoes`. Conferido contra o F não central, o `power.t.test` e α sob H0.
* `experiments/randomization_test` (novo): teste de aleatorização de Fisher.
  Re-sorteia a alocação pela receita do `experiments/design` (o escopo de cada
  fator), mantém a resposta de cada unidade e situa o F do termo (ou de um
  contraste) na distribuição; p exato por enumeração no DIC, no DBC e no
  fatorial neles, e de Monte Carlo (b + 1)/(R + 1) nos demais. Conferido
  contra o `coin` e a enumeração à mão do teste pareado.
* `experiments/effect`: cada termo guarda `argumentos` (a chamada sem plano e
  sem semente), campo opcional novo do contrato do plano — é por ele que o
  poder refaz a cadeia. Planos antigos continuam válidos.
* Parâmetro novo `significancia` (α de um teste) no glossário.
* `experiments/design` (versão 2): o quadrado latino é sorteado por Fisher &
  Yates — um quadrado padrão entre todos os da ordem t, depois linhas, colunas
  e letras permutadas —, uniforme entre todos os quadrados até t = 6 (antes,
  só as permutações do cíclico: 432 dos 576 quadrados 4 × 4) e
  aproximadamente uniforme de t = 7 em diante (Jacobson & Matthews, com
  aviso). O BIB procura, antes do não reduzido, famílias de diferenças,
  complementares, uma tabela e busca exaustiva: (6, 3) dá 10 blocos, (9, 3)
  12, (8, 4) 14, (10, 4) 15. O crossover ganha a coluna `residual` (o
  tratamento do período anterior) e a fórmula sugerida o termo residual; nos
  grupos de experimentos com base fatorial entram `(1 | local:A:B)` e as
  demais interações com o local. O composto central aceita porção fatorial
  fracionada (resolução V ou mais) pelos `geradores`, com α = n_F^(1/4). O
  fracionado com níveis nomeados guarda a codificação −1/+1 em
  `extras$codificacao`.

# trama.experiments 0.1.0

* Primeira versão: o nó `experiments/design` declara e sorteia DIC, DBC,
  quadrado latino, fatorial (em DIC ou DBC), fatorial com confundimento em
  blocos, fatorial fracionado 2^(k−p), composto central, parcelas
  subdivididas, faixas, blocos incompletos balanceados, medidas repetidas,
  crossover de Williams e grupos de experimentos; o tipo `experiments/plan`
  vira tabela para os nós de ANOVA da `trama.models`; `experiments/view`
  desenha o mapa, a hierarquia, as combinações e a ordem de execução.
* `experiments/effect` (versão 1): soma um termo à resposta simulada —
  intercepto, fixo por nível ou por contraste (polinomiais, Helmert, controle,
  digitados; magnitude = Σ cᵢτᵢ, convertida por τ = Lᵀ(LLᵀ)⁻¹m), aleatório,
  interação (por célula ou produto de contrastes), quantitativo e
  covariável —, com o valor verdadeiro guardado no plano.
* `experiments/error` (versão 1): fecha a resposta com resíduo normal,
  Poisson, binomial ou gama; na normal, sd por nível, correlação no indivíduo
  (simetria composta, AR(1)), caudas t e assimetria; parcelas perdidas (MCAR);
  completa `plano$analise` com a resposta. `tr_experiments_simulate()` roda a
  cadeia inteira com uma semente. A vista ganhou a aba `componentes`.
* `experiments/contrasts` (versão 2): no desbalanceado, `sq` passa a ser o SQ
  extra do teste de 1 gl (F × `qm_erro`, o do `car::linearHypothesis`) e
  `qm_erro` o QM do resíduo; o SQ de livro vai para `sq_livro`. Com `dentro` e
  células desiguais, os polinômios de cada grupo usam as réplicas da célula.
* `experiments/response_surface` (versão 2): com bloco, o erro puro é o resíduo
  de `y ~ bloco + ponto`, como no `rsm`.
* `experiments/boxcox` (versão 2): λ̂ na borda da grade é dito borda
  (`na_borda`), sem transformação sugerida.
