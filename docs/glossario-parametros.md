# Glossário de parâmetros

Regra: **ids de nós em inglês** (`models/shapiro`, `ml/forest`), **parâmetros e portas em português** (`resposta`, `dados`). O mesmo conceito tem um nome só em todas as coleções — quem aprende um nó reconhece o parâmetro no próximo.

A trava é `tests/testthat/test-glossario.R`: lê o catálogo das 7 coleções e lista `id: param` de cada nome fora do glossário. Renomes antigos migram documentos via `tr_collection(migrations = ...)`.

## Nomes canônicos

| Canônico | O que é | Substitui |
|---|---|---|
| `confianca` | nível de confiança de intervalos, valor livre, default 0.95 | `alfa` (valor = 1 − alfa), `nivel` |
| `significancia` | α de um teste de hipótese (rejeita quando p < α), default 0.05; quando o nó também tem intervalo, o nível dele segue em `confianca` (experiments/power) | — |
| `resposta` | coluna explicada pelo modelo | `alvo` (ml), `grupo` quando é a resposta (multi/discriminant, multi/logistic) |
| `preditores` | colunas explicativas de um modelo | `cols` quando são preditoras (ml, multi/discriminant, multi/logistic) |
| `cols` | colunas quaisquer, sem papel de modelo (data/select, multi/pca) | — |
| `grupo` | coluna de agrupamento/estrato (também o "por grupo" de data/sample) | — (não usar para resposta) |
| `variavel` | a coluna testada ou estimada | `coluna` (models/shapiro, models/one_sample_t, sampling/size_mean) |
| `dados` | porta de entrada da tabela | `data` (coleção data) |
| `metodo` | escolha do método de estimação/cálculo do nó (enum) | — |
| `operacao` | escolha da operação aritmética entre entradas (series/combine) | — |
| `suavidade` | fração da amostra em cada ajuste local (loess, 0–1) | — |
| `previsto` | coluna da previsão (classe na classificação, número na regressão); também o param que nomeia essa coluna no modo tabela | `.pred` (ml) |
| `prob_<nivel>` | coluna da probabilidade de cada classe, nível saneado por `tr_models_clean_name()` | `.prob_<classe>` (ml) |
| `validacao` | como prever o treino: `resubstituição` ou `cruzada` | — |
| `preditor` | a coluna explicativa única de um modelo de uma preditora (models/nls) | — (não usar `preditores` quando só cabe uma) |
| `grau` | grau da curva de um polinômio (models/polinomial): `automático` ou `1`–`5` | — |
| `grau_max` | maior grau testado no desdobramento polinomial (models/polinomial) | `grau` numérico (models/polinomial versão 1) |
| `equacao` | escrever a equação e o R² no gráfico (bool), em models/plot_regression e view/fit_line | — |
| `intervalo` | desenhar a faixa do intervalo de confiança (bool); o nível vai em `confianca` | — |
| `valor` | número(s) digitado(s) de uma referência fixa, vários separados por `;`, vírgula decimal (view/reference) | — |
| `texto` | texto livre escrito no gráfico (view/reference, view/annotate) | `rotulo` quando é texto, e não coluna |
| `rotulo` | sozinho, a coluna de rótulos escritos junto de cada ponto (view/labels, multi/pca); com prefixo, `rotulo_*` é o texto de um eixo (`rotulo_x`, `rotulo_y`) | — (não usar `rotulo` sozinho para texto livre) |
| `nome_*` | texto de legenda de uma linha do gráfico (`nome_serie`, `nome_sobreposta` em series/plot) | — |
| `por_cor` | uma curva por grupo de cor do gráfico de entrada (view/fit_line) | — |
| `sobreposta` | porta de uma segunda série desenhada no mesmo eixo (series/plot) | — |
| `respostas` | várias colunas resposta de um mesmo teste (multi/manova) | — |
| `tratamento`, `bloco` | colunas do fator em teste e do bloco (models/anova_*, multi/manova) | — |
| `separador` | texto literal que separa/junta valores (data/separate, data/unite) | — |
| `fracao` | fração das linhas, 0–1 (data/sample) | — |
| `reposicao` | sortear com reposição (data/sample) | — |
| `positiva` | a classe positiva de ROC/sensibilidade; vazio = o segundo nível | — |

## Homônimos permitidos

Nomes que coincidem com um proibido mas têm outro sentido: `coluna` do quadrado latino (models/anova_dql) e da tabela de contingência (models/chisq, models/fisher_exact); `nivel` como categoria da variável (sampling/proportion).
