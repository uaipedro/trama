# Glossário de parâmetros

Regra: **ids de nós em inglês** (`models/shapiro`, `ml/forest`), **parâmetros e portas em português** (`resposta`, `dados`). O mesmo conceito tem um nome só em todas as coleções — quem aprende um nó reconhece o parâmetro no próximo.

A trava é `tests/testthat/test-glossario.R`: lê o catálogo de todas as coleções e lista `id: param` de cada nome fora do glossário. Renomes antigos migram documentos via `tr_collection(migrations = ...)`.

## Nomes canônicos

| Canônico | O que é | Substitui |
|---|---|---|
| `confianca` | nível de confiança de intervalos, valor livre, default 0.95 | `alfa` (valor = 1 − alfa), `nivel` |
| `quantidade` | estimando ou conjunto de medidas calculadas | — |
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
| `tamanho` | número de observações em cada bloco consecutivo (series/range_mean); 0 usa a frequência da série | — |
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
| `tempo` | coluna com o tempo/a data de cada linha (series/from_table, series/intervencao) | — |
| `datas` | porta de entrada de uma tabela de datas, uma por linha (series/intervencao) | — |
| `fracao` | fração das linhas, 0–1 (data/sample) | — |
| `reposicao` | sortear com reposição (data/sample) | — |
| `reamostras` | quantidade de reamostragens ou permutações de Monte Carlo (models/bootstrap, models/permutation) | `replicas` (experiments) |
| `positiva` | a classe positiva de ROC/sensibilidade; vazio = o segundo nível | — |
| `termo` | linha do quadro da ANOVA que será testada (models/permutation) | — |
| `caminho` | pasta, arquivo ou banco local que fornece tabelas para consulta SQL (sql/source) | — |
| `consulta` | instrução SQL de leitura (sql/query) | — |
| `fonte` | porta de entrada da fonte consultável por SQL (sql/query) | — |
| `dinamica` | forma no tempo do efeito de uma intervenção: `imediata` ou `gradual` (ω/(1 − δB)) (series/intervencao) | `resposta` (series/intervencao versão 1) |
| `valor_critico` | limiar de \|t\| acima do qual um candidato é marcado numa busca múltipla; 0 = regra automática (series/detect_interventions) | — |
| `causa` | série (ou séries, separadas por vírgula) cuja precedência preditiva se testa (series/granger) | — |
| `posto` | número de relações de cointegração do VECM (series/vecm) | — |
| `impulso` | série que recebe o choque numa função de impulso-resposta (series/irf); `respostas` são as que o sentem | — |
| `nomes` | nomes das séries ligadas a uma porta variádica, na ordem dos fios (series/join) | — |
| `valores` | colunas numéricas, uma por série, de uma série múltipla (series/from_table_mts); o plural de `valor` | — |
| `x`, `y` | colunas numéricas postas uma contra a outra: os eixos de um gráfico (view/*), o par correlacionado (models/cor_test) e, em spatial/coordinates, as coordenadas projetadas de cada ponto | — |
| `covariaveis` | covariáveis opcionais, vários nomes: as observadas do delineamento (experiments/design) ou as colunas da tabela que `tendencia = covariavel` usa como tendência externa (spatial/coordinates) | — |
| `crs` | código EPSG do sistema de coordenadas projetado, como texto (`31983`); vazio trata como plano arbitrário; graus (CRS geográfico) são recusados (spatial/coordinates) | — |
| `unidade` | texto da unidade da distância, só para rotular eixo e alcance (`m`), em spatial/coordinates. Não é a coluna que identifica a unidade observada (view/paired, sampling/referral) | — |
| `nome` | texto livre que dá nome ao que o bloco cria ou declara: coluna nova (data/unite), termo (experiments/effect) e o título do objeto nos cartões e gráficos (spatial/coordinates) | — (ml/example usa `nome` para escolher o conjunto: esse papel é de `dataset`) |
| `dataset` | qual conjunto carregar: um dos exemplos do próprio bloco, enum (`*/example`, incluindo spatial/example), ou o nome do conjunto de um pacote (data/public) | `nome` (ml/example) |
| `estimador` | qual fórmula estima o que o bloco mede, quando há mais de uma para o mesmo estimando: média ou total (sampling/simulate), clássico ou robusto (spatial/variogram). Escolher entre algoritmos de ajuste é `metodo` | — |
| `n_classes` | em quantas faixas de distância os pares são agrupados, inteiro (spatial/variogram) | — |
| `dist_max` | distância máxima considerada, na unidade das coordenadas, com vazio = automático: a distância máxima entre pares do variograma, em que vazio usa a diagonal da caixa envolvente dos pontos dividida por três (spatial/variogram), ou o raio de busca da krigagem, em que vazio não limita (spatial/kriging) | — |
| `tendencia` | forma da tendência de larga escala que o bloco trata: a removida antes de medir a dependência espacial, enum `constante`, `1a ordem`, `2a ordem`, `covariavel` (spatial/variogram); a linha desenhada em view/points e o componente de tendência em series/holt_winters seguem o mesmo conceito | — |
| `direcao` | direção em graus, sentido horário a partir do **Norte** (0 = Norte, 90 = Leste, a bússola); vazio = todas as direções (spatial/variogram) | — |
| `tolerancia` | meia-abertura angular, em graus, em torno de `direcao` (spatial/variogram). Não é tolerância numérica | — |
| `pares_min` | menor número de pares que uma classe do variograma precisa ter para ficar; as demais saem e a nota diz quantas (spatial/variogram) | — |
| `familia` | família paramétrica do modelo ajustado: a distribuição da resposta (models/glm, models/glmer) ou o modelo teórico do variograma (spatial/variogram_fit) | — |
| `pepita_fixa` | manter a pepita no valor inicial em vez de estimá-la (bool, spatial/variogram_fit) | — |
| `pepita_inicial`, `contribuicao_inicial`, `alcance_inicial` | valores de partida do ajuste do variograma, na escala dos dados; vazio = o bloco tenta uma grade fixa de partidas (spatial/variogram_fit) | — |
| `kappa` | suavidade do modelo de Matérn, 0,1–10; só vale com `familia = matern` (spatial/variogram_fit) | — |
| `tipo` | variante de um procedimento do mesmo bloco, enum: diferença simples ou sazonal (series/diff), reta horizontal, vertical ou diagonal (view/reference), krigagem ordinária ou simples (spatial/kriging). Escolher entre algoritmos de cálculo é `metodo` | — |
| `media` | valor de uma média informado de fora dos dados, número: a média esperada (sampling/size_mean), a da covariável (experiments/effect) e a média conhecida da krigagem simples (spatial/kriging) | — (em view/paired é um bool: desenhar a linha da média) |
| `resolucao` | quantos pontos tem o lado maior da grade de predição, inteiro (spatial/kriging) | — |
| `vizinhos_max` | quantos pontos mais próximos entram na predição de cada célula; vazio = todos (spatial/kriging) | — |
| `vista` | qual das vistas de um painel exploratório desenhar, enum (spatial/explore: `completo`, `mapa`, `x`, `y`, `histograma`). Não é `painel` de view/*, que é a coluna que divide o gráfico em painéis | — |
| `mostrar` | qual grandeza o gráfico desenha, enum (spatial/map: `predito` ou `erro-padrao`) | — |
| `isolinhas` | desenhar curvas de nível sobre a superfície (bool, spatial/map) | — |
| `pontos` | como param, desenhar as observações sobre o gráfico (bool: view/boxplot, view/violin, series/plot, spatial/map). Como porta de entrada, a das localizações medidas, tipo `spatial/points` (spatial/explore, spatial/variogram, spatial/kriging) | — |

| `caminho` | arquivo ou pasta de entrada (sql/source, spatial/read_points, spatial/boundary) | — |
| `camada` | camada dentro de um arquivo com mais de uma (spatial/read_points, spatial/boundary) | — |
| `crs_saida` | EPSG de destino da reprojeção na leitura | — |
| `nomes_coords` | nomes das duas colunas de coordenada criadas na leitura de vetor | — |
| `borda` | porta e param da borda da área de estudo (spatial/coordinates); o param escolhe `nenhuma` ou `casco convexo dos pontos` | — |
| `direcoes` | direções do variograma direcional, em graus, separadas por vírgula (spatial/anisotropy) | — |
| `envelope` | desenhar a faixa de referência obtida por simulação (bool) | — |
| `n_sim` | número de simulações de Monte Carlo de uma faixa ou envelope | — |
| `semente` | semente do sorteio, para o resultado não mudar a cada execução | — |
| `razao` | razão de anisotropia geométrica, maior eixo sobre menor (≥ 1) | — |
| `angulo` | ângulo, em graus, horário a partir do Norte; no variogram_fit aponta o eixo maior | — |
| `corte` | valor que separa duas classes numa transformação (spatial/indicator) | — |
| `sentido` | lado do corte: `<=` ou `>` (spatial/indicator) | — |
| `dobras` | número de grupos da validação cruzada em k dobras | — |
| `grade` | porta de uma tabela que passa a ser a grade de predição (spatial/kriging) | — |

## Homônimos permitidos

Nomes que coincidem com um proibido mas têm outro sentido: `coluna` do quadrado latino (models/anova_dql) e da tabela de contingência (models/chisq, models/fisher_exact); `nivel` como categoria da variável (sampling/proportion).
