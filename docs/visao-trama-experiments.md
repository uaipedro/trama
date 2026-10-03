# Visão de design — `trama.experiments`

## Para que serve este documento

Registrar a direção conceitual de uma coleção para planejar experimentos e simular dados no Trama. A ideia é declarar um delineamento, sortear a alocação de forma reproduzível, **compor** a resposta termo a termo e mandar o resultado para análise em `trama.models`.

É uma visão, não uma especificação. Nomes de nós e estruturas são exemplos para discussão.

## Ideia central

Montar um experimento tem duas naturezas diferentes, e cada uma pede uma forma de interface:

1. **O delineamento é declarativo.** Fatores, níveis, blocos e a hierarquia de unidades formam uma descrição só, preenchida num nó. Não há o que aprender encadeando `fator → fator → bloco`. O aprendizado está em *ver* o resultado: quem é unidade de quê e onde cada fator é sorteado.
2. **A resposta é composicional.** Um dado experimental simulado é a soma de termos:

   ```
   y = μ + bloco + irrigação + variedade + irrigação×variedade + erro_parcela + erro
   ```

   Cada termo é um nó. Ligar, desligar ou mudar a intensidade de um termo, e ver o que acontece na análise, é o que ensina como um experimento funciona.

Essa é a mesma dupla que `trama.series` oferece com decomposição e componentes, só que no sentido contrário: lá se decompõe o que foi observado; aqui se compõe o dado e depois se verifica se `trama.models` recupera o que foi posto.

## Separação de responsabilidades

- **`trama.experiments`** planeja, sorteia e simula: delineamento, alocação reproduzível, composição da resposta, vistas do plano e poder por simulação.
- **`trama.models`** analisa, sem mudanças: `anova_dic`, `anova_dbc`, `anova_dql`, `anova_factorial`, `anova_split_plot`, `lmer`, `glm`, médias, contrastes e pressupostos.

A dependência é unidirecional: experiments pode sugerir ou configurar nós de models; models não conhece experiments. O que passa de uma coleção para a outra é uma **tabela comum** com as colunas de fatores, blocos, unidades e resposta, mais um metadado do plano. Qualquer nó de models já consome essa tabela sem contrato novo. O metadado serve só para experiments sugerir o nó de análise correto e para as vistas.

Antes de fixar o formato do metadado, comparar com o desenho de `trama.sampling` (`desenho.R`, `nos_planejar.R`, `nos_selecionar.R`), que já resolve "plano declarado → sorteio com semente → estimação". Reaproveitar a forma se servir; se não servir, registrar por quê.

## Os nós

A coleção fica pequena de propósito. Um verbo por bloco; o mesmo bloco pode aparecer várias vezes no fluxo.

### `experiments/design` — declarar e sortear

Um nó declarativo com:

- **estrutura:** predefinição (DIC, DBC, quadrado latino, fatorial, fatorial com confundimento, fatorial fracionado 2^(k−p), composto central, parcelas subdivididas, faixas, blocos incompletos balanceados, medidas repetidas, crossover, grupos de experimentos) que preenche os campos abaixo e continua editável;
- **fatores:** nome, níveis, papel (tratamento, bloco, covariável observada) e **unidade de atribuição** (parcela, subparcela, animal, período…);
- **repetições / blocos;**
- **geometria:** grade 2D (linhas × colunas) e, para medidas repetidas e crossover, o eixo de tempo/período;
- **semente:** o sorteio é parte do nó, com cada fator randomizado no escopo que sua unidade determina.

Saída: a tabela de unidades com a alocação sorteada, mais o metadado do plano.

Para cada fator, o plano precisa deixar explícito:

1. **Papel:** tratamento designado, bloco, covariável observada ou agrupamento.
2. **Unidade de atribuição:** o nível da hierarquia em que o fator é aplicado.
3. **Escopo do sorteio:** entre quais unidades ele é sorteado (todas, dentro do bloco, dentro da parcela).
4. **Mecanismo:** livre, restrito, em estágios, ou sem sorteio (fator observado, com justificativa).
5. **Reprodutibilidade:** semente, método e versão.

Fator observado não recebe linguagem de tratamento, e as vistas não sugerem interpretação causal para ele.

### `experiments/effect` — adicionar um termo

Adiciona **uma** contribuição à resposta e guarda essa contribuição numa coluna própria (`.ef_<nome>`). O tipo é um parâmetro, não um nó diferente:

| Tipo | Parâmetros | Exemplo |
|---|---|---|
| intercepto | valor | μ = 50 |
| fixo | fator, efeito por nível | irrigação: baixa = 0, alta = +4 |
| aleatório | fator ou combinação, desvio-padrão | bloco, sd = 3; `bloco:parcela`, sd = 2 |
| interação | fatores, tabela de efeitos | irrigação × variedade |
| quantitativo | fator numérico, forma (linear, quadrática) e coeficientes | dose de N com resposta quadrática |
| covariável | coluna, inclinação | peso inicial, β = 0,5 |

Um efeito fixo também pode ser declarado **por contrastes** em vez de por nível (ver [Tudo é contraste](#tudo-é-contraste)).

O nó valida o termo contra o plano: o fator existe, a combinação `bloco:parcela` só existe onde há parcela, a tabela de interação cobre todas as células.

### `experiments/error` — fechar a resposta

Soma os termos, gera o resíduo e produz a coluna de resposta. Antes dele, o que existe são só componentes. Parâmetros:

- **distribuição:** normal, Poisson, binomial e gama (a ponte para `models/glm` na GES123);
- **correlação entre medidas repetidas:** simetria composta ou AR(1) dentro do indivíduo;
- **desvio-padrão:** único ou por nível de um fator (heterocedasticidade proposital, para ver Bartlett e Levene reagirem);
- **perturbações opcionais:** caudas pesadas ou assimetria (para ver Shapiro e o gráfico de resíduos reagirem), e parcelas perdidas (desbalanceamento, os "problemas pós-planejamento" da GES123).

### `experiments/power` — poder por simulação

Recebe a cadeia `design → effect… → error` e um nó de análise, repete a cadeia N vezes com sementes derivadas e conta as rejeições. Sem fórmulas fechadas por delineamento: funciona para qualquer estrutura que a cadeia consiga gerar, inclusive modelos mistos e GLM. Também dimensiona: varia o número de repetições e desenha a curva de poder.

### `experiments/randomization_test` — teste de aleatorização

Re-sorteia a alocação com o próprio `design`, N vezes, mantendo as respostas observadas, e compara a estatística observada (F ou um contraste) com a distribuição sob H0. Mostra que a justificativa do teste vem do sorteio que foi de fato feito, e não da normalidade.

### Vistas

Uma vista com abas sobre o mesmo plano (papel `leitura`):

- **mapa:** a grade com blocos, parcelas e tratamentos;
- **hierarquia:** a árvore bloco → parcela → subparcela, marcando em que nível cada fator foi sorteado;
- **combinações:** grade de fatores × níveis com réplicas e células vazias;
- **ordem de execução**, quando relevante;
- **componentes:** a resposta empilhada por termo, espelho do `series/component`.

Selecionar uma unidade numa aba destaca a mesma unidade nas outras.

## Tudo é contraste

Princípio central da coleção, e não um tópico entre outros: toda pergunta que um experimento responde é um contraste entre médias. A ANOVA com k tratamentos é a soma de k − 1 contrastes ortogonais; o efeito principal, a interação, a tendência linear de doses e o "controle contra o resto" são todos contrastes. Comparações múltiplas são o caso em que não se planejou nenhum.

Por isso o contraste tem que aparecer **nas duas pontas** do fluxo:

**Na simulação (`experiments/effect`).** Além de "efeito por nível", o efeito fixo aceita um conjunto de contrastes com a magnitude de cada um:

```
fator: dose (0, 50, 100, 150)
linear:     -3 -1  1  3   → magnitude 4
quadrático:  1 -1 -1  1   → magnitude 1
cúbico:     -1  3 -3  1   → magnitude 0
```

O nó converte isso nos efeitos por nível e mostra a conversão. A pergunta do experimento passa a ser declarada antes do dado existir, e a análise pode ser conferida contra ela: o contraste cúbico, posto em zero, deve dar não significativo em ~95% das simulações. Interação declarada como produto de contrastes (linear da dose × irrigação) mostra que a interação também é contraste.

**Na análise.** Hoje o `models/linear_hypothesis` testa contrastes digitados à mão com **um F conjunto**. Para o ensino da GES136 faltam:

1. **Conjuntos prontos:** polinomiais ortogonais (inclusive com níveis desigualmente espaçados), Helmert, controle contra os demais, e fatoriais (efeitos principais e interação 2^k como contrastes).
2. **Desdobramento da soma de quadrados:** uma linha por contraste com SQ, F e p, e a conferência de que os SQ de um conjunto ortogonal somam o SQ de tratamento. Esse quadro é a ANOVA "aberta" que o professor desenha no quadro.
3. **Verificação de ortogonalidade** visível: a matriz Σ cᵢcⱼ/r entre os contrastes, apontando os pares não ortogonais e explicando por que os SQ deixam de somar.
4. **Contrastes na interação:** desdobrar a interação de um fatorial em contrastes de um fator dentro dos níveis do outro.
5. **Regressão como contraste:** fator quantitativo analisado pelos polinomiais ortogonais e pela regressão (`poly()`), mostrando que dão o mesmo resultado.

Isso vira o nó `experiments/contrasts`, que lê um `models/fit`; `models/linear_hypothesis` continua como está. É pré-requisito do protótipo de experiments: sem ela o ciclo "declarei o contraste → recuperei o contraste" não fecha.

## O caso que valida a ideia: parcela subdividida com e sem erro de parcela

Quatro blocos, irrigação (2 níveis) na parcela, variedade (3 níveis) na subparcela.

```r
plano <- design(estrutura = "split_plot", parcela = "irrigacao", subparcela = "variedade",
                blocos = 4, semente = 42)

com_erro <- plano |>
  effect("intercepto", 50) |>
  effect("aleatorio", "bloco", sd = 3) |>
  effect("fixo", "irrigacao", c(baixa = 0, alta = 2)) |>
  effect("fixo", "variedade", c(A = 0, B = 1, C = 3)) |>
  effect("aleatorio", "bloco:parcela", sd = 4) |>
  error(sd = 1)
```

No canvas, duas cópias do fluxo lado a lado, uma sem o `effect` de `bloco:parcela`, ambas analisadas por `models/anova_factorial` (a análise ingênua) e `models/anova_split_plot`:

- **sem erro de parcela:** as duas análises concordam;
- **com erro de parcela:** a análise ingênua testa a irrigação contra o erro errado e rejeita demais; a de parcela subdividida acerta. O `experiments/power` torna isso um número: taxa de erro tipo I da análise ingênua bem acima de 5%.

Esse é o critério de aceite do protótipo: a pessoa entende o que é unidade de erro sem ler um parágrafo sobre isso.

Outros pares didáticos que a mesma mecânica oferece: com/sem bloco (por que blocar), com/sem interação (quando o efeito principal engana), variância homogênea/heterogênea (o que os pressupostos detectam) e normal/Poisson (quando ANOVA ou GLM).

## Cobertura das ementas

Referência: [ementas UFLA 2026/01](../ementas-estatistica-ufla-2026-01.md) e [complementares USP/ESALQ](referencias-complementares-usp-esalq.md). "Models" indica que a análise já existe e experiments só precisa gerar o dado.

### GES136 — Introdução aos Planos Experimentais

| Tópico | Cobertura |
|---|---|
| Princípios (repetição, casualização, controle local) | `design` + pares com/sem bloco; **aleatorização visível** no mapa |
| DIC, DBC, quadrado latino | predefinições de `design`; análise em models (`anova_dic/dbc/dql`) |
| ANOVA e interpretação | models; experiments dá o "gabarito" (efeitos conhecidos) |
| Contrastes ortogonais e polinomiais | **Eixo central** — ver [Tudo é contraste](#tudo-é-contraste): `effect` declara por contraste; models precisa de conjuntos prontos e do desdobramento do SQ |
| Comparações múltiplas | models (Tukey, Duncan, Dunnett…); `power` mede erro tipo I por comparação vs. por experimento |
| Análise de resíduos | models; `error` com perturbações gera violações sob controle |

### GES123 — Planejamento e Análise de Experimentos

| Tópico | Cobertura |
|---|---|
| Transformação de dados | `error` não normal / heterocedástico cria o problema. **Lacuna:** Box-Cox em models |
| Fatoriais | `design` fatorial + `effect` interação; models `anova_factorial` |
| Parcelas subdivididas | caso de validação acima |
| Introdução a modelos mistos | `effect` aleatório ↔ `models/lmer`; recuperar o sd simulado ensina BLUP e componentes de variância (liga à GES122) |
| Problemas pós-planejamento | `error` com parcelas perdidas; desbalanceamento visível em "combinações" |
| GLM em experimentação | `error` com Poisson/binomial ↔ `models/glm` |

### GES122 — Modelos Lineares II

Experiments não ensina álgebra, mas produz dados com posto incompleto conhecido (células vazias num fatorial) e efeitos fixos/aleatórios com valor verdadeiro — útil para ver identificabilidade, BLUE e BLUP recuperando o que foi posto.

### GES126 — Estatística Computacional e GES124 — Inferência II

`power` é um método de simulação de Monte Carlo concreto; a curva de poder é a "função poder" da GES124 construída empiricamente.

### Complementares USP/ESALQ

| Tópico | Fonte | Cobertura |
|---|---|---|
| Testes de aleatorização | MAE0316 | **Candidato natural:** re-sortear pelo próprio `design` sob H0 e comparar a estatística. Nó `experiments/randomization_test` |
| Grupos de experimentos (vários locais/anos) | LCE5703 | `design` com fator local + `effect` aleatório de local e de local × tratamento; análise conjunta em `lmer` |
| Blocos incompletos, confundimento | Cochran & Cox; Bailey | predefinição BIB e fatorial com confundimento em `design` |
| Fatoriais fracionados, superfície de resposta, DCC | ZEA1009; Box, Hunter & Hunter; Myers & Montgomery | `design` gera a matriz (2^k-p, composto central); `effect` quantitativo com termos quadráticos gera a superfície. análise em `experiments/response_surface` |
| Medidas repetidas e crossover | Milliken & Johnson | `design` com período; `effect` aleatório de indivíduo; carryover como `effect` fixo defasado; correlação no `error` |
| Efeitos fixos vs. aleatórios | MAE0316 | par com/sem `effect` aleatório |

### Resumo

Tudo o que faltava para fechar as ementas mora em `trama.experiments`, sem mexer em `trama.models`: `experiments/contrasts` (conjuntos prontos, desdobramento do SQ, ortogonalidade), `experiments/boxcox` e `experiments/response_surface`. Os três leem ou produzem `models/fit`, então não há tipo de modelo novo.

## Decisões

**Tomadas nesta versão:**

1. Delineamento declarativo em um nó; resposta composta por `effect`/`error`.
2. A primeira versão gera alocação **e** resposta; sem resposta não se fecha o ciclo com models.
3. A primeira versão cobre todas as estruturas listadas em `design`, as quatro distribuições do `error`, correlação em medidas repetidas e o teste de aleatorização. Não há v2 planejada.
6. Contraste é o eixo: declarado no `effect`, recuperado em models.
4. Poder só por simulação.
5. models não muda; a integração é a tabela comum.

**Em aberto:**

1. Forma do metadado do plano (depois de comparar com `trama.sampling`).
2. O sorteio dentro do `design` ou num `randomize` separado? Dentro é mais simples; separado permite mostrar "plano pretendido × alocação sorteada" como dois nós.
3. Quais validações bloqueiam (fator inexistente) e quais só avisam (desbalanceamento)?
4. Como `effect` interação recebe a tabela de efeitos sem virar um formulário enorme em fatoriais maiores?
5. Quais abas das vistas são editáveis (arrastar tratamento no mapa = restrição) e quais só leem.

## Ordem de construção

Tudo acima é a primeira versão; a ordem só define o que se valida primeiro.

1. `experiments/contrasts`, `experiments/boxcox` e `experiments/response_surface` (leem ou produzem `models/fit`).
2. `design` (DBC e parcela subdividida) + mapa e hierarquia.
3. `effect` (inclusive por contraste) + `error` normal + aba de componentes.
4. O caso de validação ponta a ponta, as duas cópias lado a lado.
5. `power` e `randomization_test` sobre o mesmo caso.
6. As demais estruturas (DQL, fatorial, confundimento, fracionado, composto central, faixas, BIB, medidas repetidas, crossover, grupos de experimentos) e as distribuições não normais e correlacionadas do `error`.
7. Fixar o metadado e a API pública.

## Referências curriculares

Ementas e bibliografia em [propostas de blocos](propostas-blocos-estatistica-trama.md), [referências UFLA](referencias-estatistica-trama.md) e [referências complementares USP/ESALQ](referencias-complementares-usp-esalq.md). Obras centrais para esta coleção: Montgomery (2017), Banzatto & Kronka (2006), Hinkelmann & Kempthorne (2008), Bailey (2008), Mead (1994), Milliken & Johnson (1997) e Box, Hunter & Hunter (2005).
