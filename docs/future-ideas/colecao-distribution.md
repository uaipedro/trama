# Coleção `distribution` — distribuições e as relações entre elas

> **Estado: ideia parada.** Desenho conversado em 2026-09-09, interrompido de
> propósito antes de fechar. Não é plano de implementação; é o registro do que
> já foi decidido e da pergunta que ficou aberta, pra retomar sem redescobrir.

## A ideia

Uma segunda coleção (`trama.distribution`, id `distribution`) cujo assunto são
distribuições de probabilidade e o corpo de relações entre elas — caso
especial, soma, limite, transformação, mistura. O tipo de coisa que o pôster
de Leemis & McQueston organiza.

O valor é **didático/expositivo**: o fluxo não descreve a relação, ele a
*mostra*. Binomial(1000, 0,003) entra num nó de limite, sai Poisson(3), e um
nó de comparação diz `sup|F−G| = 0,0007`. Arrastar o `n` e ver a aproximação
apertar ao vivo é o melhor demo de recomputação incremental que o pacote pode
ter — melhor que qualquer ETL.

## O reenquadramento que destrava tudo

O erro é achar que a relação é uma **aresta**. No trama ela é um **nó**: uma
relação entre distribuições é literalmente uma função que recebe distribuição
e devolve distribuição.

```
Binomial(n=1000, p=0.003) ─→ [limite n→∞, np fixo] ─→ Poisson(3)
                          └─→ [comparar] ←────────────┘   sup|F−G| = 0,0007
```

## Fora de escopo, explicitamente

- **Reproduzir o pôster.** São ~76 distribuições e ~200 relações num mapa
  cíclico que se *navega* como um todo. O trama é um DAG que se *autora*.
  Tentar isso força a ferramenta a ser um browser de grafo, que ela não é.
- **Prova simbólica.** Demonstrar que Binomial → Poisson é trabalho de CAS,
  não de R.

A unidade de entrega não é o mapa: é **um fluxo pequeno por relação**, e a
coleção envia muitos deles como fluxos de exemplo. O mapa existe como
catálogo consultável, não como canvas.

## Decisões tomadas

**Dois tipos, ligados por adaptador.**

```
distribution/named  ──[adaptador automático]──▶  distribution/numeric
(família + parâmetros)                           (só d/p/q/r, sem nome)
```

Isso faz a **tipagem dizer a verdade sobre a matemática**: relação exata tem
assinatura `named → named` (soma de N Bernoulli devolve Binomial, *com nome*);
relação aproximada ou genérica devolve `numeric`. A distinção teórica vira
assinatura de porta, verificada pelo motor e visível na aresta.

O caminho de volta, `numeric → named`, **não** é adaptador — é um **nó
explícito** ("isso se parece com qual família, e quão bem?"). É a pergunta
interessante; merece ser passo visível com resultado próprio, não conversão
silenciosa.

Rejeitados: um tipo só "família + parâmetros" (quebra em soma de Gamas com
taxas diferentes, mistura, transformação por `g` qualquer — coisas sem forma
fechada e portanto sem nome); e um tipo só opaco (perde a identidade, que é
justamente o que a coleção existe pra mostrar).

**Discreto/contínuo é campo dentro do tipo, não tipo separado.** Metade das
relações que interessam *atravessa* a fronteira (Binomial→Normal,
Poisson→Normal, Gama→Normal). Com tipos distintos, todo teorema limite
precisaria de adaptador — e adaptador é o que o motor insere *em silêncio*. A
coisa mais interessante da coleção viraria a menos visível.

**`store`/`restore` do `distribution/numeric` guarda a receita, não a
closure.** Serializar função com `saveRDS` arrasta ambiente e quebra o hash de
conteúdo — e o pacote já tem cicatriz nesse ponto (percurso de fecho próprio
desde que `codetools` saiu). O artefato é a *construção* (operação + operandos
+ parâmetros) e `restore` reconstrói `d/p/q/r`. Efeito colateral bom: o objeto
carrega a própria linhagem, e o card sabe escrever "soma de 3 Gama(2, 1)"
mesmo sem nome de família.

**Verificação é determinística.** Avalia `d`/`p` numa grade e mede. Sem
semente, sem ruído: o número que sai é *o erro da aproximação*, e nada mais.
Reproduz bit a bit, o hash fica limpo, e a curva "erro × n" sai lisa — dá pra
ver a taxa de convergência em vez de uma nuvem. **Regra: nunca medir uma
relação com Monte Carlo quando dá pra medi-la exatamente.**

Simulação existe, noutro papel: um terceiro tipo `distribution/sample` faz a
ponte com dado real — amostrar, ajustar por máxima verossimilhança, QQ-plot
contra observado, sair pra `trama.data` como tabela. Ali a semente é parâmetro
e está tudo bem, porque o sorteio *é* o assunto.

**Dois nós de comparação, não um** — mesmo verbo em português, perguntas
diferentes:

| Nó | Entrada | Saída | Pergunta |
|---|---|---|---|
| `compare` | distribuição × distribuição | distância, sem p-valor | quão longe estão duas coisas conhecidas? |
| `gof` | amostra × distribuição | teste com p-valor | esse dado plausivelmente veio dali? |

`compare` tem param `medida`: Kolmogorov (`sup|F−G|`), variação total,
Hellinger, KL, qui-quadrado. `gof` oferece KS, qui-quadrado,
Anderson–Darling, Shapiro–Wilk.

**Relação é objeto de primeira classe.** `tr_relation(enunciado, condicao,
referencia)`, carregado pelo nó. A condição é *avaliada* nos parâmetros
correntes e o resultado entra no preview:

```r
condicao(par) -> list(status = "ok" | "borda" | "fora",
                      motivo = "np = 12, acima da faixa usual")
```

Três estados e uma frase legível — faixa de validade desenhada no card é
escopo pra depois. O fluxo passa a mostrar **as duas coisas ao mesmo tempo**:
"a teoria diz que vale sob estas condições" e "aqui o erro medido é 0,0007". O
momento pedagógico bom é o desencontro — arrasta `p` até 0,4, o card fica
vermelho *antes* de você olhar o erro, e o erro confirma. A seta do pôster,
que se aceita por fé, vira asserção com condição verificável e evidência ao
lado.

Serve à tese: o catálogo do trama já é legível por máquina; com isso a coleção
publica também **o corpo de relações como dado** — enunciado, condição, tipos
de entrada e saída. Um modelo consegue perguntar "que relações partem da
Gama?" sem ler código.

## A pergunta que ficou aberta

**Um nó por mecanismo, ou um nó por relação?**

Um corpo de relações sério tem dezenas de asserções. Uma por nó dá paleta de
200 itens, inutilizável — vira o pôster com passos extras.

A proposta em cima da mesa era **um nó por mecanismo**, quatro ou cinco no
total:

| Nó | O que faz | Cobre |
|---|---|---|
| `caso_especial` | fixa parâmetro | Exponencial = Gama(1,λ); Bernoulli = Binomial(1,p); χ² = Gama(k/2, ½) |
| `soma_iid` | soma de N independentes | n Bernoulli → Binomial; n Exponencial → Gama; n Cauchy → Cauchy |
| `limite` | comportamento assintótico | Binomial → Poisson; Poisson → Normal; t → Normal |
| `transformacao` | função de v.a. | Z² → χ²; exp(Normal) → Log-normal; F(X) → Uniforme |
| `mistura` | parâmetro aleatório | Poisson com taxa Gama → Binomial Negativa |

Com o param `relacao` sendo um **enum filtrado pela família que chegou na
entrada** — liga uma Binomial no nó `limite` e o seletor oferece só o que
parte de Binomial, com o enunciado de cada uma ao lado. Widget próprio da
coleção via `registerWidget`, que o desenho já prevê.

O efeito: **o pôster fica navegável sem nunca ser desenhado.** Não se procura
no mapa; chega-se com uma distribuição na mão e pergunta-se "para onde
daqui?". O grafo de relações existe como dado consultável, e o canvas mostra
só o caminho percorrido — que é a diferença entre um mapa e um enredo, e é o
nome do pacote.

Custo: o nó deixa de ter semântica fixa (o que faz depende de um param) e o
`fn` precisa despachar sobre a relação escolhida.

A alternativa não explorada é um nó por relação: paleta grande, semântica fixa
e legível em cada nó, nenhum despacho.

## Escopo, quando retomar

Não decidido, mas o rascunho era:

- **Fatia vertical primeiro** — ~8 famílias e ~10 relações, um exemplar de
  *cada* mecanismo. Um exemplar por mecanismo já prova o contrato inteiro.
- **Depois alargar** para ~16 famílias e ~30 relações, que é a "coleção ampla,
  mas não exaustiva" que o manifesto promete. Se o desenho por mecanismo
  vingar, alargar é preencher catálogo, não escrever nó — e isso torna a
  afirmação central do manifesto ("acrescentar o que falta é barato") um fato
  mensurável nessa coleção.

Famílias candidatas — discretas: Bernoulli, Binomial, Poisson, Geométrica,
Binomial Negativa, Hipergeométrica, Uniforme discreta. Contínuas: Uniforme,
Normal, Exponencial, Gama, Beta, χ², t, F, Log-normal, Weibull, Cauchy.

Duas relações que valem prioridade por serem bonitas de ver: **soma de n
Cauchy → Cauchy** (a média não converge, e o card mostra isso) e a
**transformada integral de probabilidade, F(X) → Uniforme** (fundamental, e
funciona pra *qualquer* família da coleção — bom teste do contrato genérico).
