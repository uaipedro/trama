---
title: Diferença detectável
description: Com os grupos do tamanho previsto, qual a menor diferença entre eles que a pesquisa detecta?
section: colecoes
collection: amostragem
node: sampling/detectable_difference
related: [sampling/size_domains, sampling/margin_levels]
---

## O que o bloco faz

Responde se dá para COMPARAR grupos: para cada par de categorias de uma mesma
variável (mulheres × homens, pretos × brancos), a menor diferença entre as duas
proporções que a pesquisa detectaria com o nível de confiança e o poder
escolhidos:

    δ = q_{1−α/2} · √(deff · p̄q̄ · (c_a/n_a + c_b/n_b))
        + q_{poder} · √(deff · (p_a q_a c_a/n_a + p_b q_b c_b/n_b))

com p_b = p_a + δ, p̄ a média das duas ponderada pelos n e c = 1 − n/N a
correção finita (1 sem a população do grupo) — a fórmula de duas proporções de
Fleiss, Levin & Paik (2003), **sem correção de continuidade** (a DMD sai um
pouco menor que com a correção de Yates/Fleiss). A tabela traz o pior caso
entre partir de p_a ou de p_b, para cima ou para baixo. Com **Distribuição**
`t`, os quantis são t com n_a + n_b − 2 gl.

A coluna `sustentavel` diz se a **diferença que importa** (10 pontos, por
padrão) é detectável. Quando não é, um resultado "sem diferença" entre os dois
grupos não quer dizer nada: é falta de gente no menor deles.

Sem cotas, o tamanho de cada grupo é a participação dele vezes o n total. Por
isso a tabela aceita a participação (com **n total**) ou a contagem direta. A
comparação é governada pelo MENOR grupo do par: dobrar o grupo grande quase não
muda a DMD.

Duas margens de erro que não se sobrepõem não são o teste da diferença, e duas
que se sobrepõem ainda podem esconder uma diferença real. É esta a conta certa
para a pergunta "dá para comparar?".

## Quando usar

Use antes da coleta para verificar se os grupos previstos permitem detectar a diferença que importa.

## Configuração

- **Variável**, **Grupo** — colunas da tabela longa (uma linha por categoria).
- **Tamanho do grupo** — coluna com a contagem, ou a participação (0 a 1) se o
  **n total** for maior que 0.
- **Proporção esperada** — p; 0,5 é o pior caso.
- **Confiança**, **Poder** — o nível do teste e a chance de detectar a diferença
  quando ela existe.
- **Efeito do desenho** — deff da pesquisa (ponderação e agrupamento).
- **Diferença que importa** — em proporção (0,10 = 10 pontos).

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("perfil", "sampling/example", dataset = "perfil_escolas") |>
  tr_add("diferencas", "sampling/detectable_difference", variavel = "variavel",
         grupo = "categoria", tamanho = "participacao", n_total = 675, deff = 1.5,
         from = "perfil")
```

## Como interpretar

Cada linha compara um par de grupos. `diferenca_detectavel_pp` é a menor diferença em pontos percentuais detectável; `sustentavel` indica se a diferença configurada como relevante supera esse limiar. A saída é Tabela `data/table` com pares de grupos, diferença detectável e sustentabilidade.

## Veja também

`sampling/size_domains`, `sampling/margin_levels`.
