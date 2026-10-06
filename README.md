<svg xmlns="http://www.w3.org/2000/svg" width="320" height="80" viewBox="0 0 320 80">
  <text x="0" y="60" font-family="Helvetica, Arial, sans-serif"
        font-size="64" font-weight="700" fill="#8b949e">
    t<tspan fill="#9b51e0">r</tspan>ama
  </text>
</svg>

![Identidade visual do trama: R em blocos, diagramas que rodam](tools/video/out/ab_full.png)

O `trama` é um pacote R para construir e executar fluxos de análise em um editor visual interativo. Cada bloco exibe o próprio resultado, e só os blocos afetados por uma alteração são recalculados.

Os fluxos são documentos JSON que também podem ser escritos em R. As operações vêm de coleções instaláveis: manipulação de dados, gráficos, séries temporais, modelos estatísticos, análise multivariada, amostragem, experimentos e aprendizado de máquina.

> **In English.** `trama` is an R package for building and running analysis flows in an interactive node editor (Shiny). Flows are stored as JSON and can also be written in R. Documentation is in Portuguese.

## Instalação

Requer R 4.1 ou posterior. Instalação pelo GitHub com o [`pak`](https://pak.r-lib.org):

```r
install.packages("pak")

pak::pak(c(
  "uaipedro/trama",
  "uaipedro/trama/collections/trama.data",
  "uaipedro/trama/collections/trama.view"
))
```

As demais coleções são opcionais (`trama.series`, `trama.models`, `trama.multi`, `trama.sampling`, `trama.experiments`, `trama.ml`, `trama.sql`):

```r
pak::pak("uaipedro/trama/collections/trama.models")
```

## Primeiro fluxo

```r
library(trama)

tr_app(tr_project(
  "meu-projeto",
  collections = c("trama.data", "trama.view")
))
```

O editor abre no navegador. Arraste blocos da paleta, conecte as portas e ajuste os parâmetros. O fluxo é salvo em `meu-projeto/flows/main.json`.

## Saiba mais

- [Documentação e site](https://uaipedro.github.io/trama/)
- [Manifesto](docs/manifesto.md): objetivo e não objetivos do projeto
- [Desenvolvimento e referência](docs/desenvolvimento.md): DSL em R, criação de coleções, arquitetura, testes
- [Mapa do repositório](docs/mapa.md)

## Licença

MIT. Detalhes sobre dependências no [manifesto](docs/manifesto.md#licença-e-abertura).
