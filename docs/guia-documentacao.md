# Guia de redação da documentação do trama

Vale para todo roxygen do pacote (`#'` em `R/*.R`) e para o que sai dele em
`man/*.Rd`. Serve como guia permanente e como prompt de revisão.

O leitor é uma pessoa que programa em R, instalou o pacote e quer usar ou
estender. Ela não conhece o interior do trama, mas conhece R: não se explica o
que é uma lista nomeada, o que é `NULL`, nem o que é uma função.

A documentação registra **o que a função recebe, o que devolve e sob que
condições falha**. O porquê das decisões de projeto já está escrito nos
parágrafos que existem — eles não são o alvo desta revisão.

---

## Prompt de revisão

Você vai escrever a documentação roxygen de um arquivo de `R/` do pacote trama.
O código, a assinatura das funções, o comportamento e os comentários internos
estão aprovados e **não podem ser alterados**. A tarefa é exclusivamente de
documentação: acrescentar `@param`, `@return` e, quando o arquivo for da API
central, `@examples`.

Siga as diretrizes abaixo. Em caso de dúvida entre descrever o que a função faz
e descrever o que ela devolve, descreva o que devolve. Em caso de dúvida sobre
o comportamento, **leia o código e os testes**; se ainda restar dúvida, **pare e
pergunte** em vez de escrever algo plausível.

---

## A. O que não se toca

1. **Assinatura, corpo e comentários internos** (`#`, não `#'`) ficam como
   estão. Se documentar exigir mudar o código, pare e relate.

2. **O título e os parágrafos de justificativa que já existem** no roxygen ficam
   como estão. Eles registram por que a função é assim, que é conhecimento caro
   e não redundante. Acrescente as tags abaixo deles; não reescreva, não resuma,
   não "melhore".

3. **`@noRd` permanece `@noRd`.** Função interna não vira pública nesta revisão.

4. **O `NAMESPACE` é mantido à mão.** Rodar `roxygen2::roxygenise()` é parte do
   trabalho, mas se o NAMESPACE mudar, pare e relate: significa que uma tag
   `@export` foi acrescentada ou perdida por engano.

---

## B. `@param`

5. Um `@param` para **cada** argumento da assinatura, na ordem em que aparecem,
   incluindo `...`. Argumento sem `@param` é WARNING no `R CMD check`.

6. O `@param` diz **o que o argumento é e o que ele aceita**, não o que a função
   faz com ele. Comece por um sintagma nominal, sem artigo inicial e sem repetir
   o nome do argumento.

   - Ruim: `@param store O store. Usado para gravar o artefato.`
   - Bom: `@param store Store de destino, como devolvido por [tr_store()].`

7. Quando o argumento tem **valor padrão com consequência**, diga a
   consequência, não o valor (o valor já aparece no `usage`).

   - Ruim: `@param create Se TRUE, cria. O padrão é TRUE.`
   - Bom: `@param create Cria as pastas do projeto quando elas não existem.
     `FALSE` só lê, e é o que se usa para inspecionar um projeto sem tocá-lo.`

8. Quando o argumento aceita **formas diferentes**, enumere as formas e o que
   cada uma significa. É o caso mais comum de dúvida do usuário.

   - `@param collections Nomes de PACOTE das coleções a registrar. Vazio usa o
     que estiver em `trama.json`.`

9. Quando o argumento tem **restrição verificada** (o código aborta se ela cair),
   a restrição entra no `@param`, com o mesmo vocabulário da mensagem de erro.

   - `@param padrao Nome do tema padrão (obrigatório): uma string não vazia que
     esteja em `temas`.`

10. `@param ...` diz **para onde os argumentos vão**, nominalmente.

    - `@param ... Parâmetros do nó, por nome. Colidir com um argumento desta
      função (`flow`, `id`, `type`, `from`) é recusado com erro; use
      [tr_set()].`

11. Argumento que é **objeto do próprio pacote** referencia a função que o
    produz, com link: `como devolvido por [tr_registry()]`, `um documento de
    [tr_doc()]`.

## C. `@return`

12. Toda função exportada tem `@return`. Sem exceção — é o pedido de revisão
    mais previsível do CRAN.

13. O `@return` diz **a classe ou estrutura** e **o significado**, nessa ordem.
    Estrutura sem significado não serve; significado sem estrutura também não.

    - Ruim: `@return O plano.`
    - Ruim: `@return Uma lista.`
    - Bom: `@return Objeto `tr_plan`: a lista de nós a executar em ordem
      topológica, com a chave de conteúdo de cada um. Nó cujo resultado já está
      no store não entra.`

14. Função de **efeito colateral** declara isso e diz para que serve a chamada.
    Nunca omita a tag.

    - `@return Nada de útil, invisível. Chamada pelo efeito: grava
      `flows/<name>.json` na pasta do projeto.`

15. Quando o retorno é **invisível**, diga. Quando é **o próprio argumento
    modificado** (padrão de pipe), diga, porque é o que permite encadear.

    - `@return O fluxo, com o nó acrescentado — invisível não, para encadear com
      `|>`.`

16. Quando a função pode devolver **formas diferentes**, enumere as duas e a
    condição de cada uma.

    - `@return O artefato gravado, ou `NULL` quando a chave não existe no store.`

17. Não documente o retorno de função que aborta em caso de erro como se ele
    fosse opcional. Erro é erro: mencione a classe quando ela for parte do
    contrato (`class = "tr_error_bad_theme"`), em uma frase.

## D. `@examples`

18. Exemplo só na **API central** — o que o README ensina. Função de motor não
    precisa, e exemplo ruim é pior que exemplo nenhum.

19. O exemplo **roda em menos de 5 segundos**, não acessa rede e não escreve
    fora de `tempdir()`. O que criar arquivo limpa com `unlink()` no fim.

20. O exemplo usa a coleção `demo`, que vem com o pacote. Não invente coleção,
    não dependa de `trama.data`.

21. O que sobe app Shiny (`tr_app()`, `tr_ui()`, `tr_server()`) vai dentro de
    `if (interactive()) { ... }`. Não use `\dontrun{}`: o CRAN pede
    explicitamente a primeira forma para app interativo.

22. O exemplo mostra **uso real, pequeno**: montar um fluxo de dois ou três nós,
    rodar, ler o resultado. Não `f(1)`, não `# veja o manual`.

23. Sem comentário narrando o óbvio dentro do exemplo. Uma linha de comentário
    só quando o passo tem armadilha.

## E. Registro e voz

24. Português, terceira pessoa, presente do indicativo para o que a função é e
    faz: "devolve", "recusa", "grava", "resolve". Pretérito só para o que já
    aconteceu no objeto ("o documento já validado").

25. Sem imperativo dirigido ao leitor ("note que", "veja", "lembre-se",
    "atenção"). Sem metacomentário ("esta função é importante", "na prática",
    "em resumo").

26. Sem primeira pessoa, sem "nós", sem "o usuário deve" — o sujeito é a
    função ou o objeto.

27. Frase curta. Uma ideia por frase. Duas linhas de `@param` são normais; cinco
    indicam que a explicação pertence ao parágrafo de justificativa, não à tag.

28. Sem metáfora e sem personificação: um nó não "quer", o plano não "decide", o
    store não "sabe". Verbos do domínio: registrar, resolver, validar, gravar,
    ler, recusar, computar, invalidar, encadear.

29. Vocabulário fixo, sempre o mesmo termo para a mesma coisa:

    | use | não use |
    |---|---|
    | documento | grafo salvo, JSON do fluxo |
    | fluxo | flow, pipeline |
    | nó | bloco (no código), node |
    | bloco | nó (na interface), card |
    | porta | slot, conector |
    | plano | execução planejada, DAG |
    | chave de conteúdo | hash, id do cache |
    | handle | referência, ponteiro |
    | preview | prévia, miniatura |
    | coleção | pacote de blocos, plugin |
    | registro | registry, catálogo (o catálogo é outra coisa) |
    | catálogo | a saída de [tr_catalog()], legível por máquina |
    | executor | runner, backend |
    | coordenador | scheduler, agendador |
    | store | cache, armazenamento |
    | artefato | resultado gravado, output |

30. Nomes de função sempre com parênteses e em link roxygen quando forem do
    pacote: `[tr_plan()]`. Nome de argumento, de campo e de arquivo em crase:
    `` `registry` ``, `` `trama.json` ``.

## F. Formato

31. Linha de roxygen com no máximo **80 colunas**, continuação de tag indentada
    com três espaços. O `R CMD check` reclama de `.Rd` com linha longa.

32. Acento é livre no roxygen e nos comentários — o `R CMD check` só acusa
    não-ASCII em **string de código**. Não escreva `ç` na documentação.

33. `@family`, `@seealso` e `@rdname` só onde já existem. Esta revisão não
    reorganiza a estrutura das páginas de ajuda.

---

## Antipadrões, com o conserto

| escreve | por que é ruim | conserto |
|---|---|---|
| `@param registry O registro.` | repete o nome, não diz o que aceita | `@param registry Registro de tipos e nós, de [tr_registry()].` |
| `@return O resultado.` | não diz estrutura nem significado | `@return Lista com um elemento por nó pendente, na ordem do plano.` |
| `@return Invisível.` | diz a visibilidade e esconde o resto | `@return O projeto atualizado, invisível.` |
| `@param x Um objeto.` | não documenta nada | leia o código e diga a classe aceita |
| `@param verbose Se TRUE, imprime mais.` | descreve a implementação | `@param verbose Relata cada nó ao computar, por [message()].` |
| "Esta função serve para…" | preâmbulo | comece pelo verbo: "Grava…", "Resolve…" |
| "Veja também a documentação." | não informa | link direto: `[tr_store()]` |
| exemplo com `\dontrun{}` num app | o CRAN recusa a forma | `if (interactive()) { ... }` |

---

## Procedimento de entrega

1. Leia o arquivo inteiro antes de escrever, e leia os testes que o exercitam
   (`tests/testthat/test-<assunto>.R`). O comportamento documentado sai do
   código e dos testes, nunca da intuição.
2. Escreva as tags abaixo dos parágrafos que já existem, sem tocá-los.
3. Rode, da raiz do pacote:

   ```sh
   Rscript -e 'roxygen2::roxygenise()'
   git diff --stat NAMESPACE     # tem que sair vazio
   Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary")'
   ```

   **Em lote paralelo, NÃO rode `roxygenise()`.** Ele reescreve `man/` inteiro,
   e dois lotes rodando ao mesmo tempo na mesma árvore se atropelam. Nesse caso
   entregue só as edições em `R/`; quem coordena gera os `.Rd` de uma vez.

4. Confira o que sobrou, no arquivo que você tocou:

   ```sh
   R CMD build . && R CMD check --no-manual trama_*.tar.gz 2>&1 \
     | grep -A3 "Undocumented arguments\|checking Rd"
   ```

5. Relate: arquivos tocados, quantas funções ganharam `@param`/`@return`,
   quantos `.Rd` ainda faltam, e **toda dúvida de comportamento que você
   resolveu lendo o código** — é nesse ponto que a documentação erra.

---

## Modelos de frase

**Título** (primeira linha do roxygen, sem ponto final):

- "Grava os temas no manifesto — o único verbo que escreve `temas`."
- "Resolve um caminho relativo contra a raiz do projeto."

**`@param` de objeto do pacote:**

- "@param doc Documento do fluxo, de [tr_doc()] ou [tr_doc_read()]."
- "@param store Store do projeto, de [tr_store()]."

**`@param` com restrição:**

- "@param id Identificador do nó no documento: string não vazia, única no
  fluxo. Id repetido é recusado com erro."

**`@return` com estrutura e significado:**

- "@return Objeto `tr_registry`: ambiente com os tipos, categorias, nós e
  adaptadores registrados. Passe-o adiante; não o edite à mão."
- "@return `TRUE` quando a chave existe no store, `FALSE` quando não. Não lê o
  artefato."

**`@return` de efeito colateral:**

- "@return Nada de útil, invisível. Chamada pelo efeito: apaga do store os
  artefatos que nenhum documento do projeto referencia."

**Exemplo da API central:**

```r
#' @examples
#' reg <- tr_registry()
#' tr_use("demo", registry = reg)
#'
#' fluxo <- tr_flow(reg) |>
#'   tr_add("a", "demo/const", value = 2) |>
#'   tr_add("b", "demo/soma", from = "a", k = 3)
#'
#' tr_flow_doc(fluxo)
```

**Exemplo que escreve em disco:**

```r
#' @examples
#' pasta <- file.path(tempdir(), "meu-projeto")
#' projeto <- tr_project(pasta, collections = "demo")
#' tr_project_save(projeto, tr_doc(), "main")
#' list.files(file.path(pasta, "flows"))
#' unlink(pasta, recursive = TRUE)
```

**Exemplo de função interativa:**

```r
#' @examples
#' if (interactive()) {
#'   tr_app(tr_project(tempdir(), collections = "demo"))
#' }
```
