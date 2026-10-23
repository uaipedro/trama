# Visão de design — `trama.publicar` e `trama.github`

> **Atualização (2026-10-19):** página, layout, componentes e o nó `publicar/publish` convergem para o plugin `trama.documentos` — ver `docs/visao-documentos.md`. As partes de GitHub (conta, versionar, release, Pages) continuam valendo. Feito na fase 4 do `trama.documentos` (#25): o `trama.publicar` foi retirado, e projetos com o nó são migrados para documento ao abrir.

## Para que serve este documento

Registrar o desenho, conversado em 2026-10-19, de como um fluxo do trama sai do computador de quem o fez: versionado no GitHub e publicado como pacote reprodutível, com uma página onde o visitante vê os resultados e usa os modelos no próprio navegador.

É uma visão, não uma especificação. Nomes de nós, parâmetros e arquivos são exemplos para discussão. Ela substitui a ideia original do issue #10, que começava pelo MLflow; o ciclo de vida de modelos (`trama.mlops`) ficou para depois e passa a se apoiar na publicação.

## O problema

Hoje um resultado do trama vive no store do projeto (`.trama/store/objects/<hash>.rds`), endereçado por conteúdo, sem nome, sem versão e sem forma de sair. O GC do store (`tr_store_gc`, `max_age_days = 7`) apaga o que nenhum fluxo alcança. Não há como dizer "a versão 1.2 do material desta dissertação", nem entregar a outra pessoa um modelo que ela consiga usar.

O pesquisador que quer publicar o material suplementar de um artigo junta à mão: exporta tabelas, salva figuras, escreve um README, sobe num repositório, se souber usar git. O fluxo que gerou tudo, que é justamente o que torna o material reprodutível, fica de fora.

## Ideia central

**Publicar é um nó do fluxo, e o GitHub é a casa do projeto.**

O pesquisador liga ao nó `publicar` o que quer mostrar (modelos, tabelas, figuras), monta a página nas propriedades do nó, confere uma prévia local e aperta "publicar". O trama cria o repositório, envia o pacote, e o GitHub Actions gera a release e a página no GitHub Pages. Quem visita a página vê os resultados, baixa o pacote e usa os modelos ali mesmo, via WebR, sem que nenhum dado saia do navegador. Quem baixa o pacote abre o fluxo no editor e reproduz tudo.

Quase ninguém publica um modelo junto com o fluxo que o produziu. É aí que o trama se diferencia, e é o que dá substância ao FAIR (seção própria abaixo).

O mesmo GitHub serve, antes de qualquer publicação, para **versionar o projeto** num repositório privado. Muita gente que usa o trama nunca usou git nem tem conta no GitHub; o trama guia esse caminho também.

## Público, em ordem de prioridade

1. **Pesquisador**: material suplementar de artigo, dissertação ou relatório, reprodutível e citável.
2. **Ensino**: professor e alunos compartilhando fluxos e modelos; a página como objeto de aula.
3. **Cliente**: entrega de um modelo para alguém prever com dados próprios.

## Três camadas

| Camada | Onde mora | O que faz |
|---|---|---|
| Conexão GitHub | plugin `trama.github` | guia de criação de conta, login, token, criar repositório, commit pela API |
| Versionar o projeto | `trama.github`, no menu do editor | "salvar versão", histórico e restaurar, num repositório **privado** |
| Publicar | coleção `trama.publicar` | nó `publicar`, pacote RO-Crate, página, prévia e envio para um repositório **próprio** da publicação |

`trama.github` é plugin, não coleção: não tem nós. O editor mostra o menu GitHub quando ele está instalado, e ele entra na instalação padrão. O núcleo continua sem dependências de rede (`httr2`, `keyring`, `gitcreds` ficam no `trama.github`). `trama.publicar` depende de `trama.github`; o inverso nunca.

Repositório do projeto e repositório da publicação são separados de propósito. O primeiro é o caderno de trabalho; o segundo é o material limpo. O rascunho nunca vaza junto com a publicação.

## Camada 1 — conexão com o GitHub

Meta: quem nunca usou git publica a primeira pesquisa sem sair do editor e sem abrir o terminal.

- **Sem conta.** A conta é criada pela pessoa no github.com (o GitHub exige um humano no cadastro). O editor abre a página certa, explica cada passo em português e lembra do GitHub Education, que dá o plano Pro de graça a estudantes e professores.
- **Login pelo device flow**, o mesmo do `gh auth login`: o editor mostra um código e um link; a pessoa autoriza no navegador. Exige registrar uma vez um OAuth App "trama" no GitHub.
- **Credencial já existente é aproveitada**: `GITHUB_PAT`, `gh auth token` ou o `gitcreds` que o `usethis` usa.
- **O token fica no chaveiro do sistema** (`keyring`), nunca no JSON do fluxo, no projeto ou no pacote.
- **Commit pela API de dados do Git** (blobs, árvore, commit, referência), sem git instalado. A referência só muda no final, então uma queda de rede no meio não deixa nada pela metade.

## Camada 2 — versionar o projeto

- Botão "salvar versão" no editor, com uma frase sobre o que mudou. Vira um commit no repositório privado do projeto.
- Histórico com as versões salvas e "restaurar" uma anterior.
- **Sempre vão**: fluxos, código R, configuração e metadados do projeto.
- **Dados ficam de fora por padrão.** O pesquisador marca quais arquivos de dados versionar, com aviso de tamanho (o GitHub recusa arquivo acima de 100 MB) e de sensibilidade.
- **O store (`.trama/store`) nunca vai**: é cache, refeito pelo fluxo.

## Camada 3 — o nó `publicar`

### Entrada

Uma porta `itens` do tipo `publicar/item`, com `multiple = TRUE` (como `models/select` já faz com modelos). Cada tipo entra por um **adaptador** registrado `<tipo> -> publicar/item`, mecanismo que o registro já tem (`tr_adapter_for`).

- `trama.publicar` traz os adaptadores de `data/table`, `view/plot` e `models/fit`.
- Uma coleção futura registra o próprio adaptador. É assim que a publicação serve a outras coleções sem conhecê-las.
- Contrato do adaptador: devolve os arquivos do item, os metadados dele e, opcionalmente, um componente de página.

### Sanitização de modelos (bloqueante)

O objeto de ajuste carrega dados de treino. Publicar o `.rds` como está publica os dados junto: risco de LGPD e de tamanho. Hoje `tr_models_serialize()` devolve o objeto inteiro (default identidade; só o xgboost tem método).

O contrato de `models/fit` ganha um genérico novo, `tr_models_strip(x)`: o que sobra do modelo para **prever**, sem os dados. O adaptador de modelo chama esse método; classe sem método vira erro no card e o item não entra.

Não basta apagar `dados`. No `lm`, `qr$qr` é a própria matriz de delineamento do treino. Verificado em 2026-10-19: `lm(mpg ~ wt + hp, mtcars)` reduzido a coeficientes, `terms`, `qr` só com `pivot` e `rank`, sem `model`, `residuals`, `fitted.values` e `effects` prevê igual ao original e cai de 7,6 KB para 1,4 KB serializado. Cada classe precisa do mesmo cuidado (rpart guarda `y` e `where`; ranger e e1071 têm os seus).

Toda classe com `tr_models_strip()` leva dois testes: a predição é igual antes e depois, e nada dos dados de treino sobra no objeto.

### Dados de entrada

Só entra no pacote o que está ligado ao nó. Dos dados de entrada não ligados vai a **descrição**: colunas, tipos, número de linhas e hash. O "como reproduzir" diz qual arquivo trazer, e o fluxo confere o hash ao abrir.

### Parâmetros

Título, descrição, palavras-chave, autores com ORCID (o padrão vem do projeto), licença (padrão CC-BY-4.0 para conteúdo e MIT para código), nome do repositório, tipo da próxima versão (correção, menor, maior) e o **layout da página** (seção seguinte).

### O pacote

Um RO-Crate: pasta com metadados JSON-LD do schema.org, o padrão de empacotamento de objetos de pesquisa.

```
ro-crate-metadata.json   metadados; modelos descritos com o vocabulário FAIR4ML
flow.json                o fluxo que gerou tudo
itens/                   modelos sanitizados (.rds + bloco web), tabelas, figuras
dados-entrada.json       descrição e hash dos dados não incluídos
ambiente.json            versões de R, trama, coleções e pacotes
README.md, CITATION.cff, LICENSE
```

O bloco `web` de cada modelo descreve o que a página precisa sem conhecer R: pacotes e versões esperadas, entradas (nome, tipo, níveis, faixas), saída e pré-processamento. Modelo cujo pré-processamento acontece fora do ajuste e não está descrito não ganha preditor na página.

### Execução

Montar o pacote numa pasta local é determinístico: o nó continua puro e cacheável. **Rodar o fluxo nunca publica.** Publicar é uma ação do card, fora da execução.

## A página

### Componentes e layout

A página é montada por **nós de componente**, cada um com propriedades próprias no painel (título, legenda, largura, altura, visível). Eles saem como `publicar/item` e entram na porta `itens`.

A ligação diz **o que** entra; **onde** cada coisa fica é editado nas propriedades do `publicar`, num editor de layout, não pela ordem das arestas. O layout é gravado nos parâmetros do nó, citando os ids dos componentes: continua no JSON e reprodutível. Componente ligado que não está no layout vai só para o pacote.

Dois formatos:

- **página**: artigo em uma coluna;
- **dashboard**: grade de linhas, colunas e abas.

Componentes do v1, poucos e bem feitos:

- **cabeçalho** automático: título, autores com ORCID, licença, versão, como citar, como reproduzir;
- **texto**: markdown com equações e citações;
- **figura** e **tabela** (com download);
- **caixa de valor**: uma métrica em destaque, para o dashboard;
- **preditor de modelo**: formulário e upload de CSV que preveem via WebR;
- **diagrama do fluxo**: o grafo desenhado, estático.

Depois: gráfico interativo, filtros WebR no dashboard, executar trechos do fluxo no navegador, temas e ABNT na página.

### Renderização: Quarto + quarto-live

O trama gera um `.qmd` a partir do layout (`format: live-html` ou `live-dashboard`, extensão [quarto-live](https://r-wasm.github.io/quarto-live/)). O Actions renderiza com Quarto; o Quarto dá de graça tema, matemática, citações e o carregamento do WebR.

Verificado em 2026-10-19, com Quarto 1.7.32 e quarto-live v0.2.0 (que trouxe webR 0.6.0, R 4.6.0): um `live-dashboard` com dois controles deslizantes e uma célula WebR que lê o `modelo.rds` sanitizado da mesma pasta, servido por um servidor HTTP simples **sem** os cabeçalhos COOP/COEP (como o GitHub Pages), previu 20,83 e, com o peso alterado, 15,01, iguais ao R local. A primeira carga levou cerca de 40 s (WebR e pacotes); as seguintes vêm do cache do navegador.

Consequências para o desenho:

- a página avisa que está "carregando o R no seu navegador";
- **a versão da extensão é fixada** no projeto gerado, nunca a mais recente; a página mostra a versão de R e dos pacotes do treino ao lado das carregadas no navegador;
- o modelo é servido pelo Pages na mesma origem da página. Arquivo de release não serve: o download redireciona sem `Access-Control-Allow-Origin`.

### Motores no WebR

Com binário WebR no repositório oficial: `lm`/`glm` (base R), `rpart`, `e1071`, `ranger`. O `xgboost` só existe no índice do R 4.5 em diante. O **FIGS fica de fora no v1**: o `figsr` não tem binário WebR; publicar um FIGS dá erro claro até o suporte entrar no próprio `figsr`.

## Prévia local

O card do `publicar` tem o botão **visualizar**: gera o pacote e o `.qmd` exatamente como sairiam, renderiza com o Quarto local e abre no navegador por um servidor local (abrir o arquivo direto não carrega o WebR). O que se vê é o que sai.

A prévia aparece junto da conferência: lista de itens, tamanhos e avisos ("repositório público", "modelo grande para o navegador"). Só depois dela o botão "publicar" fica liberado. Sem Quarto instalado, o card guia a instalação; o Actions não depende do Quarto local.

## Envio e GitHub Actions

### Primeiro publish

Num clique, o trama cria o repositório da publicação (nome sugerido a partir do título; público por padrão), grava o workflow, liga o GitHub Pages em modo Actions pela API e grava README, LICENSE e CITATION.cff.

Repositório privado com Pages exige plano pago (o GitHub Education cobre). O card avisa antes de criar.

### Cada publish

O trama faz o commit do pacote e a tag `vX.Y.Z` pela API. A tag dispara o workflow, que:

1. confere o pacote (checksums, RO-Crate válido);
2. instala Quarto fixado e um R mínimo (o knitr exige R para renderizar, não os pacotes do projeto: as células WebR só rodam no navegador);
3. renderiza a página;
4. cria a release com o pacote zipado;
5. publica no Pages.

O deploy do Pages substitui o site inteiro. Para manter versões antigas no ar, o workflow acumula as páginas numa branch `paginas`, acrescenta `/vX.Y.Z/`, aponta `/ultimo/` para a nova e só então faz o deploy.

O card acompanha: enviado → Actions rodando → release criada → página no ar, com o link final. Se o workflow falhar, mostra a etapa e o link do log.

### Erros

- **Antes de sair** (bloqueiam o botão): modelo sem `tr_models_strip()`, item sem adaptador, login ausente ou expirado, arquivo acima de 100 MB (aviso a partir de ~20 MB, porque o navegador baixa o modelo).
- **Tag já existente**: recusada, com a próxima versão sugerida. Versão publicada nunca muda.
- **Rede no meio do envio**: o commit é atômico; repetir o envio é seguro.

## FAIR

| Princípio | Como entra |
|---|---|
| Encontrável | metadados ricos no RO-Crate e no CITATION.cff; DOI por versão **depois** (integração Zenodo–GitHub, que cria um DOI a cada release) |
| Acessível | página e pacote por HTTPS público; versões imutáveis em `/vX.Y.Z/` |
| Interoperável | RO-Crate (JSON-LD, schema.org), FAIR4ML para modelos, CSV para tabelas |
| Reutilizável | licença explícita, autores com ORCID, `flow.json` + `ambiente.json` + hashes dos dados: proveniência completa e reprodução no editor |

## Testes

- Adaptadores e pacote: snapshot da estrutura gerada e validação do RO-Crate.
- `tr_models_strip()` por classe: predição igual antes e depois, sem dados de treino no objeto.
- `trama.github` contra um GitHub falso (`webfakes`), sem rede. Integração contra um repositório de teste, só quando pedida.
- Página: o CI renderiza o `.qmd`; um teste opcional abre a prévia num navegador e confere a predição, como na verificação acima (opcional porque baixa o WebR).

## Fases

1. **Base, em duas frentes paralelas.**
   - `trama.github`: guia de conta, device flow, chaveiro, aproveitar credencial existente.
   - `trama.publicar` local: tipo `publicar/item`, adaptadores de tabela, figura e modelo, `tr_models_strip()` (lm, glm, rpart, ranger, e1071, xgboost), pacote RO-Crate, página no formato artigo, prévia local.
2. **Publicar no GitHub**: botão, criação do repositório, workflow, release, Pages com `/vX.Y.Z/` e `/ultimo/`, acompanhamento no card.
3. **Versionar o projeto**: salvar versão, histórico e restaurar, reaproveitando o commit pela API.
4. **Dashboard**: formato dashboard, caixa de valor, editor de layout em grade.
5. **Depois**: DOI via Zenodo, FIGS no WebR, `trama.mlops` (registro de treinos, modelo campeão), adaptadores de outras coleções, serving por API.

## Decisões tomadas

| Pergunta | Decisão |
|---|---|
| Publicar e MLOps juntos? | Separados; publicar vem antes e serve a outras coleções |
| Unidade de publicação | O pacote do fluxo (fluxo + itens escolhidos), não um artefato solto |
| DOI | Depois; v1 publica em GitHub Releases e Pages |
| Como montar o pacote | Nó `publicar` no fluxo |
| Página | Componentes + layout editável nas propriedades; Quarto + quarto-live; página ou dashboard |
| Tecnologia da predição | Só WebR (sem atalho de exportar coeficientes para JavaScript) |
| Dados de entrada | Só entram se ligados; o resto vai como descrição + hash |
| Repositório | Um por publicação; projeto em repositório privado separado |
| Envio | Commit e tag pela API + GitHub Actions |
| Onde mora a conexão | Plugin `trama.github` |
| Versionar dados do projeto | Fora por padrão, opcionais por arquivo |
| Prévia | Local, idêntica ao que sai, antes de liberar o envio |

## Em aberto

- Quem registra e mantém o OAuth App "trama" (conta pessoal ou organização).
- Formato exato do layout nos parâmetros do nó e como o editor de layout aparece no painel.
- Até onde o pacote descreve o ambiente: lista de versões ou um `renv.lock` completo.
- Fits Python (`trama.python`) não serializam: fora do v1, com erro claro.
