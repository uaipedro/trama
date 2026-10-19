# trama.github

Plugin do trama (não é coleção: não tem blocos). Leva ao GitHub quem
nunca usou git, sem terminal: guia de criação de conta, login pelo device flow,
credencial no chaveiro do sistema e uma API interna para criar repositórios e
gravar commits e tags pela API de dados do Git. Desenho em
`docs/visao-publicar-e-github.md` (camada 1).

Instalado, o editor mostra o painel GitHub na barra de ferramentas, com a
conta e duas seções:

- **Projeto (privado):** guarda versões do projeto num repositório privado
  (camada 2 da visão, #15). "Salvar versão" com uma frase, histórico,
  restaurar uma versão e escolher que arquivos de dados entram.
- **Publicações (públicas):** os repositórios que os nós `publicar` do projeto
  criaram (lidos do `trama.publicar`, se instalado), com versões, links da
  página e da release, a situação do último workflow do Actions e "ir ao bloco".

## Versionar o projeto

O que vai numa versão (`tr_github_versionar_arquivos()`):

- **sempre:** `trama.json`, `flows/*.json`, código (R, Rmd, qmd, py, sql),
  metadados (md, cff, yml, toml, DESCRIPTION, LICENSE...) e `imagens/`;
- **dados** (todo o resto): fora até serem marcados, um por um. Acima de
  100 MB o GitHub recusa; nome que sugere dado pessoal ganha aviso;
- **nunca:** `.trama/` (store, execuções, token do editor), `.git/`, arquivos
  de sessão do R, `.Renviron`, `.env` e chaves.

Cada versão é o retrato exato dessa seleção (só o que mudou é enviado).
Restaurar baixa tudo antes de mexer, guarda o estado atual em
`.trama/restauracao/<data-hora>/`, nunca apaga dado e mantém a ligação com o
GitHub e o registro de publicações; o editor relê o projeto em seguida. A
configuração fica no `trama.json`, chave `versionar`. O repositório do projeto
é outro, nunca o de uma publicação, e não é citado no pacote público.

## No console

```r
library(trama.github)
tr_github_guia_conta(abrir = TRUE)   # passo a passo da conta
tr_github_entrar()                   # código + link; o token vai para o chaveiro
tr_github_usuario()
r <- tr_github_criar_repo("meu-projeto", privado = TRUE)
tr_github_commit(r$repo, "Primeira versão", arquivos = c("fluxos/main.json" = "main.json"))
tr_github_tag(r$repo, "v1.0.0")

# versionar um projeto
tr_github_versionar_criar(".")                # repositório privado
tr_github_versionar_marcar(".", "dados/vendas.csv")
tr_github_salvar_versao(".", "Modelo com a variável nova")
v <- tr_github_versoes(".")
tr_github_restaurar(".", v$sha[2])
tr_github_sair()
```

## Credencial

`tr_github_token()` usa, nesta ordem: o token guardado pelo trama no chaveiro,
`GITHUB_PAT`, `GITHUB_TOKEN`, `gh auth token` e o `gitcreds`. "Sair" apaga só
o que o trama guardou.

## Client ID

O login pelo navegador usa o OAuth App "trama". O Client ID vem de
`options(trama.github.client_id = ...)`, da variável `TRAMA_GITHUB_CLIENT_ID`
ou da constante `.CLIENT_ID_TRAMA` em `R/client-id.R`, que guarda o Client ID
do app oficial. As duas primeiras servem para apontar para outro app. Não há
client secret.

## Testes

Contra um GitHub falso (`webfakes`, em `tests/testthat/helper-github-falso.R`),
sem rede. As URLs mudam por `trama.github.api_url` e `trama.github.url`.
