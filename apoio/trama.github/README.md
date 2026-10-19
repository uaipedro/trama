# trama.github

Pacote de apoio do trama (não é coleção: não tem blocos). Leva ao GitHub quem
nunca usou git, sem terminal: guia de criação de conta, login pelo device flow,
credencial no chaveiro do sistema e uma API interna para criar repositórios e
gravar commits e tags pela API de dados do Git. Desenho em
`docs/visao-publicar-e-github.md` (camada 1).

Instalado, o editor mostra o menu GitHub na barra de ferramentas.

## No console

```r
library(trama.github)
tr_github_guia_conta(abrir = TRUE)   # passo a passo da conta
tr_github_entrar()                   # código + link; o token vai para o chaveiro
tr_github_usuario()
r <- tr_github_criar_repo("meu-projeto", privado = TRUE)
tr_github_commit(r$repo, "Primeira versão", arquivos = c("fluxos/main.json" = "main.json"))
tr_github_tag(r$repo, "v1.0.0")
tr_github_sair()
```

## Credencial

`tr_github_token()` usa, nesta ordem: o token guardado pelo trama no chaveiro,
`GITHUB_PAT`, `GITHUB_TOKEN`, `gh auth token` e o `gitcreds`. "Sair" apaga só
o que o trama guardou.

## Client ID

O login pelo navegador usa o OAuth App "trama". O Client ID vem de
`options(trama.github.client_id = ...)`, da variável `TRAMA_GITHUB_CLIENT_ID`
ou da constante `.CLIENT_ID_TRAMA` em `R/client-id.R` (ponto único para fixar
o valor do app oficial). Não há client secret.

## Testes

Contra um GitHub falso (`webfakes`, em `tests/testthat/helper-github-falso.R`),
sem rede. As URLs mudam por `trama.github.api_url` e `trama.github.url`.
