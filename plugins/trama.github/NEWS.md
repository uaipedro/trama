# trama.github 0.2.1

* A seção **Publicações** do painel lê as publicações dos documentos (`trama.documentos::tr_documento_publicacoes()`), no lugar do registro do `trama.publicar`, que foi retirado (#25). "Ir ao bloco" vira "Abrir o documento", que abre o painel Documentos nele.

# trama.github 0.1.2

* Agora é um **plugin** do trama: a pasta passa a `plugins/trama.github`, o campo do DESCRIPTION a `Config/trama/plugin: true` e a função do contrato a `trama_plugin()` (era `trama_extension()`), pedida pelo `trama::tr_plugins()`. Instalação pelo r-universe com o subdiretório `plugins/trama.github`.

# trama.github 0.1.1

* Para a publicação (#14): `tr_github_tags()`, `tr_github_release()`, `tr_github_pages()` e `tr_github_ativar_pages()` (Pages em modo Actions; repositório privado sem plano pago dá erro que explica), `tr_github_ambiente_tags()` (o ambiente `github-pages` passa a aceitar deploy das tags `v*`), `tr_github_execucoes()` e `tr_github_falha()` (em que job e etapa o workflow falhou).
* GitHub falso dos testes: rotas de tag por expressão regular, porque o parâmetro do webfakes não casa nome com ponto (`v1.0.0`), e as rotas novas.

# trama.github 0.1.0

* Primeira versão (#11): guia de conta, device flow, credencial no chaveiro, commit atômico e tag pela API de dados do Git, menu GitHub no editor.
