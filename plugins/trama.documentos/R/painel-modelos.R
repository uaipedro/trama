# Aba Modelos do painel Documentos: o ponto onde a galeria de modelos
# (fase 3, #24) entra.
#
# Toda ação do JS cujo nome começa com `modelos_` chega aqui (ver
# `.td_controlador()`), com o mesmo `enviar` e o mesmo `ctx`. A fase 2 só
# lista; criar, editar, duplicar, importar e exportar são da fase 3, que
# acrescenta ramos a este `switch` e desenha a aba em
# `inst/trama/modelos-ui.js`. A lista de modelos também vai em toda
# mensagem `lista` (`.td_lista_painel()`), para o "novo documento".

.td_painel_modelos <- function(m, enviar, ctx) {
  switch(m$acao,
    modelos_listar = {
      md <- tr_documento_modelos()
      enviar(list(tipo = "modelos", modelos = lapply(seq_len(nrow(md)), function(i) {
        list(nome = md$nome[i], titulo = md$titulo[i], formatos = md$formatos[i], origem = md$origem[i])
      })))
    },
    enviar(list(tipo = "erro", acao = m$acao, mensagem = sprintf("A\u{E7}\u{E3}o de modelos desconhecida: '%s'.", m$acao))))
  invisible()
}
