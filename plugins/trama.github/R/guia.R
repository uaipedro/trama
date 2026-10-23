#' Guia de criação de conta no GitHub
#'
#' O GitHub exige uma pessoa no cadastro, então o trama não cria a conta:
#' abre a página certa e explica cada passo. O editor mostra os mesmos passos
#' no menu GitHub.
#'
#' @param abrir Abrir a página de cadastro no navegador.
#' @return Lista de passos, cada um com `titulo`, `texto` e, quando há,
#'   `link` e `rotulo` (texto do link). Invisível quando imprime no console.
#' @export
tr_github_guia_conta <- function(abrir = FALSE) {
  web <- tr_github_url()
  passos <- list(
    list(titulo = "Abra o cadastro do GitHub",
         texto = "O GitHub é onde o trama guarda as versões do seu projeto e publica os resultados. A conta é gratuita.",
         link = paste0(web, "/signup"), rotulo = "Criar conta no GitHub"),
    list(titulo = "Use um e-mail que você acompanha",
         texto = "Se você é estudante ou professor, prefira o e-mail da instituição: ele ajuda na aprovação do GitHub Education."),
    list(titulo = "Escolha bem o nome de usuário",
         texto = "Ele aparece no endereço das suas publicações (nome.github.io). Prefira algo curto e profissional, como o seu nome."),
    list(titulo = "Confirme o e-mail",
         texto = "O GitHub manda um código para o e-mail. Digite-o na página para ativar a conta. Se pedir um quebra-cabeça de verificação, é normal."),
    list(titulo = "Estudante ou professor? Peça o GitHub Education",
         texto = "É gratuito e dá o plano Pro, que permite página publicada a partir de repositório privado. A aprovação pode levar alguns dias; dá para usar o trama enquanto isso.",
         link = "https://education.github.com/pack", rotulo = "Conhecer o GitHub Education"),
    list(titulo = "Volte ao trama e entre",
         texto = "No menu GitHub, clique em \"Entrar com o GitHub\". O trama mostra um código; você o digita no navegador e autoriza.")
  )
  if (!interactive() || !abrir) return(passos)
  utils::browseURL(passos[[1]]$link)
  for (i in seq_along(passos)) {
    p <- passos[[i]]
    cat(sprintf("%d. %s\n   %s\n", i, p$titulo, p$texto))
    if (!is.null(p$link)) cat("   ", p$link, "\n")
  }
  invisible(passos)
}
