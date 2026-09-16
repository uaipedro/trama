#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# `pkgs` não é variável deste pacote: é argumento NOMEADO que
# `tr_executor_pool()` passa para `mirai::mirai()`, e que só existe dentro do
# daemon, na avaliação da expressão. O verificador de código lê a expressão
# entre chaves como se fosse corpo de função daqui e não tem como saber disso —
# daí a nota de "no visible binding". Declarar aqui é o remédio padrão; mudar a
# chamada para calar o verificador mudaria o que roda no worker.
utils::globalVariables("pkgs")
