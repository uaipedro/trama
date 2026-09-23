---
title: Instalação
description: Instalar o trama sem saber programar, no R com pak ou remotes, e pelo trama-cli quando ainda não há R na máquina.
section: por-dentro
order: 2
---

## Instalar sem saber programar

Quem nunca abriu um terminal também instala o trama. No **Windows**, baixe e
rode o instalador — ele não exige nada instalado antes (nem R, nem
Node): baixa o R oficial sozinho, silenciosamente, e depois monta o atalho
"Trama" no menu Iniciar.

<a class="button button--primary" href="https://github.com/uaipedro/trama/releases/latest/download/Trama-Setup.exe">Baixar Trama-Setup.exe</a>

> O Windows vai mostrar um aviso azul ("O Windows protegeu o computador"),
> porque o instalador ainda não tem assinatura digital paga — não porque
> ele contém algo malicioso (o instalador não embute nenhum executável
> nosso; ele só baixa o instalador oficial do R e roda scripts). Clique em
> **"Mais informações"** e depois em **"Executar assim mesmo"**.

Depois de instalado, abra o atalho **Trama** no menu Iniciar: ele abre uma
tela local no navegador com as versões instaladas, as coleções disponíveis
e o botão para abrir o editor.

No **Linux**, um único comando faz a mesma coisa (baixa um R portátil e
instala o núcleo do trama, sem precisar de privilégio de administrador):

```bash
curl -fsSL https://raw.githubusercontent.com/uaipedro/trama/main/tools/installer/linux/install.sh | bash
```

Ao final, o trama aparece no menu de aplicativos como **Trama**. Rodar o
comando de novo atualiza para a versão mais recente.

## Pelo R, com pak

O pacote requer R 4.1 ou versão posterior. Enquanto não estiver disponível no
CRAN, a instalação é feita pelo GitHub com o
[`pak`](https://pak.r-lib.org):

```r
install.packages("pak")

# Núcleo e coleções recomendadas para iniciar
pak::pak(c(
  "uaipedro/trama",
  "uaipedro/trama/collections/trama.data",
  "uaipedro/trama/collections/trama.view"
))
```

As demais coleções são opcionais e instalam suas próprias dependências:

```r
pak::pak("uaipedro/trama/collections/trama.series")    # séries temporais
pak::pak("uaipedro/trama/collections/trama.models")    # modelos estatísticos
pak::pak("uaipedro/trama/collections/trama.multi")     # análise multivariada
pak::pak("uaipedro/trama/collections/trama.sampling")  # amostragem
pak::pak("uaipedro/trama/collections/trama.ml")        # aprendizado de máquina
```

Uma versão específica pode ser fixada pela tag, como em
`"uaipedro/trama@v0.1.0"`.

### Com remotes

Com o `remotes`, os pacotes devem ser instalados na ordem das dependências:
núcleo, `trama.data`, `trama.view` e, em seguida, as demais coleções.

```r
remotes::install_github("uaipedro/trama")
remotes::install_github("uaipedro/trama", subdir = "collections/trama.data")
remotes::install_github("uaipedro/trama", subdir = "collections/trama.view")
```

## Pelo trama-cli, sem R instalado

O `trama-cli` é um pacote npm que baixa um R portátil e instala o núcleo do
trama, para quem quer começar sem preparar o R primeiro:

```bash
npm install -g @uaipedro/trama-cli
```

Comandos disponíveis:

```bash
trama install            # baixa o R portátil e instala o núcleo do trama
trama create meu-fluxo   # cria um projeto, escolhe coleções, abre o editor
trama add trama.ml       # instala uma coleção adicional no projeto atual
trama update             # atualiza o núcleo e as coleções instaladas
trama open               # abre o editor no projeto da pasta atual
```

`trama create` cria a pasta do projeto, pergunta quais coleções acrescentar
além de Dados e Visualização e abre o editor no navegador ao final. Rodado
dentro de uma pasta de projeto já existente (com `trama.json`), `trama open`
abre o editor sem passar pelas perguntas de criação.

## Primeiro fluxo

Depois de instalado no R, o editor abre com:

```r
library(trama)

tr_app(tr_project(
  "meu-projeto",
  collections = c("trama.data", "trama.view")
))
```

O fluxo é salvo em `meu-projeto/flows/main.json`.
