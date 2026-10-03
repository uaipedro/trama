---
title: Instalação
description: Instalar o trama sem saber programar, no R com pak ou remotes, e pelo trama-cli quando ainda não há R na máquina.
section: por-dentro
order: 2
---

<div class="install-choice">
  <section class="install-choice__option">
    <h2>Programa</h2>
    <p>Para quem não usa R. Ele baixa o R sozinho e cria o atalho <strong>Trama</strong>.</p>
    <a class="button button--primary" href="https://github.com/uaipedro/trama/releases/latest/download/Trama-Setup.exe">Baixar para Windows</a>
    <p><a href="#instalar-sem-saber-programar">No Linux, um comando →</a></p>
  </section>
  <section class="install-choice__option">
    <h2>Pacote R</h2>
    <p>Para quem já tem R 4.1 ou mais novo: instala o núcleo e as coleções no seu R.</p>
    <p><a href="#pelo-r-com-pak">Instalar com pak →</a></p>
  </section>
</div>

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

### macOS

Ainda não há programa pronto para macOS, e o `trama-cli` também não funciona
nele (o R portátil que ele baixa só existe para Windows e Linux). No Mac,
instale o [R](https://cran.r-project.org/) e use o [pacote R](#pelo-r-com-pak).

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
pak::pak("uaipedro/trama/collections/trama.experiments")  # experimentos
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

O `trama-cli` é um programa de linha de comando escrito para Node.js e
distribuído pelo npm; por isso o Node.js 20 ou superior é exigido. Ele baixa
um R portátil e prepara o núcleo do trama com as coleções Dados e
Visualização, sem que você instale o R separadamente.

Confira a versão do Node.js com `node --version`. Se o comando não existir ou
mostrar uma versão anterior à 20, instale uma versão atual pelo
[site do Node.js](https://nodejs.org/).

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

## Se algo der errado

**`npm install` avisa `EBADENGINE` ou `trama` falha logo ao iniciar.** O
`trama-cli` exige Node.js 20 ou superior. Confira com `node --version` e
atualize pelo [site do Node.js](https://nodejs.org/).

**`trama: command not found` (ou "não é reconhecido como comando") depois do
`npm install -g`.** A pasta de programas globais do npm não está no PATH.
Veja onde ela fica com `npm prefix -g` (no Linux, os comandos ficam em
`bin/` dentro dela) e acrescente-a ao PATH; depois abra um terminal novo.

**`trama create` para com "SO não suportado: darwin".** O `trama-cli` não
roda no macOS. Use o [pacote R](#pelo-r-com-pak).

**`trama create` para com "Já existe um projeto em ...".** Já há uma pasta
com esse nome. Escolha outro nome ou entre na pasta e use `trama open`.

**`trama create` ou `trama install` para com "falha ao baixar".** O download
do R portátil ou de um pacote falhou após várias tentativas, em geral por
rede ou proxy. Rode o comando de novo: o que já foi instalado por completo é
aproveitado, e o que ficou pela metade é instalado outra vez.

**O instalador do Linux para com "glibc ... é antiga demais".** O R portátil
exige glibc 2.34 ou superior (Ubuntu 22.04, Debian 12 ou mais novos). Em
sistemas mais antigos, instale o R pelo gerenciador de pacotes e use o
[pacote R](#pelo-r-com-pak).

**No R, `library(trama)` responde "não há nenhum pacote chamado 'trama'", ou
o editor abre sem os blocos de Dados e Visualização.** Falta instalar o núcleo
ou as coleções nessa instalação do R. Rode o comando `pak::pak(...)` da seção
[Pacote R](#pelo-r-com-pak) e reinicie a sessão do R.

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
