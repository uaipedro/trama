---
title: Assistente (MCP)
description: Ligar o trama a um assistente (Claude Code, Claude Desktop, Codex, Cursor) pelo MCP. Passos do zero, escritos para uma pessoa ou para o próprio assistente seguir.
section: por-dentro
order: 4
---

Com o servidor MCP do trama, um assistente de IA monta o fluxo no editor
aberto na sua tela: cada bloco aparece e roda na hora, os números vêm do R e
você desfaz qualquer passo com `Ctrl+Z`. Funciona com qualquer cliente MCP
local.

Se você é um assistente lendo esta página para instalar o trama, siga os
passos em ordem, rode cada comando e confira a saída antes de seguir. Peça à
pessoa só o que exige ela: instalar o R, se faltar, e reiniciar o cliente.

## 1. R

O trama precisa do R 4.1 ou mais novo. Confira:

```bash
Rscript --version
```

Sem R, a pessoa instala por [cran.r-project.org](https://cran.r-project.org)
(Windows e macOS) ou pelo gerenciador do sistema (`sudo apt install r-base` no
Debian/Ubuntu). No Linux, os pacotes compilam do código-fonte e podem pedir
bibliotecas do sistema; o erro do `pak` nomeia qual falta.

## 2. Pacotes

```bash
Rscript -e 'install.packages("pak", repos = "https://cloud.r-project.org")'
Rscript -e 'pak::pak(c("uaipedro/trama", "uaipedro/trama/collections/trama.data", "uaipedro/trama/collections/trama.view", "uaipedro/trama/collections/trama.models"))'
```

`trama.data` e `trama.view` são o mínimo (dados e gráficos);
`trama.models` traz regressão, ANOVA e afins. As outras coleções
(`trama.series`, `trama.multi`, `trama.sampling`, `trama.experiments`,
`trama.ml`, `trama.spatial`) entram do mesmo jeito, com
`pak::pak("uaipedro/trama/collections/<nome>")`.

Confira:

```bash
Rscript -e 'library(trama); packageVersion("trama")'
```

## 3. Registrar o servidor

O servidor é `Rscript -e "trama::tr_mcp()"`, rodando na pasta do projeto.
Pasta sem `trama.json` vira um projeto novo com todas as coleções instaladas.

**Claude Code**, de dentro da pasta do projeto:

```bash
claude mcp add trama -- Rscript -e "trama::tr_mcp()"
```

**Codex**, em `~/.codex/config.toml`:

```toml
[mcp_servers.trama]
command = "Rscript"
args = ["-e", "trama::tr_mcp()"]
```

**Claude Desktop** (`claude_desktop_config.json`) e **Cursor**
(`.cursor/mcp.json`): esses clientes não rodam na pasta do projeto, então
passe-a:

```json
{
  "mcpServers": {
    "trama": {
      "command": "Rscript",
      "args": ["-e", "trama::tr_mcp('/caminho/do/projeto')"]
    }
  }
}
```

No Windows, use o caminho completo do `Rscript.exe` se ele não estiver no
`PATH` (por exemplo `C:\\Program Files\\R\\R-4.5.1\\bin\\Rscript.exe`).
Depois de registrar, o cliente precisa ser reiniciado (ou `/mcp` no Claude
Code) para ver as tools.

## 4. Primeiro fluxo

Peça ao assistente, por exemplo: *"Abra o trama e monte um histograma do
comprimento da sépala no iris."* Ele vai:

1. `abrir`: sobe o editor e abre o navegador;
2. `catalogo` com `busca: "exemplo"` e `"histograma"`;
3. `adicionar` `data/example` com `dataset: "iris"`, depois `view/histogram`
   com `de: ["dados"]` e `x: "Sepal.Length"`;
4. `resultado`: lê o gráfico, que chega como imagem.

## Tools

| Tool | O que faz |
| --- | --- |
| `abrir` | Abre o editor do projeto no navegador, ou se conecta ao já aberto. |
| `estado` | Nós, params, status e ligações do fluxo na tela. |
| `catalogo` | Busca blocos por palavras (até 12, o melhor primeiro). |
| `explicar` | Um bloco inteiro: params, portas, ajuda, referências. |
| `adicionar` | Põe um bloco, opcionalmente ligado a outros (`de`), e roda. |
| `ajustar` | Muda params de um nó. |
| `ligar` | Liga uma saída a uma entrada. |
| `remover` | Tira um nó. |
| `resultado` | O que o nó produziu; gráficos como imagem. |
| `desfazer` | Desfaz a última edição, do assistente ou sua. |
| `validar` | Confere um `.json` de fluxo do disco, sem editor. |

Toda edição espera o fluxo rodar (até 30 s, ajustável em `espera`) e devolve
`efeito`: o status do que mudou e de tudo abaixo. Um bloco `failed` traz a
mensagem; um `blocked` traz `causa`, os blocos acima que falharam.

## Como funciona

As tools são os comandos do [`trama-agente`](/trama/por-dentro/agentes/): nenhuma edição
passa por fora do editor. As tools de edição precisam de um editor com aba
aberta; se não houver, o servidor sobe `tr_app()` num processo R em segundo
plano e abre o navegador. Esse editor fecha junto com o assistente; um editor
que você mesmo abriu fica. O log do editor em segundo plano fica em
`.trama/mcp-editor.log`.

O servidor só fala com o editor da própria máquina, com o mesmo token do
canal de controle. Não instala pacotes: se faltar uma coleção, o assistente
diz o comando para você rodar.

## Problemas

- **"nenhuma aba do navegador conectou"**: a máquina não abriu o navegador
  (servidor sem tela, por exemplo). Abra o endereço da mensagem à mão e peça
  de novo.
- **A tool não aparece no cliente**: o cliente não acha o `Rscript`. Use o
  caminho completo no registro.
- **Bloco desconhecido**: a coleção dele não está instalada. Veja o passo 2.
