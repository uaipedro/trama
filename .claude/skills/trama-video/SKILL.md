---
name: trama-video
description: Cria vídeos e imagens de divulgação do trama (4:3, vertical e stills) escrevendo só um ROTEIRO de dados em tools/video/src/roteiros/. Use quando pedirem um vídeo, animação, GIF, post, capa ou imagem mostrando o editor do trama, seus cards, modos ou um fluxo.
---

# Vídeos do trama por roteiro

Você **não escreve Remotion**. Você escreve um arquivo de dados, o roteiro,
e o motor (`tools/video/src/motor/`) cuida de tempo, câmera, enquadramento,
som, painel de parâmetros e acabamento. Cada roteiro vira automaticamente:

| composição | formato |
| --- | --- |
| `<id>` | 4:3, 1440×1080, com trilha |
| `<id>-vertical` | 9:16, 1080×1920: o mesmo vídeo recortado no meio |
| `<id>-<nome>` / `<id>-<nome>-vertical` | imagem estática de cada plano `still` |

## Passo a passo

1. Copie `tools/video/src/roteiros/anova-modos.ts` para `roteiros/<nome>.ts`.
2. Troque `id` (PascalCase, sem espaço), os `blocos` e os `planos`.
3. Registre o roteiro em `roteiros/index.ts` (`ROTEIROS = [..., meuRoteiro]`).
4. `cd tools/video && npx tsc --noEmit` precisa passar sem erro.
5. **Olhe o resultado** (obrigatório, veja "Verificar").

## Blocos: o que existe no canvas

```ts
blocos: {
  dados: { spec: SPECS.exemplo, x: 0, y: 170, modo: "mini", duracao: "6ms",
           resultado: { tipo: "tabela", tabela: MILHO } },
}
```

- `spec` vem de um catálogo pronto: `trama/catalogo-modelos.ts` (coleção
  models) ou `trama/catalogo.ts` (coleção data). **Não invente spec.** Se o
  bloco não existe no catálogo, transcreva-o do `collections/<col>/R/` (ver
  README do vídeo).
- `resultado` precisa ser **saída real do R**, nunca número inventado. Use os
  que já estão exportados nos catálogos ou rode o fluxo e cole a saída.
- `x`, `y`: posição no canvas. Deixe ~120px de vão entre cards; o fluxo corre
  da esquerda para a direita. Card completo tem 240px de largura (ou
  `tamanho.largura`), mini tem ~90px.
- `modo`: `"completo"` (padrão), `"preview"` (só resultado), `"params"` (só
  parâmetros) ou `"mini"` (ícone e nome). Card em `mini`/`preview` mostra os
  parâmetros num **painel à esquerda** quando selecionado, igual ao editor.

## Planos: o que acontece, em ordem

| plano | efeito |
| --- | --- |
| `{ faz: "entra", bloco }` | câmera vai ao lugar, o card cai e roda |
| `{ faz: "liga", de, para, porta? }` | aresta desenhada, dado andando por ela |
| `{ faz: "param", bloco, param, valor, resultado? }` | anel no campo, valor digitado, bloco roda de novo |
| `{ faz: "modo", bloco, modo }` | anel no seletor de modo, o card muda de forma |
| `{ faz: "foco", bloco }` ou `{ faz: "foco", blocos: [...] }` | só câmera |
| `{ faz: "geral" }` | câmera no fluxo inteiro |
| `{ faz: "espera" }` | pausa para ler (a câmera respira) |
| `{ faz: "still", nome }` | marca este instante como imagem estática |

Todo plano aceita `legenda`, `destacar` (palavras da legenda na cor de
destaque), `junto: true` (começa junto com o anterior) e `dur` (em quadros,
30 = 1s; quase nunca é preciso).

## Regras que fazem o vídeo ficar bom

- **Um assunto por plano.** Não junte `entra` de dois blocos com `junto`,
  porque a câmera só pode olhar para um.
- **Legenda curta (3 a 8 palavras), no presente, dizendo o porquê**, não o
  óbvio. Ruim: "o card entra". Bom: "diga qual coluna é a resposta".
  No máximo uma legenda a cada 2 ou 3 planos.
- **`destacar` com uma ou duas palavras**, as que carregam a ideia.
- **Mostre um modo trocando por um motivo:** abrir (`params` → `completo`)
  para ver o resultado, ou recolher (`mini`) o que já foi lido antes do `geral`.
- **O `param` deve mudar algo verdadeiro.** Se o resultado muda, passe o
  `resultado` novo (saído do R); se não muda, diga por quê num comentário.
- Termine com `geral` + `still` (vira a capa) + `espera`.
- Cabe entre 25 e 45 s: de 12 a 18 planos.

## O que NÃO fazer

- Não edite `motor/`, `trama/` nem o CSS para "ajustar" um vídeo. Se o motor
  não faz algo, diga isso ao usuário em vez de contornar.
- Não use coordenada de câmera, número de quadro nem JSX num roteiro.
- Não copie as folhas de estilo à mão: depois de mexer no front do trama,
  rode `npm run sincronizar` (traz CSS, ícones e `modos.js`).

## Verificar (obrigatório)

```sh
cd tools/video
npx tsc --noEmit
npx remotion compositions src/index.ts | grep <Id>     # duração em segundos
# quadros-chave, com as guias da zona segura (linhas vermelhas):
for f in 30 150 300 450 600; do
  npx remotion still src/index.ts <Id> out/ver-$f.png --frame $f --props '{"guias":true}' --overwrite
done
npx remotion still src/index.ts <Id>-vertical out/ver-v.png --frame 300 --overwrite
```

Abra as imagens e confira:
- o card em foco e a legenda estão **entre as linhas vermelhas**;
- nenhum card fica cortado no meio de uma linha, nem sobreposto a outro
  (se ficar, afaste `x`/`y`);
- o texto do card é legível no vertical;
- a legenda não cobre o card.

Render final: `npx remotion render src/index.ts <Id> out/<id>.mp4 --codec h264 --crf 18`.
No studio (`npm run studio`), a prop `guias` liga as linhas da zona segura.
