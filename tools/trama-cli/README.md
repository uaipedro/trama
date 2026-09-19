# trama-cli

Instala e roda o [trama](https://github.com/uaipedro/trama) sem precisar já
ter R instalado.

## Instalação

```bash
npm install -g trama-cli
```

## Uso

```bash
trama install            # baixa o R portátil e instala o núcleo do trama
trama create meu-fluxo   # cria um projeto, escolhe coleções, abre o editor
trama add trama.ml       # instala uma coleção adicional no projeto atual
trama open               # abre o editor no projeto da pasta atual
```

## Por que um CLI em vez de um app com interface gráfica?

O launcher original (`tools/launcher/`, Go + Wails) esbarrou num problema
difícil de contornar sem custo: Windows Defender/SmartScreen desconfia de
qualquer executável novo sem assinatura de código. Um pacote npm herda a
cadeia de confiança do próprio Node.js — sem executável nosso pra assinar.
