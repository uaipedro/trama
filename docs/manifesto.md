# trama — o que é, e para quê

O pacote trama é uma ferramenta em R pra construir fluxos lógicos com
visualização interativa. Tem coleções prontas pra fluxos comuns e é
extensível pra blocos novos, escritos em R base ou pacotes terceiros.

## O problema

Um fluxo de trabalho analítico é, na cabeça de quem o desenha, um diagrama:
carrega isso, filtra aquilo, cruza com aquele outro, resume, compara. No
código, ele vira uma sequência linear de chamadas onde o diagrama some — e
com ele some a possibilidade de **ver o que aconteceu em cada etapa** sem
inserir um `print()` no meio e rodar tudo de novo.

Ferramentas visuais de fluxo existem (n8n, Node-RED, KNIME, Orange), mas
quase todas moram fora da linguagem: você monta o diagrama numa ilha e o
código de verdade vive em outro lugar. E as que vivem dentro do R são
específicas de um domínio.

## O objetivo

**Uma forma fácil de enxergar, editar, produzir e analisar fluxos lógicos de
funções e transformações — e, no futuro, de monitorá-los.** Eficiente,
interativa, e fácil de estender para domínios novos.

Concretamente, isso quer dizer:

- **Enxergar.** Todo nó mostra o próprio resultado no card, com o renderizador
  que faz sentido para aquele tipo de dado. Não é um diagrama do que o código
  faria — é o dado que ele produziu.
- **Editar.** Mudar um parâmetro recomputa só o que depende dele. O ciclo de
  iteração é a edição, não a reexecução.
- **Produzir.** O fluxo é um documento JSON, versionável, reproduzível fora da
  interface, e chamável de script.
- **Analisar.** Cada nó é uma função R comum, inspecionável no console. A
  ferramenta nunca é uma gaiola.
- **Monitorar** (adiante). O motor já emite eventos por unidade e mede duração
  real; falta a superfície que transforma isso em acompanhamento.

Uma **coleção ampla, mas não exaustiva**. O núcleo não sabe nada de nenhum
domínio; quem sabe são as coleções. A ambição não é cobrir tudo — é que
acrescentar o que falta seja barato.

## O horizonte

Rumo, não foco: **ser uma referência de como se constrói e se lê fluxo lógico
em R**, no mesmo sentido em que o ggplot2 mudou a forma de trabalhar com
gráficos. Montar uma ideia como fluxograma e ter isso correspondido em código
executável é um vão real, e vale mirar nele mesmo sabendo que não se chega lá
por decreto.

Isso é direção para desempatar decisões de projeto, não prazo nem promessa.

## O objetivo interno: fácil de usar, editar e adaptar

Tão importante quanto o que a ferramenta faz é quão barato é mexer nela. Três
compromissos, e cada um já tem consequência concreta no código:

**Modular.** Cinco contratos no núcleo — tipo, catálogo, documento, execução,
renderização. Tudo que não é um deles é coleção. A regra que decide: é núcleo
o que, se não estiver no contrato desde o início, não dá para acrescentar
depois sem quebrar todos os documentos salvos.

**Fácil de entender.** O comentário explica o bug real, não o código. Boa
parte do que está escrito aqui documenta uma armadilha que já mordeu alguém —
é o tipo de conhecimento que some numa reescrita e que nenhum teste captura.

**AI-friendly, de propósito e por design.** Não como slogan: como conjunto de
decisões que se pagam.

- O documento é **JSON canônico com schema publicado** (`inst/schema/`). Um
  modelo consegue escrever um fluxo inteiro sem tocar na interface.
- **Posições são opcionais** — o auto-layout preenche. É exatamente a parte
  que não se escreve bem em texto.
- **Ids qualificados e estáveis**, nomes em vez de posições, ausência
  representada de um jeito só. Formato previsível é formato que se gera.
- **O catálogo é legível por máquina**: tipos, portas, params e adaptadores
  saem num JSON só, então dá para perguntar "o que existe e o que conecta com
  o quê" sem ler código.
- **Erros são classificados** (`tr_error_*`) e viajam com mensagem acionável,
  não como texto solto do interpretador.
- **Uma coleção é R comum mais um `.js` solto**, sem bundler e sem toolchain.
  É o que torna realista alguém pedir a um modelo "me faz um nó que faz X" e
  colar o resultado.

O objetivo dessa parte é que o usuário gere os próprios componentes de fluxo
com ajuda de IA e o sistema aceite, sem que ele precise entender hash de
conteúdo, plano de execução ou o protocolo de ops. **A infraestrutura é para
ser boa e invisível.**

## Não-objetivos

Não é orquestrador de produção, nem agendador, nem substituto de `targets` ou
Airflow. Não é uma linguagem visual completa — expressão continua sendo
código. E não persegue paridade de recursos com ferramentas de fluxo
comerciais.

## Licença e abertura

MIT, com a intenção de abrir o projeto. Isso é restrição de projeto, não
detalhe administrativo: toda dependência precisa ser compatível.

O front é MIT inteiro (React, ReactDOM, xyflow, dagre). No R, `codetools` era
GPL e foi eliminado — o percurso de fecho de função é implementação própria
desde então. Resta `htmltools` (GPL >= 2), que é inevitável: o próprio Shiny
depende dela, e não há como servir uma UI Shiny sem passar por ali. Vale
registrar a ressalva em vez de fingir que não existe.
