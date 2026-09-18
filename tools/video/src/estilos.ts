// src/estilos.ts — as folhas de estilo, nesta ordem.
//
// Primeiro as COPIADAS do pacote (`scripts/sincronizar.sh`): o núcleo, a coleção
// `data` e a `models`, na mesma ordem em que o app as carrega — `models.css` diz
// no cabeçalho que conta com vir depois do `trama.css`, na mesma
// especificidade. Por último `video.css`, que desarma transição e animação de
// CSS. A ordem não é estética: invertida, o CSS do app volta a animar em tempo
// de relógio dentro de um render quadro a quadro.
import "./trama-app.css";
import "./trama-data.css";
import "./trama-models.css";
import "./video.css";
