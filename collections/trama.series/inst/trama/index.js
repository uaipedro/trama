// O card dos testes da coleção `series` é o do NÚCLEO (`trama/test`): o mesmo
// desenho dos testes da `models`, com a régua do p-valor nos testes que o têm
// (Ljung-Box, Mann-Kendall...) e os três pontinhos de 10, 5 e 1% nos de tabela
// de valores críticos (ADF, KPSS, Zivot-Andrews). O que decide entre os dois é
// o DADO — `p_valor` presente ou não —, e o `sentido` e os `criticos` que o
// `preview` do tipo já manda; nada aqui sabe o nome de um teste.
//
// Reuso explícito, como a `data` faz com a tabela: se a coleção um dia quiser
// um card próprio, troca esta linha e nada mais muda.

import { registerRenderer, getRenderer } from "trama";

registerRenderer("series/test", getRenderer("trama/test"));
