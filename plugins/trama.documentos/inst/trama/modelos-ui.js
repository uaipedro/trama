// Aba Modelos do painel Documentos: aqui entra a galeria de modelos da
// fase 3 (#24) — criar, editar, duplicar, importar, exportar.
//
// Contrato com o painel (`documentos.js`):
//   - o painel desenha `h(AbaModelos, { s, enviar, despachar })` quando a aba
//     "Modelos" está aberta; `s` é o estado do painel (`painel.js`), com
//     `s.modelos` = [{ nome, titulo, formatos, origem }] (vem em toda
//     mensagem `lista` e em `modelos`);
//   - `enviar("modelos_<acao>", {...})` chega ao R em `.td_painel_modelos()`
//     (`R/painel-modelos.R`), que responde pelo mesmo canal; mensagem nova
//     do R ganha um `case` no redutor (`painel.js`).
// A fase 2 só lista (somente leitura).

import { h } from "trama";

export function AbaModelos({ s, enviar }) {
  const modelos = s.modelos || [];
  return h("div", { className: "tr-doc-aba" }, [
    h("p", { key: "t", className: "tr-doc-dim tr-doc-nota" },
      "Modelos são extensões Quarto: formatos, cores, fontes e o esqueleto do documento."),
    modelos.length
      ? h("ul", { key: "l", className: "tr-doc-lista" }, modelos.map((m) =>
          h("li", { key: m.nome, className: "tr-doc-item" }, [
            h("div", { key: "n", className: "tr-doc-linha" }, [
              h("span", { key: "t", className: "tr-doc-nome tr-doc-cresce" }, m.titulo || m.nome),
              h("span", { key: "o", className: "tr-doc-chip" }, m.origem === "usuario" ? "seu" : "embutido"),
            ]),
            h("div", { key: "f", className: "tr-doc-dim" }, m.formatos),
          ])))
      : h("p", { key: "v", className: "tr-doc-dim" }, "Nenhum modelo encontrado."),
    h("button", { key: "r", type: "button", className: "tr-doc-link", onClick: () => enviar("modelos_listar") },
      "Recarregar"),
  ]);
}
