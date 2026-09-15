// Widgets da coleção `data`. Registrados no runtime do trama, carregados
// depois dele e antes do editor. O núcleo não sabe o que é uma coluna nem uma
// expressão de dplyr — só sabe procurar um widget registrado sob um `kind`.

import { h, registerWidget, registerRenderer, getRenderer } from "trama";

// `expr`: expressão de R. Textarea em vez de input porque condição e resumo
// crescem, e commit no blur (não a cada tecla) evita mandar op por caractere.
//
// O placeholder era `spec.label`, que repete em cinza o rótulo escrito logo
// acima do campo e não ensina nada. `spec.example` vem do catálogo (o `...` de
// `tr_param()` viaja inteiro até o front), então o campo vazio passa a mostrar
// um valor válido de verdade — que é a única forma de ensinar o formato de um
// campo de texto livre sem abrir a ajuda.
registerWidget("expr", (spec, value, onChange) =>
  h("textarea", {
    className: "nodrag tr-expr", rows: 2, spellCheck: false,
    defaultValue: value ?? spec.default,
    placeholder: spec.example ?? spec.label,
    title: spec.example ? `ex.: ${spec.example}` : undefined,
    onBlur: (e) => { if (e.target.value !== (value ?? spec.default)) onChange(e.target.value); },
  }));

// `cols`: lista de colunas separada por vírgula.
registerWidget("cols", (spec, value, onChange) =>
  h("input", {
    type: "text", className: "nodrag", spellCheck: false,
    defaultValue: value ?? spec.default,
    placeholder: spec.example ?? "col1, col2",
    title: spec.example ? `ex.: ${spec.example}` : undefined,
    onBlur: (e) => { if (e.target.value !== (value ?? spec.default)) onChange(e.target.value); },
  }));

// `path`: caminho de arquivo.
registerWidget("path", (spec, value, onChange) =>
  h("input", {
    type: "text", className: "nodrag", spellCheck: false,
    defaultValue: value ?? spec.default,
    placeholder: spec.example ?? "dados.csv",
    title: spec.example ? `ex.: ${spec.example}` : undefined,
    onBlur: (e) => { if (e.target.value !== (value ?? spec.default)) onChange(e.target.value); },
  }));

// O renderer é resolvido pelo id que o artefato de preview declara
// (`data/table`, vindo do `preview` do tipo em R). Aqui ele é apontado para a
// tabela embutida do núcleo — reuso explícito, e não herança implícita: se
// amanhã a coleção quiser uma tabela com ordenação e filtro próprios, troca
// esta linha por um componente seu e nada mais muda.
registerRenderer("data/table", getRenderer("trama/table"));
