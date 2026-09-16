// inst/www/temas.js — as regras do painel de temas, sem React.
//
// Separado de `settings.js` pelo mesmo motivo de `params.js`: o que pode dar
// errado em silêncio — perder o padrão ao apagar ou renomear, embaralhar a
// ordem dos temas, aceitar um nome que o servidor recusa — se testa com
// `node --test` puro. Tudo aqui é imutável: devolve estado novo, porque o
// painel compara o rascunho com o que o servidor mandou.

export const HEX = /^#[0-9a-fA-F]{6}$/;

// Molde para quando não há tema selecionado de onde duplicar. Na prática o
// servidor sempre manda ao menos os embutidos; isto só evita "Novo" morto.
// Espelha o `escuro` embutido (`.TR_TEMA_CAMPOS`, R/theme.R), para o molde não
// ser um quarto tema que só existe no front.
export const TEMA_BASE = { base: "minimal", tamanho: 13, fonte: "sans", fundo: "#11151c",
  texto: "#c9d1d9", eixos: "#8b949e", grade: "#212936",
  paleta: ["#5b8def", "#f59e0b", "#10b981", "#ef4444", "#a855f7", "#06b6d4", "#f472b6", "#84cc16"],
  continua: "viridis" };

// `marca` anda junto dos temas porque o rascunho do painel é o PEDIDO inteiro
// que vai pro servidor de uma vez. Se cada operação (duplicar, renomear,
// apagar) tivesse que recolocá-la, a esquecida sairia como "religar a marca"
// num salvamento que só renomeou um tema. `?? true` é o mesmo padrão do
// servidor, pro rascunho não inventar um terceiro estado antes da primeira
// mensagem.
export function copiar(estado) {
  return { temas: JSON.parse(JSON.stringify((estado && estado.temas) || {})),
           tema_padrao: estado?.tema_padrao ?? null,
           marca: estado?.marca ?? true };
}

// "novo tema", "novo tema 2", … — o primeiro sem número porque quase sempre
// só se cria um, e "novo tema 1" sugere uma série que não existe.
export function nomeLivre(base, existentes) {
  const usados = new Set(existentes);
  if (!usados.has(base)) return base;
  for (let i = 2; ; i++) if (!usados.has(`${base} ${i}`)) return `${base} ${i}`;
}

// Mesmas recusas do servidor (`.tr_settings`), ditas antes de viajar: a volta
// do servidor viria como banner longe do campo. Renomear para o próprio nome
// não é erro — é "nada mudou".
export function erroNome(nome, existentes, atual) {
  const n = String(nome ?? "").trim();
  if (!n) return "nome vazio";
  if (n === "padrão") return "'padrão' é reservado";
  if (n !== atual && existentes.includes(n)) return "já existe um tema com esse nome";
  return null;
}

export function duplicar(estado, nome) {
  const e = copiar(estado);
  const novo = nomeLivre("novo tema", Object.keys(e.temas));
  e.temas[novo] = JSON.parse(JSON.stringify(e.temas[nome] || TEMA_BASE));
  if (!e.tema_padrao) e.tema_padrao = novo;
  return { estado: e, novo };
}

// Reconstrói o objeto em vez de apagar e reinserir a chave: a ordem das chaves
// é a ordem da lista (e do trama.json), e o tema renomeado iria pro fim.
export function renomear(estado, de, para) {
  const e = copiar(estado);
  const temas = {};
  for (const [k, v] of Object.entries(e.temas)) temas[k === de ? para : k] = v;
  // `...e` e não um objeto novo com as duas chaves: aqui só a lista de temas
  // muda, e o que mais estiver no rascunho (a marca) tem que atravessar.
  return { ...e, temas, tema_padrao: e.tema_padrao === de ? para : e.tema_padrao };
}

// Apagar o padrão promove o primeiro que sobra: o servidor exige que
// `tema_padrao` exista, e deixar o usuário sem padrão por um passo seria uma
// gravação recusada.
export function apagar(estado, nome) {
  const e = copiar(estado);
  delete e.temas[nome];
  const nomes = Object.keys(e.temas);
  if (!nomes.includes(e.tema_padrao)) e.tema_padrao = nomes[0] ?? null;
  return e;
}
