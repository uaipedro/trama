// Motivo de maior peso compartilhado pelo popover e pelos nós fantasma.
export function motivoPrincipal(s, { deLabel, historico, de, origem }) {
  const [chave] = Object.entries(s.motivos || {}).filter(([, v]) => v > 0)
    .sort((a, b) => b[1] - a[1])[0] || [];
  switch (chave) {
    case "transicao": return origem ? `aparece antes de ${deLabel} nos exemplos`
      : `aparece depois de ${deLabel} nos exemplos`;
    case "historico": return `usado por você ${historico?.[origem ? `${s.id}>${de}` : `${de}>${s.id}`] || 1}×`;
    case "relacionado": return origem ? `cita ${deLabel} na ajuda` : `citado na ajuda de ${deLabel}`;
    case "etapa": return origem ? "etapa anterior" : "próxima etapa";
    case "contexto": return "combina com o fluxo";
    default: return "";
  }
}
