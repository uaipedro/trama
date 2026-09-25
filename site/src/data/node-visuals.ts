import visuals from "./node-visuals.json";

export interface NodeVisual {
  accent: string;
  icon?: string;
  hasInput: boolean;
  hasOutput: boolean;
  label?: string;
  category?: string;
  categoryLabel?: string;
  categoryOrder?: number;
}

const nodeVisuals: Record<string, NodeVisual> = visuals;

export function getNodeVisual(id: string): NodeVisual {
  const visual = nodeVisuals[id];
  if (!visual) throw new Error(`Bloco sem dados visuais no catálogo: ${id}`);
  return visual;
}
