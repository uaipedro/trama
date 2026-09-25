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
  inputColor?: string;
  outputColor?: string;
  /** Papel no fluxo: dá a cor do cabeçalho pelo token do tema. */
  role?: string;
  inputs?: NodePort[];
  outputs?: NodePort[];
  params?: NodeParam[];
}

export interface NodePort {
  name: string;
  color: string;
  required: boolean;
  multiple: boolean;
}

export interface NodeParam {
  name: string;
  kind: string;
  label: string;
  default?: unknown;
  example?: string;
  choices?: string[];
}

const nodeVisuals: Record<string, NodeVisual> = visuals;

export function getNodeVisual(id: string): NodeVisual {
  const visual = nodeVisuals[id];
  if (!visual) throw new Error(`Bloco sem dados visuais no catálogo: ${id}`);
  return visual;
}
