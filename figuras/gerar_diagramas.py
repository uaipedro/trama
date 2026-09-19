"""Diagramas vetoriais do artigo sobre o Trama."""
from pathlib import Path
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

OUT = Path(__file__).resolve().parent
COL = {'ink': '#203040', 'line': '#556678', 'blue': '#dceaf7',
       'green': '#e0efdf', 'amber': '#fff0cf', 'gray': '#eef1f3'}


def canvas():
    fig, ax = plt.subplots(figsize=(10, 2.45))
    fig.patch.set_facecolor('white')
    ax.set_xlim(0, 10); ax.set_ylim(0, 2.45); ax.axis('off')
    return fig, ax


def box(ax, x, y, w, h, title, detail='', color='blue'):
    p = FancyBboxPatch((x, y), w, h, boxstyle='round,pad=0.06,rounding_size=0.10',
                       facecolor=COL[color], edgecolor=COL['line'], linewidth=1)
    ax.add_patch(p)
    ax.text(x+w/2, y+h*0.65, title, ha='center', va='center', fontsize=10,
            weight='bold', color=COL['ink'])
    if detail:
        ax.text(x+w/2, y+h*0.29, detail, ha='center', va='center', fontsize=8.2,
                color=COL['ink'])


def arrow(ax, x1, y1, x2, y2, label=''):
    ax.add_patch(FancyArrowPatch((x1,y1),(x2,y2), arrowstyle='-|>',
                 mutation_scale=10, linewidth=1.2, color=COL['line'],
                 connectionstyle='arc3,rad=0'))
    if label:
        ax.text((x1+x2)/2, (y1+y2)/2+0.14, label, ha='center', fontsize=7.6,
                color=COL['ink'])


def save(fig, name):
    fig.savefig(OUT/name, format='pdf', bbox_inches='tight', pad_inches=0.06)
    plt.close(fig)


fig, ax = canvas()
box(ax, .15, .86, 1.55, .78, 'Navegador', 'React + React Flow')
box(ax, 2.15, .86, 1.55, .78, 'Shiny', 'operação / evento')
box(ax, 4.15, .86, 1.55, .78, 'Motor R', 'documento + plano')
box(ax, 6.15, .86, 1.55, .78, 'Executor', 'sequencial / pool', 'green')
box(ax, 8.15, .86, 1.55, .78, 'Store', 'objeto + handle', 'amber')
ax.add_patch(FancyArrowPatch((1.70,1.25),(2.15,1.25), arrowstyle='<->',
                 mutation_scale=10, linewidth=1.2, color=COL['line']))
for x in (3.70, 5.70, 7.70): arrow(ax, x, 1.25, x+.45, 1.25)
ax.text(4.95, .38, 'O navegador recebe eventos e referências; os objetos R permanecem no store.',
        ha='center', fontsize=8.8, color=COL['ink'])
save(fig, 'arquitetura.pdf')

fig, ax = canvas()
box(ax, .25, .88, 1.45, .78, 'Ler', 'reutilizado', 'gray')
box(ax, 2.20, .88, 1.45, .78, 'Filtrar', 'alterado', 'amber')
box(ax, 4.15, .88, 1.45, .78, 'Calcular', 'reexecutado', 'green')
box(ax, 6.25, 1.50, 1.5, .62, 'Por região', 'reexecutado', 'green')
box(ax, 6.25, .35, 1.5, .62, 'Por produto', 'reexecutado', 'green')
box(ax, 8.20, 1.50, 1.5, .62, 'Ordenar', 'reexecutado', 'green')
arrow(ax, 1.70, 1.27, 2.20, 1.27)
arrow(ax, 3.65, 1.27, 4.15, 1.27)
arrow(ax, 5.60, 1.30, 6.25, 1.81)
arrow(ax, 5.60, 1.24, 6.25, .66)
arrow(ax, 7.75, 1.81, 8.20, 1.81)
ax.text(3.9, .24, 'Edição do parâmetro do filtro: novas chaves apenas nos descendentes.',
        ha='center', fontsize=8.7, color=COL['ink'])
save(fig, 'recomputacao.pdf')

fig, ax = canvas()
box(ax, .18, .85, 1.55, .82, 'Tipo em R', 'preview(valor)')
box(ax, 2.16, .85, 1.55, .82, 'Handle', 'renderer + dados', 'amber')
box(ax, 4.14, .85, 1.55, .82, 'Shiny', 'evento de unidade')
box(ax, 6.12, .85, 1.55, .82, 'Runtime JS', 'registro por id', 'green')
box(ax, 8.10, .85, 1.55, .82, 'Card React', 'componente + vista')
for x in (1.73, 3.71, 5.69, 7.67): arrow(ax, x, 1.26, x+.43, 1.26)
ax.text(4.95, .33, 'A apresentação é selecionada pelo identificador do renderer, sem enviar o objeto R.',
        ha='center', fontsize=8.7, color=COL['ink'])
save(fig, 'renderizacao.pdf')
