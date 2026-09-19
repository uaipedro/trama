# Princípios do projeto trama

## Finalidade

O `trama` é um sistema para construir, executar e inspecionar fluxos de computação em R. O projeto combina uma representação visual do fluxo, um documento persistente e uma interface programática na própria linguagem.

Seu objetivo é tornar explícitas as etapas, as dependências e os resultados intermediários de uma análise, sem separar a representação visual do código que efetivamente a executa.

O editor, o documento JSON e a DSL em R são representações do mesmo fluxo. Um fluxo criado em qualquer uma delas pode ser lido, modificado e executado pelas demais.

## Princípios

### Resultado visível

Cada bloco apresenta o resultado da própria operação. A inspeção de estados intermediários integra o fluxo e não depende da inclusão temporária de comandos de impressão ou depuração.

### Execução incremental

Uma alteração invalida somente os resultados que dependem dela. O plano de execução identifica as unidades afetadas e conserva os artefatos ainda válidos.

### Documento reproduzível

O fluxo é armazenado como documento JSON, independente da sessão do editor. O formato permite versionamento, comparação, geração programática e execução posterior.

### Operações em R

Os blocos encapsulam funções R e permanecem acessíveis fora da interface visual. O sistema não substitui a linguagem nem introduz uma linguagem paralela para as operações analíticas.

### Extensão por coleções

Conhecimento de domínio é distribuído em coleções instaláveis. Cada coleção registra seus tipos, blocos, parâmetros, adaptadores e formas de apresentação.

O núcleo fornece os contratos comuns e não incorpora regras específicas de manipulação de dados, visualização, modelagem ou qualquer outro domínio analítico.

### Tipos e conexões explícitos

As portas de entrada e saída declaram os tipos aceitos, a obrigatoriedade e a multiplicidade. A compatibilidade entre blocos é verificada antes da execução.

Adaptadores podem converter tipos nas arestas quando a transformação é inequívoca. Essa conversão preserva a composição entre coleções sem exigir blocos auxiliares no documento.

### Erros como parte do protocolo

Erros possuem classes e informações suficientes para identificação da origem e apresentação de uma mensagem acionável. Falhas de uma unidade não devem ser reduzidas a texto sem estrutura.

### Independência entre interface e execução

A interface visual não transporta objetos R ativos entre processos. Workers gravam artefatos e visualizações no armazenamento endereçado por conteúdo e comunicam seus identificadores.

Essa separação permite utilizar diferentes executores sem alterar o documento ou as funções dos blocos.

## Contratos do núcleo

O núcleo é organizado em cinco contratos:

1. **Tipo:** define a identidade dos dados, sua validação e as conversões admitidas.
2. **Catálogo:** reúne tipos, blocos, parâmetros, categorias, adaptadores e renderizadores disponíveis.
3. **Documento:** registra blocos, conexões, parâmetros e metadados do fluxo.
4. **Execução:** produz o plano, agenda unidades, registra progresso e materializa resultados.
5. **Renderização:** converte artefatos em representações adequadas à interface.

Uma funcionalidade pertence ao núcleo quando é necessária à estabilidade desses contratos. Funcionalidades específicas devem ser implementadas em coleções.

## Requisitos para coleções

Uma coleção deve poder ser compreendida e utilizada por meio de seu catálogo. Tipos, portas, parâmetros e descrições fazem parte da interface pública da coleção.

Cada bloco deve declarar sua finalidade, suas entradas, suas saídas e seus parâmetros. Exemplos de preenchimento e documentação complementar devem acompanhar os campos que não forem autoexplicativos.

As dependências entre coleções devem ser explícitas. Uma coleção pode reutilizar tipos e recursos de outra, desde que preserve a ordem de carga e não duplique contratos existentes.

As extensões devem exigir apenas R e, quando houver interface própria, arquivos JavaScript ou CSS diretamente carregáveis. Ferramentas adicionais de compilação podem ser usadas, mas não integram o contrato mínimo.

## Formato e automação

O documento e o catálogo devem permanecer legíveis por máquinas. Identificadores estáveis, estruturas canônicas e ausência representada de forma uniforme reduzem ambiguidades na geração e na transformação de fluxos.

As posições visuais são opcionais porque podem ser calculadas automaticamente. O conteúdo lógico do fluxo não depende de sua disposição no editor.

Essas propriedades permitem produzir e revisar fluxos por código, ferramentas externas e sistemas de assistência, sem acesso ao estado interno da interface.

## Critérios de qualidade

O desenvolvimento do projeto observa os seguintes critérios:

1. Um documento válido produz o mesmo plano lógico, independentemente de ter sido criado no editor ou em R.
2. A recomputação preserva resultados cujo conteúdo e dependências não foram alterados.
3. O núcleo pode ser testado sem carregar coleções de domínio.
4. Cada coleção responde por seus testes, erros classificados e documentação.
5. A execução em outro processo não altera a semântica do fluxo.
6. O formato persistido evolui com compatibilidade ou migração explícita.

## Escopo

O `trama` abrange a construção visual e programática de fluxos, sua execução local, a inspeção de resultados intermediários e a extensão por coleções.

Regiões de fluxo acrescentam execução passo a passo para operações cujo resultado depende de uma sequência, como algoritmos incrementais. O histórico produzido retorna ao fluxo como dado comum.

O projeto pode oferecer progresso, cancelamento e registro de execução quando esses recursos decorrem dos contratos do motor.

## Limites de escopo

O `trama` não é um orquestrador de infraestrutura, um serviço de agendamento ou um substituto para sistemas como `targets` e Airflow.

O projeto não pretende converter toda expressão R em elementos visuais. Expressões continuam sendo código quando essa for a representação mais adequada.

Também não se busca reproduzir integralmente ferramentas comerciais de automação. Funcionalidades são incorporadas quando fortalecem os contratos do projeto ou atendem a fluxos analíticos em R.

## Evolução do projeto

As decisões de evolução devem preservar a equivalência entre editor, documento e código; a separação entre núcleo e domínio; e a possibilidade de inspecionar cada etapa do fluxo.

Novos recursos não devem comprometer a reprodutibilidade do documento, a execução incremental ou a interoperabilidade das coleções.

O acompanhamento de execuções constitui uma direção possível, pois o motor já registra eventos e duração por unidade. Sua incorporação depende de uma interface compatível com os contratos existentes.

## Licença e abertura

O `trama` é distribuído sob a licença MIT. A escolha permite uso, modificação e redistribuição do código nos termos da licença.

As dependências da interface utilizam licenças compatíveis com a distribuição do projeto. No ambiente R, `htmltools` utiliza GPL e integra a cadeia de dependências do Shiny.

As licenças das dependências devem ser verificadas quando novos componentes forem incorporados.
