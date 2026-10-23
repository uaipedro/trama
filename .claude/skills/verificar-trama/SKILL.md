---
name: verificar-trama
description: Use ao terminar uma mudança no trama e antes de dizer que está pronto, commitar ou pedir revisão — decide e roda só os testes afetados (coleção mexida + dependentes, JS do editor, site). Também quando pedirem "rodar os testes", "checar", "verificar".
---

# Verificar o trama

Nunca rode a suíte inteira por reflexo. Use o roteador:

```sh
Rscript tools/check.R --plano      # o que seria rodado (sempre olhe antes)
Rscript tools/check.R --rapido     # durante o trabalho: só test-<arquivo>.R pareado
Rscript tools/check.R              # ao fim: pacotes afetados + dependentes
Rscript tools/check.R --base main  # antes de commit/PR: tudo desde main
Rscript tools/check.R trama.models # alvo explícito: suíte inteira dele (+ dependentes sem --rapido)
```

Saída 0 = ok; 1 = falha, com o nome do pacote no fim.

Os pacotes rodam em paralelo (um Rscript cada, até núcleos − 1; `--jobs N` muda, `--jobs 1` serializa). A saída de cada um sai inteira quando ele termina, com a linha `mais lentos:` mostrando os 5 arquivos que mais custaram.

## Grafo (lido dos DESCRIPTION)

`trama` ← `trama.data` ← `trama.view` ← {`models`, `series`, `sampling`, `spatial`} ; `models` ← {`experiments`, `ml`, `multi`}. (`sql` depende de `data`.)
Mexer em `trama.data` roda quase tudo; em `trama.series`, só ela.

## Gotchas

- Mudar `R/foo.R` do núcleo roda só as coleções que usam (símbolo ou string, direto ou via quem chama no núcleo) algo de `foo.R`, mais os dependentes delas. App, sessão, transporte, projeto, CLI não chegam a coleção nenhuma. `--amplo` volta ao antigo (todas). `inst/`, DESCRIPTION e NAMESPACE do núcleo ainda rodam todas. Em iteração use `--rapido`; rode o completo uma vez no fim.
- Subir só `Version:` no DESCRIPTION não dispara testes (é ignorado de propósito).
- Templates (`collections/*/inst/templates`) também são testados pelo núcleo em `test-template.R`.
- Falha deixa `tests/testthat/_problems/` e `testthat-problems.rds`: lixo, não commitar.
- Testes de coleção dependem de `pkgload::load_all` na ordem de carga; não rode `test_dir` direto sem carregar as dependências — o roteador já faz isso.
- `--rapido` não pega quebra em dependentes. Não declare pronto só com ele.
