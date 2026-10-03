# Como contribuir

Obrigado pelo interesse. O trama é um núcleo genérico (`R/`, `inst/www/`) e
coleções com o domínio estatístico (`collections/trama.*`).

1. Leia `AGENTS.md`: as regras que não se quebram valem para pessoas também
   (núcleo sem domínio, um verbo um bloco, glossário de params travado).
2. Antes de mexer, veja onde as coisas estão em `docs/mapa.md` e o
   vocabulário em `CONTEXT.md`.
3. **Rigor numérico.** Bloco ou mudança estatística precisa de teste com
   oráculo publicado (exemplo resolvido ou pacote de referência, tolerância
   declarada) e de referências conferidas na fonte. Mudou resultado ou
   padrão: suba `version` do nó e registre em `docs/revisao-metodologica.md`.
4. Verifique antes de abrir o PR:

   ```sh
   Rscript tools/check.R --base origin/main   # afetados + dependentes
   node --test 'tests/js/*.test.mjs'          # módulos puros do editor
   ```

   Mudança de interface: abra o editor (`tr_app()`) e teste o fluxo à mão.
5. Tocou uma coleção que vai ser publicada: suba `Version` e escreva no
   `NEWS.md` dela; se usa API nova do núcleo, suba o piso `trama (>= x.y.z)`.

Nunca commite dados de cliente. Mensagens de commit em português, no estilo
`tipo(escopo): resumo` (`feat`, `fix`, `docs`, `ci`, `chore`...).
