# Este launcher foi substituído

O launcher GUI (Go + Wails) deste diretório foi substituído pelo
[`trama-cli`](../trama-cli/), um CLI via npm. Motivo: executáveis nativos
não assinados esbarram no Windows Defender/SmartScreen sem contorno
gratuito e confiável; um pacote npm herda a cadeia de confiança do próprio
Node.js.

Ver `docs/plans/2026-09-19-trama-cli-design.md` para o raciocínio completo.
Este código fica no repositório como referência (a lógica de download/
checksum do R portátil e o fix do Rtools foram portados quase
verbatim para o trama-cli) mas não recebe mais releases.

**Atualização (2026-09-23):** para quem não tem Node/npm nem R, o caminho
"clique e funciona" agora é o instalador nativo em `tools/installer/`
(`Trama-Setup.exe` no Windows, `install.sh` no Linux) — não uma revivência
deste launcher em Go. Design: `docs/plans/2026-09-23-instalador-windows-design.md`.
Motivo do desenho novo (por que não reaproveitar este código, ver também
`docs/future-ideas/paleta-semantica-e-instalador.md`, item (b)): o exe
gerado não grava nenhum executável nosso em disco — ele só baixa e roda o
instalador oficial do R e chama `Rscript` —, o que evita a causa provável
da quarentena em massa deste launcher (descompactar um R portátil).
