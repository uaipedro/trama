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
