# r-universe

Os pacotes do trama são publicados em binário pelo
[`uaipedro.r-universe.dev`](https://github.com/uaipedro/uaipedro.r-universe.dev)
— o repositório GitHub que registra esse universo (com o app do r-universe
instalado nele) e cujo `packages.json` diz quais pacotes ele constrói, a
partir de qual repositório e subpasta.

**A fonte da verdade é `packages.json` nesse repositório
(`uaipedro/uaipedro.r-universe.dev`), não a cópia abaixo.** O arquivo
`packages.json` deste diretório é só uma cópia de referência, para quem
mexe no instalador sem acesso a esse outro repositório poder ver a lista
sem sair daqui; ele não é lido por nada em tempo de execução (nem pelo
r-universe, nem por `bootstrap.R`, nem pelo `trama.launcher`). Ao adicionar
ou renomear um pacote/coleção, edite o `packages.json` de
`uaipedro/uaipedro.r-universe.dev` primeiro e depois copie a mudança para
cá.
