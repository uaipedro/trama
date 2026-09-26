# Pendências de referências

Referências que não puderam ser conferidas na fonte e por isso ficaram fora dos blocos.

## trama.sampling

Nenhuma pendência: todas as referências usadas foram conferidas em 2026-09-25 (DOIs no Crossref; Cochran 1977 e Kish 1965 no catálogo da Wiley; Bolfarine & Bussab 2005 no catálogo da Blucher, ISBN 9788521203674). Kish (1992), "Weighting for unequal P_i" (Journal of Official Statistics), fonte do deff de ponderação, não foi usada: o Crossref não a devolve e não houve conferência na fonte; o bloco cita Kish (1965).

## trama.models

Nenhuma pendência: todas as referências usadas foram conferidas em 2026-09-25 (DOIs no Crossref, com autor, ano, título e periódico conferidos; Montgomery 2017, 9. ed., Wiley, ISBN 9781119113478; Banzatto & Kronka 2006, 4. ed., Funep, ISBN 85-87632-71-X; Pimentel-Gomes 2009, 15. ed., FEALQ; Siegel & Castellan 2006, 2. ed., Artmed, ISBN 9788536307299; Dobson & Barnett 2008, 3. ed., Chapman & Hall/CRC; Rencher & Schaalje 2008, 2. ed., Wiley; Searle 1971, Wiley; Fox & Weisberg 2019, 3. ed., Sage; Levene 1960 em Olkin et al., Stanford University Press, p. 278-292 — no Open Library, catálogos de editora e citação do pacote). Os livros-texto só entraram nos blocos cujo método eles cobrem com certeza; Pimentel-Gomes e Banzatto & Kronka ficaram fora do Waller-Duncan (não conferido que tratam o teste), e Montgomery fora dos blocos de Shapiro-Wilk.

## trama.ml

- Friedman (2001), "Greedy function approximation: A gradient boosting machine", *The Annals of Statistics*, 29(5), doi:10.1214/aos/1013203451 — DOI conferido no Crossref, mas as páginas não vêm no registro nem no OpenAlex e a página da editora não abriu; ficou fora do `ml/xgboost` (que cita Chen & Guestrin 2016 e ESL).

## trama.multi

Nenhuma pendência: todas as referências usadas foram conferidas em 2026-09-25 (DOIs no Crossref, com autor, ano, título, periódico, volume(número) e páginas; Efron & Stein 1981, p. 586-596, no Project Euclid; Johnson & Wichern 2007, 6. ed., Pearson Prentice Hall, ISBN 9780131877153; Mingoti 2005, Editora UFMG, ISBN 9788570414519; Ferreira 2018, 3. ed., Editora UFLA, ISBN 9788581270630 — no catálogo da livraria da Editora UFLA). Hair et al. (*Análise multivariada de dados*) ficou fora: a edição de referência varia entre as bibliografias e não houve conferência de uma edição específica. O resumo de Tukey (1958) sobre o jackknife não foi usado: o DOI resolve para o bloco de resumos do volume, não para um artigo; os blocos citam Quenouille (1956) e Efron & Stein (1981). Kott, P. S. (2001), The delete-a-group jackknife, *Journal of Official Statistics* 17(4), 521-526 (sem DOI): conferido no PDF do arquivo da JOS na SCB (https://www.scb.se/contentassets/ca21efb41fee47d293bbee5bf7be7fb3/the-delete-a-group-jackknife.pdf) e na página da RTI; fica nos blocos de jackknife com a URL.

## trama.series

Conferidas em 2026-09-25: todos os DOIs no Crossref (autor, ano, título, periódico, volume, número e páginas); Morettin & Toloi 2006, 2. ed., Blucher, ISBN 9788521203896 (catálogo da editora); Box, Jenkins, Reinsel & Ljung 2015, 5. ed., Wiley, ISBN 9781118675021; Hyndman & Athanasopoulos 2021, 3. ed., OTexts (https://otexts.com/fpp3/, citação da própria página); Cleveland et al. 1990, Journal of Official Statistics 6(1), 3-73 (sem DOI; conferido na listagem da revista e na documentação de `stats::stl`); Siegel & Castellan 2006 (já conferido em trama.models). Holt entra pela reedição de 2004 no International Journal of Forecasting (o memorando original de 1957 não tem registro conferível).

Fora dos blocos por não terem sido conferidas na fonte:

- Paiva, D. A. (2020), *Estudo de testes para tendência em séries temporais*, dissertação (UFLA) — guiou a escolha e a formulação dos testes (ver `docs/fontes.md`), mas não foi localizada no repositório institucional nesta rodada.
- Morais, T. S. T. (2012), *Estudo temporal do nível médio do mar em diferentes oceanos*, dissertação (UFLA) — o campo `fonte` do `series/fisher` a cita; o bloco cita Fisher (1929).
- Kendall, M. G., *Rank Correlation Methods* (Griffin), fonte usual da variância do Mann-Kendall — edição e ano não conferidos; o bloco cita Mann (1945).
