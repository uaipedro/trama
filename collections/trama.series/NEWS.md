# trama.series 0.4.1

* Parâmetros que só valem para certa escolha de outro parâmetro agora declaram `trama::tr_when()` e somem do card quando não se aplicam. Exige `trama (>= 0.2.0)`.

# trama.series 0.4.0

Versão sobe de 0.3.0 para 0.4.0: a coesão das coleções muda resultados
(ids em inglês com migração, testes em `data/test`, álgebra de séries com
`series/combine`/`series/detrend`, regressor como componente próprio).

## 0.3.0

## Rigor metodológico (fase 3, revisão)

- `series/cox_stuart` e `series/pettitt`: nova `correcao =
  "bootstrap_blocos"` (padrão `nenhuma`, sem mudança), o mesmo bootstrap de
  blocos móveis do `series/mann_kendall` (blocos de round(√n), 1999
  reamostras, semente do nó) sobre a soma dos sinais dos pares (Cox-Stuart,
  mesmo pareamento) e sobre K (Pettitt; ponto de mudança inalterado).
  Oráculo da mecânica: bootstrap à mão com a mesma semente, igual
  exatamente. Medido sem tendência/ruptura, AR(1), 1000 réplicas por caso,
  rejeição a 5% (nenhuma → bootstrap), n = 60 / 120: Cox-Stuart phi 0,3:
  11,1 → 4,4% / 8,1 → 3,8%; phi 0,6: 22,8 → 5,5% / 24,7 → 6,6%. Pettitt phi
  0,3: 16,5 → 3,5% / 18,1 → 4,6%; phi 0,6: 45,5 → 8,7% / 54,8 → 7,8%. Poder:
  Cox-Stuart (tendência 1,8) 41 / 78% (phi 0,3), 24 / 48% (0,6); Pettitt
  (degrau 1,5) 89 / 100% (0,3), 59 / 86% (0,6). Com phi = 0,6 o Pettitt fica
  acima do nominal — documentado.

- `series/mann_kendall` (`pre_branqueamento`): regra de Yue et al. (2002)
  conferida no texto (Hydrol. Process. 16:1807-1829, p. 1822-1823): o AR(1)
  é removido sempre, sem condição de significância — o teste do r1 (eq. B.1,
  10%) só seleciona estações na aplicação (p. 1825). Igual ao
  `modifiedmk::tfpwmk` e a esta implementação; sem mudança de resultado.
  Diferença documentada: o r1 da eq. 14a do artigo é n/(n − 1) vezes o do
  `stats::acf`, que o `modifiedmk` e este bloco usam.

- `series/mann_kendall`: nova `correcao = "bootstrap_blocos"` — o mesmo S,
  p-valor por bootstrap de blocos móveis (Kundzewicz & Robson 2004,
  doi:10.1623/hysj.49.1.7.53993; Künsch 1989): blocos de round(√n), 1999
  reamostras, p = (1 + #{|S*| ≥ |S|})/(B + 1), semente do nó. Oráculo da
  mecânica: o mesmo bootstrap escrito à mão com a mesma semente, igual
  exatamente. Medido sem tendência, AR(1), 1000 réplicas por caso (erro de
  Monte Carlo 0,7-0,9 ponto), rejeição a 5% (nenhuma → bootstrap): phi 0,3,
  n = 60: 16,0% → 7,2%; n = 120: 13,6% → 5,5%; phi 0,6, n = 60: 31,0% →
  9,0%; n = 120: 31,2% → 7,7%. Poder (tendência de 1,8 unidades ao longo da
  série): 77% / 96% (phi 0,3, n = 60 / 120) e 43% / 66% (phi 0,6). Nível
  nominal só com autocorrelação moderada e n ≈ 120; com phi = 0,6 reduz o
  excesso sem zerá-lo — é opção, não padrão, e a ajuda diz isso. A regra de
  bloco do `modifiedmk::bbsmk` (autocorrelações significativas seguidas + 1,
  blocos de 3-4) rejeitou 17-19% com phi = 0,6 (300 réplicas) e não foi
  adotada.

- `series/intervencao`: novo parâmetro `resposta` — `imediata` (padrão, sem
  mudança) ou `gradual`, a função de transferência ω/(1 − δB) de Box & Tiao
  (1975) para degrau e pulso. δ pela verossimilhança perfilada, erro-padrão
  pela hessiana da verossimilhança completa; no degrau, linha
  `efeito_longo_prazo` = ω/(1 − δ) (método delta). Oráculo: `TSA::arimax(
  transfer = list(c(1, 0)))` 1.3.1 (Cryer & Chan 2008, cap. 11), airmiles em
  log, ARIMA(0,1,1)(0,1,1)₁₂, 2001-09: degrau ω = −0,35892, δ = −0,29605;
  pulso ω = −0,34591, δ = 0,69468; coeficientes e erros-padrão a 1e-3
  (diferença medida < 3e-5). `TSA` entra em Suggests só pelos dados (o teste
  não carrega o namespace, que sobrescreve `fitted.Arima`).

- `series/zivot_andrews` (versão 4): o padrão passa a ser `selecao = "fixa"`
  com `defasagens = -1` = regra l4 de Schwert (1989; eq. 13a do NBER
  Technical Working Paper 73, conferida no texto), trunc(4·(n/100)^(1/4));
  `t_sig` continua opção, com teto l12 = trunc(12·(n/100)^(1/4)) quando
  `defasagens = -1`, e o aviso abaixo de 100 observações. `0` é zero
  defasagens nas duas escolhas (antes, com `t_sig`, `0` era o teto
  automático). Medido sob passeio aleatório, modelo de nível, 1000 réplicas
  por n (erro de Monte Carlo ≈ 1 ponto), rejeição a 5%: t_sig 29,3% / 19,8% /
  14,6% (n = 30 / 50 / 100); fixa l12 12,0% / 5,3% / 5,6%; fixa l4 8,0% /
  6,9% / 6,2%. A l4 é o padrão por errar menos na série curta; abaixo de 40
  observações a `nota` segue avisando.

# trama.series 0.2.0

## Rigor metodológico (fase 3)

- `series/mann_kendall`: novo parâmetro `correcao` — `nenhuma` (padrão, sem
  mudança de resultado), `hamed_rao` (variância × n/n* pelas autocorrelações
  significativas dos postos da série sem a tendência de Sen; Hamed & Rao 1998)
  ou `pre_branqueamento` (livre de tendência; Yue et al. 2002). Oráculo:
  `modifiedmk::mmkh` e `modifiedmk::tfpwmk` (1.6) iguais a 1e-8 (diferença
  medida < 1e-14) em seis séries, com empates; `modifiedmk` entra em Suggests.
  n/n* ≤ 0 é recusado (`tr_series_error_fit`) em vez de NaN. Empate passa a ser
  igualdade exata (`match`), não o texto de `table()`; sem efeito nas séries
  dos testes. Medido sem tendência, AR(1) phi = 0,6, n = 60, 2000 réplicas:
  rejeição a 5% de 30,7% (nenhuma), 21,1% (Hamed-Rao) e 39,4%
  (pré-branqueamento) — nenhuma correção devolve o nível, e a ajuda diz isso.

- `series/phillips_perron` (versão 3): com `constante`, o p-valor passa a ser
  o da superfície de resposta de MacKinnon (1996), `urca::punitroot`, no lugar
  da tabela τ_μ de Fuller interpolada e presa em [0,01; 0,99] (e na linha
  n = 25 para série menor). Conferido: devolve 1/5/10% nos críticos
  assintóticos de MacKinnon (2010) e fica a menos de 0,002 das colunas de 1% e
  5% de Fuller (n = 25 e 100). Série com menos de 25 observações ganha aviso
  na `nota` nos dois determinísticos: medido sob passeio aleatório (4000
  réplicas), com n = 12 o teste rejeita a 5% em 7,0% (constante) e 10,2%
  (tendência); com n = 25, 5,9% e 4,9%.

- `series/regression` com `erro = "arma"`: série que o modelo reproduz sem
  resíduo (ou GLS singular) é recusada com `tr_series_error_singular_fit` e
  mensagem própria, em vez de "não convergiu"; AR do erro com raiz inversa
  ≥ 0,9 emite `tr_series_warn_near_unit_root` e o aviso vai para a `nota` dos
  três F (medido na fase 1: com phi = 0,9 o F de tendência por GLS ainda
  rejeita 17% sob H0).

- `series/zivot_andrews` (versão 3): `t_sig` passa a seguir a regra exata de
  Zivot & Andrews (1992, seção 4) — k do geral para o específico em CADA
  corte, t de cada corte com o seu k, mínimo nos cortes (a versão 2 escolhia o
  corte primeiro e o k só nele). Oráculo: força bruta com a regressão do
  `urca::ur.za` (1e-10) em quatro séries, uma em que as regras divergem
  (-4,335 → -4,738). `fixa` com `defasagens = 0` passa a ser zero defasagens
  (antes caía em trunc((n − 1)^(1/3))). Nelson-Plosser, modelo A, k = 8:
  -5,576386 (real) e -5,823666 (nominal), 1929, recalculados no `urca::nporg`
  (a tabela do artigo não foi conferida no PDF). Medido sob passeio aleatório
  (nível, 300 réplicas): `t_sig` rejeita a 5% em 31% (n = 30), 27% (50) e 13%
  (100), contra 10%, 6% e 5% com k fixo; a `nota` avisa abaixo de 100.

- `series/forecast`: novo parâmetro `intervalo` — `normal` (padrão, sem
  mudança) ou `bootstrap` (resíduos reamostrados, 5000 trajetórias, semente do
  nó; só ARIMA e ETS). Oráculo: `forecast::forecast(bootstrap = TRUE,
  npaths = 5000)` com a mesma semente, igual a 1e-12 (ARIMA e ETS no
  `AirPassengers`).

- Novo bloco `series/intervencao`: modelo de intervenção de Box & Tiao (1975),
  forma de ordem zero — ARIMA com regressor de degrau, pulso ou rampa numa
  data informada, por `forecast::Arima(xreg = )`; tabela com estimativa,
  erro-padrão, IC de Wald 95%, p e efeito em % (série em log). Oráculo: a
  mesma chamada do `forecast::Arima`, coeficientes e erros-padrão a 1e-8
  (Seatbelts com degrau = coluna `law`; Nile com pulso e rampa). Exemplo:
  lei do cinto (1983-02), log(drivers), ARIMA(1,0,0)(1,1,1)₁₂: ω = −0,2397
  (EP 0,0433), −21,3%. Valores publicados de Harvey & Durbin (1986) não
  foram conferidos na fonte e não entram como oráculo.

## Rigor metodológico (fase 1)

- `series/fisher` (versão 2): convenções de Fisher (1929). O g passa a usar só
  as m = (N − 1) ÷ 2 ordenadas de Fourier, sem a de Nyquist (qui-quadrado com um
  grau, contra dois das outras); o p-valor é a série exata inteira, e não só o
  primeiro termo; o crítico a 5% publicado é o quantil exato. Novo parâmetro
  `remover`: `reta` (padrão, a da versão 1) ou `media` (formulação original).
  Oráculo: `GeneCycle::fisher.g.test` (Wichert, Fokianos & Strimmer 2004)
  reproduzido a 1e-10 em seis séries do `datasets`. Os valores publicados da
  página mudam na terceira casa (p do `lh`: 0,052 → 0,062; decisões iguais).

- `series/phillips_perron` (versão 2): novo parâmetro `deterministico` —
  `tendência` (padrão, idêntico à versão 1: `stats::PP.test`) ou `constante`,
  que tem mais poder em série sem tendência (Phillips & Perron 1988). O Z(t)
  com só constante é a forma geral (Hamilton 1994, eq. 17.6.8) com as
  convenções do `PP.test`; p-valor pela tabela τ_μ de Fuller (1976).
  Oráculos: `stats::PP.test` e `tseries::pp.test` iguais a 1e-12 no caso com
  tendência; `aTSA::pp.test` (tipo 2) igual a 1e-10 no Z(t) com constante,
  `urca::ur.pp` a menos de 0,5% (normalização de MacKinnon). A `nota` do caso
  com tendência deixou de dizer "sempre".

- `series/zivot_andrews` (versão 2): as defasagens passam a ser escolhidas do
  geral para o específico, como no artigo (Perron 1989; Zivot & Andrews 1992):
  do teto para baixo, fica o primeiro k cuja última diferença defasada tem
  |t| >= 1,645 (10%). Novo parâmetro `selecao` (`t_sig`, padrão, ou `fixa`, o
  comportamento da versão 1); `defasagens` vira o teto (0 = Schwert 1989,
  trunc(12·(n/100)^(1/4))). A `nota` e a coluna `defasagens` dizem o k usado.
  Oráculos: a regressão do corte reproduz `urca::ur.za` a 1e-10; com k = 8 o
  bloco dá, no PNB real (-5,58, 1929) e nominal (-5,82, 1929) de
  Nelson-Plosser (`urca::nporg`), valores recomputados a partir dos dados do
  pacote nporg — não conferidos na tabela do PDF do artigo de Zivot & Andrews
  (1992). Muda o resultado padrão.

- `series/regression`: opção de erro ARMA por mínimos quadrados generalizados
  (`erro = "arma"`, ordens `ar` e `ma`; `nlme::gls` + `corARMA`, por máxima
  verossimilhança). O padrão continua MQO (`erro = "independente"`), sem mudar
  resultado. Com GLS, `series/f_global`, `series/f_sazonal` e
  `series/f_tendencia` passam a F de Wald com a covariância do GLS (a `nota`
  diz). Oráculos: `stats::arima(xreg = , method = "ML")` (coeficientes a 1e-3,
  log-verossimilhança a 1e-4), MQO de Prais-Winsten com o phi estimado (1e-8) e
  o F de Wald refeito à mão (1e-8). Medido: sem tendência e erro AR(1)
  phi = 0,6, n = 120, o F de tendência rejeita a 5% em 37% por MQO e 8% por GLS
  (phi = 0,9: 69% e 17%). `nlme` entra em Imports (pacote recomendado do R).
