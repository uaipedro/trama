# trama.series (desenvolvimento)

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
  bloco reproduz Zivot & Andrews (1992) no PNB real (-5,58, 1929) e nominal
  (-5,82, 1929) de Nelson-Plosser (`urca::nporg`). Muda o resultado padrão.

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
