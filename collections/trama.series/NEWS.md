# trama.series (desenvolvimento)

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
