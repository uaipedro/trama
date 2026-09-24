/* Tela de início do launcher: barra lateral + abas trocadas no cliente (sem
 * round-trip ao servidor), dados vindos por `session$sendCustomMessage()`
 * (ver R/app.R) e ações disparadas por `Shiny.setInputValue(..., {priority:
 * "event"})` nos MESMOS nomes de input que o server já usava (tl_atualizar,
 * tl_instalar_<nome>, tl_remover_<nome>, tl_voltar_versao, tl_reparar,
 * tl_abrir_logs, tl_tentar_instalar, tl_novo_projeto(_nome), tl_abrir_pasta
 * (_btn), tl_abrir_projeto) — só a UI mudou, o contrato com o server não.
 */
(function () {
  "use strict";

  var VIEWS = ["inicio", "projetos", "colecoes", "atualizacao", "ajuda"];
  var estado = { colecoes: [], projetos: [] };

  function esc(s) {
    return String(s == null ? "" : s).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c];
    });
  }

  function anunciar(msg) {
    var live = document.getElementById("tl-live");
    if (live) live.textContent = msg;
  }

  // --- navegação entre abas -------------------------------------------
  function viewAtual() {
    var h = location.hash.replace("#", "");
    if (VIEWS.indexOf(h) !== -1) return h;
    var p = new URLSearchParams(location.search).get("aba");
    if (VIEWS.indexOf(p) !== -1) return p;
    return "inicio";
  }

  function mostrarView(nome, opts) {
    opts = opts || {};
    if (VIEWS.indexOf(nome) === -1) nome = "inicio";
    VIEWS.forEach(function (v) {
      var sec = document.getElementById("view-" + v);
      var tab = document.getElementById("tl-tab-" + v);
      var ativo = v === nome;
      if (sec) sec.hidden = !ativo;
      if (tab) tab.setAttribute("aria-selected", ativo ? "true" : "false");
    });
    if (!opts.semHash) {
      history.replaceState(null, "", "#" + nome);
    }
    var painel = document.getElementById("view-" + nome);
    if (painel && opts.foco !== false) painel.focus({ preventScroll: true });
    if (opts.focoCampo) {
      var campo = document.getElementById(opts.focoCampo);
      if (campo) campo.focus();
    }
  }

  // --- coleções ----------------------------------------------------------
  var ICONE_PACOTE =
    '<svg viewBox="0 0 24 24"><path d="M12 3 4 7v10l8 4 8-4V7Z"/><path d="M4 7l8 4 8-4"/><path d="M12 11v10"/></svg>';

  function cardColecaoHtml(c) {
    var atributos = ["type=\"button\"", 'class="tl-card"', 'data-colecao="' + esc(c.nome) + '"'];
    var podeClicar = c.instalada || c.disponivel;
    if (!podeClicar) atributos.push("disabled");
    var selo = c.instalada
      ? '<span class="tl-card-selo">Instalada</span>'
      : "";
    var requer = c.requer ? '<p class="tl-card-requer">' + esc(c.requer) + "</p>" : "";
    var rodape = c.instalada
      ? "Abrir projeto novo com ela →"
      : c.disponivel
      ? "Instalar →"
      : "Indisponível nesta release";
    return (
      "<button " + atributos.join(" ") + ">" +
      '<div class="tl-card-topo"><span class="tl-card-ic">' + ICONE_PACOTE + "</span>" + selo + "</div>" +
      '<h3 class="tl-card-titulo">' + esc(c.titulo || c.nome) + "</h3>" +
      requer +
      '<span class="tl-card-rodape">' + rodape + "</span>" +
      "</button>"
    );
  }

  function renderColecoes(lista) {
    estado.colecoes = lista || [];
    var html = estado.colecoes.length
      ? estado.colecoes.map(cardColecaoHtml).join("")
      : '<p class="tl-vazio">Nenhuma coleção disponível nesta release.</p>';
    ["tl-cards-colecoes", "tl-cards-colecoes-full"].forEach(function (id) {
      var el = document.getElementById(id);
      if (el) el.innerHTML = html;
    });
  }

  function tratarCliqueColecao(botao) {
    var nome = botao.dataset.colecao;
    var item = estado.colecoes.filter(function (c) { return c.nome === nome; })[0];
    if (!item) return;
    if (item.instalada) {
      abrirModalNovoProjeto(item.nome);
      anunciar("Novo projeto com " + (item.titulo || item.nome) + ": escolha um nome e confirme.");
      return;
    }
    if (item.disponivel) {
      Shiny.setInputValue("tl_instalar_" + nome, Date.now(), { priority: "event" });
    }
  }

  // --- projetos ------------------------------------------------------
  var ICONE_PASTA =
    '<svg viewBox="0 0 24 24"><path d="M3 7a1 1 0 0 1 1-1h5l2 2h9a1 1 0 0 1 1 1v9a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1Z"/></svg>';
  var ICONE_ENGRENAGEM =
    '<svg viewBox="0 0 24 24"><circle cx="12" cy="12" r="3"/><path d="M19.4 15a1.6 1.6 0 0 0 .32 1.77l.06.06a1.94 1.94 0 1 1-2.75 2.75l-.06-.06a1.6 1.6 0 0 0-1.77-.32 1.6 1.6 0 0 0-1 1.47V21a1.94 1.94 0 1 1-3.88 0v-.09a1.6 1.6 0 0 0-1.05-1.47 1.6 1.6 0 0 0-1.77.32l-.06.06a1.94 1.94 0 1 1-2.75-2.75l.06-.06a1.6 1.6 0 0 0 .32-1.77 1.6 1.6 0 0 0-1.47-1H3a1.94 1.94 0 1 1 0-3.88h.09A1.6 1.6 0 0 0 4.56 10a1.6 1.6 0 0 0-.32-1.77l-.06-.06A1.94 1.94 0 1 1 6.93 5.4l.06.06a1.6 1.6 0 0 0 1.77.32H8.8a1.6 1.6 0 0 0 1-1.47V4a1.94 1.94 0 1 1 3.88 0v.09a1.6 1.6 0 0 0 1 1.47 1.6 1.6 0 0 0 1.77-.32l.06-.06a1.94 1.94 0 1 1 2.75 2.75l-.06.06a1.6 1.6 0 0 0-.32 1.77V9.8a1.6 1.6 0 0 0 1.47 1H21a1.94 1.94 0 1 1 0 3.88h-.09a1.6 1.6 0 0 0-1.47 1Z"/></svg>';

  function projetoRecenteHtml(p) {
    return (
      '<div class="tl-recente-card" data-nome="' + esc(p.nome.toLowerCase()) + '">' +
      '<button type="button" class="tl-recente-abrir" data-abrir-caminho="' + esc(p.caminho) + '">' +
      '<span class="tl-recente-ic">' + ICONE_PASTA + "</span>" +
      '<span><span class="tl-recente-nome">' + esc(p.nome) + "</span><br>" +
      '<span class="tl-recente-meta">' + (p.aberto ? '<span class="tl-recente-aberto">Aberto</span>' : esc(p.modificado || "")) + "</span></span>" +
      "</button>" +
      '<button type="button" class="tl-recente-colecoes" title="Coleções" aria-label="Coleções de ' + esc(p.nome) + '" data-colecoes-caminho="' + esc(p.caminho) + '" data-colecoes-nome="' + esc(p.nome) + '">' +
      ICONE_ENGRENAGEM +
      "</button>" +
      "</div>"
    );
  }

  function projetoItemHtml(p) {
    return (
      '<div class="tl-projeto-item" data-nome="' + esc(p.nome.toLowerCase()) + '">' +
      '<div class="tl-projeto-info"><div class="tl-projeto-nome">' + esc(p.nome) + "</div>" +
      '<div class="tl-projeto-meta">' + (p.aberto ? '<span class="tl-recente-aberto">Aberto</span> · ' : "") + esc(p.modificado || "") + "</div></div>" +
      '<div class="tl-projeto-acoes">' +
      '<button type="button" class="tl-btn tl-btn-sm" data-abrir-caminho="' + esc(p.caminho) + '">Abrir</button>' +
      '<button type="button" class="tl-btn tl-btn-sm" data-colecoes-caminho="' + esc(p.caminho) + '" data-colecoes-nome="' + esc(p.nome) + '">Coleções</button>' +
      "</div>" +
      "</div>"
    );
  }

  function renderProjetos(lista) {
    estado.projetos = lista || [];
    var recentes = document.getElementById("tl-recentes-lista");
    if (recentes) {
      recentes.innerHTML = estado.projetos.length
        ? estado.projetos.slice(0, 6).map(projetoRecenteHtml).join("")
        : '<p class="tl-vazio">Nenhum projeto ainda. Crie o primeiro em "Novo projeto".</p>';
    }
    var full = document.getElementById("tl-lista-projetos-full");
    if (full) {
      full.innerHTML = estado.projetos.length
        ? estado.projetos.map(projetoItemHtml).join("")
        : '<p class="tl-vazio">Nenhum projeto ainda.</p>';
    }
  }

  function filtrarProjetos(termo) {
    termo = (termo || "").toLowerCase().trim();
    document.querySelectorAll("#tl-recentes-lista [data-nome], #tl-lista-projetos-full [data-nome]").forEach(function (el) {
      el.style.display = !termo || el.dataset.nome.indexOf(termo) !== -1 ? "" : "none";
    });
  }

  // --- diálogos (modais) -------------------------------------------------
  var NUCLEO = [
    { nome: "trama.data", titulo: "Dados", nucleo: true },
    { nome: "trama.view", titulo: "Visualização", nucleo: true }
  ];
  var focoAnterior = null;
  var colecoesProjetoAtual = null;

  function opcoesColecoes() {
    return NUCLEO.concat(
      estado.colecoes.map(function (c) {
        return { nome: c.nome, titulo: c.titulo || c.nome, instalada: c.instalada, disponivel: c.disponivel };
      })
    );
  }

  function checklistHtml(opcoes, marcadas) {
    return opcoes
      .map(function (o) {
        var marcada = marcadas.indexOf(o.nome) !== -1;
        var indisponivel = !o.nucleo && !o.disponivel && !o.instalada;
        var nota = o.nucleo
          ? ""
          : o.instalada
          ? ""
          : o.disponivel
          ? "Será instalada"
          : "Indisponível nesta release";
        return (
          '<label class="tl-check-item">' +
          '<input type="checkbox" data-colecao="' + esc(o.nome) + '"' +
          (marcada ? " checked" : "") + (indisponivel ? " disabled" : "") + ">" +
          '<span class="tl-check-titulo">' + esc(o.titulo) + "</span>" +
          (nota ? '<span class="tl-check-nota">' + esc(nota) + "</span>" : "") +
          "</label>"
        );
      })
      .join("");
  }

  function colecoesMarcadas(containerId) {
    var container = document.getElementById(containerId);
    if (!container) return [];
    return Array.prototype.slice
      .call(container.querySelectorAll('input[type="checkbox"][data-colecao]:checked'))
      .map(function (el) { return el.dataset.colecao; });
  }

  function abrirModal(id, focoInicial) {
    var camada = document.getElementById(id);
    if (!camada) return;
    focoAnterior = document.activeElement;
    camada.hidden = false;
    (focoInicial || camada.querySelector("input, button")).focus();
  }

  function fecharModal(camada) {
    if (!camada || camada.hidden) return;
    camada.hidden = true;
    if (focoAnterior && typeof focoAnterior.focus === "function") focoAnterior.focus();
  }

  function modalAberta() {
    return document.querySelector(".tl-modal-camada:not([hidden])");
  }

  // --- novo projeto ------------------------------------------------------
  function abrirModalNovoProjeto(preMarcar) {
    var marcadas = ["trama.data", "trama.view"];
    if (preMarcar && marcadas.indexOf(preMarcar) === -1) marcadas.push(preMarcar);
    document.getElementById("tl-modal-novo-colecoes").innerHTML = checklistHtml(opcoesColecoes(), marcadas);
    var nomeInput = document.getElementById("tl-modal-novo-nome");
    if (nomeInput) nomeInput.value = "";
    abrirModal("tl-modal-novo-projeto", nomeInput);
  }

  function confirmarNovoProjeto() {
    var nomeInput = document.getElementById("tl-modal-novo-nome");
    var nome = nomeInput ? nomeInput.value : "";
    if (!nome || !nome.trim()) {
      if (nomeInput) nomeInput.focus();
      anunciar("Digite um nome para o projeto.");
      return;
    }
    var colecoes = colecoesMarcadas("tl-modal-novo-colecoes");
    Shiny.setInputValue("tl_novo_projeto_nome", nome);
    Shiny.setInputValue("tl_novo_projeto_colecoes", colecoes);
    Shiny.setInputValue("tl_novo_projeto", Date.now(), { priority: "event" });
    fecharModal(document.getElementById("tl-modal-novo-projeto"));
  }

  // --- coleções de um projeto existente -----------------------------------
  function pedirColecoesProjeto(caminho, nome) {
    colecoesProjetoAtual = { caminho: caminho, nome: nome };
    Shiny.setInputValue("tl_colecoes_projeto", caminho, { priority: "event" });
  }

  function receberColecoesProjeto(msg) {
    if (!colecoesProjetoAtual || colecoesProjetoAtual.caminho !== msg.caminho) return;
    document.getElementById("tl-modal-colecoes-titulo").textContent = "Coleções de " + msg.nome;
    document.getElementById("tl-modal-colecoes-sub").textContent =
      "Ligue ou desligue coleções deste projeto. O que faltar é instalado ao salvar.";
    var lista = document.getElementById("tl-modal-colecoes-lista");
    lista.innerHTML = checklistHtml(opcoesColecoes(), msg.colecoes || []);
    abrirModal("tl-modal-colecoes-projeto", lista.querySelector('input[type="checkbox"]'));
  }

  function confirmarColecoesProjeto() {
    if (!colecoesProjetoAtual) return;
    var colecoes = colecoesMarcadas("tl-modal-colecoes-lista");
    Shiny.setInputValue(
      "tl_salvar_colecoes_projeto",
      { caminho: colecoesProjetoAtual.caminho, colecoes: colecoes },
      { priority: "event" }
    );
    fecharModal(document.getElementById("tl-modal-colecoes-projeto"));
  }

  // --- projeto pede coleção que falta na lib ------------------------------
  function receberProjetoFaltando(msg) {
    var texto =
      "'" + msg.nome + "' usa " + msg.faltando.join(", ") +
      (msg.faltando.length > 1 ? ", que não estão instaladas" : ", que não está instalada") +
      " nesta biblioteca.";
    if (!msg.instalavel) {
      texto += " Essa coleção não consta no manifesto desta release — não é possível instalar por aqui.";
    }
    document.getElementById("tl-modal-faltando-texto").textContent = texto;
    // `style.display` direto, não o atributo `hidden`: `.tl-btn` já define
    // `display:inline-flex` com a mesma especificidade de `[hidden]`, e
    // como vem depois no CSS ganha do atributo — o botão ficava visível
    // mesmo com `hidden = true`. Inline style sempre ganha da folha.
    var botao = document.getElementById("tl-modal-faltando-instalar");
    var mostrarBotao = !!msg.instalavel;
    botao.style.display = mostrarBotao ? "" : "none";
    if (mostrarBotao) {
      botao.dataset.caminho = msg.caminho;
    } else {
      delete botao.dataset.caminho;
    }
    abrirModal("tl-modal-projeto-faltando", mostrarBotao ? botao : undefined);
  }

  function confirmarInstalarEAbrir() {
    var botao = document.getElementById("tl-modal-faltando-instalar");
    var caminho = botao ? botao.dataset.caminho : null;
    if (!caminho) return;
    Shiny.setInputValue("tl_instalar_e_abrir_projeto", caminho, { priority: "event" });
    fecharModal(document.getElementById("tl-modal-projeto-faltando"));
  }

  // --- status / versão -------------------------------------------------
  function pacotesTabelaHtml(pacotes) {
    var linhas = (pacotes || [])
      .map(function (p) {
        return (
          "<tr><td>" + esc(p.nome) + "</td><td>" + esc(p.instalada || "—") + "</td><td>" + esc(p.disponivel || "—") + "</td></tr>"
        );
      })
      .join("");
    return (
      '<table class="tl-tabela"><tr><th>Pacote</th><th>Instalada</th><th>Disponível</th></tr>' + linhas + "</table>"
    );
  }

  function acaoHtml(acao) {
    if (!acao) return "";
    if (acao.tipo === "link") {
      return '<a class="tl-btn" href="' + esc(acao.href) + '" target="_blank" rel="noopener">' + esc(acao.rotulo) + "</a>";
    }
    return '<button type="button" class="tl-btn tl-btn-primario" data-acao="' + esc(acao.input) + '">' + esc(acao.rotulo) + "</button>";
  }

  function renderStatus(st) {
    document.body.classList.toggle("tl-ocupado", !!st.ocupado);

    var dot = document.getElementById("tl-status-dot");
    var texto = document.getElementById("tl-status-texto");
    var classeDot = { atualizado: "tl-ok", desatualizado: "tl-aviso", trocar_r: "tl-err", sem_internet: "tl-err", nenhuma: "" }[st.selo] || "";
    if (dot) dot.className = "tl-status-dot" + (st.ocupado ? " tl-ocupado" : classeDot ? " " + classeDot : "");
    if (texto) texto.textContent = st.pill_texto;

    var navDot = document.getElementById("tl-nav-dot-atualizacao");
    if (navDot) navDot.hidden = !(st.selo === "desatualizado" || st.selo === "trocar_r" || st.selo === "sem_internet");

    var heroAcao = document.getElementById("tl-hero-acao");
    if (heroAcao) heroAcao.innerHTML = st.selo === "atualizado" || st.selo === "nenhuma" ? "" : acaoHtml(st.acao);

    var corpo = document.getElementById("tl-versao-corpo");
    if (corpo) {
      var partes = [];
      partes.push('<div class="tl-versao-linhas">');
      partes.push("<p>Release instalada: <strong>" + esc(st.release_instalada || "nenhuma") + "</strong></p>");
      partes.push("<p>R instalado: <strong>" + esc(st.r_instalado) + "</strong> (exigido: " + esc(st.r_exigido || "—") + ")</p>");
      partes.push("</div>");
      partes.push(pacotesTabelaHtml(st.pacotes));
      if (st.aviso) partes.push('<p class="tl-aviso">' + esc(st.aviso) + "</p>");
      if (st.selo === "atualizado" && st.release_instalada) {
        partes.push('<span class="tl-selo tl-selo-ok">Atualizado</span>');
      } else if (st.acao) {
        partes.push(acaoHtml(st.acao));
      }
      corpo.innerHTML = partes.join("");
    }

    var voltar = document.getElementById("tl-btn-voltar-versao");
    if (voltar) voltar.disabled = !st.tem_anteriores;
  }

  // --- ligação com o Shiny ----------------------------------------------
  function ligarShiny() {
    Shiny.addCustomMessageHandler("tl-status", renderStatus);
    Shiny.addCustomMessageHandler("tl-colecoes", renderColecoes);
    Shiny.addCustomMessageHandler("tl-projetos", renderProjetos);
    Shiny.addCustomMessageHandler("tl-colecoes-projeto", receberColecoesProjeto);
    Shiny.addCustomMessageHandler("tl-projeto-faltando", receberProjetoFaltando);
  }

  function abrirPastaExistente() {
    var campo = document.getElementById("tl-pasta");
    var caminho = campo ? campo.value : "";
    Shiny.setInputValue("tl_abrir_pasta", caminho);
    Shiny.setInputValue("tl_abrir_pasta_btn", Date.now(), { priority: "event" });
  }

  function init() {
    mostrarView(viewAtual(), { semHash: true, foco: false });

    document.addEventListener("click", function (e) {
      var navBtn = e.target.closest(".tl-nav-item, .tl-status-pill");
      if (navBtn && navBtn.dataset.view) {
        mostrarView(navBtn.dataset.view);
        return;
      }
      if (document.body.classList.contains("tl-ocupado")) return;

      var acaoBtn = e.target.closest("[data-acao]");
      if (acaoBtn && !acaoBtn.disabled) {
        Shiny.setInputValue(acaoBtn.dataset.acao, Date.now(), { priority: "event" });
        return;
      }
      var cardBtn = e.target.closest(".tl-card[data-colecao]");
      if (cardBtn && !cardBtn.disabled) {
        tratarCliqueColecao(cardBtn);
        return;
      }
      var abrirBtn = e.target.closest("[data-abrir-caminho]");
      if (abrirBtn) {
        Shiny.setInputValue("tl_abrir_projeto", abrirBtn.dataset.abrirCaminho, { priority: "event" });
        return;
      }
      var colecoesBtn = e.target.closest("[data-colecoes-caminho]");
      if (colecoesBtn) {
        pedirColecoesProjeto(colecoesBtn.dataset.colecoesCaminho, colecoesBtn.dataset.colecoesNome);
        return;
      }
      if (e.target.id === "tl-btn-novo-projeto-topo" || e.target.id === "tl-btn-criar-projeto") {
        abrirModalNovoProjeto();
        return;
      }
      if (e.target.id === "tl-btn-abrir-pasta") {
        abrirPastaExistente();
        return;
      }
      if (e.target.id === "tl-modal-novo-criar") {
        confirmarNovoProjeto();
        return;
      }
      if (e.target.id === "tl-modal-colecoes-salvar") {
        confirmarColecoesProjeto();
        return;
      }
      if (e.target.id === "tl-modal-faltando-instalar") {
        confirmarInstalarEAbrir();
        return;
      }
      var fecharBtn = e.target.closest("[data-fechar-modal]");
      if (fecharBtn) {
        fecharModal(fecharBtn.closest(".tl-modal-camada"));
        return;
      }
      if (e.target.classList && e.target.classList.contains("tl-modal-camada")) {
        fecharModal(e.target);
        return;
      }
    });

    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") {
        var aberta = modalAberta();
        if (aberta) fecharModal(aberta);
      }
    });

    window.addEventListener("hashchange", function () {
      mostrarView(viewAtual(), { semHash: true });
    });

    var busca = document.getElementById("tl-busca");
    if (busca) busca.addEventListener("input", function () { filtrarProjetos(busca.value); });

    var modalNomeCampo = document.getElementById("tl-modal-novo-nome");
    if (modalNomeCampo) {
      modalNomeCampo.addEventListener("keydown", function (e) {
        if (e.key === "Enter") confirmarNovoProjeto();
      });
    }
    var pasta = document.getElementById("tl-pasta");
    if (pasta) {
      pasta.addEventListener("keydown", function (e) {
        if (e.key === "Enter") abrirPastaExistente();
      });
    }

    if (window.Shiny && Shiny.addCustomMessageHandler) {
      ligarShiny();
    } else {
      document.addEventListener("shiny:sessioninitialized", ligarShiny);
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
