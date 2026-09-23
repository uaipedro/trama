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
      mostrarView("projetos", { focoCampo: "tl-novo-nome" });
      var campo = document.getElementById("tl-novo-nome");
      if (campo && !campo.value) campo.value = item.titulo || item.nome;
      anunciar("Escolha um nome e crie o projeto com " + (item.titulo || item.nome) + ".");
      return;
    }
    if (item.disponivel) {
      Shiny.setInputValue("tl_instalar_" + nome, Date.now(), { priority: "event" });
    }
  }

  // --- projetos ------------------------------------------------------
  var ICONE_PASTA =
    '<svg viewBox="0 0 24 24"><path d="M3 7a1 1 0 0 1 1-1h5l2 2h9a1 1 0 0 1 1 1v9a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1Z"/></svg>';

  function projetoRecenteHtml(p) {
    return (
      '<button type="button" class="tl-recente-card" data-abrir-caminho="' + esc(p.caminho) + '" data-nome="' + esc(p.nome.toLowerCase()) + '">' +
      '<span class="tl-recente-ic">' + ICONE_PASTA + "</span>" +
      '<span><span class="tl-recente-nome">' + esc(p.nome) + "</span><br>" +
      '<span class="tl-recente-meta">' + (p.aberto ? '<span class="tl-recente-aberto">Aberto</span>' : esc(p.modificado || "")) + "</span></span>" +
      "</button>"
    );
  }

  function projetoItemHtml(p) {
    return (
      '<div class="tl-projeto-item" data-nome="' + esc(p.nome.toLowerCase()) + '">' +
      '<div class="tl-projeto-info"><div class="tl-projeto-nome">' + esc(p.nome) + "</div>" +
      '<div class="tl-projeto-meta">' + (p.aberto ? '<span class="tl-recente-aberto">Aberto</span> · ' : "") + esc(p.modificado || "") + "</div></div>" +
      '<button type="button" class="tl-btn tl-btn-sm" data-abrir-caminho="' + esc(p.caminho) + '">Abrir</button>' +
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
  }

  function criarProjeto() {
    var campo = document.getElementById("tl-novo-nome");
    var nome = campo ? campo.value : "";
    Shiny.setInputValue("tl_novo_projeto_nome", nome);
    Shiny.setInputValue("tl_novo_projeto", Date.now(), { priority: "event" });
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
      if (e.target.id === "tl-btn-novo-projeto-topo") {
        mostrarView("projetos", { focoCampo: "tl-novo-nome" });
        return;
      }
      if (e.target.id === "tl-btn-criar-projeto") {
        criarProjeto();
        return;
      }
      if (e.target.id === "tl-btn-abrir-pasta") {
        abrirPastaExistente();
        return;
      }
    });

    window.addEventListener("hashchange", function () {
      mostrarView(viewAtual(), { semHash: true });
    });

    var busca = document.getElementById("tl-busca");
    if (busca) busca.addEventListener("input", function () { filtrarProjetos(busca.value); });

    var novoNome = document.getElementById("tl-novo-nome");
    if (novoNome) {
      novoNome.addEventListener("keydown", function (e) {
        if (e.key === "Enter") criarProjeto();
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
