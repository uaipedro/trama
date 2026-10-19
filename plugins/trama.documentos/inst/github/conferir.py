# Gerado pelo trama.documentos: confere o pacote antes de publicar.
# - ro-crate-metadata.json é RO-Crate 1.1 (descritor, dataset raiz e campos);
# - todo arquivo descrito existe, com o tamanho e o sha256 declarados;
# - todo arquivo do pacote está descrito (fora .github, .git e a extensão).
import hashlib, json, os, sys

erros = []
with open("ro-crate-metadata.json", encoding="utf-8") as f:
    meta = json.load(f)
ctx = meta.get("@context")
ctx = ctx if isinstance(ctx, list) else [ctx]
if "https://w3id.org/ro/crate/1.1/context" not in ctx:
    erros.append("@context sem o RO-Crate 1.1")
grafo = {e.get("@id"): e for e in meta.get("@graph", [])}
desc = grafo.get("ro-crate-metadata.json", {})
if desc.get("about", {}).get("@id") != "./":
    erros.append("descritor sem about ./")
raiz = grafo.get("./")
if raiz is None:
    erros.append("sem o dataset raiz ./")
    raiz = {}
for campo in ("name", "description", "datePublished", "license", "hasPart"):
    if campo not in raiz:
        erros.append("dataset raiz sem " + campo)
partes = [p["@id"] for p in raiz.get("hasPart", [])]
for rel in partes:
    if rel.endswith("/"):
        if not os.path.isdir(rel):
            erros.append("pasta descrita e ausente: " + rel)
        continue
    if not os.path.isfile(rel):
        erros.append("arquivo descrito e ausente: " + rel)
        continue
    e = grafo.get(rel, {})
    with open(rel, "rb") as f:
        dado = f.read()
    if e.get("sha256") != hashlib.sha256(dado).hexdigest():
        erros.append("sha256 não confere: " + rel)
    if str(e.get("contentSize")) != str(len(dado)):
        erros.append("tamanho não confere: " + rel)
for pasta, subs, arqs in os.walk("."):
    subs[:] = [s for s in subs if s not in (".git", ".github", "_extensions")]
    for a in arqs:
        rel = os.path.relpath(os.path.join(pasta, a), ".").replace(os.sep, "/")
        if rel in ("ro-crate-metadata.json", ".gitattributes"):
            continue
        if rel not in partes:
            erros.append("arquivo fora do ro-crate-metadata.json: " + rel)
if erros:
    print("O pacote não confere:\n- " + "\n- ".join(erros), file=sys.stderr)
    sys.exit(1)
print("Pacote conferido: %d partes, sha256 e tamanhos certos." % len(partes))
