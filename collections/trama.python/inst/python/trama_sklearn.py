"""Lado Python da trama.python. Só funções puras: recebem pandas, devolvem numpy/bytes."""
import pickle

import numpy as np
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.model_selection import KFold, StratifiedKFold, cross_val_predict


def _novo(tarefa, n_arvores, seed):
    cls = RandomForestClassifier if tarefa == "classificacao" else RandomForestRegressor
    return cls(n_estimators=int(n_arvores), random_state=int(seed))


def ajustar(X, y, tarefa, n_arvores, seed):
    return _novo(tarefa, n_arvores, seed).fit(X, np.asarray(y))


def prever(m, X):
    previsto = m.predict(X)
    prob = m.predict_proba(X) if hasattr(m, "predict_proba") else None
    classes = [str(c) for c in m.classes_] if hasattr(m, "classes_") else None
    return {"previsto": previsto, "prob": prob, "classes": classes}


def prever_cv(X, y, tarefa, n_arvores, seed, k):
    m = _novo(tarefa, n_arvores, seed)
    y = np.asarray(y)
    if tarefa == "classificacao":
        cv = StratifiedKFold(n_splits=int(k), shuffle=True, random_state=int(seed))
        prob = cross_val_predict(m, X, y, cv=cv, method="predict_proba")
        classes = [str(c) for c in np.unique(y)]
        return {"previsto": np.asarray(classes)[prob.argmax(axis=1)], "prob": prob, "classes": classes}
    cv = KFold(n_splits=int(k), shuffle=True, random_state=int(seed))
    return {"previsto": cross_val_predict(m, X, y, cv=cv), "prob": None, "classes": None}


def importancia(m):
    return np.asarray(m.feature_importances_)


def gravar(m):
    # numpy uint8 e não bytes: o reticulate não converte `bytes` em raw, e uma
    # referência Python viva iria parar no RDS.
    return np.frombuffer(pickle.dumps(m), dtype=np.uint8).copy()


def ler(b):
    return pickle.loads(np.asarray(b, dtype=np.uint8).tobytes())
