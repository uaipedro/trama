// `export * from "react"` NÃO sintetiza export ESM nomeado nenhum aqui — o
// esbuild só resolve `export *` de um módulo CJS até `default`; os nomes
// (useState, Component, etc.) somem em silêncio, e todo `import { useState }
// from "react"` no editor voltaria `undefined`. Nomear explicitamente é a
// única forma confiável — API pública completa do React 18, não só o que o
// editor usa hoje: coleções (JS delas) também importam "react" pelo mesmo
// importmap.
import React, {
  Children, Component, Fragment, Profiler, PureComponent, StrictMode, Suspense,
  cloneElement, createContext, createElement, createFactory, createRef,
  forwardRef, isValidElement, lazy, memo, startTransition,
  useCallback, useContext, useDebugValue, useDeferredValue, useEffect, useId,
  useImperativeHandle, useInsertionEffect, useLayoutEffect, useMemo,
  useReducer, useRef, useState, useSyncExternalStore, useTransition, version,
} from "react";

// React 18 não publica build ESM — só CJS. Bundlado por esbuild, `react-dom`
// (também CJS) chama `require("react")` de dentro do wrapper de interop
// CJS->ESM, e esse `require` não existe em browser nenhum: sem isto, o app
// carrega e quebra na hora com "Dynamic require of 'react' is not supported"
// assim que qualquer coisa toca `react-dom`. `require` GLOBAL (não
// `window.require`) porque o wrapper do esbuild testa `typeof require`, e é
// isso que resolve como identificador livre dentro de um módulo ES.
if (typeof globalThis !== "undefined") {
  globalThis.__tr_modules = globalThis.__tr_modules || {};
  globalThis.__tr_modules.react = React;
  Object.assign(globalThis.__tr_modules.react, {
    Children, Component, Fragment, Profiler, PureComponent, StrictMode, Suspense,
    cloneElement, createContext, createElement, createFactory, createRef,
    forwardRef, isValidElement, lazy, memo, startTransition,
    useCallback, useContext, useDebugValue, useDeferredValue, useEffect, useId,
    useImperativeHandle, useInsertionEffect, useLayoutEffect, useMemo,
    useReducer, useRef, useState, useSyncExternalStore, useTransition, version,
    default: React,
  });
  if (typeof globalThis.require === "undefined") {
    globalThis.require = function (id) {
      var m = globalThis.__tr_modules[id];
      if (m) return m;
      throw new Error('trama: sem shim de require() para "' + id + '"');
    };
  }
}

export default React;
export {
  Children, Component, Fragment, Profiler, PureComponent, StrictMode, Suspense,
  cloneElement, createContext, createElement, createFactory, createRef,
  forwardRef, isValidElement, lazy, memo, startTransition,
  useCallback, useContext, useDebugValue, useDeferredValue, useEffect, useId,
  useImperativeHandle, useInsertionEffect, useLayoutEffect, useMemo,
  useReducer, useRef, useState, useSyncExternalStore, useTransition, version,
};
