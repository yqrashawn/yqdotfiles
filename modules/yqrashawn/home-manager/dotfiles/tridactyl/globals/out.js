(() => {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __commonJS = (cb, mod) => function __require() {
    return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
  };
  var __export = (target, all) => {
    for (var name in all)
      __defProp(target, name, { get: all[name], enumerable: true });
  };
  var __copyProps = (to, from, except, desc) => {
    if (from && typeof from === "object" || typeof from === "function") {
      for (let key of __getOwnPropNames(from))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
    }
    return to;
  };
  var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
    // If the importer is in node compatibility mode or this is not an ESM
    // file that has been converted to a CommonJS file using a Babel-
    // compatible transform (i.e. "__esModule" has not been set), then set
    // "default" to the CommonJS "module.exports" for node compatibility.
    isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
    mod
  ));

  // (disabled):util
  var require_util = __commonJS({
    "(disabled):util"() {
    }
  });

  // node_modules/@thi.ng/cache/index.js
  var cache_exports = {};
  __export(cache_exports, {
    LRUCache: () => LRUCache,
    MRUCache: () => MRUCache,
    TLRUCache: () => TLRUCache
  });

  // node_modules/@thi.ng/compare/compare.js
  var compare = (a, b) => {
    if (a === b) {
      return 0;
    }
    if (a == null) {
      return b == null ? 0 : -1;
    }
    if (b == null) {
      return a == null ? 0 : 1;
    }
    if (typeof a.compare === "function") {
      return a.compare(b);
    }
    if (typeof b.compare === "function") {
      return -b.compare(a);
    }
    return a < b ? -1 : a > b ? 1 : 0;
  };

  // node_modules/@thi.ng/errors/deferror.js
  var defError = (prefix, suffix = (msg) => msg !== void 0 ? ": " + msg : "") => class extends Error {
    constructor(msg) {
      super(prefix(msg) + suffix(msg));
    }
  };

  // node_modules/@thi.ng/errors/illegal-arguments.js
  var IllegalArgumentError = defError(() => "illegal argument(s)");
  var illegalArgs = (msg) => {
    throw new IllegalArgumentError(msg);
  };

  // node_modules/@thi.ng/errors/out-of-bounds.js
  var OutOfBoundsError = defError(() => "index out of bounds");
  var outOfBounds = (index) => {
    throw new OutOfBoundsError(index);
  };
  var ensureIndex = (index, min2, max2) => (index < min2 || index >= max2) && outOfBounds(index);

  // node_modules/@thi.ng/random/arandom.js
  var INV_MAX = 1 / 2 ** 32;
  var ARandom = class {
    float(norm = 1) {
      return this.int() * INV_MAX * norm;
    }
    probability(p) {
      return this.float() < p;
    }
    norm(norm = 1) {
      return (this.int() * INV_MAX - 0.5) * 2 * norm;
    }
    normMinMax(min2, max2) {
      const x = this.minmax(min2, max2);
      return this.float() < 0.5 ? x : -x;
    }
    minmax(min2, max2) {
      return this.float() * (max2 - min2) + min2;
    }
    minmaxInt(min2, max2) {
      min2 |= 0;
      const range3 = (max2 | 0) - min2;
      return range3 ? min2 + this.int() % range3 : min2;
    }
    minmaxUint(min2, max2) {
      min2 >>>= 0;
      const range3 = (max2 >>> 0) - min2;
      return range3 ? min2 + this.int() % range3 : min2;
    }
  };

  // node_modules/@thi.ng/random/system.js
  var random = Math.random;
  var SystemRandom = class extends ARandom {
    int() {
      return random() * 4294967296 >>> 0;
    }
    float(norm = 1) {
      return random() * norm;
    }
    norm(norm = 1) {
      return (random() - 0.5) * 2 * norm;
    }
  };
  var SYSTEM = new SystemRandom();

  // node_modules/@thi.ng/checks/is-arraylike.js
  var isArrayLike = (x) => x != null && typeof x !== "function" && x.length !== void 0;

  // node_modules/@thi.ng/compare/index.js
  var compare_exports = {};
  __export(compare_exports, {
    OPERATORS: () => OPERATORS,
    compare: () => compare,
    compareByKey: () => compareByKey,
    compareByKeys2: () => compareByKeys2,
    compareByKeys3: () => compareByKeys3,
    compareByKeys4: () => compareByKeys4,
    compareNumAsc: () => compareNumAsc,
    compareNumDesc: () => compareNumDesc,
    eq: () => eq,
    gt: () => gt,
    gte: () => gte,
    lt: () => lt,
    lte: () => lte,
    neq: () => neq,
    numericOp: () => numericOp,
    reverse: () => reverse,
    stringOp: () => stringOp
  });

  // node_modules/@thi.ng/compare/keys.js
  var getKey = (k) => typeof k === "function" ? k : (x) => x[k];
  function compareByKey(a, cmp = compare) {
    const k = getKey(a);
    return (x, y) => cmp(k(x), k(y));
  }
  function compareByKeys2(a, b, cmpA = compare, cmpB = compare) {
    const ka = getKey(a);
    const kb = getKey(b);
    return (x, y) => {
      let res = cmpA(ka(x), ka(y));
      return res === 0 ? cmpB(kb(x), kb(y)) : res;
    };
  }
  function compareByKeys3(a, b, c, cmpA = compare, cmpB = compare, cmpC = compare) {
    const ka = getKey(a);
    const kb = getKey(b);
    const kc = getKey(c);
    return (x, y) => {
      let res = cmpA(ka(x), ka(y));
      return res === 0 ? (res = cmpB(kb(x), kb(y))) === 0 ? cmpC(kc(x), kc(y)) : res : res;
    };
  }
  function compareByKeys4(a, b, c, d, cmpA = compare, cmpB = compare, cmpC = compare, cmpD = compare) {
    const ka = getKey(a);
    const kb = getKey(b);
    const kc = getKey(c);
    const kd = getKey(d);
    return (x, y) => {
      let res = cmpA(ka(x), ka(y));
      return res === 0 ? (res = cmpB(kb(x), kb(y))) === 0 ? (res = cmpC(kc(x), kc(y))) === 0 ? cmpD(kd(x), kd(y)) : res : res : res;
    };
  }

  // node_modules/@thi.ng/compare/numeric.js
  var compareNumAsc = (a, b) => a - b;
  var compareNumDesc = (a, b) => b - a;

  // node_modules/@thi.ng/compare/ops.js
  var OPERATORS = {
    "=": eq,
    "!=": neq,
    "<": lt,
    "<=": lte,
    ">=": gte,
    ">": gt
  };
  var __ensure = (op) => {
    if (typeof op === "string") {
      if (op in OPERATORS)
        return OPERATORS[op];
      else
        throw new Error(`invalid operator: ${op}`);
    }
    return op;
  };
  var stringOp = (op, x) => {
    const impl = __ensure(op);
    return (y) => impl(String(y), x);
  };
  var numericOp = (op, x) => {
    const impl = __ensure(op);
    return (y) => typeof y === "number" && impl(y, x);
  };
  function eq(a, b) {
    return a === b;
  }
  function neq(a, b) {
    return a !== b;
  }
  function lt(a, b) {
    return a < b;
  }
  function lte(a, b) {
    return a <= b;
  }
  function gte(a, b) {
    return a >= b;
  }
  function gt(a, b) {
    return a > b;
  }

  // node_modules/@thi.ng/compare/reverse.js
  var reverse = (cmp) => (a, b) => -cmp(a, b);

  // node_modules/@thi.ng/equiv/index.js
  var OBJP = Object.getPrototypeOf({});
  var FN = "function";
  var STR = "string";
  var equiv = (a, b) => {
    let proto;
    if (a === b) {
      return true;
    }
    if (a != null) {
      if (typeof a.equiv === FN) {
        return a.equiv(b);
      }
    } else {
      return a == b;
    }
    if (b != null) {
      if (typeof b.equiv === FN) {
        return b.equiv(a);
      }
    } else {
      return a == b;
    }
    if (typeof a === STR || typeof b === STR) {
      return false;
    }
    if ((proto = Object.getPrototypeOf(a), proto == null || proto === OBJP) && (proto = Object.getPrototypeOf(b), proto == null || proto === OBJP)) {
      return equivObject(a, b);
    }
    if (typeof a !== FN && a.length !== void 0 && typeof b !== FN && b.length !== void 0) {
      return equivArrayLike(a, b);
    }
    if (a instanceof Set && b instanceof Set) {
      return equivSet(a, b);
    }
    if (a instanceof Map && b instanceof Map) {
      return equivMap(a, b);
    }
    if (a instanceof Date && b instanceof Date) {
      return a.getTime() === b.getTime();
    }
    if (a instanceof RegExp && b instanceof RegExp) {
      return a.toString() === b.toString();
    }
    return a !== a && b !== b;
  };
  var equivArrayLike = (a, b, _equiv = equiv) => {
    let l = a.length;
    if (l === b.length) {
      while (l-- > 0 && _equiv(a[l], b[l]))
        ;
    }
    return l < 0;
  };
  var equivSet = (a, b, _equiv = equiv) => a.size === b.size && _equiv([...a.keys()].sort(), [...b.keys()].sort());
  var equivMap = (a, b, _equiv = equiv) => a.size === b.size && _equiv([...a].sort(), [...b].sort());
  var equivObject = (a, b, _equiv = equiv) => {
    if (Object.keys(a).length !== Object.keys(b).length) {
      return false;
    }
    for (let k in a) {
      if (!b.hasOwnProperty(k) || !_equiv(a[k], b[k])) {
        return false;
      }
    }
    return true;
  };

  // node_modules/@thi.ng/transducers/index.js
  var transducers_exports = {};
  __export(transducers_exports, {
    $$reduce: () => $$reduce,
    Range: () => Range,
    Reduced: () => Reduced,
    __iter: () => __iter,
    add: () => add,
    asIterable: () => asIterable,
    assocMap: () => assocMap,
    assocObj: () => assocObj,
    autoObj: () => autoObj,
    benchmark: () => benchmark,
    buildKernel1d: () => buildKernel1d,
    buildKernel2d: () => buildKernel2d,
    cat: () => cat,
    choices: () => choices,
    comp: () => comp2,
    compR: () => compR,
    concat: () => concat,
    conj: () => conj,
    consume: () => consume,
    converge: () => converge,
    convolve1d: () => convolve1d,
    convolve2d: () => convolve2d,
    count: () => count,
    curve: () => curve,
    cycle: () => cycle,
    dedupe: () => dedupe,
    deepTransform: () => deepTransform,
    delayed: () => delayed2,
    distinct: () => distinct,
    div: () => div,
    drop: () => drop,
    dropNth: () => dropNth,
    dropWhile: () => dropWhile,
    dup: () => dup,
    duplicate: () => duplicate,
    ensureReduced: () => ensureReduced,
    ensureTransducer: () => ensureTransducer,
    every: () => every,
    extendSides: () => extendSides,
    fill: () => fill,
    fillN: () => fillN,
    filter: () => filter,
    filterFuzzy: () => filterFuzzy,
    flatten: () => flatten,
    flatten1: () => flatten1,
    flattenWith: () => flattenWith,
    frequencies: () => frequencies,
    groupBinary: () => groupBinary,
    groupByMap: () => groupByMap,
    groupByObj: () => groupByObj,
    indexed: () => indexed,
    interleave: () => interleave,
    interpolate: () => interpolate,
    interpolateHermite: () => interpolateHermite,
    interpolateLinear: () => interpolateLinear,
    interpose: () => interpose,
    isReduced: () => isReduced,
    iterate: () => iterate,
    iterator: () => iterator,
    iterator1: () => iterator1,
    juxtR: () => juxtR,
    keep: () => keep,
    keyPermutations: () => keyPermutations,
    keySelector: () => keySelector,
    keys: () => keys,
    labeled: () => labeled,
    last: () => last,
    length: () => length,
    line: () => line,
    lookup1d: () => lookup1d,
    lookup2d: () => lookup2d,
    lookup3d: () => lookup3d,
    map: () => map,
    mapA: () => mapA,
    mapDeep: () => mapDeep,
    mapIndexed: () => mapIndexed,
    mapKeys: () => mapKeys,
    mapNth: () => mapNth,
    mapVals: () => mapVals,
    mapcat: () => mapcat,
    mapcatIndexed: () => mapcatIndexed,
    matchFirst: () => matchFirst,
    matchLast: () => matchLast,
    max: () => max,
    maxCompare: () => maxCompare,
    maxMag: () => maxMag,
    mean: () => mean,
    min: () => min,
    minCompare: () => minCompare,
    minMag: () => minMag,
    minMax: () => minMax,
    movingAverage: () => movingAverage,
    movingMedian: () => movingMedian,
    mul: () => mul,
    multiplex: () => multiplex,
    multiplexObj: () => multiplexObj,
    noop: () => noop,
    normCount: () => normCount,
    normFrequencies: () => normFrequencies,
    normFrequenciesAuto: () => normFrequenciesAuto,
    normRange: () => normRange,
    normRange2d: () => normRange2d,
    normRange3d: () => normRange3d,
    padLast: () => padLast,
    padSides: () => padSides,
    page: () => page,
    pairs: () => pairs,
    palindrome: () => palindrome,
    partition: () => partition,
    partitionBy: () => partitionBy,
    partitionOf: () => partitionOf,
    partitionSort: () => partitionSort,
    partitionSync: () => partitionSync,
    partitionTime: () => partitionTime,
    partitionWhen: () => partitionWhen,
    peek: () => peek2,
    permutations: () => permutations,
    permutationsN: () => permutationsN,
    pluck: () => pluck,
    push: () => push,
    pushCopy: () => pushCopy,
    pushSort: () => pushSort,
    range: () => range,
    range2d: () => range2d,
    range3d: () => range3d,
    rangeNd: () => rangeNd,
    rechunk: () => rechunk,
    reduce: () => reduce,
    reduceRight: () => reduceRight,
    reduced: () => reduced,
    reducer: () => reducer,
    reductions: () => reductions,
    rename: () => rename,
    renamer: () => renamer,
    repeat: () => repeat,
    repeatedly: () => repeatedly,
    repeatedly2d: () => repeatedly2d,
    repeatedly3d: () => repeatedly3d,
    reverse: () => reverse2,
    run: () => run,
    sample: () => sample,
    scan: () => scan,
    selectKeys: () => selectKeys,
    sideEffect: () => sideEffect,
    slidingWindow: () => slidingWindow,
    some: () => some,
    sortedKeys: () => sortedKeys,
    step: () => step,
    str: () => str,
    streamShuffle: () => streamShuffle,
    streamSort: () => streamSort,
    struct: () => struct,
    sub: () => sub,
    swizzle: () => swizzle2,
    symmetric: () => symmetric,
    take: () => take,
    takeLast: () => takeLast,
    takeNth: () => takeNth,
    takeWhile: () => takeWhile,
    throttle: () => throttle,
    throttleTime: () => throttleTime,
    toggle: () => toggle,
    trace: () => trace,
    transduce: () => transduce,
    transduceRight: () => transduceRight,
    tween: () => tween,
    unreduced: () => unreduced,
    vals: () => vals,
    wordWrap: () => wordWrap,
    wrapSides: () => wrapSides,
    zip: () => zip
  });

  // node_modules/@thi.ng/checks/implements-function.js
  var implementsFunction = (x, fn) => x != null && typeof x[fn] === "function";

  // node_modules/@thi.ng/transducers/ensure.js
  var ensureTransducer = (x) => implementsFunction(x, "xform") ? x.xform() : x;

  // node_modules/@thi.ng/api/api.js
  var SEMAPHORE = Symbol();
  var NO_OP = () => {
  };

  // node_modules/@thi.ng/checks/is-iterable.js
  var isIterable = (x) => x != null && typeof x[Symbol.iterator] === "function";

  // node_modules/@thi.ng/errors/illegal-arity.js
  var IllegalArityError = defError(() => "illegal arity");
  var illegalArity = (n) => {
    throw new IllegalArityError(n);
  };

  // node_modules/@thi.ng/transducers/reduced.js
  var Reduced = class {
    value;
    constructor(val) {
      this.value = val;
    }
    deref() {
      return this.value;
    }
  };
  var reduced = (x) => new Reduced(x);
  var isReduced = (x) => x instanceof Reduced;
  var ensureReduced = (x) => x instanceof Reduced ? x : new Reduced(x);
  var unreduced = (x) => x instanceof Reduced ? x.deref() : x;

  // node_modules/@thi.ng/transducers/reduce.js
  var parseArgs = (args) => args.length === 2 ? [void 0, args[1]] : args.length === 3 ? [args[1], args[2]] : illegalArity(args.length);
  function reduce(...args) {
    const rfn = args[0];
    const init = rfn[0];
    const complete = rfn[1];
    const reduce22 = rfn[2];
    args = parseArgs(args);
    const acc = args[0] == null ? init() : args[0];
    const xs = args[1];
    return unreduced(
      complete(
        implementsFunction(xs, "$reduce") ? xs.$reduce(reduce22, acc) : isArrayLike(xs) ? reduceArray(reduce22, acc, xs) : reduceIterable(reduce22, acc, xs)
      )
    );
  }
  function reduceRight(...args) {
    const rfn = args[0];
    const init = rfn[0];
    const complete = rfn[1];
    const reduce22 = rfn[2];
    args = parseArgs(args);
    let acc = args[0] == null ? init() : args[0];
    const xs = args[1];
    for (let i = xs.length; i-- > 0; ) {
      acc = reduce22(acc, xs[i]);
      if (isReduced(acc)) {
        acc = acc.deref();
        break;
      }
    }
    return unreduced(complete(acc));
  }
  var reduceArray = (rfn, acc, xs) => {
    for (let i = 0, n = xs.length; i < n; i++) {
      acc = rfn(acc, xs[i]);
      if (isReduced(acc)) {
        acc = acc.deref();
        break;
      }
    }
    return acc;
  };
  var reduceIterable = (rfn, acc, xs) => {
    for (let x of xs) {
      acc = rfn(acc, x);
      if (isReduced(acc)) {
        acc = acc.deref();
        break;
      }
    }
    return acc;
  };
  var reducer = (init, rfn) => [init, (acc) => acc, rfn];
  var $$reduce = (rfn, args) => {
    const n = args.length - 1;
    return isIterable(args[n]) ? args.length > 1 ? reduce(rfn.apply(null, args.slice(0, n)), args[n]) : reduce(rfn(), args[0]) : void 0;
  };

  // node_modules/@thi.ng/transducers/push.js
  function push(xs) {
    return xs ? [...xs] : reducer(
      () => [],
      (acc, x) => (acc.push(x), acc)
    );
  }

  // node_modules/@thi.ng/transducers/iterator.js
  function* iterator(xform, xs) {
    const rfn = ensureTransducer(xform)(push());
    const complete = rfn[1];
    const reduce3 = rfn[2];
    for (let x of xs) {
      const y = reduce3([], x);
      if (isReduced(y)) {
        yield* unreduced(complete(y.deref()));
        return;
      }
      if (y.length) {
        yield* y;
      }
    }
    yield* unreduced(complete([]));
  }
  function* iterator1(xform, xs) {
    const reduce3 = ensureTransducer(xform)([NO_OP, NO_OP, (_, x) => x])[2];
    for (let x of xs) {
      let y = reduce3(SEMAPHORE, x);
      if (isReduced(y)) {
        y = unreduced(y.deref());
        if (y !== SEMAPHORE) {
          yield y;
        }
        return;
      }
      if (y !== SEMAPHORE) {
        yield y;
      }
    }
  }
  var __iter = (xform, args, impl = iterator1) => {
    const n = args.length - 1;
    return isIterable(args[n]) ? args.length > 1 ? impl(xform.apply(null, args.slice(0, n)), args[n]) : impl(xform(), args[0]) : void 0;
  };

  // node_modules/@thi.ng/transducers/compr.js
  var compR = (rfn, fn) => [rfn[0], rfn[1], fn];

  // node_modules/@thi.ng/transducers/map.js
  function map(fn, src) {
    return isIterable(src) ? iterator1(map(fn), src) : (rfn) => {
      const r = rfn[2];
      return compR(rfn, (acc, x) => r(acc, fn(x)));
    };
  }
  var mapA = (fn, src) => [
    ...map(fn, src)
  ];

  // node_modules/@thi.ng/transducers/transduce.js
  function transduce(...args) {
    return $transduce(transduce, reduce, args);
  }
  function transduceRight(...args) {
    return $transduce(transduceRight, reduceRight, args);
  }
  var $transduce = (tfn, rfn, args) => {
    let acc, xs;
    switch (args.length) {
      case 4:
        xs = args[3];
        acc = args[2];
        break;
      case 3:
        xs = args[2];
        break;
      case 2:
        return map((x) => tfn(args[0], args[1], x));
      default:
        illegalArity(args.length);
    }
    return rfn(ensureTransducer(args[0])(args[1]), acc, xs);
  };

  // node_modules/@thi.ng/transducers/run.js
  var NO_OP_REDUCER = [NO_OP, NO_OP, NO_OP];
  function run(tx, ...args) {
    if (args.length === 1) {
      transduce(tx, NO_OP_REDUCER, args[0]);
    } else {
      const fx = args[0];
      transduce(tx, [NO_OP, NO_OP, (_, x) => fx(x)], args[1]);
    }
  }

  // node_modules/@thi.ng/transducers/step.js
  var step = (tx, unwrap = true) => {
    const { 1: complete, 2: reduce3 } = ensureTransducer(tx)(push());
    let done = false;
    return (x) => {
      if (!done) {
        let acc = reduce3([], x);
        done = isReduced(acc);
        if (done) {
          acc = complete(acc.deref());
        }
        return acc.length === 1 && unwrap ? acc[0] : acc.length > 0 ? acc : void 0;
      }
    };
  };

  // node_modules/@thi.ng/compose/comp.js
  function comp(...fns) {
    let [a, b, c, d, e, f, g, h, i, j] = fns;
    switch (fns.length) {
      case 0:
        illegalArity(0);
      case 1:
        return a;
      case 2:
        return (...xs) => a(b(...xs));
      case 3:
        return (...xs) => a(b(c(...xs)));
      case 4:
        return (...xs) => a(b(c(d(...xs))));
      case 5:
        return (...xs) => a(b(c(d(e(...xs)))));
      case 6:
        return (...xs) => a(b(c(d(e(f(...xs))))));
      case 7:
        return (...xs) => a(b(c(d(e(f(g(...xs)))))));
      case 8:
        return (...xs) => a(b(c(d(e(f(g(h(...xs))))))));
      case 9:
        return (...xs) => a(b(c(d(e(f(g(h(i(...xs)))))))));
      case 10:
      default:
        const fn = (...xs) => a(b(c(d(e(f(g(h(i(j(...xs))))))))));
        return fns.length === 10 ? fn : comp(fn, ...fns.slice(10));
    }
  }
  function compL(...fns) {
    return comp.apply(null, fns.reverse());
  }
  var compI = compL;

  // node_modules/@thi.ng/transducers/comp.js
  function comp2(...fns) {
    fns = fns.map(ensureTransducer);
    return comp.apply(null, fns);
  }

  // node_modules/@thi.ng/transducers/consume.js
  var consume = (src) => {
    for (let _ of src)
      ;
  };

  // node_modules/@thi.ng/checks/is-function.js
  var isFunction = (x) => typeof x === "function";

  // node_modules/@thi.ng/transducers/deep-transform.js
  var deepTransform = (spec) => {
    if (isFunction(spec)) {
      return spec;
    }
    const mapfns = Object.keys(spec[1] || {}).reduce(
      (acc, k) => (acc[k] = deepTransform(spec[1][k]), acc),
      {}
    );
    return (x) => {
      const res = { ...x };
      for (let k in mapfns) {
        res[k] = mapfns[k](res[k]);
      }
      return spec[0](res);
    };
  };

  // node_modules/@thi.ng/transducers/juxtr.js
  function juxtR(...rs) {
    let [a, b, c] = rs;
    const n = rs.length;
    switch (n) {
      case 1: {
        const r = a[2];
        return [
          () => [a[0]()],
          (acc) => [a[1](acc[0])],
          (acc, x) => {
            const aa1 = r(acc[0], x);
            if (isReduced(aa1)) {
              return reduced([unreduced(aa1)]);
            }
            return [aa1];
          }
        ];
      }
      case 2: {
        const ra = a[2];
        const rb = b[2];
        return [
          () => [a[0](), b[0]()],
          (acc) => [a[1](acc[0]), b[1](acc[1])],
          (acc, x) => {
            const aa1 = ra(acc[0], x);
            const aa2 = rb(acc[1], x);
            if (isReduced(aa1) || isReduced(aa2)) {
              return reduced([unreduced(aa1), unreduced(aa2)]);
            }
            return [aa1, aa2];
          }
        ];
      }
      case 3: {
        const ra = a[2];
        const rb = b[2];
        const rc = c[2];
        return [
          () => [a[0](), b[0](), c[0]()],
          (acc) => [a[1](acc[0]), b[1](acc[1]), c[1](acc[2])],
          (acc, x) => {
            const aa1 = ra(acc[0], x);
            const aa2 = rb(acc[1], x);
            const aa3 = rc(acc[2], x);
            if (isReduced(aa1) || isReduced(aa2) || isReduced(aa3)) {
              return reduced([
                unreduced(aa1),
                unreduced(aa2),
                unreduced(aa3)
              ]);
            }
            return [aa1, aa2, aa3];
          }
        ];
      }
      default:
        return [
          () => rs.map((r) => r[0]()),
          (acc) => rs.map((r, i) => r[1](acc[i])),
          (acc, x) => {
            let done = false;
            const res = [];
            for (let i = 0; i < n; i++) {
              let a2 = rs[i][2](acc[i], x);
              if (isReduced(a2)) {
                done = true;
                a2 = unreduced(a2);
              }
              res[i] = a2;
            }
            return done ? reduced(res) : res;
          }
        ];
    }
  }

  // node_modules/@thi.ng/transducers/lookup.js
  var lookup1d = (src) => (i) => src[i];
  var lookup2d = (src, width) => (i) => src[i[0] + i[1] * width];
  var lookup3d = (src, width, height) => {
    const stridez = width * height;
    return (i) => src[i[0] + i[1] * width + i[2] * stridez];
  };

  // node_modules/@thi.ng/transducers/renamer.js
  var renamer = (kmap) => {
    const ks = Object.keys(kmap);
    const [a2, b2, c2] = ks;
    const [a1, b1, c1] = ks.map(
      (k) => kmap[k] === true ? k : kmap[k]
    );
    switch (ks.length) {
      case 3:
        return (x) => {
          const res = {};
          let v;
          (v = x[c1]) !== void 0 && (res[c2] = v);
          (v = x[b1]) !== void 0 && (res[b2] = v);
          (v = x[a1]) !== void 0 && (res[a2] = v);
          return res;
        };
      case 2:
        return (x) => {
          const res = {};
          let v;
          (v = x[b1]) !== void 0 && (res[b2] = v);
          (v = x[a1]) !== void 0 && (res[a2] = v);
          return res;
        };
      case 1:
        return (x) => {
          const res = {};
          let v;
          (v = x[a1]) !== void 0 && (res[a2] = v);
          return res;
        };
      default:
        return (x) => {
          let k, kk, v;
          const res = {};
          for (let i = ks.length - 1; i >= 0; i--) {
            k = ks[i];
            kk = kmap[k];
            (v = x[kk === true ? k : kk]) !== void 0 && (res[k] = v);
          }
          return res;
        };
    }
  };

  // node_modules/@thi.ng/transducers/key-selector.js
  var keySelector = (keys2) => renamer(keys2.reduce((acc, x) => (acc[x] = x, acc), {}));

  // node_modules/@thi.ng/transducers/internal/mathop.js
  var __mathop = (rfn, fn, initDefault, args) => {
    const res = $$reduce(rfn, args);
    if (res !== void 0) {
      return res;
    }
    const init = args[0] || initDefault;
    return reducer(() => init, fn);
  };

  // node_modules/@thi.ng/transducers/add.js
  function add(...args) {
    return __mathop(add, (acc, x) => acc + x, 0, args);
  }

  // node_modules/@thi.ng/transducers/assoc-map.js
  function assocMap(xs) {
    return xs ? reduce(assocMap(), xs) : reducer(
      () => /* @__PURE__ */ new Map(),
      (acc, [k, v]) => acc.set(k, v)
    );
  }

  // node_modules/@thi.ng/transducers/assoc-obj.js
  function assocObj(xs) {
    return xs ? reduce(assocObj(), xs) : reducer(
      () => ({}),
      (acc, [k, v]) => (acc[k] = v, acc)
    );
  }

  // node_modules/@thi.ng/transducers/auto-obj.js
  function autoObj(prefix, xs) {
    let id = 0;
    return xs ? reduce(autoObj(prefix), xs) : reducer(
      () => ({}),
      (acc, v) => (acc[prefix + id++] = v, acc)
    );
  }

  // node_modules/@thi.ng/transducers/conj.js
  function conj(xs) {
    return xs ? reduce(conj(), xs) : reducer(
      () => /* @__PURE__ */ new Set(),
      (acc, x) => acc.add(x)
    );
  }

  // node_modules/@thi.ng/transducers/count.js
  function count(...args) {
    const res = $$reduce(count, args);
    if (res !== void 0) {
      return res;
    }
    let offset = args[0] || 0;
    let step2 = args[1] || 1;
    return reducer(
      () => offset,
      (acc, _) => acc + step2
    );
  }

  // node_modules/@thi.ng/transducers/div.js
  function div(init, xs) {
    return xs ? reduce(div(init), xs) : reducer(
      () => init,
      (acc, x) => acc / x
    );
  }

  // node_modules/@thi.ng/transducers/every.js
  function every(...args) {
    const res = $$reduce(every, args);
    if (res !== void 0) {
      return res;
    }
    const pred = args[0];
    return reducer(
      () => true,
      pred ? (acc, x) => pred(x) ? acc : reduced(false) : (acc, x) => x ? acc : reduced(false)
    );
  }

  // node_modules/@thi.ng/transducers/fill.js
  function fill(...args) {
    const res = $$reduce(fill, args);
    if (res !== void 0) {
      return res;
    }
    let start = args[0] || 0;
    return reducer(
      () => [],
      (acc, x) => (acc[start++] = x, acc)
    );
  }
  function fillN(...args) {
    return fill(...args);
  }

  // node_modules/@thi.ng/compose/identity.js
  var identity = (x) => x;

  // node_modules/@thi.ng/transducers/internal/group-opts.js
  var __groupByOpts = (opts) => ({
    key: (x) => x,
    group: push(),
    ...opts
  });

  // node_modules/@thi.ng/transducers/group-by-map.js
  function groupByMap(...args) {
    const res = $$reduce(groupByMap, args);
    if (res !== void 0) {
      return res;
    }
    const opts = __groupByOpts(args[0]);
    const [init, complete, reduce3] = opts.group;
    return [
      () => /* @__PURE__ */ new Map(),
      (acc) => {
        for (let k of acc.keys()) {
          acc.set(k, complete(acc.get(k)));
        }
        return acc;
      },
      (acc, x) => {
        const k = opts.key(x);
        return acc.set(
          k,
          acc.has(k) ? reduce3(acc.get(k), x) : reduce3(init(), x)
        );
      }
    ];
  }

  // node_modules/@thi.ng/transducers/frequencies.js
  function frequencies(...args) {
    return $$reduce(frequencies, args) || groupByMap({ key: args[0] || identity, group: count() });
  }

  // node_modules/@thi.ng/transducers/group-by-obj.js
  function groupByObj(...args) {
    const res = $$reduce(groupByObj, args);
    if (res) {
      return res;
    }
    const opts = __groupByOpts(args[0]);
    const [_init, complete, _reduce] = opts.group;
    return [
      () => ({}),
      (acc) => {
        for (let k in acc) {
          acc[k] = complete(acc[k]);
        }
        return acc;
      },
      (acc, x) => {
        const k = opts.key(x);
        acc[k] = acc[k] ? _reduce(acc[k], x) : _reduce(_init(), x);
        return acc;
      }
    ];
  }

  // node_modules/@thi.ng/transducers/group-binary.js
  var branchPred = (key, b, l, r) => (x) => key(x) & b ? r : l;
  var groupBinary = (bits, key, branch, leaf, left = "l", right = "r") => {
    const init = branch || (() => ({}));
    let rfn = groupByObj({
      key: branchPred(key, 1, left, right),
      group: leaf || push()
    });
    for (let i = 2, maxIndex = 1 << bits; i < maxIndex; i <<= 1) {
      rfn = groupByObj({
        key: branchPred(key, i, left, right),
        group: [init, rfn[1], rfn[2]]
      });
    }
    return [init, rfn[1], rfn[2]];
  };

  // node_modules/@thi.ng/transducers/last.js
  function last(xs) {
    return xs ? reduce(last(), xs) : reducer(NO_OP, (_, x) => x);
  }

  // node_modules/@thi.ng/transducers/max.js
  function max(xs) {
    return xs ? reduce(max(), xs) : reducer(
      () => -Infinity,
      (acc, x) => Math.max(acc, x)
    );
  }

  // node_modules/@thi.ng/transducers/max-compare.js
  function maxCompare(...args) {
    const res = $$reduce(maxCompare, args);
    if (res !== void 0) {
      return res;
    }
    const init = args[0];
    const cmp = args[1] || compare;
    return reducer(init, (acc, x) => cmp(acc, x) >= 0 ? acc : x);
  }

  // node_modules/@thi.ng/transducers/max-mag.js
  function maxMag(xs) {
    return xs ? reduce(maxMag(), xs) : reducer(
      () => 0,
      (acc, x) => Math.abs(x) > Math.abs(acc) ? x : acc
    );
  }

  // node_modules/@thi.ng/transducers/mean.js
  function mean(xs) {
    let n = 1;
    return xs ? reduce(mean(), xs) : [
      () => n = 0,
      (acc) => n > 1 ? acc / n : acc,
      (acc, x) => (n++, acc + x)
    ];
  }

  // node_modules/@thi.ng/transducers/min.js
  function min(xs) {
    return xs ? reduce(min(), xs) : reducer(
      () => Infinity,
      (acc, x) => Math.min(acc, x)
    );
  }

  // node_modules/@thi.ng/transducers/min-compare.js
  function minCompare(...args) {
    const res = $$reduce(minCompare, args);
    if (res !== void 0) {
      return res;
    }
    const init = args[0];
    const cmp = args[1] || compare;
    return reducer(init, (acc, x) => cmp(acc, x) <= 0 ? acc : x);
  }

  // node_modules/@thi.ng/transducers/min-mag.js
  function minMag(xs) {
    return xs ? reduce(minMag(), xs) : reducer(
      () => Infinity,
      (acc, x) => Math.abs(x) < Math.abs(acc) ? x : acc
    );
  }

  // node_modules/@thi.ng/transducers/min-max.js
  var minMax = () => juxtR(min(), max());

  // node_modules/@thi.ng/transducers/mul.js
  function mul(...args) {
    return __mathop(mul, (acc, x) => acc * x, 1, args);
  }

  // node_modules/@thi.ng/transducers/norm-count.js
  function normCount(...args) {
    const res = $$reduce(normCount, args);
    if (res !== void 0) {
      return res;
    }
    const norm = args[0];
    return [() => 0, (acc) => acc / norm, (acc) => acc + 1];
  }

  // node_modules/@thi.ng/transducers/norm-frequencies.js
  function normFrequencies(...args) {
    return $$reduce(normFrequencies, args) || groupByMap({
      key: args[1] || identity,
      group: normCount(args[0])
    });
  }

  // node_modules/@thi.ng/transducers/norm-frequencies-auto.js
  function normFrequenciesAuto(...args) {
    const res = $$reduce(normFrequenciesAuto, args);
    if (res !== void 0) {
      return res;
    }
    const [init, complete, reduce3] = frequencies(...args);
    let norm = 0;
    return [
      init,
      (acc) => {
        acc = complete(acc);
        for (let p of acc) {
          acc.set(p[0], p[1] / norm);
        }
        return acc;
      },
      (acc, x) => (norm++, reduce3(acc, x))
    ];
  }

  // node_modules/@thi.ng/transducers/push-copy.js
  var pushCopy = () => reducer(
    () => [],
    (acc, x) => ((acc = acc.slice()).push(x), acc)
  );

  // node_modules/@thi.ng/transducers/push-sort.js
  function pushSort(cmp = compare, xs) {
    return xs ? [...xs].sort(cmp) : [
      () => [],
      (acc) => acc.sort(cmp),
      (acc, x) => (acc.push(x), acc)
    ];
  }

  // node_modules/@thi.ng/transducers/reductions.js
  function reductions(rfn, xs) {
    const [init, complete, _reduce] = rfn;
    return xs ? reduce(reductions(rfn), xs) : [
      () => [init()],
      (acc) => (acc[acc.length - 1] = complete(acc[acc.length - 1]), acc),
      (acc, x) => {
        const res = _reduce(acc[acc.length - 1], x);
        if (isReduced(res)) {
          acc.push(res.deref());
          return reduced(acc);
        }
        acc.push(res);
        return acc;
      }
    ];
  }

  // node_modules/@thi.ng/transducers/some.js
  function some(...args) {
    const res = $$reduce(some, args);
    if (res !== void 0) {
      return res;
    }
    const pred = args[0];
    return reducer(
      () => false,
      pred ? (acc, x) => pred(x) ? reduced(true) : acc : (acc, x) => x ? reduced(true) : acc
    );
  }

  // node_modules/@thi.ng/transducers/str.js
  function str(sep, xs) {
    sep = sep || "";
    let first = true;
    return xs ? [...xs].join(sep) : reducer(
      () => "",
      (acc, x) => (acc = first ? acc + x : acc + sep + x, first = false, acc)
    );
  }

  // node_modules/@thi.ng/transducers/sub.js
  function sub(...args) {
    return __mathop(sub, (acc, x) => acc - x, 0, args);
  }

  // node_modules/@thi.ng/transducers/benchmark.js
  function benchmark(src) {
    return isIterable(src) ? iterator1(benchmark(), src) : (rfn) => {
      const r = rfn[2];
      let prev = Date.now();
      return compR(rfn, (acc, _) => {
        const t = Date.now();
        const x = t - prev;
        prev = t;
        return r(acc, x);
      });
    };
  }

  // node_modules/@thi.ng/transducers/cat.js
  var cat = () => (rfn) => {
    const r = rfn[2];
    return compR(rfn, (acc, x) => {
      if (x) {
        for (let y of unreduced(x)) {
          acc = r(acc, y);
          if (isReduced(acc)) {
            break;
          }
        }
      }
      return isReduced(x) ? ensureReduced(acc) : acc;
    });
  };

  // node_modules/@thi.ng/transducers/converge.js
  function converge(...args) {
    return __iter(converge, args) || ((rfn) => {
      const r = rfn[2];
      const pred = args[0];
      let prev = SEMAPHORE;
      let done = false;
      return compR(rfn, (acc, x) => {
        if (done || prev !== SEMAPHORE && pred(prev, x)) {
          done = true;
          return ensureReduced(r(acc, x));
        }
        prev = x;
        return r(acc, x);
      });
    });
  }

  // node_modules/@thi.ng/transducers/range.js
  function range(from, to, step2) {
    return new Range(from, to, step2);
  }
  var Range = class {
    from;
    to;
    step;
    constructor(from, to, step2) {
      if (from === void 0) {
        from = 0;
        to = Infinity;
      } else if (to === void 0) {
        to = from;
        from = 0;
      }
      step2 = step2 === void 0 ? from < to ? 1 : -1 : step2;
      this.from = from;
      this.to = to;
      this.step = step2;
    }
    *[Symbol.iterator]() {
      let { from, to, step: step2 } = this;
      if (step2 > 0) {
        while (from < to) {
          yield from;
          from += step2;
        }
      } else if (step2 < 0) {
        while (from > to) {
          yield from;
          from += step2;
        }
      }
    }
    $reduce(rfn, acc) {
      const step2 = this.step;
      if (step2 > 0) {
        for (let i = this.from, n = this.to; i < n && !isReduced(acc); i += step2) {
          acc = rfn(acc, i);
        }
      } else {
        for (let i = this.from, n = this.to; i > n && !isReduced(acc); i += step2) {
          acc = rfn(acc, i);
        }
      }
      return acc;
    }
  };

  // node_modules/@thi.ng/transducers/range2d.js
  function* range2d(...args) {
    let fromX, toX, stepX;
    let fromY, toY, stepY;
    switch (args.length) {
      case 6:
        stepX = args[4];
        stepY = args[5];
      case 4:
        [fromX, toX, fromY, toY] = args;
        break;
      case 2:
        [toX, toY] = args;
        fromX = fromY = 0;
        break;
      default:
        illegalArity(args.length);
    }
    const rx = range(fromX, toX, stepX);
    for (let y of range(fromY, toY, stepY)) {
      for (let x of rx) {
        yield [x, y];
      }
    }
  }

  // node_modules/@thi.ng/transducers/zip.js
  function* zip(...src) {
    const iters = src.map((s) => s[Symbol.iterator]());
    while (true) {
      const tuple = [];
      for (let i of iters) {
        let v = i.next();
        if (v.done) {
          return;
        }
        tuple.push(v.value);
      }
      yield tuple;
    }
  }

  // node_modules/@thi.ng/transducers/convolve.js
  var buildKernel1d = (weights, w) => {
    const w2 = w >> 1;
    return [...zip(weights, range(-w2, w2 + 1))];
  };
  var buildKernel2d = (weights, w, h = w) => {
    const w2 = w >> 1;
    const h2 = h >> 1;
    return [...zip(weights, range2d(-w2, w2 + 1, -h2, h2 + 1))];
  };
  var kernelLookup1d = (src, x, width, wrap, border) => wrap ? ({ 0: w, 1: ox }) => {
    const xx = x < -ox ? width + ox : x >= width - ox ? ox - 1 : x + ox;
    return w * src[xx];
  } : ({ 0: w, 1: ox }) => {
    return x < -ox || x >= width - ox ? border : w * src[x + ox];
  };
  var kernelLookup2d = (src, x, y, width, height, wrap, border) => wrap ? ({ 0: w, 1: { 0: ox, 1: oy } }) => {
    const xx = x < -ox ? width + ox : x >= width - ox ? ox - 1 : x + ox;
    const yy = y < -oy ? height + oy : y >= height - oy ? oy - 1 : y + oy;
    return w * src[yy * width + xx];
  } : ({ 0: w, 1: { 0: ox, 1: oy } }) => {
    return x < -ox || y < -oy || x >= width - ox || y >= height - oy ? border : w * src[(y + oy) * width + x + ox];
  };
  var kernelError = () => illegalArgs(`no kernel or kernel config`);
  function convolve1d(opts, indices) {
    if (indices) {
      return iterator1(convolve1d(opts), indices);
    }
    const { src, width } = opts;
    const wrap = opts.wrap !== false;
    const border = opts.border || 0;
    const rfn = opts.reduce || add;
    let kernel = opts.kernel;
    if (!kernel) {
      !(opts.weights && opts.kwidth) && kernelError();
      kernel = buildKernel1d(opts.weights, opts.kwidth);
    }
    return map(
      (p) => transduce(
        map(kernelLookup1d(src, p, width, wrap, border)),
        rfn(),
        kernel
      )
    );
  }
  function convolve2d(opts, indices) {
    if (indices) {
      return iterator1(convolve2d(opts), indices);
    }
    const { src, width, height } = opts;
    const wrap = opts.wrap !== false;
    const border = opts.border || 0;
    const rfn = opts.reduce || add;
    let kernel = opts.kernel;
    if (!kernel) {
      !(opts.weights && opts.kwidth && opts.kheight) && kernelError();
      kernel = buildKernel2d(opts.weights, opts.kwidth, opts.kheight);
    }
    return map(
      (p) => transduce(
        map(kernelLookup2d(src, p[0], p[1], width, height, wrap, border)),
        rfn(),
        kernel
      )
    );
  }

  // node_modules/@thi.ng/transducers/dedupe.js
  function dedupe(...args) {
    return __iter(dedupe, args) || ((rfn) => {
      const r = rfn[2];
      const equiv2 = args[0];
      let prev = SEMAPHORE;
      return compR(
        rfn,
        equiv2 ? (acc, x) => {
          acc = prev !== SEMAPHORE && equiv2(prev, x) ? acc : r(acc, x);
          prev = x;
          return acc;
        } : (acc, x) => {
          acc = prev === x ? acc : r(acc, x);
          prev = x;
          return acc;
        }
      );
    });
  }

  // node_modules/@thi.ng/compose/delayed.js
  var delayed = (x, t) => new Promise((resolve2) => setTimeout(() => resolve2(x), t));

  // node_modules/@thi.ng/transducers/delayed.js
  var delayed2 = (t) => map((x) => delayed(x, t));

  // node_modules/@thi.ng/transducers/distinct.js
  function distinct(...args) {
    return __iter(distinct, args) || ((rfn) => {
      const r = rfn[2];
      const opts = args[0] || {};
      const key = opts.key;
      const seen = (opts.cache || (() => /* @__PURE__ */ new Set()))();
      return compR(
        rfn,
        key ? (acc, x) => {
          const k = key(x);
          return !seen.has(k) ? (seen.add(k), r(acc, x)) : acc;
        } : (acc, x) => !seen.has(x) ? (seen.add(x), r(acc, x)) : acc
      );
    });
  }

  // node_modules/@thi.ng/math/interval.js
  var clamp0 = (x) => x > 0 ? x : 0;
  var inRange = (x, min2, max2) => x >= min2 && x <= max2;

  // node_modules/@thi.ng/transducers/throttle.js
  function throttle(pred, src) {
    return isIterable(src) ? iterator1(throttle(pred), src) : (rfn) => {
      const r = rfn[2];
      const _pred = pred();
      return compR(rfn, (acc, x) => _pred(x) ? r(acc, x) : acc);
    };
  }

  // node_modules/@thi.ng/transducers/drop-nth.js
  function dropNth(n, src) {
    if (isIterable(src)) {
      return iterator1(dropNth(n), src);
    }
    n = clamp0(n - 1);
    return throttle(() => {
      let skip = n;
      return () => skip-- > 0 ? true : (skip = n, false);
    });
  }

  // node_modules/@thi.ng/transducers/drop-while.js
  function dropWhile(...args) {
    return __iter(dropWhile, args) || ((rfn) => {
      const r = rfn[2];
      const pred = args[0];
      let ok = true;
      return compR(
        rfn,
        (acc, x) => (ok = ok && pred(x)) ? acc : r(acc, x)
      );
    });
  }

  // node_modules/@thi.ng/transducers/drop.js
  function drop(n, src) {
    return isIterable(src) ? iterator1(drop(n), src) : (rfn) => {
      const r = rfn[2];
      let m = n;
      return compR(
        rfn,
        (acc, x) => m > 0 ? (m--, acc) : r(acc, x)
      );
    };
  }

  // node_modules/@thi.ng/transducers/duplicate.js
  function duplicate(n = 1, src) {
    return isIterable(src) ? iterator(duplicate(n), src) : (rfn) => {
      const r = rfn[2];
      return compR(rfn, (acc, x) => {
        for (let i = n; i >= 0 && !isReduced(acc); i--) {
          acc = r(acc, x);
        }
        return acc;
      });
    };
  }

  // node_modules/@thi.ng/transducers/filter.js
  function filter(pred, src) {
    return isIterable(src) ? iterator1(filter(pred), src) : (rfn) => {
      const r = rfn[2];
      return compR(rfn, (acc, x) => pred(x) ? r(acc, x) : acc);
    };
  }

  // node_modules/@thi.ng/arrays/fuzzy-match.js
  var fuzzyMatch = (domain, query, equiv2 = equiv) => {
    const nd = domain.length;
    const nq = query.length;
    if (nq > nd) {
      return false;
    }
    if (nq === nd) {
      return equiv2(query, domain);
    }
    next:
      for (let i = 0, j = 0; i < nq; i++) {
        const q = query[i];
        while (j < nd) {
          if (equiv2(domain[j++], q)) {
            continue next;
          }
        }
        return false;
      }
    return true;
  };

  // node_modules/@thi.ng/transducers/filter-fuzzy.js
  function filterFuzzy(...args) {
    const iter = args.length > 1 && __iter(filterFuzzy, args);
    if (iter) {
      return iter;
    }
    const query = args[0];
    const { key, equiv: equiv2 } = args[1] || {};
    return filter(
      (x) => fuzzyMatch(key != null ? key(x) : x, query, equiv2)
    );
  }

  // node_modules/@thi.ng/checks/is-string.js
  var isString = (x) => typeof x === "string";

  // node_modules/@thi.ng/transducers/flatten-with.js
  function flattenWith(fn, src) {
    return isIterable(src) ? iterator(flattenWith(fn), isString(src) ? [src] : src) : (rfn) => {
      const reduce3 = rfn[2];
      const flatten3 = (acc, x) => {
        const xx = fn(x);
        if (xx) {
          for (let y of xx) {
            acc = flatten3(acc, y);
            if (isReduced(acc)) {
              break;
            }
          }
          return acc;
        }
        return reduce3(acc, x);
      };
      return compR(rfn, flatten3);
    };
  }

  // node_modules/@thi.ng/checks/is-not-string-iterable.js
  var isNotStringAndIterable = (x) => x != null && typeof x !== "string" && typeof x[Symbol.iterator] === "function";

  // node_modules/@thi.ng/transducers/flatten.js
  function flatten(src) {
    return flattenWith(
      (x) => isNotStringAndIterable(x) ? x : void 0,
      src
    );
  }

  // node_modules/@thi.ng/api/fn.js
  var identity2 = (x) => x;

  // node_modules/@thi.ng/transducers/mapcat.js
  function mapcat(fn, src) {
    return isIterable(src) ? iterator(mapcat(fn), src) : comp2(map(fn), cat());
  }

  // node_modules/@thi.ng/transducers/flatten1.js
  function flatten1(src) {
    return mapcat(identity2, src);
  }

  // node_modules/@thi.ng/transducers/map-indexed.js
  function mapIndexed(...args) {
    return __iter(mapIndexed, args) || ((rfn) => {
      const r = rfn[2];
      const fn = args[0];
      let i = args[1] || 0;
      return compR(rfn, (acc, x) => r(acc, fn(i++, x)));
    });
  }

  // node_modules/@thi.ng/transducers/indexed.js
  function indexed(...args) {
    const iter = __iter(indexed, args);
    if (iter) {
      return iter;
    }
    const from = args[0] || 0;
    return mapIndexed((i, x) => [from + i, x]);
  }

  // node_modules/@thi.ng/transducers/interleave.js
  function interleave(sep, src) {
    return isIterable(src) ? iterator(interleave(sep), src) : (rfn) => {
      const r = rfn[2];
      const _sep = typeof sep === "function" ? sep : () => sep;
      return compR(rfn, (acc, x) => {
        acc = r(acc, _sep());
        return isReduced(acc) ? acc : r(acc, x);
      });
    };
  }

  // node_modules/@thi.ng/transducers/norm-range.js
  function* normRange(n, includeLast = true) {
    if (n > 0) {
      for (let i = 0, m = includeLast ? n + 1 : n; i < m; i++) {
        yield i / n;
      }
    }
  }
  function* normRange2d(nx, ny, includeLastX = true, includeLastY = true) {
    const rx = [...normRange(nx, includeLastX)];
    for (let y of normRange(ny, includeLastY)) {
      yield* map((x) => [x, y], rx);
    }
  }
  function* normRange3d(nx, ny, nz, includeLastX = true, includeLastY = true, includeLastZ = true) {
    const sliceXY = [...normRange2d(nx, ny, includeLastX, includeLastY)];
    for (let z of normRange(nz, includeLastZ)) {
      yield* map((xy) => [...xy, z], sliceXY);
    }
  }

  // node_modules/@thi.ng/transducers/partition.js
  function partition(...args) {
    const iter = __iter(partition, args, iterator);
    if (iter) {
      return iter;
    }
    let size = args[0], all, step2;
    if (typeof args[1] == "number") {
      step2 = args[1];
      all = args[2];
    } else {
      step2 = size;
      all = args[1];
    }
    return ([init, complete, reduce3]) => {
      let buf = [];
      let skip = 0;
      return [
        init,
        (acc) => {
          if (all && buf.length > 0) {
            acc = reduce3(acc, buf);
            buf = [];
          }
          return complete(acc);
        },
        (acc, x) => {
          if (skip <= 0) {
            if (buf.length < size) {
              buf.push(x);
            }
            if (buf.length === size) {
              acc = reduce3(acc, buf);
              buf = step2 < size ? buf.slice(step2) : [];
              skip = step2 - size;
            }
          } else {
            skip--;
          }
          return acc;
        }
      ];
    };
  }

  // node_modules/@thi.ng/transducers/interpolate.js
  function interpolate(fn, window2, n, src) {
    return isIterable(src) ? iterator(interpolate(fn, window2, n), src) : comp2(
      partition(window2, 1),
      mapcat((chunk) => map((t) => fn(chunk, t), normRange(n, false)))
    );
  }

  // node_modules/@thi.ng/math/mix.js
  var mix = (a, b, t) => a + (b - a) * t;
  var mixHermite = (a, b, c, d, t) => {
    const y1 = 0.5 * (c - a);
    const y2 = 1.5 * (b - c) + 0.5 * (d - a);
    return ((y2 * t + a - b + y1 - y2) * t + y1) * t + b;
  };

  // node_modules/@thi.ng/transducers/interpolate-hermite.js
  function interpolateHermite(n, src) {
    return interpolate(
      (chunk, t) => mixHermite(...chunk, t),
      4,
      n,
      src
    );
  }

  // node_modules/@thi.ng/transducers/interpolate-linear.js
  function interpolateLinear(n, src) {
    return interpolate(
      (chunk, t) => mix(...chunk, t),
      2,
      n,
      src
    );
  }

  // node_modules/@thi.ng/transducers/interpose.js
  function interpose(sep, src) {
    return isIterable(src) ? iterator(interpose(sep), src) : (rfn) => {
      const r = rfn[2];
      const _sep = typeof sep === "function" ? sep : () => sep;
      let first = true;
      return compR(rfn, (acc, x) => {
        if (first) {
          first = false;
          return r(acc, x);
        }
        acc = r(acc, _sep());
        return isReduced(acc) ? acc : r(acc, x);
      });
    };
  }

  // node_modules/@thi.ng/transducers/keep.js
  function keep(...args) {
    return __iter(keep, args) || ((rfn) => {
      const r = rfn[2];
      const pred = args[0] || identity;
      return compR(
        rfn,
        (acc, x) => pred(x) != null ? r(acc, x) : acc
      );
    });
  }

  // node_modules/@thi.ng/transducers/labeled.js
  function labeled(id, src) {
    return isIterable(src) ? iterator1(labeled(id), src) : map(isFunction(id) ? (x) => [id(x), x] : (x) => [id, x]);
  }

  // node_modules/@thi.ng/transducers/length.js
  function length(n = 0, src) {
    return isIterable(src) ? iterator1(length(n), src) : map(
      n === 0 ? (x) => x.length : (x) => x.length + n
    );
  }

  // node_modules/@thi.ng/transducers/map-deep.js
  function mapDeep(spec, src) {
    return isIterable(src) ? iterator1(mapDeep(spec), src) : map(deepTransform(spec));
  }

  // node_modules/@thi.ng/transducers/map-keys.js
  function mapKeys(...args) {
    const iter = __iter(mapKeys, args);
    if (iter) {
      return iter;
    }
    const keys2 = args[0];
    const copy = args[1] !== false;
    return map((x) => {
      const res = copy ? Object.assign({}, x) : x;
      for (let k in keys2) {
        res[k] = keys2[k](x[k], x);
      }
      return res;
    });
  }

  // node_modules/@thi.ng/transducers/map-nth.js
  function mapNth(...args) {
    const iter = __iter(mapNth, args);
    if (iter) {
      return iter;
    }
    let n = args[0] - 1;
    let offset;
    let fn;
    if (typeof args[1] === "number") {
      offset = args[1];
      fn = args[2];
    } else {
      fn = args[1];
      offset = 0;
    }
    return (rfn) => {
      const r = rfn[2];
      let skip = 0, off = offset;
      return compR(rfn, (acc, x) => {
        if (off === 0) {
          if (skip === 0) {
            skip = n;
            return r(acc, fn(x));
          }
          skip--;
        } else {
          off--;
        }
        return r(acc, x);
      });
    };
  }

  // node_modules/@thi.ng/transducers/map-vals.js
  function mapVals(...args) {
    const iter = __iter(mapVals, args);
    if (iter) {
      return iter;
    }
    const fn = args[0];
    const copy = args[1] !== false;
    return map((x) => {
      const res = copy ? {} : x;
      for (let k in x) {
        res[k] = fn(x[k]);
      }
      return res;
    });
  }

  // node_modules/@thi.ng/transducers/mapcat-indexed.js
  function mapcatIndexed(...args) {
    return __iter(mapcatIndexed, args, iterator) || comp2(mapIndexed(args[0], args[1]), cat());
  }

  // node_modules/@thi.ng/transducers/take.js
  function take(n, src) {
    return isIterable(src) ? iterator(take(n), src) : (rfn) => {
      const r = rfn[2];
      let m = n;
      return compR(
        rfn,
        (acc, x) => --m > 0 ? r(acc, x) : m === 0 ? ensureReduced(r(acc, x)) : reduced(acc)
      );
    };
  }

  // node_modules/@thi.ng/transducers/match-first.js
  function matchFirst(pred, src) {
    return isIterable(src) ? [...iterator1(matchFirst(pred), src)][0] : comp2(filter(pred), take(1));
  }

  // node_modules/@thi.ng/transducers/internal/drain.js
  var __drain = (buf, complete, reduce3) => (acc) => {
    while (buf.length && !isReduced(acc)) {
      acc = reduce3(acc, buf.shift());
    }
    return complete(acc);
  };

  // node_modules/@thi.ng/transducers/take-last.js
  function takeLast(n, src) {
    return isIterable(src) ? iterator(takeLast(n), src) : ([init, complete, reduce3]) => {
      const buf = [];
      return [
        init,
        __drain(buf, complete, reduce3),
        (acc, x) => {
          if (buf.length === n) {
            buf.shift();
          }
          buf.push(x);
          return acc;
        }
      ];
    };
  }

  // node_modules/@thi.ng/transducers/match-last.js
  function matchLast(pred, src) {
    return isIterable(src) ? [...iterator(matchLast(pred), src)][0] : comp2(filter(pred), takeLast(1));
  }

  // node_modules/@thi.ng/transducers/moving-average.js
  function movingAverage(period, src) {
    return isIterable(src) ? iterator1(movingAverage(period), src) : (rfn) => {
      period |= 0;
      period < 2 && illegalArgs("period must be >= 2");
      const reduce3 = rfn[2];
      const window2 = [];
      let sum = 0;
      return compR(rfn, (acc, x) => {
        const n = window2.push(x);
        sum += x;
        n > period && (sum -= window2.shift());
        return n >= period ? reduce3(acc, sum / period) : acc;
      });
    };
  }

  // node_modules/@thi.ng/transducers/internal/sort-opts.js
  var __sortOpts = (opts) => ({
    key: (x) => x,
    compare,
    ...opts
  });

  // node_modules/@thi.ng/transducers/moving-median.js
  function movingMedian(...args) {
    const iter = __iter(movingMedian, args);
    if (iter) {
      return iter;
    }
    const { key, compare: compare2 } = __sortOpts(args[1]);
    const n = args[0];
    const m = n >> 1;
    return comp2(
      partition(n, 1, true),
      map(
        (window2) => window2.slice().sort((a, b) => compare2(key(a), key(b)))[m]
      )
    );
  }

  // node_modules/@thi.ng/compose/juxt.js
  function juxt(...fns) {
    const [a, b, c, d, e, f, g, h] = fns;
    switch (fns.length) {
      case 1:
        return (x) => [a(x)];
      case 2:
        return (x) => [a(x), b(x)];
      case 3:
        return (x) => [a(x), b(x), c(x)];
      case 4:
        return (x) => [a(x), b(x), c(x), d(x)];
      case 5:
        return (x) => [a(x), b(x), c(x), d(x), e(x)];
      case 6:
        return (x) => [a(x), b(x), c(x), d(x), e(x), f(x)];
      case 7:
        return (x) => [a(x), b(x), c(x), d(x), e(x), f(x), g(x)];
      case 8:
        return (x) => [a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x)];
      default:
        return (x) => {
          let res = new Array(fns.length);
          for (let i = fns.length; i-- > 0; ) {
            res[i] = fns[i](x);
          }
          return res;
        };
    }
  }

  // node_modules/@thi.ng/transducers/multiplex.js
  function multiplex(...args) {
    return map(
      juxt.apply(
        null,
        args.map(
          (xf) => Array.isArray(xf) ? step(xf[0], xf[1]) : step(xf)
        )
      )
    );
  }

  // node_modules/@thi.ng/checks/is-array.js
  var isArray = Array.isArray;

  // node_modules/@thi.ng/transducers/rename.js
  function rename(...args) {
    const iter = args.length > 2 && __iter(rename, args);
    if (iter) {
      return iter;
    }
    let kmap = args[0];
    if (isArray(kmap)) {
      kmap = kmap.reduce((acc, k, i) => (acc[k] = i, acc), {});
    }
    if (args[1]) {
      const ks = Object.keys(kmap);
      return map(
        (y) => transduce(
          comp2(
            map((k) => [k, y[kmap[k]]]),
            filter((x) => x[1] !== void 0)
          ),
          args[1],
          ks
        )
      );
    } else {
      return map(renamer(kmap));
    }
  }

  // node_modules/@thi.ng/transducers/multiplex-obj.js
  function multiplexObj(...args) {
    const iter = __iter(multiplexObj, args);
    if (iter) {
      return iter;
    }
    const [xforms, rfn] = args;
    const ks = Object.keys(xforms);
    return comp2(
      multiplex.apply(null, ks.map((k) => xforms[k])),
      rename(ks, rfn)
    );
  }

  // node_modules/@thi.ng/transducers/noop.js
  var noop = () => (rfn) => rfn;

  // node_modules/@thi.ng/transducers/pad-last.js
  function padLast(n, fill2, src) {
    return isIterable(src) ? iterator(padLast(n, fill2), src) : ([init, complete, reduce3]) => {
      let m = 0;
      return [
        init,
        (acc) => {
          let rem = m % n;
          if (rem > 0) {
            while (++rem <= n && !isReduced(acc)) {
              acc = reduce3(acc, fill2);
            }
          }
          return complete(acc);
        },
        (acc, x) => (m++, reduce3(acc, x))
      ];
    };
  }

  // node_modules/@thi.ng/transducers/page.js
  function page(...args) {
    return __iter(page, args) || comp2(drop(args[0] * (args[1] || 10)), take(args[1] || 10));
  }

  // node_modules/@thi.ng/transducers/partition-by.js
  function partitionBy(...args) {
    return __iter(partitionBy, args, iterator) || (([init, complete, reduce3]) => {
      const fn = args[0];
      const f = args[1] === true ? fn() : fn;
      let prev = SEMAPHORE;
      let chunk;
      return [
        init,
        (acc) => {
          if (chunk && chunk.length) {
            acc = reduce3(acc, chunk);
            chunk = null;
          }
          return complete(acc);
        },
        (acc, x) => {
          const curr = f(x);
          if (prev === SEMAPHORE) {
            prev = curr;
            chunk = [x];
          } else if (curr === prev) {
            chunk.push(x);
          } else {
            chunk && (acc = reduce3(acc, chunk));
            chunk = isReduced(acc) ? null : [x];
            prev = curr;
          }
          return acc;
        }
      ];
    });
  }

  // node_modules/@thi.ng/transducers/partition-of.js
  function partitionOf(sizes, src) {
    return isIterable(src) ? iterator(partitionOf(sizes), src) : partitionBy(() => {
      let i = 0, j = 0;
      return () => {
        if (i++ === sizes[j]) {
          i = 1;
          j = (j + 1) % sizes.length;
        }
        return j;
      };
    }, true);
  }

  // node_modules/@thi.ng/transducers/partition-sort.js
  function partitionSort(...args) {
    const iter = __iter(partitionSort, args, iterator);
    if (iter) {
      return iter;
    }
    const { key, compare: compare2 } = __sortOpts(args[1]);
    return comp2(
      partition(args[0], true),
      mapcat(
        (window2) => window2.slice().sort((a, b) => compare2(key(a), key(b)))
      )
    );
  }

  // node_modules/@thi.ng/errors/illegal-state.js
  var IllegalStateError = defError(() => "illegal state");
  var illegalState = (msg) => {
    throw new IllegalStateError(msg);
  };

  // node_modules/@thi.ng/transducers/partition-sync.js
  function partitionSync(...args) {
    const iter = __iter(partitionSync, args, iterator);
    if (iter)
      return iter;
    const { key, mergeOnly, reset, all, backPressure } = {
      key: identity,
      mergeOnly: false,
      reset: true,
      all: true,
      backPressure: 0,
      ...args[1]
    };
    const requiredKeys = isArray(args[0]) ? new Set(args[0]) : args[0];
    const currKeys = /* @__PURE__ */ new Set();
    const cache = /* @__PURE__ */ new Map();
    let curr = {};
    const xform = ([init, complete, reduce3]) => {
      let first = true;
      if (mergeOnly || backPressure < 1) {
        return [
          init,
          (acc) => {
            if (reset && all && currKeys.size > 0 || !reset && first) {
              acc = reduce3(acc, curr);
              curr = {};
              currKeys.clear();
              first = false;
            }
            return complete(acc);
          },
          (acc, x) => {
            const k = key(x);
            if (requiredKeys.has(k)) {
              curr[k] = x;
              currKeys.add(k);
              if (mergeOnly || requiredInputs(requiredKeys, currKeys)) {
                acc = reduce3(acc, curr);
                first = false;
                if (reset) {
                  curr = {};
                  currKeys.clear();
                } else {
                  curr = { ...curr };
                }
              }
            }
            return acc;
          }
        ];
      } else {
        return [
          init,
          (acc) => {
            if (all && currKeys.size > 0) {
              acc = reduce3(acc, collect(cache, currKeys));
              cache.clear();
              currKeys.clear();
            }
            return complete(acc);
          },
          (acc, x) => {
            const k = key(x);
            if (requiredKeys.has(k)) {
              let slot = cache.get(k);
              !slot && cache.set(k, slot = []);
              slot.length >= backPressure && illegalState(
                `max back pressure (${backPressure}) exceeded for input: ${String(
                  k
                )}`
              );
              slot.push(x);
              currKeys.add(k);
              while (requiredInputs(requiredKeys, currKeys)) {
                acc = reduce3(acc, collect(cache, currKeys));
                first = false;
                if (isReduced(acc))
                  break;
              }
            }
            return acc;
          }
        ];
      }
    };
    xform.keys = () => requiredKeys;
    xform.clear = () => {
      cache.clear();
      requiredKeys.clear();
      currKeys.clear();
      curr = {};
    };
    xform.add = (id) => {
      requiredKeys.add(id);
    };
    xform.delete = (id, clean = true) => {
      cache.delete(id);
      requiredKeys.delete(id);
      if (clean) {
        currKeys.delete(id);
        delete curr[id];
      }
    };
    return xform;
  }
  var requiredInputs = (required, curr) => {
    if (curr.size < required.size)
      return false;
    for (let id of required) {
      if (!curr.has(id))
        return false;
    }
    return true;
  };
  var collect = (cache, currKeys) => {
    const curr = {};
    for (let id of currKeys) {
      const slot = cache.get(id);
      curr[id] = slot.shift();
      !slot.length && currKeys.delete(id);
    }
    return curr;
  };

  // node_modules/@thi.ng/transducers/partition-time.js
  function partitionTime(period, src) {
    return isIterable(src) ? iterator(partitionTime(period), src) : partitionBy(() => {
      let last3 = 0;
      return () => {
        const t = Date.now();
        t - last3 >= period && (last3 = t);
        return last3;
      };
    }, true);
  }

  // node_modules/@thi.ng/transducers/partition-when.js
  function partitionWhen(...args) {
    return __iter(partitionWhen, args, iterator) || (([init, complete, reduce3]) => {
      const pred = args[0];
      const f = args[1] === true ? pred() : pred;
      let chunk;
      return [
        init,
        (acc) => {
          if (chunk && chunk.length) {
            acc = reduce3(acc, chunk);
            chunk = null;
          }
          return complete(acc);
        },
        (acc, x) => {
          if (f(x)) {
            chunk && (acc = reduce3(acc, chunk));
            chunk = isReduced(acc) ? null : [x];
          } else {
            chunk ? chunk.push(x) : chunk = [x];
          }
          return acc;
        }
      ];
    });
  }

  // node_modules/@thi.ng/arrays/peek.js
  var peek = (buf) => buf[buf.length - 1];

  // node_modules/@thi.ng/transducers/peek.js
  function peek2(src) {
    return map(peek, src);
  }

  // node_modules/@thi.ng/transducers/pluck.js
  function pluck(key, src) {
    return isIterable(src) ? iterator1(pluck(key), src) : map((x) => x[key]);
  }

  // node_modules/@thi.ng/transducers/rechunk.js
  function rechunk(...args) {
    const iter = __iter(rechunk, args, iterator);
    if (iter)
      return iter;
    return ([init, complete, reduce3]) => {
      let buf = "";
      const re = args[0] || /\r?\n/;
      return [
        init,
        (acc) => {
          if (buf)
            acc = reduce3(acc, buf);
          return complete(acc);
        },
        (acc, chunk) => {
          buf += chunk;
          const res = buf.split(re);
          if (res.length > 1) {
            buf = res.pop();
            for (let l of res) {
              acc = reduce3(acc, l);
              if (isReduced(acc)) {
                buf = "";
                break;
              }
            }
          }
          return acc;
        }
      ];
    };
  }

  // node_modules/@thi.ng/transducers/sample.js
  function sample(...args) {
    const iter = __iter(sample, args);
    if (iter) {
      return iter;
    }
    const prob = args[0];
    const rnd = args[1] || SYSTEM;
    return (rfn) => {
      const r = rfn[2];
      return compR(
        rfn,
        (acc, x) => rnd.probability(prob) ? r(acc, x) : acc
      );
    };
  }

  // node_modules/@thi.ng/transducers/scan.js
  function scan(...args) {
    return args.length > 2 && __iter(scan, args, iterator) || (([inito, completeo, reduceo]) => {
      const [initi, completei, reducei] = args[0];
      let acc = args.length > 1 && args[1] != null ? args[1] : initi();
      return [
        inito,
        (_acc) => {
          let a = completei(acc);
          if (a !== acc) {
            _acc = unreduced(reduceo(_acc, a));
          }
          acc = a;
          return completeo(_acc);
        },
        (_acc, x) => {
          acc = reducei(acc, x);
          if (isReduced(acc)) {
            return ensureReduced(reduceo(_acc, acc.deref()));
          }
          return reduceo(_acc, acc);
        }
      ];
    });
  }

  // node_modules/@thi.ng/transducers/select-keys.js
  function selectKeys(keys2, src) {
    return isIterable(src) ? iterator1(selectKeys(keys2), src) : map(keySelector(keys2));
  }

  // node_modules/@thi.ng/transducers/side-effect.js
  var sideEffect = (fn) => map((x) => (fn(x), x));

  // node_modules/@thi.ng/api/deref.js
  var isDeref = (x) => x != null && typeof x["deref"] === "function";
  var deref = (x) => isDeref(x) ? x.deref() : x;

  // node_modules/@thi.ng/transducers/sliding-window.js
  function slidingWindow(...args) {
    const iter = __iter(slidingWindow, args);
    if (iter)
      return iter;
    const size = args[0];
    const partial2 = args[1] !== false;
    return (rfn) => {
      const reduce3 = rfn[2];
      let buf = [];
      return compR(rfn, (acc, x) => {
        buf.push(x);
        const _size = deref(size);
        if (partial2 || buf.length >= _size) {
          acc = reduce3(acc, buf);
          buf = buf.slice(buf.length >= _size ? 1 : 0, _size);
        }
        return acc;
      });
    };
  }

  // node_modules/@thi.ng/errors/assert.js
  var import_meta = {};
  var AssertionError = defError(() => "Assertion failed");
  var assert = (typeof process !== "undefined" && process.env !== void 0 ? true : import_meta.env ? import_meta.env.MODE !== "production" || !!import_meta.env.UMBRELLA_ASSERTS || !!import_meta.env.VITE_UMBRELLA_ASSERTS : true) ? (test, msg) => {
    if (typeof test === "function" && !test() || !test) {
      throw new AssertionError(
        typeof msg === "function" ? msg() : msg
      );
    }
  } : () => {
  };

  // node_modules/@thi.ng/arrays/shuffle.js
  var shuffleRange = (buf, start = 0, end = buf.length, rnd = SYSTEM) => {
    assert(
      start >= 0 && end >= start && end <= buf.length,
      `illegal range ${start}..${end}`
    );
    if (end - start > 1) {
      for (let i = end; i-- > start; ) {
        const a = rnd.minmax(start, i + 1) | 0;
        const t = buf[a];
        buf[a] = buf[i];
        buf[i] = t;
      }
    }
    return buf;
  };
  var shuffle = (buf, n = buf.length, rnd = SYSTEM) => shuffleRange(buf, 0, n, rnd);

  // node_modules/@thi.ng/checks/is-map.js
  var isMap = (x) => x instanceof Map;

  // node_modules/@thi.ng/checks/is-node.js
  var isNode = () => typeof process === "object" && typeof process.versions === "object" && typeof process.versions.node !== "undefined";

  // node_modules/@thi.ng/checks/is-number.js
  var isNumber = (x) => typeof x === "number";

  // node_modules/@thi.ng/checks/is-plain-object.js
  var OBJP2 = Object.getPrototypeOf;
  var isPlainObject = (x) => {
    let p;
    return x != null && typeof x === "object" && ((p = OBJP2(x)) === null || OBJP2(p) === null);
  };

  // node_modules/@thi.ng/checks/is-transferable.js
  var isTransferable = (x) => x instanceof ArrayBuffer || typeof SharedArrayBuffer !== "undefined" && x instanceof SharedArrayBuffer || typeof MessagePort !== "undefined" && x instanceof MessagePort;

  // node_modules/@thi.ng/checks/is-typedarray.js
  var isTypedArray = (x) => !!x && (x instanceof Float32Array || x instanceof Float64Array || x instanceof Uint32Array || x instanceof Int32Array || x instanceof Uint8Array || x instanceof Int8Array || x instanceof Uint16Array || x instanceof Int16Array || x instanceof Uint8ClampedArray);

  // node_modules/@thi.ng/transducers/stream-shuffle.js
  function streamShuffle(...args) {
    return __iter(streamShuffle, args, iterator) || (([init, complete, reduce3]) => {
      let n;
      let maxSwaps;
      let rnd = SYSTEM;
      const opts = args[0];
      if (isPlainObject(opts)) {
        n = opts.n;
        maxSwaps = opts.max || n;
        opts.rnd && (rnd = opts.rnd);
      } else {
        n = args[0];
        maxSwaps = args[1] || n;
      }
      const buf = [];
      return [
        init,
        (acc) => {
          if (buf.length) {
            shuffle(buf, Math.min(maxSwaps, buf.length), rnd);
            for (let i = 0, n2 = buf.length; i < n2 && !isReduced(acc); i++) {
              acc = reduce3(acc, buf[i]);
            }
          }
          buf.length = 0;
          acc = complete(acc);
          return acc;
        },
        (acc, x) => {
          buf.push(x);
          if (buf.length === n) {
            shuffle(buf, Math.min(maxSwaps, n), rnd);
            acc = reduce3(acc, buf.shift());
          }
          return acc;
        }
      ];
    });
  }

  // node_modules/@thi.ng/arrays/binary-search.js
  var binarySearch = (buf, x, key = (x2) => x2, cmp = compare, low = 0, high = buf.length - 1) => {
    const kx = key(x);
    while (low <= high) {
      const mid = low + high >>> 1;
      const c = cmp(key(buf[mid]), kx);
      if (c < 0) {
        low = mid + 1;
      } else if (c > 0) {
        high = mid - 1;
      } else {
        return mid;
      }
    }
    return -low - 1;
  };

  // node_modules/@thi.ng/transducers/stream-sort.js
  function streamSort(...args) {
    const iter = __iter(streamSort, args, iterator);
    if (iter) {
      return iter;
    }
    const { key, compare: compare2 } = __sortOpts(args[1]);
    const n = args[0];
    return ([init, complete, reduce3]) => {
      const buf = [];
      return [
        init,
        __drain(buf, complete, reduce3),
        (acc, x) => {
          const idx = binarySearch(buf, x, key, compare2);
          buf.splice(idx < 0 ? -(idx + 1) : idx, 0, x);
          if (buf.length === n) {
            acc = reduce3(acc, buf.shift());
          }
          return acc;
        }
      ];
    };
  }

  // node_modules/@thi.ng/transducers/struct.js
  function struct(fields, src) {
    return isIterable(src) ? iterator(struct(fields), src) : comp2(
      partitionOf(fields.map((f) => f[1])),
      partition(fields.length),
      rename(fields.map((f) => f[0])),
      mapKeys(
        fields.reduce(
          (acc, f) => f[2] ? (acc[f[0]] = f[2], acc) : acc,
          {}
        ),
        false
      )
    );
  }

  // node_modules/@thi.ng/arrays/swizzle.js
  var swizzle = (order) => {
    const [a, b, c, d, e, f, g, h] = order;
    switch (order.length) {
      case 0:
        return () => [];
      case 1:
        return (x) => [x[a]];
      case 2:
        return (x) => [x[a], x[b]];
      case 3:
        return (x) => [x[a], x[b], x[c]];
      case 4:
        return (x) => [x[a], x[b], x[c], x[d]];
      case 5:
        return (x) => [x[a], x[b], x[c], x[d], x[e]];
      case 6:
        return (x) => [x[a], x[b], x[c], x[d], x[e], x[f]];
      case 7:
        return (x) => [x[a], x[b], x[c], x[d], x[e], x[f], x[g]];
      case 8:
        return (x) => [x[a], x[b], x[c], x[d], x[e], x[f], x[g], x[h]];
      default:
        return (x) => {
          const res = [];
          for (let i = order.length; i-- > 0; ) {
            res[i] = x[order[i]];
          }
          return res;
        };
    }
  };

  // node_modules/@thi.ng/transducers/swizzle.js
  function swizzle2(order, src) {
    return isIterable(src) ? iterator1(swizzle2(order), src) : map(swizzle(order));
  }

  // node_modules/@thi.ng/transducers/take-nth.js
  function takeNth(n, src) {
    if (isIterable(src)) {
      return iterator1(takeNth(n), src);
    }
    n = clamp0(n - 1);
    return throttle(() => {
      let skip = 0;
      return () => skip === 0 ? (skip = n, true) : (skip--, false);
    });
  }

  // node_modules/@thi.ng/transducers/take-while.js
  function takeWhile(...args) {
    return __iter(takeWhile, args) || ((rfn) => {
      const r = rfn[2];
      const pred = args[0];
      let ok = true;
      return compR(
        rfn,
        (acc, x) => (ok = ok && pred(x)) ? r(acc, x) : reduced(acc)
      );
    });
  }

  // node_modules/@thi.ng/transducers/throttle-time.js
  function throttleTime(delay2, src) {
    return isIterable(src) ? iterator1(throttleTime(delay2), src) : throttle(() => {
      let last3 = 0;
      return () => {
        const t = Date.now();
        return t - last3 >= delay2 ? (last3 = t, true) : false;
      };
    });
  }

  // node_modules/@thi.ng/transducers/toggle.js
  function toggle(on, off, initial = false, src) {
    return isIterable(src) ? iterator1(toggle(on, off, initial), src) : ([init, complete, reduce3]) => {
      let state = initial;
      return [
        init,
        complete,
        (acc) => reduce3(acc, (state = !state) ? on : off)
      ];
    };
  }

  // node_modules/@thi.ng/transducers/trace.js
  var trace = (prefix = "") => sideEffect((x) => console.log(prefix, x));

  // node_modules/@thi.ng/transducers/word-wrap.js
  function wordWrap(...args) {
    const iter = __iter(wordWrap, args, iterator);
    if (iter) {
      return iter;
    }
    const lineLength = args[0];
    const { delim, always } = {
      delim: 1,
      always: true,
      ...args[1]
    };
    return partitionBy(() => {
      let n = 0;
      let flag = false;
      return (w) => {
        n += w.length + delim;
        if (n > lineLength + (always ? 0 : delim)) {
          flag = !flag;
          n = w.length + delim;
        }
        return flag;
      };
    }, true);
  }

  // node_modules/@thi.ng/transducers/as-iterable.js
  function* asIterable(src) {
    yield* src;
  }

  // node_modules/@thi.ng/arrays/ensure-iterable.js
  var ensureIterable = (x) => {
    (x == null || !x[Symbol.iterator]) && illegalArgs(`value is not iterable: ${x}`);
    return x;
  };

  // node_modules/@thi.ng/arrays/ensure-array.js
  var ensureArray = (x) => isArray(x) ? x : [...ensureIterable(x)];
  var ensureArrayLike = (x) => isArrayLike(x) ? x : [...ensureIterable(x)];

  // node_modules/@thi.ng/random/weighted-random.js
  var weightedRandom = (choices2, weights, rnd = SYSTEM) => {
    const n = choices2.length;
    assert(n > 0, "no choices given");
    const opts = weights ? choices2.map((x, i) => [weights[i] || 0, x]).sort((a, b) => b[0] - a[0]) : choices2.map((x) => [1, x]);
    const total = opts.reduce((acc, o) => acc + o[0], 0);
    total <= 0 && console.warn("total weights <= 0");
    return () => {
      const r = rnd.float(total);
      let sum = total;
      for (let i = 0; i < n; i++) {
        sum -= opts[i][0];
        if (sum <= r) {
          return opts[i][1];
        }
      }
      return void 0;
    };
  };

  // node_modules/@thi.ng/transducers/repeatedly.js
  function* repeatedly(fn, n = Infinity) {
    for (let i = 0; i < n; i++) {
      yield fn(i);
    }
  }

  // node_modules/@thi.ng/transducers/choices.js
  var choices = (choices2, weights, rnd = SYSTEM) => repeatedly(
    weights ? weightedRandom(ensureArray(choices2), weights, rnd) : () => choices2[rnd.float(choices2.length) | 0]
  );

  // node_modules/@thi.ng/transducers/concat.js
  function* concat(...xs) {
    for (let x of xs) {
      x != null && (yield* ensureIterable(x));
    }
  }

  // node_modules/@thi.ng/transducers/curve.js
  function* curve(start, end, steps = 10, rate = 0.1) {
    const c = Math.exp(
      -Math.log((Math.abs(end - start) + rate) / rate) / steps
    );
    const offset = (start < end ? end + rate : end - rate) * (1 - c);
    steps > 0 && (yield start);
    for (let x = start; steps-- > 0; ) {
      yield x = offset + x * c;
    }
  }

  // node_modules/@thi.ng/transducers/cycle.js
  function* cycle(input, num = Infinity) {
    if (num < 1)
      return;
    let cache = [];
    for (let i of input) {
      cache.push(i);
      yield i;
    }
    if (cache.length > 0) {
      while (--num > 0) {
        yield* cache;
      }
    }
  }

  // node_modules/@thi.ng/transducers/dup.js
  function dup(x) {
    return isString(x) ? x + x : isArray(x) ? x.concat(x) : (x = ensureArray(x), concat(x, x));
  }

  // node_modules/@thi.ng/transducers/repeat.js
  function* repeat(x, n = Infinity) {
    while (n-- > 0) {
      yield x;
    }
  }

  // node_modules/@thi.ng/transducers/extend-sides.js
  function* extendSides(src, numLeft = 1, numRight = numLeft) {
    let prev = SEMAPHORE;
    for (let x of src) {
      if (numLeft > 0 && prev === SEMAPHORE) {
        yield* repeat(x, numLeft);
        numLeft = 0;
      }
      yield x;
      prev = x;
    }
    if (numRight > 0 && prev !== SEMAPHORE) {
      yield* repeat(prev, numRight);
    }
  }

  // node_modules/@thi.ng/transducers/iterate.js
  function* iterate(fn, seed, num = Infinity) {
    for (let i = 1; i <= num; i++) {
      yield seed;
      seed = fn(seed, i);
    }
  }

  // node_modules/@thi.ng/transducers/pairs.js
  function* pairs(x) {
    for (let k in x) {
      if (x.hasOwnProperty(k)) {
        yield [k, x[k]];
      }
    }
  }

  // node_modules/@thi.ng/transducers/permutations.js
  function* permutations(...src) {
    const n = src.length - 1;
    if (n < 0) {
      return;
    }
    const step2 = new Array(n + 1).fill(0);
    const realized = src.map(ensureArrayLike);
    const total = realized.reduce((acc, x) => acc * x.length, 1);
    for (let i = 0; i < total; i++) {
      const tuple = [];
      for (let j = n; j >= 0; j--) {
        const r = realized[j];
        let s = step2[j];
        if (s === r.length) {
          step2[j] = s = 0;
          j > 0 && step2[j - 1]++;
        }
        tuple[j] = r[s];
      }
      step2[n]++;
      yield tuple;
    }
  }
  var permutationsN = (n, m = n, offsets) => {
    if (offsets && offsets.length < n) {
      illegalArgs(`insufficient offsets, got ${offsets.length}, needed ${n}`);
    }
    const seqs = [];
    while (n-- > 0) {
      const o = offsets ? offsets[n] : 0;
      seqs[n] = range(o, o + m);
    }
    return permutations.apply(null, seqs);
  };

  // node_modules/@thi.ng/transducers/key-permutations.js
  var keyPermutations = (spec) => map(
    (x) => assocObj(partition(2, x)),
    permutations(...mapcat(([k, v]) => [[k], v], pairs(spec)))
  );

  // node_modules/@thi.ng/transducers/keys.js
  function* keys(x) {
    for (let k in x) {
      if (x.hasOwnProperty(k)) {
        yield k;
      }
    }
  }

  // node_modules/@thi.ng/transducers/line.js
  var line = (start, end, steps = 10) => {
    const delta = end - start;
    return map((t) => start + delta * t, normRange(steps));
  };

  // node_modules/@thi.ng/transducers/pad-sides.js
  var padSides = (src, x, numLeft = 1, numRight = numLeft) => numLeft > 0 ? numRight > 0 ? concat(repeat(x, numLeft), src, repeat(x, numRight)) : concat(repeat(x, numLeft), src) : numRight > 0 ? concat(src, repeat(x, numRight)) : concat(src);

  // node_modules/@thi.ng/transducers/reverse.js
  function* reverse2(input) {
    const _input = ensureArray(input);
    let n = _input.length;
    while (n-- > 0) {
      yield _input[n];
    }
  }

  // node_modules/@thi.ng/transducers/palindrome.js
  function palindrome(x) {
    return isString(x) ? str("", concat([x], reverse2(x))) : isArray(x) ? x.concat(x.slice().reverse()) : (x = ensureArray(x), concat(x, reverse2(x)));
  }

  // node_modules/@thi.ng/transducers/range3d.js
  function* range3d(...args) {
    let fromX, toX, stepX;
    let fromY, toY, stepY;
    let fromZ, toZ, stepZ;
    switch (args.length) {
      case 9:
        stepX = args[6];
        stepY = args[7];
        stepZ = args[8];
      case 6:
        [fromX, toX, fromY, toY, fromZ, toZ] = args;
        break;
      case 3:
        [toX, toY, toZ] = args;
        fromX = fromY = fromZ = 0;
        break;
      default:
        illegalArity(args.length);
    }
    const rx = range(fromX, toX, stepX);
    const ry = range(fromY, toY, stepY);
    for (let z of range(fromZ, toZ, stepZ)) {
      for (let y of ry) {
        for (let x of rx) {
          yield [x, y, z];
        }
      }
    }
  }

  // node_modules/@thi.ng/transducers/range-nd.js
  var rangeNd = (min2, max2) => permutations.apply(
    null,
    max2 ? [...map(([a, b]) => range(a, b), zip(min2, max2))] : [...map(range, min2)]
  );

  // node_modules/@thi.ng/transducers/repeatedly2d.js
  function* repeatedly2d(fn, cols, rows) {
    for (let y = 0; y < rows; y++) {
      for (let x = 0; x < cols; x++) {
        yield fn(x, y);
      }
    }
  }

  // node_modules/@thi.ng/transducers/repeatedly3d.js
  function* repeatedly3d(fn, cols, rows, slices) {
    for (let z = 0; z < slices; z++) {
      for (let y = 0; y < rows; y++) {
        for (let x = 0; x < cols; x++) {
          yield fn(x, y, z);
        }
      }
    }
  }

  // node_modules/@thi.ng/transducers/sorted-keys.js
  function* sortedKeys(x, cmp = compare) {
    yield* Object.keys(x).sort(cmp);
  }

  // node_modules/@thi.ng/transducers/symmetric.js
  function* symmetric(src) {
    let head = void 0;
    for (let x of src) {
      head = { x, n: head };
      yield x;
    }
    while (head) {
      yield head.x;
      head = head.n;
    }
  }

  // node_modules/@thi.ng/transducers/tween.js
  function* tween(opts) {
    const { min: min2, max: max2, num, init, mix: mix2, stops } = opts;
    const easing = opts.easing || ((x) => x);
    let l = stops.length;
    if (l < 1)
      return;
    if (l === 1) {
      yield* repeat(mix2(init(stops[0][1], stops[0][1]), 0), num);
    }
    stops.sort((a, b) => a[0] - b[0]);
    stops[l - 1][0] < max2 && stops.push([max2, stops[l - 1][1]]);
    stops[0][0] > min2 && stops.unshift([min2, stops[0][1]]);
    const range3 = max2 - min2;
    let start = stops[0][0];
    let end = stops[1][0];
    let delta = end - start;
    let interval = init(stops[0][1], stops[1][1]);
    let i = 1;
    l = stops.length;
    for (let t of normRange(num)) {
      t = min2 + range3 * t;
      if (t > end) {
        while (i < l && t > stops[i][0])
          i++;
        start = stops[i - 1][0];
        end = stops[i][0];
        delta = end - start;
        interval = init(stops[i - 1][1], stops[i][1]);
      }
      yield mix2(interval, easing(delta !== 0 ? (t - start) / delta : 0));
    }
  }

  // node_modules/@thi.ng/transducers/vals.js
  function* vals(x) {
    for (let k in x) {
      if (x.hasOwnProperty(k)) {
        yield x[k];
      }
    }
  }

  // node_modules/@thi.ng/transducers/wrap-sides.js
  function* wrapSides(src, numLeft = 1, numRight = numLeft) {
    const _src = ensureArray(src);
    !(inRange(numLeft, 0, _src.length) && inRange(numRight, 0, _src.length)) && illegalArgs(`allowed wrap range: [0..${_src.length}]`);
    if (numLeft > 0) {
      for (let m = _src.length, i = m - numLeft; i < m; i++) {
        yield _src[i];
      }
    }
    yield* _src;
    if (numRight > 0) {
      for (let i = 0; i < numRight; i++) {
        yield _src[i];
      }
    }
  }

  // node_modules/@thi.ng/dcons/alist.js
  var AList = class _AList {
    _head;
    _length = 0;
    constructor(src) {
      src && this.into(src);
    }
    get length() {
      return this._length;
    }
    get head() {
      return this._head;
    }
    [Symbol.iterator]() {
      return _iterate("next", this._head);
    }
    reverseIterator() {
      return _iterate("prev", this.tail);
    }
    clear() {
      this.release();
    }
    compare(o, cmp = compare) {
      let n = this._length;
      if (n < o._length) {
        return -1;
      } else if (n > o._length) {
        return 1;
      } else if (n === 0) {
        return 0;
      } else {
        let ca = this._head;
        let cb = o._head;
        let res = 0;
        for (; n-- > 0 && res === 0; ) {
          res = cmp(ca.value, cb.value);
          ca = ca.next;
          cb = cb.next;
        }
        return res;
      }
    }
    concat(...slices) {
      const res = this.copy();
      for (let slice of slices) {
        res.into(slice);
      }
      return res;
    }
    equiv(o) {
      if (!(o instanceof _AList || isArrayLike(o)) || this._length !== o.length) {
        return false;
      }
      if (!this._length || this === o)
        return true;
      const iter = o[Symbol.iterator]();
      let cell = this._head;
      for (let n = this._length; n-- > 0; ) {
        if (!equiv(cell.value, iter.next().value)) {
          return false;
        }
        cell = cell.next;
      }
      return true;
    }
    filter(fn) {
      const res = this.empty();
      this.traverse((x) => (fn(x.value) && res.append(x.value), true));
      return res;
    }
    find(value) {
      return this.traverse((x) => x.value !== value);
    }
    findWith(fn) {
      return this.traverse((x) => !fn(x.value));
    }
    first() {
      return this._head && this._head.value;
    }
    insertSorted(value, cmp) {
      cmp = cmp || compare;
      for (let cell = this._head, n = this._length; n-- > 0; ) {
        if (cmp(value, cell.value) <= 0) {
          return this.insertBefore(cell, value);
        }
        cell = cell.next;
      }
      return this.append(value);
    }
    into(src) {
      for (let x of src) {
        this.append(x);
      }
      return this;
    }
    nth(n, notFound) {
      const cell = this.nthCell(n);
      return cell ? cell.value : notFound;
    }
    nthCellUnsafe(n) {
      let cell;
      let dir;
      if (n <= this._length >>> 1) {
        cell = this._head;
        dir = "next";
      } else {
        cell = this.tail;
        dir = "prev";
        n = this._length - n - 1;
      }
      while (n-- > 0 && cell) {
        cell = cell[dir];
      }
      return cell;
    }
    peek() {
      return this.tail && this.tail.value;
    }
    /** {@inheritDoc @thi.ng/transducers#IReducible.$reduce} */
    $reduce(rfn, acc) {
      let cell = this._head;
      for (let n = this._length; n-- > 0 && !isReduced(acc); ) {
        acc = rfn(acc, cell.value);
        cell = cell.next;
      }
      return acc;
    }
    reduce(rfn, initial) {
      return this.$reduce(rfn, initial);
    }
    release() {
      let cell = this._head;
      if (!cell)
        return true;
      let next;
      for (let i = this._length; i-- > 0; ) {
        next = cell.next;
        delete cell.value;
        delete cell.prev;
        delete cell.next;
        cell = next;
      }
      this._head = void 0;
      this._length = 0;
      return true;
    }
    reverse() {
      let head = this._head;
      let tail = this.tail;
      let n = (this._length >>> 1) + (this._length & 1);
      while (head && tail && n > 0) {
        const t = head.value;
        head.value = tail.value;
        tail.value = t;
        head = head.next;
        tail = tail.prev;
        n--;
      }
      return this;
    }
    setHead(v) {
      const cell = this._head;
      if (cell) {
        cell.value = v;
        return cell;
      }
      return this.prepend(v);
    }
    setNth(n, v) {
      const cell = this.nthCell(n);
      !cell && outOfBounds(n);
      cell.value = v;
      return cell;
    }
    setTail(v) {
      const cell = this.tail;
      if (cell) {
        cell.value = v;
        return cell;
      }
      return this.append(v);
    }
    swap(a, b) {
      if (a !== b) {
        const t = a.value;
        a.value = b.value;
        b.value = t;
      }
      return this;
    }
    toArray(out = []) {
      this.traverse((x) => (out.push(x.value), true));
      return out;
    }
    toJSON() {
      return this.toArray();
    }
    toString() {
      let res = [];
      this.traverse((x) => (res.push(String(x.value)), true));
      return res.join(", ");
    }
    traverse(fn, start = this._head, end) {
      if (!this._head)
        return;
      let cell = start;
      do {
        if (!fn(cell))
          break;
        cell = cell.next;
      } while (cell !== end);
      return cell;
    }
    _map(res, fn) {
      this.traverse((x) => (res.append(fn(x.value)), true));
      return res;
    }
  };
  function* _iterate(dir, cell) {
    while (cell) {
      yield cell.value;
      cell = cell[dir];
    }
  }

  // node_modules/@thi.ng/dcons/dcons.js
  var DCons = class _DCons extends AList {
    _tail;
    constructor(src) {
      super();
      src && this.into(src);
    }
    get tail() {
      return this._tail;
    }
    append(value) {
      if (this._tail) {
        const cell = { value, prev: this._tail };
        this._tail.next = cell;
        this._tail = cell;
        this._length++;
        return cell;
      } else {
        return this.prepend(value);
      }
    }
    asHead(cell) {
      if (cell === this._head) {
        return this;
      }
      this.remove(cell);
      this._head.prev = cell;
      cell.next = this._head;
      cell.prev = void 0;
      this._head = cell;
      this._length++;
      return this;
    }
    asTail(cell) {
      if (cell === this._tail) {
        return this;
      }
      this.remove(cell);
      this._tail.next = cell;
      cell.prev = this._tail;
      cell.next = void 0;
      this._tail = cell;
      this._length++;
      return this;
    }
    /** @deprecated use {@link DCons.prepend} */
    cons(value) {
      this.prepend(value);
      return this;
    }
    copy() {
      return new _DCons(this);
    }
    *cycle() {
      while (true) {
        yield* this;
      }
    }
    drop() {
      const cell = this._head;
      if (cell) {
        this._head = cell.next;
        if (this._head) {
          this._head.prev = void 0;
        } else {
          this._tail = void 0;
        }
        this._length--;
        return cell.value;
      }
    }
    empty() {
      return new _DCons();
    }
    insertAfter(cell, value) {
      const newCell = { value, next: cell.next, prev: cell };
      if (cell.next) {
        cell.next.prev = newCell;
      } else {
        this._tail = newCell;
      }
      cell.next = newCell;
      this._length++;
      return newCell;
    }
    insertAfterNth(n, x) {
      if (n < 0) {
        n += this._length;
      }
      if (n >= this._length - 1) {
        return this.append(x);
      } else {
        ensureIndex(n, 0, this._length);
        return this.insertAfter(this.nthCellUnsafe(n), x);
      }
    }
    insertBefore(cell, value) {
      const newCell = { value, next: cell, prev: cell.prev };
      if (cell.prev) {
        cell.prev.next = newCell;
      } else {
        this._head = newCell;
      }
      cell.prev = newCell;
      this._length++;
      return newCell;
    }
    insertBeforeNth(n, x) {
      if (n < 0) {
        n += this._length;
      }
      if (n <= 0) {
        return this.prepend(x);
      } else {
        ensureIndex(n, 0, this._length);
        return this.insertBefore(this.nthCellUnsafe(n), x);
      }
    }
    map(fn) {
      return this._map(new _DCons(), fn);
    }
    nth(n, notFound) {
      const cell = this.nthCell(n);
      return cell ? cell.value : notFound;
    }
    nthCell(n) {
      if (n < 0) {
        n += this._length;
      }
      if (n < 0 || n >= this._length) {
        return;
      }
      return this.nthCellUnsafe(n);
    }
    pop() {
      const cell = this._tail;
      if (!cell) {
        return;
      }
      this._tail = cell.prev;
      if (this._tail) {
        this._tail.next = void 0;
      } else {
        this._head = void 0;
      }
      this._length--;
      return cell.value;
    }
    prepend(value) {
      const cell = { value, next: this._head };
      if (this._head) {
        this._head.prev = cell;
      } else {
        this._tail = cell;
      }
      this._head = cell;
      this._length++;
      return cell;
    }
    push(value) {
      this.append(value);
      return this;
    }
    release() {
      this._tail = void 0;
      return super.release();
    }
    remove(cell) {
      if (cell.prev) {
        cell.prev.next = cell.next;
      } else {
        this._head = cell.next;
      }
      if (cell.next) {
        cell.next.prev = cell.prev;
      } else {
        this._tail = cell.prev;
      }
      this._length--;
      return this;
    }
    rotateLeft() {
      switch (this._length) {
        case 0:
        case 1:
          return this;
        case 2:
          return this.swap(this._head, this._tail);
        default:
          return this.push(this.drop());
      }
    }
    rotateRight() {
      switch (this._length) {
        case 0:
        case 1:
          return this;
        case 2:
          return this.swap(this._head, this._tail);
        default:
          const x = this.peek();
          this.pop();
          this.prepend(x);
          return this;
      }
    }
    /** {@inheritDoc @thi.ng/api#ISeqable.seq} */
    seq(start = 0, end = this.length) {
      if (start >= end || start < 0)
        return;
      let cell = this.nthCell(start);
      const last3 = this.nthCell(end - 1);
      const $seq = (cell2) => ({
        first() {
          return cell2.value;
        },
        next() {
          return cell2 !== last3 && cell2.next ? $seq(cell2.next) : void 0;
        }
      });
      return cell ? $seq(cell) : void 0;
    }
    /**
     * Shuffles list by probabilistically moving cells to head or tail
     * positions.
     *
     * @remarks
     * Supports configurable iterations and custom PRNG via
     * [`IRandom`](https://docs.thi.ng/umbrella/random/interfaces/IRandom.html)
     * (default:
     * [`SYSTEM`](https://docs.thi.ng/umbrella/random/variables/SYSTEM.html)).
     *
     * Default iterations: `ceil(3/2 * log2(n))`
     *
     * @param iter -
     * @param rnd -
     */
    shuffle(iter, rnd = SYSTEM) {
      if (this._length < 2)
        return this;
      for (iter = iter !== void 0 ? iter : Math.ceil(1.5 * Math.log2(this._length)); iter > 0; iter--) {
        let cell = this._head;
        while (cell) {
          const next = cell.next;
          rnd.probability(0.5) ? this.asHead(cell) : this.asTail(cell);
          cell = next;
        }
      }
      return this;
    }
    slice(from = 0, to = this.length) {
      let a = from < 0 ? from + this._length : from;
      let b = to < 0 ? to + this._length : to;
      if (a < 0 || b < 0) {
        illegalArgs("invalid indices: ${from} / ${to}");
      }
      const res = new _DCons();
      let cell = this.nthCell(a);
      while (cell && ++a <= b) {
        res.push(cell.value);
        cell = cell.next;
      }
      return res;
    }
    /**
     * Merge sort implementation based on Simon Tatham's algorithm:
     * https://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
     *
     * @remarks
     * Uses [`compare()`](https://docs.thi.ng/umbrella/compare/functions/compare.html) as default comparator.
     *
     * @param cmp -
     */
    sort(cmp = compare) {
      if (!this._length)
        return this;
      let inSize = 1;
      while (true) {
        let p = this._head;
        this._head = void 0;
        this._tail = void 0;
        let numMerges = 0;
        while (p) {
          numMerges++;
          let q = p;
          let psize = 0;
          for (let i = 0; i < inSize; i++) {
            psize++;
            q = q.next;
            if (!q)
              break;
          }
          let qsize = inSize;
          while (psize > 0 || qsize > 0 && q) {
            let e;
            if (psize === 0) {
              e = q;
              q = q.next;
              qsize--;
            } else if (!q || qsize === 0) {
              e = p;
              p = p.next;
              psize--;
            } else if (cmp(p.value, q.value) <= 0) {
              e = p;
              p = p.next;
              psize--;
            } else {
              e = q;
              q = q.next;
              qsize--;
            }
            if (this._tail) {
              this._tail.next = e;
            } else {
              this._head = e;
            }
            e.prev = this._tail;
            this._tail = e;
          }
          p = q;
        }
        this._tail.next = void 0;
        if (numMerges <= 1) {
          return this;
        }
        inSize *= 2;
      }
    }
    splice(at, del = 0, insert) {
      let cell;
      if (typeof at === "number") {
        if (at < 0) {
          at += this._length;
        }
        ensureIndex(at, 0, this._length);
        cell = this.nthCellUnsafe(at);
      } else {
        cell = at;
      }
      const removed = new _DCons();
      if (del > 0) {
        while (cell && del-- > 0) {
          this.remove(cell);
          removed.push(cell.value);
          cell = cell.next;
        }
      } else if (cell) {
        cell = cell.next;
      }
      if (insert) {
        if (cell) {
          for (let i of insert) {
            this.insertBefore(cell, i);
          }
        } else {
          for (let i of insert) {
            this.push(i);
          }
        }
      }
      return removed;
    }
  };

  // node_modules/@thi.ng/cache/lru.js
  var LRUCache = class _LRUCache {
    map;
    items;
    opts;
    _size;
    constructor(pairs2, opts) {
      const _opts = Object.assign(
        {
          maxlen: Infinity,
          maxsize: Infinity,
          map: () => /* @__PURE__ */ new Map(),
          ksize: () => 0,
          vsize: () => 0
        },
        opts
      );
      this.map = _opts.map();
      this.items = new DCons();
      this._size = 0;
      this.opts = _opts;
      if (pairs2) {
        this.into(pairs2);
      }
    }
    get length() {
      return this.items.length;
    }
    get size() {
      return this._size;
    }
    [Symbol.iterator]() {
      return this.entries();
    }
    entries() {
      return map((e) => [e.k, e], this.items);
    }
    keys() {
      return map((e) => e.k, this.items);
    }
    values() {
      return map((e) => e.v, this.items);
    }
    copy() {
      const c = this.empty();
      c.items = this.items.copy();
      let cell = c.items.head;
      while (cell) {
        c.map.set(cell.value.k, cell);
        cell = cell.next;
      }
      return c;
    }
    empty() {
      return new _LRUCache(null, this.opts);
    }
    release() {
      this._size = 0;
      this.map.clear();
      const release = this.opts.release;
      if (release) {
        let e;
        while (e = this.items.drop()) {
          release(e.k, e.v);
        }
        return true;
      }
      return this.items.release();
    }
    has(key) {
      return this.map.has(key);
    }
    get(key, notFound) {
      const e = this.map.get(key);
      if (e) {
        return this.resetEntry(e);
      }
      return notFound;
    }
    set(key, value) {
      const size = this.opts.ksize(key) + this.opts.vsize(value);
      const e = this.map.get(key);
      this._size += Math.max(0, size - (e ? e.value.s : 0));
      this.ensureSize() && this.doSetEntry(e, key, value, size);
      return value;
    }
    into(pairs2) {
      for (let p of pairs2) {
        this.set(p[0], p[1]);
      }
      return this;
    }
    getSet(key, retrieve) {
      const e = this.map.get(key);
      if (e) {
        return Promise.resolve(this.resetEntry(e));
      }
      return retrieve().then((v) => this.set(key, v));
    }
    delete(key) {
      const e = this.map.get(key);
      if (e) {
        this.removeEntry(e);
        return true;
      }
      return false;
    }
    resetEntry(e) {
      this.items.asTail(e);
      return e.value.v;
    }
    ensureSize() {
      const release = this.opts.release;
      const maxs = this.opts.maxsize;
      const maxl = this.opts.maxlen;
      while (this._size > maxs || this.length >= maxl) {
        const e = this.items.drop();
        if (!e) {
          return false;
        }
        this.map.delete(e.k);
        release && release(e.k, e.v);
        this._size -= e.s;
      }
      return true;
    }
    removeEntry(e) {
      const ee = e.value;
      this.map.delete(ee.k);
      this.items.remove(e);
      this.opts.release && this.opts.release(ee.k, ee.v);
      this._size -= ee.s;
    }
    doSetEntry(e, k, v, s) {
      if (e) {
        e.value.v = v;
        e.value.s = s;
        this.items.asTail(e);
      } else {
        this.items.push({ k, v, s });
        this.map.set(k, this.items.tail);
      }
    }
  };

  // node_modules/@thi.ng/cache/mru.js
  var MRUCache = class _MRUCache extends LRUCache {
    constructor(pairs2, opts) {
      super(pairs2, opts);
    }
    empty() {
      return new _MRUCache(null, this.opts);
    }
    resetEntry(e) {
      this.items.asHead(e);
      return e.value.v;
    }
    doSetEntry(e, k, v, s) {
      if (e) {
        e.value.v = v;
        e.value.s = s;
        this.items.asHead(e);
      } else {
        this.items.prepend({ k, v, s });
        this.map.set(k, this.items.head);
      }
    }
  };

  // node_modules/@thi.ng/cache/tlru.js
  var TLRUCache = class _TLRUCache extends LRUCache {
    constructor(pairs2, opts) {
      opts = Object.assign({ ttl: 60 * 60 * 1e3 }, opts);
      super(pairs2, opts);
    }
    empty() {
      return new _TLRUCache(null, this.opts);
    }
    has(key) {
      return this.get(key) !== void 0;
    }
    get(key, notFound) {
      const e = this.map.get(key);
      if (e) {
        if (e.value.t >= Date.now()) {
          return this.resetEntry(e);
        }
        this.removeEntry(e);
      }
      return notFound;
    }
    set(key, value, ttl = this.opts.ttl) {
      const size = this.opts.ksize(key) + this.opts.vsize(value);
      const e = this.map.get(key);
      this._size += Math.max(0, size - (e ? e.value.s : 0));
      if (this.ensureSize()) {
        const t = Date.now() + ttl;
        if (e) {
          e.value.v = value;
          e.value.s = size;
          e.value.t = t;
          this.items.asTail(e);
        } else {
          this.items.push({
            k: key,
            v: value,
            s: size,
            t
          });
          this.map.set(key, this.items.tail);
        }
      }
      return value;
    }
    getSet(key, retrieve, ttl = this.opts.ttl) {
      const e = this.get(key);
      if (e) {
        return Promise.resolve(e);
      }
      return retrieve().then((v) => this.set(key, v, ttl));
    }
    prune() {
      const now = Date.now();
      let cell = this.items.head;
      while (cell) {
        if (cell.value.t < now) {
          this.removeEntry(cell);
        }
        cell = cell.next;
      }
    }
    ensureSize() {
      const maxs = this.opts.maxsize;
      const maxl = this.opts.maxlen;
      const now = Date.now();
      let cell = this.items.head;
      while (cell && (this._size > maxs || this.length >= maxl)) {
        if (cell.value.t < now) {
          this.removeEntry(cell);
        }
        cell = cell.next;
      }
      return super.ensureSize();
    }
  };

  // node_modules/@thi.ng/compose/index.js
  var compose_exports = {};
  __export(compose_exports, {
    Delay: () => Delay,
    comp: () => comp,
    compI: () => compI,
    compL: () => compL,
    complement: () => complement,
    constantly: () => constantly,
    delay: () => delay,
    delayed: () => delayed,
    identity: () => identity,
    ifDef: () => ifDef,
    juxt: () => juxt,
    partial: () => partial,
    promisify: () => promisify,
    threadFirst: () => threadFirst,
    threadLast: () => threadLast,
    trampoline: () => trampoline
  });

  // node_modules/@thi.ng/compose/complement.js
  function complement(f) {
    return (...xs) => !f(...xs);
  }

  // node_modules/@thi.ng/compose/constantly.js
  var constantly = (x) => () => x;

  // node_modules/@thi.ng/compose/delay.js
  var delay = (body) => new Delay(body);
  var Delay = class {
    value;
    body;
    realized;
    constructor(body) {
      this.body = body;
      this.realized = false;
    }
    deref() {
      if (!this.realized) {
        this.value = this.body();
        this.realized = true;
      }
      return this.value;
    }
    isRealized() {
      return this.realized;
    }
  };

  // node_modules/@thi.ng/compose/ifdef.js
  var ifDef = (f, x) => x != null ? f(x) : void 0;

  // node_modules/@thi.ng/compose/partial.js
  function partial(fn, ...args) {
    let [a, b, c, d, e, f, g, h] = args;
    switch (args.length) {
      case 1:
        return (...xs) => fn(a, ...xs);
      case 2:
        return (...xs) => fn(a, b, ...xs);
      case 3:
        return (...xs) => fn(a, b, c, ...xs);
      case 4:
        return (...xs) => fn(a, b, c, d, ...xs);
      case 5:
        return (...xs) => fn(a, b, c, d, e, ...xs);
      case 6:
        return (...xs) => fn(a, b, c, d, e, f, ...xs);
      case 7:
        return (...xs) => fn(a, b, c, d, e, f, g, ...xs);
      case 8:
        return (...xs) => fn(a, b, c, d, e, f, g, h, ...xs);
      default:
        illegalArgs();
    }
  }

  // node_modules/@thi.ng/compose/promisify.js
  var promisify = (fn) => new Promise(
    (resolve2, reject) => fn((err, result) => err != null ? reject(err) : resolve2(result))
  );

  // node_modules/@thi.ng/compose/thread-first.js
  var threadFirst = (init, ...fns) => fns.reduce(
    (acc, expr) => typeof expr === "function" ? expr(acc) : expr[0](acc, ...expr.slice(1)),
    init
  );

  // node_modules/@thi.ng/compose/thread-last.js
  var threadLast = (init, ...fns) => fns.reduce(
    (acc, expr) => typeof expr === "function" ? expr(acc) : expr[0](...expr.slice(1), acc),
    init
  );

  // node_modules/@thi.ng/compose/trampoline.js
  var trampoline = (f) => {
    while (typeof f === "function") {
      f = f();
    }
    return f;
  };

  // node_modules/@thi.ng/csp/index.js
  var csp_exports = {};
  __export(csp_exports, {
    Channel: () => Channel,
    DroppingBuffer: () => DroppingBuffer,
    FixedBuffer: () => FixedBuffer,
    Mult: () => Mult,
    PubSub: () => PubSub,
    SlidingBuffer: () => SlidingBuffer
  });

  // node_modules/@thi.ng/csp/buffer.js
  var FixedBuffer = class {
    buf;
    limit;
    constructor(limit = 1) {
      this.buf = new DCons();
      this.limit = limit;
    }
    get length() {
      return this.buf.length;
    }
    isEmpty() {
      return this.buf.length === 0;
    }
    isFull() {
      return this.buf.length >= this.limit;
    }
    release() {
      return this.buf.release();
    }
    push(x) {
      if (!this.isFull()) {
        this.buf.push(x);
        return true;
      }
      return false;
    }
    drop() {
      if (!this.isEmpty()) {
        return this.buf.drop();
      }
    }
  };
  var DroppingBuffer = class extends FixedBuffer {
    constructor(limit = 1) {
      super(limit);
    }
    isFull() {
      return false;
    }
    push(x) {
      if (this.buf.length < this.limit) {
        this.buf.push(x);
      }
      return true;
    }
  };
  var SlidingBuffer = class extends FixedBuffer {
    constructor(limit = 1) {
      super(limit);
    }
    isFull() {
      return false;
    }
    push(x) {
      if (this.buf.length >= this.limit) {
        this.buf.drop();
      }
      this.buf.push(x);
      return true;
    }
  };

  // node_modules/@thi.ng/csp/channel.js
  var Channel = class _Channel {
    static constantly(x, delay2) {
      const chan = new _Channel(delay2 ? delayed2(delay2) : null);
      chan.produce(() => x);
      return chan;
    }
    static repeatedly(fn, delay2) {
      const chan = new _Channel(delay2 ? delayed2(delay2) : null);
      chan.produce(fn);
      return chan;
    }
    static cycle(src, delay2) {
      return _Channel.from(cycle(src), delay2 ? delayed2(delay2) : null);
    }
    static range(...args) {
      const [from, to, step2, delay2] = args;
      return _Channel.from(
        range(from, to, step2),
        delay2 !== void 0 ? delayed2(delay2) : null
      );
    }
    /**
     * Constructs new channel which closes automatically after given period.
     *
     * @param delay - time in ms
     */
    static timeout(delay2) {
      const chan = new _Channel(`timeout-${_Channel.NEXT_ID++}`);
      setTimeout(() => chan.close(), delay2);
      return chan;
    }
    /**
     * Shorthand for: `Channel.timeout(delay).take()`
     *
     * @param delay - time in ms
     */
    static sleep(delay2) {
      return _Channel.timeout(delay2).read();
    }
    /**
     * Creates new channel with single value from given promise, then closes
     * automatically iff promise has been resolved.
     *
     * @param p - promise
     */
    static fromPromise(p) {
      const chan = new _Channel();
      p.then(
        (x) => (async () => {
          await chan.write(x);
          await chan.close();
          return x;
        })()
      );
      return chan;
    }
    static from(...args) {
      let close, tx;
      switch (args.length) {
        case 1:
          break;
        case 2:
          if (typeof args[1] === "boolean") {
            close = args[1];
          } else {
            tx = args[1];
          }
          break;
        case 3:
          tx = args[1];
          close = args[2];
          break;
        default:
          illegalArity(args.length);
      }
      const chan = new _Channel(tx);
      chan.into(args[0], close);
      return chan;
    }
    /**
     * Takes an array of channels and blocks until any of them becomes
     * readable (or has been closed). The returned promised resolves into
     * an array of `[value, channel]`. Channel order is repeatedly
     * shuffled for each read attempt.
     *
     * @param chans - source channels
     */
    static select(chans) {
      return new Promise((resolve2) => {
        const _select = () => {
          for (let c of shuffle(chans)) {
            if (c.isReadable() || c.isClosed()) {
              c.read().then((x) => resolve2([x, c]));
              return;
            }
          }
          _Channel.SCHEDULE.call(null, _select, 0);
        };
        _Channel.SCHEDULE.call(null, _select, 0);
      });
    }
    /**
     * Takes an array of channels to merge into new channel. Any closed
     * channels will be automatically removed from the input selection.
     * Once all inputs are closed, the target channel will close too (by
     * default).
     *
     * @remarks
     * If `named` is true, the merged channel will have tuples of:
     * `[src-id, val]` If false (default), only received values will be
     * forwarded.
     *
     * @param chans - source channels
     * @param out - result channel
     * @param close - true, if result closes
     * @param named - true, to emit labeled tuples
     */
    static merge(chans, out, close = true, named = false) {
      out = out || new _Channel();
      (async () => {
        while (true) {
          let [x, ch] = await _Channel.select(chans);
          if (x === void 0) {
            chans.splice(chans.indexOf(ch), 1);
            if (!chans.length) {
              close && await out.close();
              break;
            }
          } else {
            await out.write(named ? [ch.id, x] : x);
          }
        }
      })();
      return out;
    }
    /**
     * Takes an array of channels to merge into new channel of tuples.
     * Whereas `Channel.merge()` realizes a sequential merging with no
     * guarantees about ordering of the output.
     *
     * @remarks
     * The output channel of this function will collect values from all
     * channels and a new tuple is emitted only once a new value has
     * been read from ALL channels. Therefore the overall throughput is
     * dictated by the slowest of the inputs.
     *
     * Once any of the inputs closes, the process is terminated and the
     * output channel is closed too (by default).
     *
     * @example
     * ```ts
     * Channel.mergeTuples([
     *   Channel.from([1, 2, 3]),
     *   Channel.from([10, 20, 30]),
     *   Channel.from([100, 200, 300])
     * ]).consume();
     *
     * // chan-0 : [ 1, 10, 100 ]
     * // chan-0 : [ 2, 20, 200 ]
     * // chan-0 : [ 3, 30, 300 ]
     * // chan-0 done
     *
     * Channel.mergeTuples([
     *   Channel.from([1, 2, 3]),
     *   Channel.from([10, 20, 30]),
     *   Channel.from([100, 200, 300])
     * ], null, false).consume();
     * ```
     *
     * @param chans - source channels
     * @param out - result channel
     * @param closeOnFirst - true, if result closes when first input is done
     * @param closeOutput - true, if result closes when all inputs are done
     */
    static mergeTuples(chans, out, closeOnFirst = true, closeOutput = true) {
      out = out || new _Channel();
      (async () => {
        let buf = [];
        let orig = [...chans];
        let sel = new Set(chans);
        let n = chans.length;
        while (true) {
          let [x, ch] = await _Channel.select([...sel]);
          let idx = orig.indexOf(ch);
          if (x === void 0) {
            if (closeOnFirst || chans.length === 1) {
              break;
            }
            chans.splice(idx, 1);
          }
          buf[idx] = x;
          sel.delete(ch);
          if (--n === 0) {
            await out.write(buf);
            buf = [];
            n = chans.length;
            sel = new Set(chans);
          }
        }
        closeOutput && await out.close();
      })();
      return out;
    }
    static MAX_WRITES = 1024;
    static NEXT_ID = 0;
    static SCHEDULE = typeof setImmediate === "function" ? setImmediate : setTimeout;
    static RFN = [
      () => null,
      (acc) => acc,
      (acc, x) => acc.push(x)
    ];
    id;
    onerror;
    state;
    buf;
    tx;
    writes;
    reads;
    txbuf;
    isBusy;
    constructor(...args) {
      let id, buf, tx, err;
      let [a, b] = args;
      switch (args.length) {
        case 0:
          break;
        case 1:
          if (typeof a === "string") {
            id = a;
          } else if (maybeBuffer(a)) {
            buf = a;
          } else {
            tx = a;
          }
          break;
        case 2:
          if (typeof a === "string") {
            id = a;
            if (maybeBuffer(b)) {
              buf = b;
            } else {
              tx = b;
            }
          } else {
            [tx, err] = args;
          }
          break;
        case 3:
          if (isFunction(args[1]) && isFunction(args[2])) {
            [id, tx, err] = args;
          } else {
            [id, buf, tx] = args;
          }
          break;
        case 4:
          [id, buf, tx, err] = args;
          break;
        default:
          illegalArity(args.length);
      }
      this.id = id || `chan-${_Channel.NEXT_ID++}`;
      buf = buf || 1;
      this.buf = typeof buf === "number" ? new FixedBuffer(buf) : buf;
      this.writes = new DCons();
      this.reads = new DCons();
      this.txbuf = new DCons();
      this.tx = tx ? tx(_Channel.RFN) : null;
      this.onerror = tx && (err || defaultErrorHandler);
      this.state = 0;
      this.isBusy = false;
    }
    channel() {
      return this;
    }
    write(value) {
      return new Promise((resolve2) => {
        if (this.state !== 0) {
          resolve2(false);
        }
        if (this.writes.length < _Channel.MAX_WRITES) {
          this.writes.push({
            value: this.tx ? async () => {
              try {
                if (isReduced(this.tx[2](this.txbuf, value))) {
                  this.state = 1;
                }
              } catch (e) {
                this.onerror(e, this, value);
              }
            } : () => value,
            resolve: resolve2
          });
          this.process();
        } else {
          throw new Error(
            `channel stalled (${_Channel.MAX_WRITES} unprocessed writes)`
          );
        }
      });
    }
    read() {
      return new Promise((resolve2) => {
        if (this.state === 2) {
          resolve2(void 0);
        }
        this.reads.push(resolve2);
        this.process();
      });
    }
    tryRead(timeout2 = 1e3) {
      return new Promise((resolve2) => {
        (async () => resolve2(
          (await _Channel.select([this, _Channel.timeout(timeout2)]))[0]
        ))();
      });
    }
    close(flush = false) {
      if (this.state === 0) {
        this.state = 1;
        flush && this.flush();
        return this.process();
      }
    }
    isClosed() {
      return this.state !== 0;
    }
    isReadable() {
      return this.state !== 2 && this.buf && this.buf.length > 0 || this.writes && this.writes.length > 0 || this.txbuf && this.txbuf.length > 0;
    }
    consume(fn = (x) => console.log(this.id, ":", x)) {
      return (async () => {
        let x;
        while ((x = null, x = await this.read()) !== void 0) {
          await fn(x);
        }
      })();
    }
    produce(fn, close = true) {
      return (async () => {
        while (!this.isClosed()) {
          const val = await fn();
          if (val === void 0) {
            close && await this.close();
            break;
          }
          await this.write(val);
        }
      })();
    }
    consumeWhileReadable(fn = (x) => console.log(this.id, ":", x)) {
      return (async () => {
        let x;
        while (this.isReadable()) {
          x = await this.read();
          if (x === void 0) {
            break;
          }
          await fn(x);
          x = null;
        }
      })();
    }
    reduce(rfn, acc) {
      return (async () => {
        const [init, complete, reduce3] = rfn;
        acc = acc != null ? acc : init();
        let x;
        while ((x = null, x = await this.read()) !== void 0) {
          acc = reduce3(acc, x);
          if (isReduced(acc)) {
            acc = acc.deref();
            break;
          }
        }
        return unreduced(complete(acc));
      })();
    }
    transduce(tx, rfn, acc) {
      return (async () => {
        const _rfn = tx(rfn);
        return unreduced(_rfn[1](await this.reduce(_rfn, acc)));
      })();
    }
    into(src, close = true) {
      return (async () => {
        for (let x of src) {
          if (this.isClosed()) {
            break;
          }
          await this.write(x);
        }
        close && await this.close();
      })();
    }
    pipe(dest, close = true) {
      if (!(dest instanceof _Channel)) {
        dest = new _Channel(dest);
      }
      this.consume((x) => dest.write(x)).then(() => {
        close && dest.close();
      });
      return dest;
    }
    split(pred, truthy, falsey, close = true) {
      if (!(truthy instanceof _Channel)) {
        truthy = new _Channel();
      }
      if (!(falsey instanceof _Channel)) {
        falsey = new _Channel();
      }
      this.consume((x) => (pred(x) ? truthy : falsey).write(x)).then(
        () => {
          close && (truthy.close(), falsey.close());
        }
      );
      return [truthy, falsey];
    }
    concat(chans, close = true) {
      return (async () => {
        for (let c of chans) {
          await c.consume((x) => this.write(x));
        }
        close && await this.close();
      })();
    }
    release() {
      if (this.state === 1) {
        this.state = 2;
        this.flush();
        this.buf.release();
        delete this.reads;
        delete this.writes;
        delete this.buf;
        delete this.txbuf;
        delete this.tx;
        delete this.isBusy;
        delete this.onerror;
      }
    }
    async process() {
      if (!this.isBusy) {
        this.isBusy = true;
        const { buf, txbuf, reads, writes } = this;
        let doProcess = true;
        while (doProcess) {
          while (reads.length && (txbuf.length || buf.length)) {
            if (txbuf.length) {
              const val = txbuf.drop();
              if (val !== void 0) {
                reads.drop()(val);
              }
            } else {
              const val = await buf.drop().value();
              if (val !== void 0) {
                reads.drop()(val);
              }
            }
          }
          while (writes.length && !buf.isFull()) {
            const put = writes.drop();
            buf.push(put);
            put.resolve(true);
          }
          if (this.state === 1) {
            if (this.tx && !writes.length) {
              try {
                this.tx[1](this.txbuf);
              } catch (e) {
                this.onerror(e, this);
              }
            }
            if (!this.isReadable()) {
              this.release();
              return;
            }
          }
          doProcess = reads.length && (txbuf.length || buf.length) || writes.length && !buf.isFull();
        }
        this.isBusy = false;
      }
    }
    flush() {
      let op;
      while (op = this.reads.drop()) {
        op();
      }
      while (op = this.writes.drop()) {
        op.resolve(false);
      }
      this.buf.release();
    }
  };
  var defaultErrorHandler = (e, chan, val) => console.log(
    chan.id,
    "error occurred",
    e.message,
    val !== void 0 ? val : ""
  );
  var maybeBuffer = (x) => x instanceof FixedBuffer || typeof x === "number";

  // node_modules/@thi.ng/csp/mult.js
  var Mult = class _Mult {
    static nextID = 0;
    src;
    taps;
    tapID = 0;
    constructor(...args) {
      let id, src;
      switch (args.length) {
        case 2:
          id = args[0];
          src = args[1];
          break;
        case 1:
          if (typeof args[0] === "string") {
            id = args[0];
          } else {
            src = args[0];
          }
          break;
        case 0:
          id = "mult" + _Mult.nextID++;
          break;
        default:
          illegalArity(args.length);
      }
      if (src instanceof Channel) {
        this.src = src;
      } else {
        this.src = new Channel(id, src);
      }
      this.taps = new DCons();
      this.process();
    }
    get id() {
      return this.src && this.src.id;
    }
    set id(id) {
      this.src && (this.src.id = id);
    }
    channel() {
      return this.src;
    }
    write(val) {
      if (this.src) {
        return this.src.write(val);
      }
      return Promise.resolve(false);
    }
    close(flush = false) {
      return this.src ? this.src.close(flush) : void 0;
    }
    tap(ch) {
      if (this.taps) {
        if (!(ch instanceof Channel)) {
          ch = new Channel(this.src.id + "-tap" + this.tapID++, ch);
        } else if (this.taps.find(ch)) {
          return ch;
        }
        this.taps.push(ch);
        return ch;
      }
    }
    untap(ch) {
      if (this.taps) {
        const t = this.taps.find(ch);
        if (t) {
          this.taps.remove(t);
          return true;
        }
      }
      return false;
    }
    untapAll(close = true) {
      if (this.taps) {
        let tap = this.taps.head;
        while (tap) {
          close && tap.value.close();
          this.taps.remove(tap);
          tap = tap.next;
        }
        return true;
      }
      return false;
    }
    async process() {
      let x;
      while ((x = null, x = await this.src.read()) !== void 0) {
        let t = this.taps.head;
        while (t) {
          if (!await t.value.write(x)) {
            this.taps.remove(t);
          }
          t = t.next;
        }
      }
      for (let t of this.taps) {
        await t.close();
      }
      delete this.src;
      delete this.taps;
      delete this.tapID;
    }
  };

  // node_modules/@thi.ng/csp/pubsub.js
  var PubSub = class _PubSub {
    static NEXT_ID = 0;
    src;
    fn;
    topics;
    constructor(...args) {
      switch (args.length) {
        case 2:
          this.src = args[0];
          this.fn = args[1];
          break;
        case 1:
          this.src = new Channel("pubsub" + _PubSub.NEXT_ID++);
          this.fn = args[0];
          break;
        default:
          illegalArity(args.length);
      }
      this.topics = {};
      this.process();
    }
    get id() {
      return this.src && this.src.id;
    }
    set id(id) {
      this.src && (this.src.id = id);
    }
    channel() {
      return this.src;
    }
    write(val) {
      if (this.src) {
        return this.src.write(val);
      }
      return Promise.resolve(false);
    }
    close(flush = false) {
      return this.src ? this.src.close(flush) : void 0;
    }
    /**
     * Creates a new topic subscription channel and returns it.
     * Each topic is managed by its own {@link Mult} and can have arbitrary
     * number of subscribers. If the optional transducer is given, it will
     * only be applied to the new subscription channel.
     *
     * The special "*" topic can be used to subscribe to all messages and
     * acts as multiplexed pass-through of the source channel.
     *
     * @param id - topic id
     * @param tx - transducer for new subscription
     */
    sub(id, tx) {
      let topic = this.topics[id];
      if (!topic) {
        this.topics[id] = topic = new Mult(this.src.id + "-" + id);
      }
      return topic.tap(tx);
    }
    unsub(id, ch) {
      let topic = this.topics[id];
      if (topic) {
        return topic.untap(ch);
      }
      return false;
    }
    unsubAll(id, close = true) {
      let topic = this.topics[id];
      if (topic) {
        return topic.untapAll(close);
      }
      return false;
    }
    async process() {
      let x;
      while ((x = null, x = await this.src.read()) !== void 0) {
        const id = await this.fn(x);
        let topic = this.topics[id];
        topic && await topic.write(x);
        topic = this.topics["*"];
        topic && await topic.write(x);
      }
      for (let id of Object.keys(this.topics)) {
        this.topics[id].close();
      }
      delete this.src;
      delete this.topics;
      delete this.fn;
    }
  };

  // node_modules/@thi.ng/iterators/index.js
  var iterators_exports = {};
  __export(iterators_exports, {
    ReducedValue: () => ReducedValue,
    butLast: () => butLast,
    cached: () => cached,
    concat: () => concat2,
    constantly: () => constantly2,
    consume: () => consume2,
    cycle: () => cycle2,
    dedupe: () => dedupe2,
    dedupeWith: () => dedupeWith,
    dense: () => dense,
    drop: () => drop2,
    dropNth: () => dropNth2,
    dropWhile: () => dropWhile2,
    ensureIterable: () => ensureIterable2,
    ensureIterator: () => ensureIterator,
    every: () => every2,
    filter: () => filter2,
    flatten: () => flatten2,
    flattenWith: () => flattenWith2,
    fnil: () => fnil,
    fork: () => fork,
    frequencies: () => frequencies2,
    groupBy: () => groupBy,
    identity: () => identity3,
    indexed: () => indexed2,
    interleave: () => interleave2,
    interpose: () => interpose2,
    iterate: () => iterate2,
    iterator: () => iterator2,
    juxt: () => juxt2,
    last: () => last2,
    map: () => map2,
    mapIndexed: () => mapIndexed2,
    mapcat: () => mapcat2,
    maybeIterator: () => maybeIterator,
    maybeObjectIterator: () => maybeObjectIterator,
    objectIterator: () => objectIterator,
    partition: () => partition2,
    partitionBy: () => partitionBy2,
    randomSample: () => randomSample,
    range: () => range2,
    reduce: () => reduce2,
    reduced: () => reduced2,
    reductions: () => reductions2,
    repeat: () => repeat2,
    repeatedly: () => repeatedly2,
    reverse: () => reverse3,
    run: () => run2,
    some: () => some2,
    take: () => take2,
    takeLast: () => takeLast2,
    takeNth: () => takeNth2,
    takeWhile: () => takeWhile2,
    walk: () => walk,
    walkIterator: () => walkIterator,
    walkable: () => walkable,
    zip: () => zip2
  });

  // node_modules/@thi.ng/iterators/iterator.js
  var iterator2 = (x) => x[Symbol.iterator]();
  var maybeIterator = (x) => x != null && x[Symbol.iterator] && x[Symbol.iterator]() || void 0;

  // node_modules/@thi.ng/iterators/butlast.js
  function* butLast(input) {
    let iter = iterator2(input);
    let v;
    let prev;
    let first = true;
    while (v = iter.next(), !v.done) {
      if (!first) {
        yield prev;
      }
      prev = v.value;
      first = false;
    }
  }

  // node_modules/@thi.ng/iterators/cached.js
  var cached = (input) => {
    let cache = [];
    let iter = iterator2(input);
    let done = false;
    return function() {
      let i = 0;
      return {
        [Symbol.iterator]() {
          return this;
        },
        next() {
          if (i < cache.length) {
            return { done: false, value: cache[i++] };
          } else if (!done) {
            let v = iter.next();
            if (!v.done) {
              i++;
              cache.push(v.value);
              return { done: false, value: v.value };
            }
            done = true;
          }
          return { done };
        }
      };
    };
  };

  // node_modules/@thi.ng/iterators/ensure.js
  var ensureIterable2 = (x) => !(x != null && x[Symbol.iterator]) ? illegalArgs(`value is not iterable: ${x}`) : x;
  var ensureIterator = (x) => ensureIterable2(iterator2(x));

  // node_modules/@thi.ng/iterators/concat.js
  function* concat2(...inputs) {
    let iter = iterator2(inputs);
    let v;
    while (v = iter.next(), !v.done) {
      if (v.value != null) {
        yield* ensureIterable2(v.value);
      }
    }
  }

  // node_modules/@thi.ng/iterators/constantly.js
  var constantly2 = (x) => () => x;

  // node_modules/@thi.ng/iterators/consume.js
  var consume2 = (iter, n = Infinity) => {
    while (n-- > 0 && !iter.next().done) {
    }
    return iter;
  };

  // node_modules/@thi.ng/iterators/cycle.js
  function* cycle2(input) {
    let cache = [];
    let iter = iterator2(input);
    let v;
    while (v = iter.next(), !v.done) {
      cache.push(v.value);
      yield v.value;
    }
    if (cache.length > 0) {
      while (true) {
        yield* cache;
      }
    }
  }

  // node_modules/@thi.ng/iterators/dedupe-with.js
  function* dedupeWith(equiv2, input) {
    let iter = iterator2(input);
    let v;
    let prev;
    while (v = iter.next(), !v.done) {
      if (prev === void 0 || !equiv2(prev, v.value)) {
        prev = v.value;
        yield v.value;
      }
    }
  }

  // node_modules/@thi.ng/iterators/dedupe.js
  function* dedupe2(input) {
    let iter = iterator2(input);
    let v;
    let prev;
    while (v = iter.next(), !v.done) {
      if (v.value !== prev) {
        prev = v.value;
        yield v.value;
      }
    }
  }

  // node_modules/@thi.ng/iterators/filter.js
  function* filter2(pred, input) {
    let iter = iterator2(input);
    let v;
    while (v = iter.next(), !v.done) {
      if (pred(v.value) === true) {
        yield v.value;
      }
    }
  }

  // node_modules/@thi.ng/iterators/dense.js
  var dense = (input) => filter2((x) => x != null, input);

  // node_modules/@thi.ng/iterators/take.js
  function* take2(n, input) {
    let iter = iterator2(input);
    while (n-- > 0) {
      let v = iter.next();
      if (v.done) {
        break;
      }
      yield v.value;
    }
  }

  // node_modules/@thi.ng/iterators/drop-nth.js
  function* dropNth2(n, input) {
    let iter = ensureIterable2(iterator2(input));
    do {
      yield* take2(n - 1, iter);
    } while (!iter.next().done);
  }

  // node_modules/@thi.ng/iterators/drop-while.js
  function* dropWhile2(pred, input) {
    let iter = ensureIterator(input);
    let v;
    while (v = iter.next(), !v.done && pred(v.value) === true) {
    }
    if (v.value !== void 0) {
      yield v.value;
      yield* iter;
    }
  }

  // node_modules/@thi.ng/iterators/drop.js
  function* drop2(n, input) {
    let iter = ensureIterator(input);
    consume2(iter, n);
    yield* iter;
  }

  // node_modules/@thi.ng/iterators/every.js
  var every2 = (pred, input) => {
    let iter = iterator2(input);
    let v;
    let empty = true;
    while (v = iter.next(), !v.done) {
      if (pred(v.value) !== true) {
        return false;
      }
      empty = false;
    }
    return !empty;
  };

  // node_modules/@thi.ng/iterators/flatten-with.js
  function* flattenWith2(tx, input) {
    let iter = iterator2(input);
    let v, val, res;
    while (v = iter.next(), !v.done) {
      val = v.value;
      if (val != null && (res = tx(val)) !== void 0) {
        yield* flattenWith2(tx, res);
      } else {
        yield val;
      }
    }
  }

  // node_modules/@thi.ng/iterators/map.js
  function* map2(fn, ...inputs) {
    let v;
    let n = inputs.length;
    switch (n) {
      case 0:
        return;
      case 1:
        let iter = iterator2(inputs[0]);
        while (v = iter.next(), !v.done) {
          yield fn(v.value);
        }
        return;
      default:
        let iters = inputs.map(iterator2);
        while (true) {
          let args = [];
          for (let i = 0; i < n; i++) {
            v = iters[i].next();
            if (v.done) {
              return;
            }
            args.push(v.value);
          }
          yield fn.apply(null, args);
        }
    }
  }

  // node_modules/@thi.ng/iterators/object-iterator.js
  var objectIterator = (x) => map2((k) => [k, x[k]], Object.keys(x));
  var maybeObjectIterator = (x) => x != null && typeof x === "object" && Object.prototype.toString.call(x) !== "[object Generator]" && objectIterator(x) || void 0;

  // node_modules/@thi.ng/iterators/flatten.js
  var flatten2 = (input, includeObjects = true) => flattenWith2(
    (x) => typeof x !== "string" && (maybeIterator(x) || includeObjects && maybeObjectIterator(x)) || void 0,
    input
  );

  // node_modules/@thi.ng/iterators/fnil.js
  var fnil = (fn, ...ctors) => {
    let [cta, ctb, ctc] = ctors;
    switch (ctors.length) {
      case 1:
        return (...args) => {
          if (args[0] == null) {
            args[0] = cta();
          }
          return fn.apply(null, args);
        };
      case 2:
        return (...args) => {
          if (args[0] == null) {
            args[0] = cta();
          }
          if (args[1] == null) {
            args[1] = ctb();
          }
          return fn.apply(null, args);
        };
      case 3:
        return (...args) => {
          if (args[0] == null) {
            args[0] = cta();
          }
          if (args[1] == null) {
            args[1] = ctb();
          }
          if (args[2] == null) {
            args[2] = ctc();
          }
          return fn.apply(null, args);
        };
      default:
        return illegalArity(ctors.length);
    }
  };

  // node_modules/@thi.ng/iterators/fork.js
  var fork = (src, cacheLimit = 16) => {
    const iter = iterator2(src);
    const cache = new DCons();
    const forks = [];
    let done = false;
    let total = 0;
    const consume3 = () => {
      if (!done) {
        if (cache.length === cacheLimit) {
          cache.drop();
          for (let i = forks.length - 1; i >= 0; i--) {
            forks[i] = Math.max(forks[i] - 1, 0);
          }
        }
        const v = iter.next();
        if (!v.done) {
          cache.push(v.value);
          total++;
        }
        done = v.done;
        return v.value;
      }
    };
    return function() {
      const id = forks.length;
      forks.push(0);
      return {
        [Symbol.iterator]() {
          return this;
        },
        next() {
          if (forks[id] < cache.length) {
            if (forks[id] < total) {
              return { done: false, value: cache.nth(forks[id]++) };
            }
          } else {
            const value = consume3();
            if (!done) {
              forks[id]++;
            }
            return { done, value };
          }
        }
      };
    };
  };

  // node_modules/@thi.ng/iterators/frequencies.js
  function* frequencies2(input, key) {
    let freqs = {};
    let iter = iterator2(input);
    let v;
    while (v = iter.next(), !v.done) {
      let k = key ? key(v.value) : v.value;
      let id = JSON.stringify(k);
      let bin = freqs[id];
      if (bin) {
        bin[1]++;
      } else {
        freqs[id] = [k, 1];
      }
    }
    yield* Object.keys(freqs).map((id) => freqs[id]);
  }

  // node_modules/@thi.ng/iterators/group-by.js
  var groupBy = (key, input) => {
    let groups = {};
    let iter = iterator2(input);
    let v;
    while (v = iter.next(), !v.done) {
      let id = JSON.stringify(key(v.value));
      let g = groups[id];
      if (g) {
        g.push(v.value);
      } else {
        groups[id] = [v.value];
      }
    }
    return groups;
  };

  // node_modules/@thi.ng/iterators/identity.js
  var identity3 = (x) => x;

  // node_modules/@thi.ng/iterators/range.js
  function* range2(from, to, step2) {
    if (from === void 0) {
      from = 0;
      to = Infinity;
    } else if (to === void 0) {
      to = from;
      from = 0;
    }
    step2 = step2 === void 0 ? from < to ? 1 : -1 : step2;
    if (step2 > 0) {
      while (from < to) {
        yield from;
        from += step2;
      }
    } else if (step2 < 0) {
      while (from > to) {
        yield from;
        from += step2;
      }
    }
  }

  // node_modules/@thi.ng/iterators/map-indexed.js
  var mapIndexed2 = (fn, ...inputs) => map2.apply(null, [fn, range2()].concat(inputs));

  // node_modules/@thi.ng/iterators/indexed.js
  var indexed2 = (input) => mapIndexed2((i, x) => [i, x], input);

  // node_modules/@thi.ng/iterators/interleave.js
  function* interleave2(...inputs) {
    let n = inputs.length;
    if (n === 0) {
      illegalArity(0);
    }
    let iter = cycle2(map2(iterator2, inputs));
    let chunk = [];
    let v;
    while (true) {
      for (let i = 0; i < n; i++) {
        if (v = iter.next().value.next(), v.done) {
          return;
        }
        chunk[i] = v.value;
      }
      yield* chunk;
    }
  }

  // node_modules/@thi.ng/iterators/interpose.js
  function* interpose2(x, input) {
    let iter = iterator2(input);
    let v = iter.next();
    while (!v.done) {
      yield v.value;
      v = iter.next();
      if (!v.done) {
        yield x;
      }
    }
  }

  // node_modules/@thi.ng/iterators/iterate.js
  function* iterate2(fn, seed) {
    while (true) {
      yield seed;
      seed = fn(seed);
    }
  }

  // node_modules/@thi.ng/iterators/juxt.js
  var juxt2 = (...fns) => (x) => {
    let res = [];
    for (let i = 0; i < fns.length; i++) {
      res[i] = fns[i](x);
    }
    return res;
  };

  // node_modules/@thi.ng/iterators/last.js
  var last2 = (input) => {
    let iter = iterator2(input);
    let v;
    let prev;
    while (v = iter.next(), !v.done) {
      prev = v.value;
    }
    return prev;
  };

  // node_modules/@thi.ng/iterators/mapcat.js
  function* mapcat2(fn, ...inputs) {
    inputs.unshift(fn);
    let iter = map2.apply(null, inputs);
    let v;
    while (v = iter.next(), !v.done) {
      if (v.value != null) {
        yield* ensureIterable2(v.value);
      }
    }
  }

  // node_modules/@thi.ng/iterators/partition-by.js
  function* partitionBy2(fn, input) {
    let iter = iterator2(input);
    let chunk = [];
    let v;
    let prev;
    while (v = iter.next(), !v.done) {
      let curr = fn(v.value);
      if (prev !== void 0 && prev !== curr) {
        yield chunk;
        chunk = [];
      }
      prev = curr;
      chunk.push(v.value);
    }
    if (chunk.length > 0) {
      yield chunk;
    }
  }

  // node_modules/@thi.ng/iterators/partition.js
  function* partition2(n, step2, input, all = false) {
    if (n < 1) {
      illegalArgs(`invalid partition size: ${n}`);
    }
    if (step2 < 1) {
      illegalArgs(`invalid step size: ${step2}`);
    }
    let iter = iterator2(input);
    let chunk = [];
    while (true) {
      let i = chunk.length;
      while (i++ < n) {
        let v = iter.next();
        if (v.done) {
          if (all && chunk.length > 0) {
            yield chunk;
          }
          return;
        }
        chunk.push(v.value);
      }
      yield chunk;
      chunk = step2 < n ? chunk.slice(step2) : [];
      if (step2 > n) {
        consume2(iter, step2 - n);
      }
    }
  }

  // node_modules/@thi.ng/iterators/random-sample.js
  function* randomSample(prob, input) {
    let iter = iterator2(input);
    let random2 = Math.random;
    let v;
    while (v = iter.next(), !v.done) {
      if (random2() < prob) {
        yield v.value;
      }
    }
  }

  // node_modules/@thi.ng/iterators/reduce.js
  var ReducedValue = class {
    value;
    constructor(v) {
      this.value = v;
    }
  };
  var reduce2 = (rfn, acc, input) => {
    let iter = iterator2(input);
    let v;
    let _acc = acc;
    while (v = iter.next(), !v.done) {
      _acc = rfn(_acc, v.value);
      if (_acc instanceof ReducedValue) {
        return _acc.value;
      }
    }
    return _acc;
  };
  var reduced2 = (x) => new ReducedValue(x);

  // node_modules/@thi.ng/iterators/reductions.js
  function* reductions2(rfn, acc, input) {
    let iter = iterator2(input);
    let v;
    let _acc = acc;
    let empty = true;
    while (v = iter.next(), !v.done) {
      _acc = rfn(_acc, v.value);
      if (_acc instanceof ReducedValue) {
        yield _acc.value;
        return;
      }
      yield _acc;
      empty = false;
    }
    if (empty) {
      yield _acc;
    }
  }

  // node_modules/@thi.ng/iterators/repeat.js
  function* repeat2(x, n = Infinity) {
    while (n-- > 0) {
      yield x;
    }
  }

  // node_modules/@thi.ng/iterators/repeatedly.js
  function* repeatedly2(fn, n = Infinity) {
    while (n-- > 0) {
      yield fn();
    }
  }

  // node_modules/@thi.ng/iterators/reverse.js
  function* reverse3(input) {
    if (!(input.constructor === Array || input.length !== void 0)) {
      input = [...input];
    }
    let n = input.length;
    while (n-- > 0) {
      yield input[n];
    }
  }

  // node_modules/@thi.ng/iterators/run.js
  var run2 = (fn, input) => {
    let iter = iterator2(input);
    let v;
    while (v = iter.next(), !v.done) {
      fn(v.value);
    }
  };

  // node_modules/@thi.ng/iterators/some.js
  var some2 = (pred, input) => {
    let iter = iterator2(input);
    let v;
    while (v = iter.next(), !v.done) {
      if (pred(v.value) === true) {
        return v.value;
      }
    }
  };

  // node_modules/@thi.ng/iterators/take-last.js
  function* takeLast2(n, input) {
    let iter = iterator2(input);
    let v;
    let prev = [];
    while (v = iter.next(), !v.done) {
      prev.push(v.value);
      if (prev.length > n) {
        prev = prev.slice(1);
      }
    }
    yield* prev;
  }

  // node_modules/@thi.ng/iterators/take-nth.js
  function* takeNth2(n, input) {
    let iter = iterator2(input);
    let v;
    while (v = iter.next(), !v.done) {
      yield v.value;
      consume2(iter, n - 1);
    }
  }

  // node_modules/@thi.ng/iterators/take-while.js
  function* takeWhile2(pred, input) {
    let iter = iterator2(input);
    let v;
    while (v = iter.next(), !v.done && pred(v.value)) {
      yield v.value;
    }
  }

  // node_modules/@thi.ng/iterators/walk.js
  var walkable = (x) => typeof x !== "string" ? maybeIterator(x) || maybeObjectIterator(x) : void 0;
  function walk(fn, ...args) {
    let children;
    let input;
    let postOrder;
    if (args.length === 3) {
      [children, input, postOrder] = args;
    } else if (args.length === 2 && typeof args[0] === "function") {
      [children, input] = args;
    } else {
      [input, postOrder] = args;
    }
    let inner = (iter) => {
      let v;
      while (v = iter.next(), !v.done) {
        if (!postOrder) {
          fn(v.value);
        }
        let cvals;
        if (children) {
          cvals = children(v.value);
        } else {
          cvals = v.value;
        }
        if ((cvals = walkable(cvals)) !== void 0) {
          inner(cvals);
        }
        if (postOrder) {
          fn(v.value);
        }
      }
    };
    inner(iterator2([input]));
  }
  function walkIterator(input, ...args) {
    let children;
    let postOrder;
    if (args.length === 2) {
      [children, postOrder] = args;
    } else if (typeof args[0] === "function") {
      children = args[0];
    } else {
      postOrder = args[0];
    }
    let walk2 = function* (iter) {
      let v;
      while (v = iter.next(), !v.done) {
        if (!postOrder) {
          yield v.value;
        }
        let cvals;
        if (children) {
          cvals = children(v.value);
        } else {
          cvals = v.value;
        }
        if ((cvals = walkable(cvals)) !== void 0) {
          yield* walk2(cvals);
        }
        if (postOrder) {
          yield v.value;
        }
      }
    };
    return walk2(iterator2([input]));
  }

  // node_modules/@thi.ng/iterators/zip.js
  var zip2 = (keys2, vals2, target) => {
    let kiter = iterator2(keys2);
    let viter = iterator2(vals2);
    let k;
    let v;
    target = target || {};
    while (true) {
      k = kiter.next();
      v = viter.next();
      if (k.done || v.done) {
        return target;
      }
      target[k.value] = v.value;
    }
  };

  // node_modules/@thi.ng/rstream/index.js
  var rstream_exports = {};
  __export(rstream_exports, {
    ASidechain: () => ASidechain,
    CloseMode: () => CloseMode,
    LOGGER: () => LOGGER,
    MetaStream: () => MetaStream,
    PubSub: () => PubSub2,
    Resolver: () => Resolver,
    SidechainPartition: () => SidechainPartition,
    SidechainToggle: () => SidechainToggle,
    SidechainTrigger: () => SidechainTrigger,
    State: () => State,
    Stream: () => Stream,
    StreamMerge: () => StreamMerge,
    StreamSync: () => StreamSync,
    Subscription: () => Subscription,
    SyncRAF: () => SyncRAF,
    Tunnel: () => Tunnel,
    __nextID: () => __nextID,
    __optsWithID: () => __optsWithID,
    bisect: () => bisect,
    debounce: () => debounce,
    defInlineWorker: () => defInlineWorker,
    defWorker: () => defWorker,
    forkBuffer: () => forkBuffer,
    forkJoin: () => forkJoin,
    fromAtom: () => fromAtom,
    fromDOMEvent: () => fromDOMEvent,
    fromEvent: () => fromEvent,
    fromInterval: () => fromInterval,
    fromIterable: () => fromIterable,
    fromIterableSync: () => fromIterableSync,
    fromNodeJS: () => fromNodeJS,
    fromObject: () => fromObject,
    fromPromise: () => fromPromise,
    fromPromises: () => fromPromises,
    fromRAF: () => fromRAF,
    fromView: () => fromView,
    fromViewUnsafe: () => fromViewUnsafe,
    fromWorker: () => fromWorker,
    isFirstOrLastInput: () => isFirstOrLastInput,
    isSubscribable: () => isSubscribable,
    joinBuffer: () => joinBuffer,
    linesFromNodeJS: () => linesFromNodeJS,
    merge: () => merge,
    metaStream: () => metaStream,
    postWorker: () => postWorker,
    pubsub: () => pubsub,
    reactive: () => reactive,
    resolve: () => resolve,
    setLogger: () => setLogger,
    sidechainPartition: () => sidechainPartition,
    sidechainPartitionRAF: () => sidechainPartitionRAF,
    sidechainToggle: () => sidechainToggle,
    sidechainTrigger: () => sidechainTrigger,
    stream: () => stream,
    subscription: () => subscription,
    sync: () => sync,
    syncRAF: () => syncRAF,
    timeout: () => timeout,
    toggle: () => toggle2,
    trace: () => trace2,
    transduce: () => transduce2,
    trigger: () => trigger,
    tunnel: () => tunnel,
    tween: () => tween2,
    tweenNumber: () => tweenNumber
  });

  // node_modules/@thi.ng/rstream/api.js
  var State = /* @__PURE__ */ ((State2) => {
    State2[State2["IDLE"] = 0] = "IDLE";
    State2[State2["ACTIVE"] = 1] = "ACTIVE";
    State2[State2["DONE"] = 2] = "DONE";
    State2[State2["UNSUBSCRIBED"] = 3] = "UNSUBSCRIBED";
    State2[State2["ERROR"] = 4] = "ERROR";
    return State2;
  })(State || {});
  var CloseMode = /* @__PURE__ */ ((CloseMode2) => {
    CloseMode2[CloseMode2["NEVER"] = 0] = "NEVER";
    CloseMode2[CloseMode2["FIRST"] = 1] = "FIRST";
    CloseMode2[CloseMode2["LAST"] = 2] = "LAST";
    return CloseMode2;
  })(CloseMode || {});

  // node_modules/@thi.ng/logger/api.js
  var LogLevel = /* @__PURE__ */ ((LogLevel2) => {
    LogLevel2[LogLevel2["FINE"] = 0] = "FINE";
    LogLevel2[LogLevel2["DEBUG"] = 1] = "DEBUG";
    LogLevel2[LogLevel2["INFO"] = 2] = "INFO";
    LogLevel2[LogLevel2["WARN"] = 3] = "WARN";
    LogLevel2[LogLevel2["SEVERE"] = 4] = "SEVERE";
    LogLevel2[LogLevel2["NONE"] = 5] = "NONE";
    return LogLevel2;
  })(LogLevel || {});

  // node_modules/@thi.ng/logger/null.js
  var NULL_LOGGER = Object.freeze({
    level: LogLevel.NONE,
    enabled: () => false,
    fine() {
    },
    debug() {
    },
    info() {
    },
    warn() {
    },
    severe() {
    }
  });

  // node_modules/@thi.ng/rstream/idgen.js
  var NEXT_ID = 0;
  var __nextID = () => NEXT_ID++;
  var __optsWithID = (prefix, opts) => !opts || !opts.id ? { ...opts, id: prefix + "-" + __nextID() } : opts;

  // node_modules/@thi.ng/rstream/logger.js
  var LOGGER = NULL_LOGGER;
  var setLogger = (logger) => LOGGER = logger;

  // node_modules/@thi.ng/rstream/subscription.js
  var subscription = (sub2, opts) => new Subscription(sub2, opts);
  var Subscription = class _Subscription {
    constructor(wrapped, opts) {
      this.wrapped = wrapped;
      opts = __optsWithID(`sub`, {
        closeIn: CloseMode.LAST,
        closeOut: CloseMode.LAST,
        cache: true,
        ...opts
      });
      this.parent = opts.parent;
      this.id = opts.id;
      this.closeIn = opts.closeIn;
      this.closeOut = opts.closeOut;
      this.cacheLast = opts.cache;
      opts.xform && (this.xform = opts.xform(push()));
    }
    id;
    closeIn;
    closeOut;
    parent;
    __owner;
    xform;
    cacheLast;
    last = SEMAPHORE;
    state = State.IDLE;
    subs = [];
    deref() {
      return this.last !== SEMAPHORE ? this.last : void 0;
    }
    getState() {
      return this.state;
    }
    setState(state) {
      this.state = state;
    }
    subscribe(sub2, opts = {}) {
      this.ensureState();
      let $sub;
      if (sub2 instanceof _Subscription && !opts.xform) {
        sub2.ensureState();
        assert(!sub2.parent, `sub '${sub2.id}' already has a parent`);
        sub2.parent = this;
        $sub = sub2;
      } else {
        $sub = new _Subscription(sub2, { ...opts, parent: this });
      }
      this.subs.push($sub);
      this.setState(State.ACTIVE);
      $sub.setState(State.ACTIVE);
      this.last != SEMAPHORE && $sub.next(this.last);
      return $sub;
    }
    transform(...args) {
      let sub2;
      let opts;
      if (isPlainObject(peek(args))) {
        opts = args.pop();
        sub2 = { error: opts.error };
      }
      return this.subscribe(
        sub2,
        __optsWithID(
          "xform",
          args.length > 0 ? {
            ...opts,
            // @ts-ignore
            xform: comp2(...args)
          } : opts
        )
      );
    }
    /**
     * Syntax sugar for {@link Subscription.transform} when using a single
     * [`map()`](https://docs.thi.ng/umbrella/transducers/functions/map.html)
     * transducer only. The given function `fn` is used as `map`'s
     * transformation fn.
     *
     * @param fn -
     * @param opts -
     */
    map(fn, opts) {
      return this.transform(map(fn), opts || {});
    }
    unsubscribe(sub2) {
      return sub2 ? this.unsubscribeChild(sub2) : this.unsubscribeSelf();
    }
    unsubscribeSelf() {
      LOGGER.debug(this.id, "unsub self");
      this.parent && this.parent.unsubscribe(this);
      this.state < State.UNSUBSCRIBED && (this.state = State.UNSUBSCRIBED);
      this.release();
      return true;
    }
    unsubscribeChild(sub2) {
      LOGGER.debug(this.id, "unsub child", sub2.id);
      const idx = this.subs.indexOf(sub2);
      if (idx >= 0) {
        this.subs.splice(idx, 1);
        if (this.closeOut === CloseMode.FIRST || !this.subs.length && this.closeOut !== CloseMode.NEVER) {
          this.unsubscribe();
        }
        return true;
      }
      return false;
    }
    next(x) {
      if (this.state >= State.DONE)
        return;
      this.xform ? this.dispatchXform(x) : this.dispatch(x);
    }
    done() {
      LOGGER.debug(this.id, "entering done()");
      if (this.state >= State.DONE)
        return;
      if (this.xform) {
        if (!this.dispatchXformDone())
          return;
      }
      this.state = State.DONE;
      if (this.dispatchTo("done")) {
        this.state < State.UNSUBSCRIBED && this.unsubscribe();
      }
      LOGGER.debug(this.id, "exiting done()");
    }
    error(e) {
      const sub2 = this.wrapped;
      const hasErrorHandler = sub2 && sub2.error;
      hasErrorHandler && LOGGER.debug(this.id, "attempting wrapped error handler");
      return hasErrorHandler && sub2.error(e) || this.unhandledError(e);
    }
    unhandledError(e) {
      (LOGGER !== NULL_LOGGER ? LOGGER : console).warn(
        this.id,
        "unhandled error:",
        e
      );
      this.unsubscribe();
      this.state = State.ERROR;
      return false;
    }
    dispatchTo(type, x) {
      let s = this.wrapped;
      if (s) {
        try {
          s[type] && s[type](x);
        } catch (e) {
          if (!this.error(e))
            return false;
        }
      }
      const subs = type === "next" ? this.subs : [...this.subs];
      for (let i = subs.length; i-- > 0; ) {
        s = subs[i];
        try {
          s[type] && s[type](x);
        } catch (e) {
          if (type === "error" || !s.error || !s.error(e)) {
            return this.unhandledError(e);
          }
        }
      }
      return true;
    }
    dispatch(x) {
      LOGGER.debug(this.id, "dispatch", x);
      this.cacheLast && (this.last = x);
      this.dispatchTo("next", x);
    }
    dispatchXform(x) {
      let acc;
      try {
        acc = this.xform[2]([], x);
      } catch (e) {
        this.error(e);
        return;
      }
      if (this.dispatchXformVals(acc)) {
        isReduced(acc) && this.done();
      }
    }
    dispatchXformDone() {
      let acc;
      try {
        acc = this.xform[1]([]);
      } catch (e) {
        return this.error(e);
      }
      return this.dispatchXformVals(acc);
    }
    dispatchXformVals(acc) {
      const uacc = unreduced(acc);
      for (let i = 0, n = uacc.length; i < n && this.state < State.DONE; i++) {
        this.dispatch(uacc[i]);
      }
      return this.state < State.ERROR;
    }
    ensureState() {
      if (this.state >= State.DONE) {
        illegalState(`operation not allowed in state ${this.state}`);
      }
    }
    release() {
      this.subs.length = 0;
      delete this.parent;
      delete this.xform;
      delete this.last;
    }
  };

  // node_modules/@thi.ng/rstream/asidechain.js
  var ASidechain = class extends Subscription {
    sideSub;
    constructor(opts) {
      super(void 0, opts);
    }
    unsubscribe(sub2) {
      const res = super.unsubscribe(sub2);
      if (!sub2 || !this.subs.length) {
        this.sideSub.unsubscribe();
      }
      return res;
    }
    done() {
      this.sideSub.unsubscribe();
      super.done();
    }
  };

  // node_modules/@thi.ng/rstream/stream.js
  function stream(src, opts) {
    return new Stream(src, opts);
  }
  var reactive = (val, opts) => {
    const res = new Stream(opts);
    res.next(val);
    return res;
  };
  var Stream = class extends Subscription {
    src;
    _cancel;
    _inited;
    constructor(src, opts) {
      const [_src, _opts] = isFunction(src) ? [src, opts || {}] : [void 0, src || {}];
      super(
        _opts.error ? { error: _opts.error } : void 0,
        __optsWithID("stream", _opts)
      );
      this.src = _src;
      this._inited = false;
    }
    subscribe(sub2, opts = {}) {
      const $sub = super.subscribe(sub2, opts);
      if (!this._inited) {
        if (this.src) {
          try {
            this._cancel = this.src(this) || (() => void 0);
          } catch (e) {
            let s = this.wrapped;
            if (!s || !s.error || !s.error(e)) {
              this.unhandledError(e);
            }
          }
        }
        this._inited = true;
      }
      return $sub;
    }
    unsubscribe(sub2) {
      const res = super.unsubscribe(sub2);
      if (res && (!sub2 || (!this.subs || !this.subs.length) && this.closeOut !== CloseMode.NEVER)) {
        this.cancel();
      }
      return res;
    }
    done() {
      this.cancel();
      super.done();
      delete this.src;
      delete this._cancel;
    }
    error(e) {
      if (super.error(e))
        return true;
      this.cancel();
      return false;
    }
    cancel() {
      if (this._cancel) {
        LOGGER.debug(this.id, "cancel");
        const f = this._cancel;
        delete this._cancel;
        f();
      }
    }
  };

  // node_modules/@thi.ng/rstream/atom.js
  var fromAtom = (atom, opts) => {
    opts = __optsWithID("atom", {
      emitFirst: true,
      changed: (a, b) => a !== b,
      ...opts
    });
    return stream((stream2) => {
      atom.addWatch(stream2.id, (_, prev, curr) => {
        if (opts.changed(prev, curr)) {
          stream2.next(curr);
        }
      });
      opts.emitFirst && stream2.next(atom.deref());
      return () => atom.removeWatch(stream2.id);
    }, opts);
  };

  // node_modules/@thi.ng/arrays/find.js
  var findIndex = (buf, x, equiv2 = equiv) => {
    for (let i = buf.length; i-- > 0; ) {
      if (equiv2(x, buf[i]))
        return i;
    }
    return -1;
  };

  // node_modules/@thi.ng/associative/dissoc.js
  function dissoc(coll, keys2) {
    for (let k of keys2) {
      coll.delete(k);
    }
    return coll;
  }

  // node_modules/@thi.ng/associative/internal/equiv.js
  var __equivMap = (a, b) => {
    if (a === b) {
      return true;
    }
    if (!(b instanceof Map) || a.size !== b.size) {
      return false;
    }
    for (let p of a.entries()) {
      if (!equiv(b.get(p[0]), p[1])) {
        return false;
      }
    }
    return true;
  };
  var __equivSet = (a, b) => {
    if (a === b) {
      return true;
    }
    if (!(b instanceof Set) || a.size !== b.size) {
      return false;
    }
    for (let k of a.keys()) {
      if (!b.has(k)) {
        return false;
      }
    }
    return true;
  };

  // node_modules/@thi.ng/api/mixin.js
  var mixin = (behaviour, sharedBehaviour = {}) => {
    const instanceKeys = Reflect.ownKeys(behaviour);
    const sharedKeys = Reflect.ownKeys(sharedBehaviour);
    const typeTag = Symbol("isa");
    function _mixin(clazz) {
      for (let key of instanceKeys) {
        const existing = Object.getOwnPropertyDescriptor(
          clazz.prototype,
          key
        );
        if (!existing || existing.configurable) {
          Object.defineProperty(clazz.prototype, key, {
            value: behaviour[key],
            writable: true
          });
        } else {
        }
      }
      Object.defineProperty(clazz.prototype, typeTag, { value: true });
      return clazz;
    }
    for (let key of sharedKeys) {
      Object.defineProperty(_mixin, key, {
        value: sharedBehaviour[key],
        enumerable: sharedBehaviour.propertyIsEnumerable(key)
      });
    }
    Object.defineProperty(_mixin, Symbol.hasInstance, {
      value: (x) => !!x[typeTag]
    });
    return _mixin;
  };

  // node_modules/@thi.ng/associative/internal/inspect.js
  var inspect = null;
  isNode() && Promise.resolve().then(() => __toESM(require_util(), 1)).then((m) => {
    inspect = m.inspect;
  });
  var inspectSet = (coll, opts) => [...map((x) => inspect(x, opts), coll)].join(", ");
  var inspectMap = (coll, opts) => [
    ...map(
      ([k, v]) => `${inspect(k, opts)} => ${inspect(v, opts)}`,
      coll
    )
  ].join(", ");
  var __inspectable = mixin({
    [Symbol.for("nodejs.util.inspect.custom")](depth, opts) {
      const name = this[Symbol.toStringTag];
      const childOpts = {
        ...opts,
        depth: opts.depth === null ? null : opts.depth - 1
      };
      return depth >= 0 ? [
        `${name}(${this.size || 0}) {`,
        inspect ? this instanceof Set ? inspectSet(this, childOpts) : this instanceof Map ? inspectMap(this, childOpts) : "" : "",
        "}"
      ].join(" ") : opts.stylize(`[${name}]`, "special");
    }
  });

  // node_modules/@thi.ng/associative/into.js
  function into(dest, src) {
    if (isMap(dest)) {
      for (let x of src) {
        dest.set(x[0], x[1]);
      }
    } else {
      for (let x of src) {
        dest.add(x);
      }
    }
    return dest;
  }

  // node_modules/@thi.ng/associative/array-set.js
  var __defProp2 = Object.defineProperty;
  var __getOwnPropDesc2 = Object.getOwnPropertyDescriptor;
  var __decorateClass = (decorators, target, key, kind) => {
    var result = kind > 1 ? void 0 : kind ? __getOwnPropDesc2(target, key) : target;
    for (var i = decorators.length - 1, decorator; i >= 0; i--)
      if (decorator = decorators[i])
        result = (kind ? decorator(target, key, result) : decorator(result)) || result;
    if (kind && result)
      __defProp2(target, key, result);
    return result;
  };
  var __private = /* @__PURE__ */ new WeakMap();
  var __vals = (inst) => __private.get(inst).vals;
  var ArraySet = class extends Set {
    constructor(vals2, opts = {}) {
      super();
      __private.set(this, { equiv: opts.equiv || equiv, vals: [] });
      vals2 && this.into(vals2);
    }
    *[Symbol.iterator]() {
      yield* __vals(this);
    }
    get [Symbol.species]() {
      return ArraySet;
    }
    get [Symbol.toStringTag]() {
      return "ArraySet";
    }
    get size() {
      return __vals(this).length;
    }
    copy() {
      const { equiv: equiv2, vals: vals2 } = __private.get(this);
      const s = new ArraySet(null, { equiv: equiv2 });
      __private.get(s).vals = vals2.slice();
      return s;
    }
    empty() {
      return new ArraySet(null, this.opts());
    }
    clear() {
      __vals(this).length = 0;
    }
    first() {
      if (this.size) {
        return __vals(this)[0];
      }
    }
    add(key) {
      !this.has(key) && __vals(this).push(key);
      return this;
    }
    into(keys2) {
      return into(this, keys2);
    }
    has(key) {
      return this.get(key, SEMAPHORE) !== SEMAPHORE;
    }
    /**
     * Returns the canonical value for `x`, if present. If the set
     * contains no equivalent for `x`, returns `notFound`.
     *
     * @param key - search key
     * @param notFound - default value
     */
    get(key, notFound) {
      const { equiv: equiv2, vals: vals2 } = __private.get(this);
      const i = findIndex(vals2, key, equiv2);
      return i >= 0 ? vals2[i] : notFound;
    }
    delete(key) {
      const { equiv: equiv2, vals: vals2 } = __private.get(this);
      for (let i = vals2.length; i-- > 0; ) {
        if (equiv2(vals2[i], key)) {
          vals2.splice(i, 1);
          return true;
        }
      }
      return false;
    }
    disj(keys2) {
      return dissoc(this, keys2);
    }
    equiv(o) {
      return __equivSet(this, o);
    }
    /**
     * The value args given to the callback `fn` MUST be treated as
     * readonly/immutable. This could be enforced via TS, but would
     * break ES6 Set interface contract.
     *
     * @param fn -
     * @param thisArg -
     */
    forEach(fn, thisArg) {
      const vals2 = __vals(this);
      for (let i = vals2.length; i-- > 0; ) {
        const v = vals2[i];
        fn.call(thisArg, v, v, this);
      }
    }
    *entries() {
      for (let v of __vals(this)) {
        yield [v, v];
      }
    }
    *keys() {
      yield* __vals(this);
    }
    *values() {
      yield* __vals(this);
    }
    opts() {
      return { equiv: __private.get(this).equiv };
    }
  };
  ArraySet = __decorateClass([
    __inspectable
  ], ArraySet);

  // node_modules/@thi.ng/associative/equiv-map.js
  var __defProp3 = Object.defineProperty;
  var __getOwnPropDesc3 = Object.getOwnPropertyDescriptor;
  var __decorateClass2 = (decorators, target, key, kind) => {
    var result = kind > 1 ? void 0 : kind ? __getOwnPropDesc3(target, key) : target;
    for (var i = decorators.length - 1, decorator; i >= 0; i--)
      if (decorator = decorators[i])
        result = (kind ? decorator(target, key, result) : decorator(result)) || result;
    if (kind && result)
      __defProp3(target, key, result);
    return result;
  };
  var __private2 = /* @__PURE__ */ new WeakMap();
  var __map = (map3) => __private2.get(map3).map;
  var EquivMap = class extends Map {
    /**
     * Creates a new instance with optional initial key-value pairs and provided
     * options. If no `opts` are given, uses `ArraySet` for storing canonical
     * keys and
     * [`equiv()`](https://docs.thi.ng/umbrella/equiv/functions/equiv.html) for
     * checking key equivalence.
     *
     * @param pairs - key-value pairs
     * @param opts - config options
     */
    constructor(pairs2, opts) {
      super();
      const _opts = { equiv, keys: ArraySet, ...opts };
      __private2.set(this, {
        keys: new _opts.keys(null, { equiv: _opts.equiv }),
        map: /* @__PURE__ */ new Map(),
        opts: _opts
      });
      if (pairs2) {
        this.into(pairs2);
      }
    }
    [Symbol.iterator]() {
      return this.entries();
    }
    get [Symbol.species]() {
      return EquivMap;
    }
    get [Symbol.toStringTag]() {
      return "EquivMap";
    }
    get size() {
      return __private2.get(this).keys.size;
    }
    clear() {
      const { keys: keys2, map: map3 } = __private2.get(this);
      keys2.clear();
      map3.clear();
    }
    empty() {
      return new EquivMap(null, __private2.get(this).opts);
    }
    copy() {
      const { keys: keys2, map: map3, opts } = __private2.get(this);
      const m = new EquivMap();
      __private2.set(m, {
        keys: keys2.copy(),
        map: new Map(map3),
        opts
      });
      return m;
    }
    equiv(o) {
      return __equivMap(this, o);
    }
    delete(key) {
      const { keys: keys2, map: map3 } = __private2.get(this);
      key = keys2.get(key, SEMAPHORE);
      if (key !== SEMAPHORE) {
        map3.delete(key);
        keys2.delete(key);
        return true;
      }
      return false;
    }
    dissoc(keys2) {
      return dissoc(this, keys2);
    }
    /**
     * The key & value args given the callback `fn` MUST be treated as
     * readonly/immutable. This could be enforced via TS, but would
     * break ES6 Map interface contract.
     *
     * @param fn -
     * @param thisArg -
     */
    forEach(fn, thisArg) {
      for (let pair of __map(this)) {
        fn.call(thisArg, pair[1], pair[0], this);
      }
    }
    get(key, notFound) {
      const { keys: keys2, map: map3 } = __private2.get(this);
      key = keys2.get(key, SEMAPHORE);
      if (key !== SEMAPHORE) {
        return map3.get(key);
      }
      return notFound;
    }
    has(key) {
      return __private2.get(this).keys.has(key);
    }
    set(key, value) {
      const { keys: keys2, map: map3 } = __private2.get(this);
      const k = keys2.get(key, SEMAPHORE);
      if (k !== SEMAPHORE) {
        map3.set(k, value);
      } else {
        keys2.add(key);
        map3.set(key, value);
      }
      return this;
    }
    into(pairs2) {
      return into(this, pairs2);
    }
    entries() {
      return __map(this).entries();
    }
    keys() {
      return __map(this).keys();
    }
    values() {
      return __map(this).values();
    }
    opts() {
      return __private2.get(this).opts;
    }
  };
  EquivMap = __decorateClass2([
    __inspectable
  ], EquivMap);

  // node_modules/@thi.ng/errors/unsupported.js
  var UnsupportedOperationError = defError(
    () => "unsupported operation"
  );
  var unsupported = (msg) => {
    throw new UnsupportedOperationError(msg);
  };

  // node_modules/@thi.ng/rstream/pubsub.js
  var pubsub = (opts) => new PubSub2(opts);
  var PubSub2 = class extends Subscription {
    topicfn;
    topics;
    constructor(opts) {
      super(
        void 0,
        __optsWithID("pubsub", {
          xform: opts.xform
        })
      );
      this.topicfn = opts.topic;
      this.topics = new EquivMap(void 0, {
        equiv: opts.equiv
      });
    }
    /**
     * Unsupported. Use {@link PubSub.(subscribeTopic:1)} instead.
     */
    subscribe() {
      return unsupported(`use subscribeTopic() instead`);
    }
    /**
     * Unsupported. Use {@link PubSub.(subscribeTopic:1)} instead.
     */
    transform() {
      return unsupported(`use subscribeTopic() instead`);
    }
    subscribeTopic(topicID, sub2, opts) {
      let t = this.topics.get(topicID);
      !t && this.topics.set(
        topicID,
        t = subscription(
          void 0,
          __optsWithID("topic", {
            closeOut: CloseMode.NEVER
          })
        )
      );
      return t.subscribe(sub2, opts);
    }
    transformTopic(topicID, xform, opts = {}) {
      return this.subscribeTopic(
        topicID,
        { error: opts.error },
        {
          ...opts,
          xform
        }
      );
    }
    unsubscribeTopic(topicID, sub2) {
      const t = this.topics.get(topicID);
      return t ? t.unsubscribe(sub2) : false;
    }
    unsubscribe(sub2) {
      if (!sub2) {
        for (let t of this.topics.values()) {
          t.unsubscribe();
        }
        this.topics.clear();
        return super.unsubscribe();
      }
      return unsupported();
    }
    done() {
      for (let t of this.topics.values()) {
        t.done();
      }
      super.done();
    }
    dispatch(x) {
      LOGGER.debug(this.id, "dispatch", x);
      this.cacheLast && (this.last = x);
      const t = this.topicfn(x);
      if (t !== void 0) {
        const sub2 = this.topics.get(t);
        if (sub2) {
          try {
            sub2.next && sub2.next(x);
          } catch (e) {
            if (!sub2.error || !sub2.error(e)) {
              return this.unhandledError(e);
            }
          }
        }
      }
    }
  };

  // node_modules/@thi.ng/rstream/bisect.js
  var bisect = (pred, truthy, falsy) => {
    const sub2 = new PubSub2({ topic: pred, id: `bisect-${__nextID()}` });
    truthy && sub2.subscribeTopic(true, truthy);
    falsy && sub2.subscribeTopic(false, falsy);
    return sub2;
  };

  // node_modules/@thi.ng/rstream/checks.js
  var isSubscribable = (x) => implementsFunction(x, "subscribe");
  var isFirstOrLastInput = (mode, num) => mode === CloseMode.FIRST || mode === CloseMode.LAST && !num;

  // node_modules/@thi.ng/rstream/iterable.js
  var fromIterable = (src, opts = {}) => stream((stream2) => {
    const iter = src[Symbol.iterator]();
    const id = setInterval(() => {
      let val;
      if ((val = iter.next()).done) {
        clearInterval(id);
        stream2.closeIn !== CloseMode.NEVER && stream2.done();
      } else {
        stream2.next(val.value);
      }
    }, opts.delay || 0);
    return () => clearInterval(id);
  }, __optsWithID("iterable", opts));
  var fromIterableSync = (src, opts) => stream((stream2) => {
    for (let s of src) {
      stream2.next(s);
    }
    stream2.closeIn !== CloseMode.NEVER && stream2.done();
  }, __optsWithID("iterable-sync", opts));

  // node_modules/@thi.ng/rstream/metastream.js
  var metaStream = (factory, opts) => new MetaStream(factory, opts);
  var MetaStream = class extends Subscription {
    factory;
    stream;
    sub;
    emitLast;
    doneRequested;
    constructor(factory, opts = {}) {
      super(void 0, __optsWithID("metastram", opts));
      this.factory = factory;
      this.emitLast = opts.emitLast === true;
      this.doneRequested = false;
    }
    next(x) {
      if (this.state < State.DONE) {
        if (this.stream) {
          this.stream.unsubscribe(this.sub);
        }
        let stream2 = this.factory(x);
        if (stream2) {
          this.stream = stream2;
          this.sub = this.stream.subscribe({
            next: (x2) => {
              stream2 === this.stream && super.dispatch(x2);
              this.doneRequested && this.done();
            },
            done: () => {
              this.stream.unsubscribe(this.sub);
              if (stream2 === this.stream) {
                this.stream = void 0;
                this.sub = void 0;
              }
            },
            error: (e) => super.error(e),
            __owner: this
          });
        }
      }
    }
    done() {
      if (this.emitLast && !this.doneRequested) {
        this.doneRequested = true;
      } else {
        if (this.stream) {
          this.detach(true);
        }
        this.closeIn !== CloseMode.NEVER && super.done();
      }
    }
    unsubscribe(sub2) {
      if (this.stream && (!sub2 || this.subs.length === 1)) {
        this.detach(!sub2);
      }
      return super.unsubscribe(sub2);
    }
    detach(force) {
      if (force || this.closeOut !== CloseMode.NEVER) {
        assert(!!this.stream, "input stream already removed");
        this.stream.unsubscribe(this.sub);
        delete this.stream;
        delete this.sub;
      }
    }
  };

  // node_modules/@thi.ng/rstream/debounce.js
  var debounce = (delay2, opts) => metaStream(
    (x) => fromIterable([x], { delay: delay2 }),
    __optsWithID("debounce", {
      emitLast: true,
      ...opts
    })
  );

  // node_modules/@thi.ng/rstream/defworker.js
  var defInlineWorker = (src) => defWorker(new Blob([src], { type: "text/javascript" }));
  var defWorker = (worker) => worker instanceof Worker ? worker : isFunction(worker) ? worker() : new Worker(
    worker instanceof Blob ? URL.createObjectURL(worker) : worker
  );

  // node_modules/@thi.ng/rstream/event.js
  var fromEvent = (src, name, listenerOpts = false, streamOpts) => {
    const result = stream((stream2) => {
      let listener = (e) => stream2.next(e);
      src.addEventListener(name, listener, listenerOpts);
      return () => src.removeEventListener(name, listener, listenerOpts);
    }, __optsWithID(`event-${name}`, streamOpts));
    streamOpts?.init !== void 0 && result.next(streamOpts.init);
    return result;
  };
  var fromDOMEvent = (src, name, listenerOpts = false, streamOpts) => fromEvent(src, name, listenerOpts, streamOpts);

  // node_modules/@thi.ng/rstream/internal/remove.js
  var __removeAllIDs = (impl, ids) => {
    let ok = true;
    for (let id of ids) {
      ok = impl.removeID(id) && ok;
    }
    return ok;
  };

  // node_modules/@thi.ng/rstream/sync.js
  var sync = (opts) => new StreamSync(opts);
  var StreamSync = class extends Subscription {
    /**
     * maps actual inputs to their virtual input subs
     */
    sources;
    /**
     * maps real source IDs to their actual input
     */
    idSources;
    /**
     * maps (potentially aliased) input IDs to their actual src.id
     */
    realSourceIDs;
    /**
     * maps real src.id to (potentially aliased) input IDs
     */
    invRealSourceIDs;
    psync;
    clean;
    constructor(opts) {
      const psync = partitionSync(/* @__PURE__ */ new Set(), {
        key: (x) => x[0],
        mergeOnly: opts.mergeOnly === true,
        reset: opts.reset === true,
        all: opts.all !== false,
        backPressure: opts.backPressure || 0
      });
      const mapv = mapVals((x) => x[1]);
      super(
        void 0,
        __optsWithID("streamsync", {
          ...opts,
          xform: opts.xform ? comp2(psync, mapv, opts.xform) : comp2(psync, mapv)
        })
      );
      this.sources = /* @__PURE__ */ new Map();
      this.realSourceIDs = /* @__PURE__ */ new Map();
      this.invRealSourceIDs = /* @__PURE__ */ new Map();
      this.idSources = /* @__PURE__ */ new Map();
      this.psync = psync;
      this.clean = !!opts.clean;
      opts.src && this.addAll(opts.src);
    }
    add(src, id) {
      id || (id = src.id);
      this.ensureState();
      this.psync.add(id);
      this.realSourceIDs.set(id, src.id);
      this.invRealSourceIDs.set(src.id, id);
      this.idSources.set(src.id, src);
      this.sources.set(
        src,
        src.subscribe(
          {
            next: (x) => (
              // if received value is sub, add it as source
              x[1] instanceof Subscription ? this.add(x[1]) : this.next(x)
            ),
            done: () => this.markDone(src),
            __owner: this
          },
          { xform: labeled(id), id: `in-${id}` }
        )
      );
    }
    addAll(src) {
      for (let id in src) {
        this.psync.add(id);
      }
      for (let id in src) {
        this.add(src[id], id);
      }
    }
    remove(src) {
      const sub2 = this.sources.get(src);
      if (sub2) {
        const id = this.invRealSourceIDs.get(src.id);
        LOGGER.info(`removing src: ${src.id} (${id})`);
        this.psync.delete(id, this.clean);
        this.realSourceIDs.delete(id);
        this.invRealSourceIDs.delete(src.id);
        this.idSources.delete(src.id);
        this.sources.delete(src);
        sub2.unsubscribe();
        return true;
      }
      return false;
    }
    removeID(id) {
      const src = this.getSourceForID(id);
      return src ? this.remove(src) : false;
    }
    removeAll(src) {
      for (let s of src) {
        this.psync.delete(this.invRealSourceIDs.get(s.id));
      }
      let ok = true;
      for (let s of src) {
        ok = this.remove(s) && ok;
      }
      return ok;
    }
    removeAllIDs(ids) {
      return __removeAllIDs(this, ids);
    }
    getSourceForID(id) {
      return this.idSources.get(this.realSourceIDs.get(id));
    }
    getSources() {
      const res = {};
      for (let [id, src] of this.idSources) {
        res[this.invRealSourceIDs.get(id)] = src;
      }
      return res;
    }
    unsubscribe(sub2) {
      if (!sub2) {
        LOGGER.debug(this.id, "unsub sources");
        for (let s of this.sources.values()) {
          s.unsubscribe();
        }
        this.sources.clear();
        this.psync.clear();
        this.realSourceIDs.clear();
        this.invRealSourceIDs.clear();
        this.idSources.clear();
      }
      return super.unsubscribe(sub2);
    }
    markDone(src) {
      this.remove(src);
      isFirstOrLastInput(this.closeIn, this.sources.size) && this.done();
    }
  };

  // node_modules/@thi.ng/rstream/tunnel.js
  var tunnel = (opts) => new Tunnel(opts);
  var Tunnel = class extends Subscription {
    workers;
    src;
    transferables;
    terminate;
    interrupt;
    index;
    constructor(opts) {
      super(void 0, { id: opts.id || `tunnel-${__nextID()}` });
      this.src = opts.src;
      this.workers = new Array(opts.maxWorkers || 1);
      this.transferables = opts.transferables;
      this.terminate = opts.terminate || 1e3;
      this.interrupt = opts.interrupt || false;
      this.index = 0;
    }
    next(x) {
      if (this.state < State.DONE) {
        let tx;
        if (this.transferables) {
          tx = this.transferables(x);
        }
        let worker = this.workers[this.index];
        if (this.interrupt && worker) {
          worker.terminate();
          worker = null;
        }
        if (!worker) {
          this.workers[this.index++] = worker = defWorker(this.src);
          this.index %= this.workers.length;
          worker.addEventListener(
            "message",
            (e) => this.dispatch(e.data)
          );
          worker.addEventListener(
            "error",
            (e) => this.error(e)
          );
        }
        worker.postMessage(x, tx || []);
      }
    }
    done() {
      super.done();
      if (this.terminate > 0) {
        setTimeout(() => {
          LOGGER.info("terminating workers...");
          this.workers.forEach((worker) => worker && worker.terminate());
          delete this.workers;
        }, this.terminate);
      }
    }
  };

  // node_modules/@thi.ng/rstream/forkjoin.js
  var forkJoin = (opts) => {
    const numWorkers = opts.numWorkers || navigator.hardwareConcurrency || 4;
    const workerIDs = range(numWorkers);
    return sync({
      src: transduce(
        map((id) => [
          String(id),
          opts.src.transform(map((x) => opts.fork(id, numWorkers, x))).subscribe(
            tunnel({
              src: opts.worker,
              transferables: opts.transferables,
              interrupt: opts.interrupt === true,
              terminate: opts.terminate,
              id: String(id)
            })
          )
        ]),
        assocObj(),
        workerIDs
      ),
      xform: comp2(
        // form result tuple in original order
        map((results) => [...map((id) => results[id], workerIDs)]),
        // apply user join function
        map(opts.join)
      ),
      reset: true,
      backPressure: opts.backPressure
    });
  };
  var forkBuffer = (minChunkSize = 1) => (id, numWorkers, buf) => {
    const chunkSize = Math.max(minChunkSize, buf.length / numWorkers | 0);
    return id < numWorkers - 1 ? buf.slice(id * chunkSize, (id + 1) * chunkSize) : buf.slice(id * chunkSize);
  };
  var joinBuffer = (fn) => fn ? (parts) => [...mapcat(fn, parts)] : (parts) => Array.prototype.concat.apply([], parts);

  // node_modules/@thi.ng/rstream/interval.js
  var fromInterval = (delay2, opts) => {
    opts = __optsWithID("interval", {
      num: Infinity,
      ...opts
    });
    return stream((stream2) => {
      let i = 0;
      let count2 = opts.num;
      stream2.next(i++);
      let id = setInterval(() => {
        stream2.next(i++);
        if (--count2 <= 0) {
          clearInterval(id);
          stream2.closeIn !== CloseMode.NEVER && stream2.done();
        }
      }, delay2);
      return () => clearInterval(id);
    }, opts);
  };

  // node_modules/@thi.ng/rstream/merge.js
  var merge = (opts) => new StreamMerge(opts);
  var StreamMerge = class extends Subscription {
    sources;
    constructor(opts) {
      opts = opts || {};
      super(void 0, __optsWithID("streammerge", opts));
      this.sources = /* @__PURE__ */ new Map();
      opts.src && this.addAll(opts.src);
    }
    add(src) {
      this.ensureState();
      this.sources.set(
        src,
        src.subscribe(
          {
            next: (x) => x instanceof Subscription ? this.add(x) : this.next(x),
            done: () => this.markDone(src),
            __owner: this
          },
          { id: `in-${src.id}` }
        )
      );
    }
    addAll(src) {
      for (let s of src) {
        this.add(s);
      }
    }
    remove(src) {
      const sub2 = this.sources.get(src);
      if (sub2) {
        this.sources.delete(src);
        sub2.unsubscribe();
        return true;
      }
      return false;
    }
    removeID(id) {
      for (let s of this.sources) {
        if (s[0].id === id) {
          return this.remove(s[0]);
        }
      }
      return false;
    }
    removeAll(src) {
      let ok = true;
      for (let s of src) {
        ok = this.remove(s) && ok;
      }
      return ok;
    }
    removeAllIDs(ids) {
      return __removeAllIDs(this, ids);
    }
    unsubscribe(sub2) {
      if (!sub2) {
        for (let s of this.sources.values()) {
          s.unsubscribe();
        }
        this.state = State.DONE;
        this.sources.clear();
      }
      return super.unsubscribe(sub2);
    }
    markDone(src) {
      this.remove(src);
      isFirstOrLastInput(this.closeIn, this.sources.size) && this.done();
    }
  };

  // node_modules/@thi.ng/rstream/nodejs.js
  var fromNodeJS = (stdout, stderr, close = true) => {
    const ingest = stream();
    stdout.on("data", (data) => ingest.next(data));
    stderr && stderr.on("data", (data) => ingest.error(data));
    close && stdout.on("close", () => ingest.done());
    return ingest;
  };
  var linesFromNodeJS = (stdout, stderr, re, close) => fromNodeJS(stdout, stderr, close).transform(rechunk(re));

  // node_modules/@thi.ng/rstream/object.js
  var fromObject = (src, opts = {}) => {
    const id = opts.id || `obj${__nextID()}`;
    const keys2 = opts.keys || Object.keys(src);
    const _opts = opts.dedupe !== false ? {
      xform: dedupe(opts.equiv || ((a, b) => a === b)),
      ...opts
    } : opts;
    const streams = {};
    for (let k of keys2) {
      streams[k] = subscription(void 0, {
        ..._opts,
        id: `${id}-${String(k)}`
      });
    }
    const res = {
      streams,
      next(state) {
        for (let k of keys2) {
          const val = state[k];
          streams[k].next(
            opts.defaults && val === void 0 ? opts.defaults[k] : val
          );
        }
      },
      done() {
        for (let k of keys2) {
          streams[k].done();
        }
      }
    };
    opts.initial !== false && res.next(src);
    return res;
  };

  // node_modules/@thi.ng/rstream/post-worker.js
  var postWorker = (worker, transfer = false, terminate = 0) => {
    const _worker = defWorker(worker);
    return {
      next(x) {
        if (x instanceof Promise) {
          x.then((y) => this.next(y));
          return;
        }
        let tx;
        if (transfer) {
          const ta = isTypedArray(x);
          if (ta || isTransferable(x)) {
            tx = [ta ? x.buffer : x];
          }
        }
        _worker.postMessage(x, tx || []);
      },
      done() {
        if (terminate > 0) {
          setTimeout(() => {
            LOGGER.info("terminating worker...");
            _worker.terminate();
          }, terminate);
        }
      }
    };
  };

  // node_modules/@thi.ng/rstream/promise.js
  var fromPromise = (src, opts) => {
    let canceled = false;
    let isError = false;
    let err = {};
    src.catch((e) => {
      err = e;
      isError = true;
    });
    return stream((stream2) => {
      src.then(
        (x) => {
          if (!canceled && stream2.getState() < State.DONE) {
            if (isError) {
              stream2.error(err);
              err = null;
            } else {
              stream2.next(x);
              stream2.closeIn !== CloseMode.NEVER && stream2.done();
            }
          }
        },
        (e) => stream2.error(e)
      );
      return () => {
        canceled = true;
      };
    }, __optsWithID("promise", opts));
  };

  // node_modules/@thi.ng/rstream/promises.js
  var fromPromises = (promises, opts) => fromPromise(
    Promise.all(promises),
    __optsWithID("promises", opts)
  ).transform(mapcat((x) => x));

  // node_modules/@thi.ng/rstream/raf.js
  var fromRAF = (opts = {}) => isNode() ? fromInterval(16, opts) : stream((stream2) => {
    let i = 0;
    let isActive = true;
    const loop = (time) => {
      isActive && stream2.next(opts.timestamp ? time : i++);
      isActive && (id = requestAnimationFrame(loop));
    };
    let id = requestAnimationFrame(loop);
    return () => {
      isActive = false;
      cancelAnimationFrame(id);
    };
  }, __optsWithID("raf", opts));

  // node_modules/@thi.ng/rstream/resolve.js
  var resolve = (opts) => new Resolver(opts);
  var Resolver = class extends Subscription {
    outstanding = 0;
    fail;
    constructor(opts = {}) {
      super(void 0, __optsWithID("resolve"));
      this.fail = opts.fail;
    }
    next(x) {
      this.outstanding++;
      x.then(
        (y) => {
          if (this.state < State.DONE) {
            this.dispatch(y);
            if (--this.outstanding === 0) {
              this.done();
            }
          } else {
            LOGGER.warn(`resolved value in state ${this.state} (${x})`);
          }
        },
        (e) => {
          if (this.fail) {
            this.fail(e);
          } else {
            this.error(e);
          }
        }
      );
    }
    done() {
      if (this.parent && this.parent.getState() === State.DONE && this.outstanding === 0) {
        super.done();
      }
    }
  };

  // node_modules/@thi.ng/rstream/sidechain-partition.js
  var sidechainPartition = (src, side, opts) => src.subscribe(new SidechainPartition(side, opts));
  var sidechainPartitionRAF = (src) => sidechainPartition(src, fromRAF()).transform(map(peek));
  var SidechainPartition = class extends ASidechain {
    buf;
    constructor(side, opts) {
      opts = __optsWithID("sidepart", opts);
      super(opts);
      const pred = opts.pred || (() => true);
      this.buf = [];
      this.sideSub = side.subscribe({
        next: (x) => {
          if (this.buf.length && pred(x)) {
            this.dispatch(this.buf);
            this.buf = [];
          }
        },
        done: () => {
          if (this.buf.length) {
            this.dispatch(this.buf);
          }
          this.done();
          delete this.buf;
        }
      });
    }
    next(x) {
      if (this.state < State.DONE) {
        this.buf.push(x);
      }
    }
  };

  // node_modules/@thi.ng/rstream/sidechain-toggle.js
  var sidechainToggle = (src, side, opts) => src.subscribe(new SidechainToggle(side, opts));
  var SidechainToggle = class extends ASidechain {
    isActive;
    constructor(side, opts) {
      opts = __optsWithID("sidetoggle", opts);
      super(opts);
      this.isActive = !!opts.initial;
      const pred = opts.pred || (() => true);
      this.sideSub = side.subscribe({
        next: (x) => {
          if (pred(x)) {
            this.isActive = !this.isActive;
          }
        },
        done: () => this.done()
      });
    }
    next(x) {
      if (this.isActive && this.state < State.DONE) {
        super.next(x);
      }
    }
  };

  // node_modules/@thi.ng/rstream/sidechain-trigger.js
  var sidechainTrigger = (src, side, opts) => src.subscribe(new SidechainTrigger(side, opts));
  var SidechainTrigger = class extends ASidechain {
    buf = SEMAPHORE;
    constructor(side, opts) {
      opts = __optsWithID("sidetrigger", opts);
      super(opts);
      const pred = opts.pred || (() => true);
      this.sideSub = side.subscribe({
        next: (x) => {
          if (this.buf !== SEMAPHORE && pred(x)) {
            this.dispatch(this.buf);
          }
        },
        done: () => this.done()
      });
    }
    next(x) {
      if (this.state < State.DONE) {
        this.buf = x;
      }
    }
  };

  // node_modules/@thi.ng/rstream/sync-raf.js
  var syncRAF = (src, opts) => src.subscribe(new SyncRAF(__optsWithID(`syncraf-${src.id}`, opts)));
  var SyncRAF = class extends Subscription {
    queued;
    raf;
    constructor(opts) {
      super(void 0, opts);
    }
    next(x) {
      if (this.state >= State.DONE)
        return;
      this.queued = x;
      if (!this.raf) {
        const update = () => {
          if (this.state < State.DONE)
            super.next(this.queued);
          this._clean();
        };
        this.raf = isNode() ? setTimeout(update, 16) : requestAnimationFrame(update);
      }
    }
    done() {
      this._clean();
      super.done();
    }
    error(e) {
      this._clean();
      return super.error(e);
    }
    _clean() {
      if (this.raf) {
        isNode() ? clearTimeout(this.raf) : cancelAnimationFrame(this.raf);
      }
      this.raf = this.queued = void 0;
    }
  };

  // node_modules/@thi.ng/rstream/timeout.js
  var timeout = (timeoutMs, opts) => new Timeout(timeoutMs, opts);
  var Timeout = class extends Subscription {
    timeoutMs;
    timeoutId;
    errorObj;
    resetTimeout;
    constructor(timeoutMs, opts) {
      opts = __optsWithID("timeout", opts);
      super(void 0, opts);
      this.timeoutMs = timeoutMs;
      this.errorObj = opts.error;
      this.resetTimeout = opts.reset === true;
      this.reset();
    }
    next(x) {
      if (this.resetTimeout) {
        clearTimeout(this.timeoutId);
        this.reset();
      }
      super.next(x);
    }
    reset() {
      this.timeoutId = setTimeout(() => {
        if (this.state < State.DONE) {
          this.dispatchTo(
            "error",
            this.errorObj || new Error(
              `Timeout '${this.id}' after ${this.timeoutMs} ms`
            )
          );
        }
      }, this.timeoutMs);
    }
    release() {
      clearTimeout(this.timeoutId);
      super.release();
    }
  };

  // node_modules/@thi.ng/rstream/trace.js
  var trace2 = (prefix) => ({
    next(x) {
      prefix ? console.log(prefix, x) : console.log(x);
    },
    done() {
      prefix ? console.log(prefix, "done") : console.log("done");
    },
    error(e) {
      prefix ? console.log(prefix, "error", e) : console.log("error", e);
      return false;
    }
  });

  // node_modules/@thi.ng/rstream/transduce.js
  var transduce2 = (src, xform, rfn, init) => {
    let acc = init !== void 0 ? init : rfn[0]();
    let sub2;
    return new Promise((resolve2, reject) => {
      sub2 = src.subscribe(
        {
          next(x) {
            let _acc;
            try {
              _acc = rfn[2](acc, x);
            } catch (e) {
              reject(e);
              return;
            }
            if (isReduced(_acc)) {
              resolve2(_acc.deref());
            } else {
              acc = _acc;
            }
          },
          done() {
            resolve2(acc);
          },
          error(e) {
            reject(e);
            return false;
          }
        },
        { xform }
      );
    }).then(
      (fulfilled) => {
        sub2.unsubscribe();
        return fulfilled;
      },
      (rejected) => {
        sub2.unsubscribe();
        throw rejected;
      }
    );
  };

  // node_modules/@thi.ng/rstream/toggle.js
  function toggle2(initial, opts) {
    const sub2 = new Subscription(void 0, {
      xform: toggle(true, false, !initial),
      ...opts
    });
    sub2.next();
    return sub2;
  }

  // node_modules/@thi.ng/rstream/trigger.js
  function trigger(x = true, opts) {
    return fromIterableSync([x], __optsWithID("trigger", opts));
  }

  // node_modules/@thi.ng/rstream/tween.js
  var tween2 = (src, initial, mix2, stop, clock) => sync({
    src: {
      src,
      _: clock == null ? fromRAF() : isNumber(clock) ? fromInterval(clock) : clock
    },
    closeIn: CloseMode.FIRST
  }).transform(
    scan(
      reducer(
        () => initial,
        (acc, { src: src2 }) => mix2(acc, src2)
      )
    ),
    dedupe(stop || (() => false))
  );
  var tweenNumber = (src, init = 0, speed = 0.05, eps = 1e-3, clock) => tween2(
    src,
    init,
    (a, b) => a + (b - a) * speed,
    (a, b) => Math.abs(a - b) < eps,
    clock
  );

  // node_modules/@thi.ng/paths/path.js
  var toPath = (path) => isArray(path) ? path : isString(path) ? path.length > 0 ? path.split(".") : [] : path != null ? [path] : [];

  // node_modules/@thi.ng/paths/getter.js
  var defGetterUnsafe = (path) => defGetter(path);
  function defGetter(path) {
    const ks = toPath(path);
    const [a, b, c, d] = ks;
    switch (ks.length) {
      case 0:
        return identity2;
      case 1:
        return (s) => s != null ? s[a] : void 0;
      case 2:
        return (s) => s != null ? (s = s[a]) != null ? s[b] : void 0 : void 0;
      case 3:
        return (s) => s != null ? (s = s[a]) != null ? (s = s[b]) != null ? s[c] : void 0 : void 0 : void 0;
      case 4:
        return (s) => s != null ? (s = s[a]) != null ? (s = s[b]) != null ? (s = s[c]) != null ? s[d] : void 0 : void 0 : void 0 : void 0;
      default:
        return (s) => {
          const n = ks.length - 1;
          let res = s;
          for (let i = 0; res != null && i <= n; i++) {
            res = res[ks[i]];
          }
          return res;
        };
    }
  }

  // node_modules/@thi.ng/atom/idgen.js
  var NEXT_ID2 = 0;
  var nextID = () => NEXT_ID2++;

  // node_modules/@thi.ng/atom/view.js
  var View = class {
    id;
    parent;
    path;
    state;
    tx;
    unprocessed;
    isDirty;
    isLazy;
    constructor(parent, path, tx, lazy = true, equiv2 = equiv) {
      this.parent = parent;
      this.id = `view-${nextID()}`;
      this.tx = tx || ((x) => x);
      this.path = toPath(path);
      this.isDirty = true;
      this.isLazy = lazy;
      const lookup = defGetterUnsafe(this.path);
      const state = this.parent.deref();
      this.unprocessed = state ? lookup(state) : void 0;
      if (!lazy) {
        this.state = this.tx(this.unprocessed);
        this.unprocessed = void 0;
      }
      parent.addWatch(this.id, (_, prev, curr) => {
        const pval = prev ? lookup(prev) : prev;
        const val = curr ? lookup(curr) : curr;
        if (!equiv2(val, pval)) {
          if (lazy) {
            this.unprocessed = val;
          } else {
            this.state = this.tx(val);
          }
          this.isDirty = true;
        }
      });
    }
    get value() {
      return this.deref();
    }
    /**
     * Returns view's value. If the view has a transformer, the
     * transformed value is returned. The transformer is only run once
     * per value change.
     *
     * @remarks
     * See class comments about difference between lazy/eager behaviors.
     */
    deref() {
      if (this.isDirty) {
        if (this.isLazy) {
          this.state = this.tx(this.unprocessed);
          this.unprocessed = void 0;
        }
        this.isDirty = false;
      }
      return this.state;
    }
    /**
     * Returns true, if the view's value has changed since last
     * [`IDeref`](https://docs.thi.ng/umbrella/api/interfaces/IDeref.html).deref}.
     */
    changed() {
      return this.isDirty;
    }
    /**
     * Like
     * [`IDeref`](https://docs.thi.ng/umbrella/api/interfaces/IDeref.html).deref},
     * but doesn't update view's cached state and dirty flag if value has
     * changed.
     *
     * @remarks
     * If there's an unprocessed value change, returns result of this sub's
     * transformer or else the cached value.
     *
     * **Important:** Use this function only if the view has none or or a
     * stateless transformer. Else might cause undefined/inconsistent behavior
     * when calling `view` or
     * [`IDeref`](https://docs.thi.ng/umbrella/api/interfaces/IDeref.html).deref}
     * subsequently.
     */
    view() {
      return this.isDirty && this.isLazy ? this.tx(this.unprocessed) : this.state;
    }
    /**
     * Disconnects this view from parent state, marks itself
     * dirty/changed and sets its unprocessed raw value to `undefined`.
     */
    release() {
      this.unprocessed = void 0;
      if (!this.isLazy) {
        this.state = this.tx(void 0);
      }
      this.isDirty = true;
      return this.parent.removeWatch(this.id);
    }
  };

  // node_modules/@thi.ng/rstream/view.js
  var fromViewUnsafe = (atom, opts) => fromView(atom, opts);
  function fromView(atom, opts) {
    opts = __optsWithID("view", opts);
    return new Stream((stream2) => {
      let isActive = true;
      const tx = opts.tx;
      const view = new View(
        atom,
        opts.path,
        tx ? (x) => isActive && (x = tx(x), stream2.next(x), x) : (x) => isActive && (stream2.next(x), x),
        false,
        opts.equiv
      );
      return () => {
        isActive = false;
        view.release();
      };
    }, opts);
  }

  // node_modules/@thi.ng/rstream/worker.js
  var fromWorker = (worker, opts) => {
    const _worker = defWorker(worker);
    opts = __optsWithID("worker", opts);
    return stream((stream2) => {
      const msgListener = (e) => {
        stream2.next(e.data);
      };
      const errListener = (e) => {
        stream2.error(e.data);
      };
      _worker.addEventListener("message", msgListener);
      _worker.addEventListener("error", errListener);
      return () => {
        _worker.removeEventListener("message", msgListener);
        _worker.removeEventListener("error", errListener);
        if (opts.terminate !== false) {
          LOGGER.info("terminating worker", _worker);
          _worker.terminate();
        }
      };
    }, opts);
  };

  // node_modules/@thi.ng/rstream-csp/index.js
  var rstream_csp_exports = {};
  __export(rstream_csp_exports, {
    fromChannel: () => fromChannel
  });
  var fromChannel = (src, opts) => {
    opts = { id: `channel-${src.id}`, closeChannel: true, ...opts };
    return new Stream((stream2) => {
      let isActive = true;
      (async () => {
        let x;
        while ((x = null, x = await src.read()) !== void 0) {
          if (x === void 0 || !isActive) {
            break;
          }
          stream2.next(x);
        }
        stream2.done();
      })();
      return () => {
        if (opts.closeChannel !== false) {
          src.close(true);
          LOGGER.info("closed channel", src.id);
        }
        isActive = false;
      };
    }, opts);
  };

  // node_modules/@thi.ng/seq/index.js
  var seq_exports = {};
  __export(seq_exports, {
    aseq: () => aseq,
    concat: () => concat3,
    concatA: () => concatA,
    cons: () => cons,
    ensureSeq: () => ensureSeq,
    isSeq: () => isSeq,
    isSeqable: () => isSeqable,
    iterator: () => iterator3,
    lazyseq: () => lazyseq,
    rseq: () => rseq
  });

  // node_modules/@thi.ng/seq/array.js
  var aseq = (buf, start = 0, end) => {
    if (!buf)
      return;
    end === void 0 && (end = buf.length);
    return start < end ? {
      first() {
        return buf[start];
      },
      next() {
        return aseq(buf, start + 1, end);
      }
    } : void 0;
  };
  var rseq = (buf, start, end = -1) => {
    if (!buf)
      return;
    start === void 0 && (start = buf.length - 1);
    return start > end ? {
      first() {
        return buf[start];
      },
      next() {
        return rseq(buf, start - 1, end);
      }
    } : void 0;
  };

  // node_modules/@thi.ng/seq/ensure.js
  var isSeq = (x) => implementsFunction(x, "first") && implementsFunction(x, "next");
  var isSeqable = (x) => implementsFunction(x, "seq");
  var ensureSeq = (x) => implementsFunction(x, "seq") ? x.seq() : isArrayLike(x) ? aseq(x) : x != null ? x : void 0;

  // node_modules/@thi.ng/seq/concat.js
  var concat3 = (...args) => {
    const seqs = [];
    for (let i = 0, n = args.length; i < n; i++) {
      const x = ensureSeq(args[i]);
      x && seqs.push(x);
    }
    const $seq = (curr, i) => {
      if (!curr) {
        curr = seqs[++i];
      }
      return curr ? {
        first() {
          return curr.first();
        },
        next() {
          return $seq(curr.next(), i);
        }
      } : void 0;
    };
    return $seq(seqs[0], 0);
  };
  var concatA = (...args) => {
    const seqs = [];
    for (let i = 0, n = args.length; i < n; i++) {
      const x = args[i];
      x && x.length && seqs.push(x);
    }
    const $seq = (i, j) => {
      if (!seqs[i] || j >= seqs[i].length) {
        i++;
        j = 0;
      }
      return i < seqs.length ? {
        first() {
          return seqs[i][j];
        },
        next() {
          return $seq(i, j + 1);
        }
      } : void 0;
    };
    return $seq(0, 0);
  };

  // node_modules/@thi.ng/seq/cons.js
  var cons = (head, seq) => ({
    first() {
      return head;
    },
    next() {
      return seq;
    }
  });

  // node_modules/@thi.ng/seq/iterator.js
  function* iterator3(src) {
    let seq = ensureSeq(src);
    while (seq) {
      yield seq.first();
      seq = seq.next();
    }
  }

  // node_modules/@thi.ng/seq/lazyseq.js
  var lazyseq = (fn) => {
    let called = false;
    let seq;
    const ensure = () => {
      if (!called) {
        seq = fn();
        called = true;
      }
      return seq;
    };
    return {
      first() {
        return ensure() !== void 0 ? seq.first() : void 0;
      },
      next() {
        return ensure() !== void 0 ? seq.next() : void 0;
      }
    };
  };

  // index.js
  function setupThi(global) {
    if (!global)
      return;
    if (!global._thi)
      global._thi = {};
    global._thi.cache = cache_exports;
    global._thi.compare = compare_exports;
    global._thi.compose = compose_exports;
    global._thi.csp = csp_exports;
    global._thi.iterators = iterators_exports;
    global._thi.rstream = rstream_exports;
    global._thi.rstreamCsp = rstream_csp_exports;
    global._thi.seq = seq_exports;
    global._thi.transducers = transducers_exports;
  }
  try {
    if (tri) {
      setupThi(tri);
    }
  } catch (err) {
    console.error(err);
  }
  try {
    if (window) {
      setupThi(window);
    }
  } catch (err) {
    console.error(err);
  }
})();
