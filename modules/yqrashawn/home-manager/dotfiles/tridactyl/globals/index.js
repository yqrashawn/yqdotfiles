import * as cache from "@thi.ng/cache"
import * as compare from "@thi.ng/compare"
import * as compose from "@thi.ng/compose"
import * as csp from "@thi.ng/csp"
import * as iterators from "@thi.ng/iterators"
import * as rstream from "@thi.ng/rstream"
import * as rstreamCsp from "@thi.ng/rstream-csp"
import * as seq from "@thi.ng/seq"
import * as transducers from "@thi.ng/transducers"

function setupThi(global) {
  // console.log("global", global)
  if (!global) return
  if (!global._thi) global._thi = {}
  global._thi.cache = cache
  global._thi.compare = compare
  global._thi.compose = compose
  global._thi.csp = csp
  global._thi.iterators = iterators
  global._thi.rstream = rstream
  global._thi.rstreamCsp = rstreamCsp
  global._thi.seq = seq
  global._thi.transducers = transducers
}

try {
  if (tri) {
    setupThi(tri)
    // console.log("setup thi on tri")
  }
} catch (err) {
  console.error(err)
}

try {
  if (window) {
    setupThi(window)
    // console.log("setup thi on window")
  }
} catch (err) {
  console.error(err)
}
