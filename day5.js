/**
 * @type {[number, number][]}
 */
const orderings = [
  // part 1 of input
];

/**
 * @type {number[][]}
 */
const lists = [
  // part 2 of input
];

/**
 * Adjacency list graph representation
 * @param {[number, number][]} pos
 * @returns {Map<number, number[]>}
 */
function makeGraph(pos) {
  const m = new Map();
  for (const [l, r] of pos) {
    if (!m.has(l)) {
      m.set(l, []);
    }
    if (!m.has(r)) {
      m.set(r, []);
    }
    m.get(l).push(r);
  }
  return m;
}

/**
 * Topologically sort graph nodes
 * @param {Map<number, number[]>} g
 * @returns {number[]}
 */
function topSort(g) {
  const unvisited = new Set(g.keys());
  const res = [];

  while (true) {
    const next = unvisited.values().next();
    if (next.done) {
      return res;
    }
    const marks = new Set(); // for cycle detection

    visit(next.value);

    /**
     * @param {number} n
     */
    function visit(n) {
      if (!unvisited.has(n)) {
        return;
      }
      if (marks.has(n)) {
        throw new Error("CYCLE_DETECTED");
      }

      marks.add(n);

      const children = g.get(n);
      for (const child of children) {
        visit(child);
      }

      unvisited.delete(n);
      res.unshift(n);
    }
  }
}

/**
 * @param {Map<number, number[]>} g
 * @param {number[]} nodes
 * @returns {Map<number, number[]>}
 */
function subgraph(g, nodes) {
  const subentries = [];
  for (const [k, v] of g) {
    if (nodes.includes(k)) {
      subentries.push([k, v.filter((n) => nodes.includes(n))]);
    }
  }
  return new Map(subentries);
}

/**
 * @param {Map<number, number[]>} g
 * @param {number[]} nodes
 * @returns {boolean}
 */
function isTopSorted(g, list) {
  const sub = subgraph(g, list);
  try {
    const sorted = topSort(sub);
    for (let i = 0; i < list.length - 1; i++) {
      if (sorted.indexOf(list[i]) > sorted.indexOf(list[i + 1])) {
        return false;
      }
    }
    return true;
  } catch (e) {
    if (e.message == "CYCLE_DETECTED") {
      return false;
    }
    throw e;
  }
}

/**
 * @param {number[]} list
 * @returns {number}
 */
function midpoint(list) {
  const len = list.length;
  if (len % 2 == 0) {
    throw new Error("ODD_NUMBERED_LIST_EXPECTED");
  }
  return list[(len - 1) / 2];
}

// graph has cycles!
const graph = makeGraph(orderings);

const firstSub = subgraph(graph, lists[0]);

const totalOrdered = lists
  .filter((l) => isTopSorted(graph, l))
  .map((l) => midpoint(l))
  .reduce((acc, v) => acc + v, 0);

const totalUnordered = lists
  .filter((l) => !isTopSorted(graph, l))
  .map((l) => topSort(subgraph(graph, l)))
  .map((l) => midpoint(l))
  .reduce((acc, v) => acc + v, 0);

console.log(totalOrdered, totalUnordered);
