/**
 * expecting all strings to be same length
 * @type {string[]}
 */
const input = [
  // FILL IN INPUT HERE
];

function* stencils(input, size) {
  const paddingCol = Array.from({ length: size - 1 })
    .map(() => "*")
    .join("");
  const padded = input.map((i) => i + paddingCol);
  const paddingRow = Array.from({ length: padded[0].length })
    .map(() => "*")
    .join("");
  padded.push(...Array.from({ length: size - 1 }).map(() => paddingRow));

  for (let r = 0; r <= padded.length - size; r++) {
    for (let c = 0; c <= padded[0].length - size; c++) {
      yield padded.slice(r, r + size).map((row) => row.slice(c, c + size));
    }
  }
}

function printTotal(input, stencilSize, matcher) {
  let total = 0;
  for (const stencil of stencils(input, stencilSize)) {
    const found = matcher(stencil);
    total += found;
  }
  console.log("Total: ", total);
}

function countX_mas(stencil) {
  const diag1 =
    (stencil[0][0] == "M" && stencil[1][1] == "A" && stencil[2][2] == "S") ||
    (stencil[0][0] == "S" && stencil[1][1] == "A" && stencil[2][2] == "M");
  const diag2 =
    (stencil[2][0] == "M" && stencil[1][1] == "A" && stencil[0][2] == "S") ||
    (stencil[2][0] == "S" && stencil[1][1] == "A" && stencil[0][2] == "M");
  return +(diag1 && diag2);
}

function countXmas(stencil) {
  const ver =
    (stencil[0][0] == "X" && stencil[1][0] == "M" && stencil[2][0] == "A" && stencil[3][0] == "S") ||
    (stencil[0][0] == "S" && stencil[1][0] == "A" && stencil[2][0] == "M" && stencil[3][0] == "X");
  const hor =
    (stencil[0][0] == "X" && stencil[0][1] == "M" && stencil[0][2] == "A" && stencil[0][3] == "S") ||
    (stencil[0][0] == "S" && stencil[0][1] == "A" && stencil[0][2] == "M" && stencil[0][3] == "X");

  const diag1 =
    (stencil[0][0] == "X" && stencil[1][1] == "M" && stencil[2][2] == "A" && stencil[3][3] == "S") ||
    (stencil[0][0] == "S" && stencil[1][1] == "A" && stencil[2][2] == "M" && stencil[3][3] == "X");
  const diag2 =
    (stencil[3][0] == "X" && stencil[2][1] == "M" && stencil[1][2] == "A" && stencil[0][3] == "S") ||
    (stencil[3][0] == "S" && stencil[2][1] == "A" && stencil[1][2] == "M" && stencil[0][3] == "X");
  return +ver + hor + diag1 + diag2;
}

printTotal(input, 3, countX_mas);
printTotal(input, 4, countXmas);
