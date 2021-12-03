let toBinary = binaryString => int_of_string("0b" ++ binaryString)
let binaryNot = binary => binary->lnot
let binaryToDecimal = binary => binary->Js.Int.toStringWithRadix(~radix=10)->Belt.Int.fromString

let significantBits = (~flag=false, lines) => {
  let initial1 = lines[0]->Js.String2.length->Belt.Array.makeBy(_ => 0)
  let initial0 = lines[0]->Js.String2.length->Belt.Array.makeBy(_ => 0)
  let (bit1, bit0) = lines->Js.Array2.reduce(((bit1, bit0), binary) => {
    let bits = binary->Js.String2.split("")

    bits->Js.Array2.forEachi((bit, i) => {
      if bit == "1" {
        bit1[i] = bit1[i] + 1
      } else {
        bit0[i] = bit0[i] + 1
      }
    })

    (bit1, bit0)
  }, (initial1, initial0))

  if !flag {
    initial1->Js.Array2.mapi((_, i) => bit1[i] >= bit0[i] ? 1 : 0)->Js.Array2.joinWith("")
  } else {
    initial1->Js.Array2.mapi((_, i) => bit1[i] >= bit0[i] ? 0 : 1)->Js.Array2.joinWith("")
  }
}

module Part01 = {
  let make = input => {
    let lines = input->Js.String2.split("\n")

    let mostSignificant = significantBits(lines)
    let leastSignificant =
      mostSignificant
      ->Js.String2.split("")
      ->Js.Array2.map(bit => bit == "1" ? "0" : "1")
      ->Js.Array2.joinWith("")

    let gamma = mostSignificant->toBinary->binaryToDecimal
    let epsilon = leastSignificant->toBinary->binaryToDecimal

    switch (gamma, epsilon) {
    | (Some(gamma), Some(epsilon)) => gamma * epsilon
    | _ => 0
    }
  }
}

let rec findOxygenOrCo2Rating = (lines, mostSignificant, i, flag) => {
  let bit = mostSignificant[i]

  let lines = lines->Js.Array2.filter(line => {
    let splitLine = line->Js.String2.split("")
    splitLine[i] == bit
  })

  if lines->Js.Array2.length == 1 {
    lines[0]
  } else {
    findOxygenOrCo2Rating(lines, significantBits(lines, ~flag)->Js.String2.split(""), i + 1, flag)
  }
}

module Part02 = {
  let make = input => {
    let lines = input->Js.String2.split("\n")
    let mostSignificant = significantBits(lines)->Js.String2.split("")
    let least = significantBits(~flag=true, lines)->Js.String2.split("")

    let oxygen = findOxygenOrCo2Rating(lines, mostSignificant, 0, false)->toBinary->binaryToDecimal
    let co2 = findOxygenOrCo2Rating(lines, least, 0, true)->toBinary->binaryToDecimal

    switch (oxygen, co2) {
    | (Some(oxygen), Some(co2)) => oxygen * co2
    | _ => 0
    }
  }
}

Solution.make(Part01.make, "day03/input")
