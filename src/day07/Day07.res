let sum = (arr, fn) => arr->Js.Array2.reduce((acc, i) => acc + fn(i), 0)

module Part01 = {
  let make = input => {
    let numbers =
      input
      ->Js.String2.split(",")
      ->Js.Array2.map(Belt.Int.fromString)
      ->Js.Array2.map(Belt.Option.getExn)

    Belt.Array.range(0, numbers->Js.Math.maxMany_int)->Js.Array2.reduce(
      (minFuelUsed, pos) =>
        Js.Math.min_int(minFuelUsed, numbers->sum(i => Js.Math.abs_int(i - pos))),
      Js.Int.max,
    )
  }
}

module Part02 = {
  let fuelUse = dist => dist /. 2. *. (1. +. dist)

  let make = input => {
    let numbers =
      input
      ->Js.String2.split(",")
      ->Js.Array2.map(Belt.Int.fromString)
      ->Js.Array2.map(Belt.Option.getExn)

    Belt.Array.range(1, numbers->Js.Math.maxMany_int)->Js.Array2.reduce((acc, pos) => {
      Js.Math.min_int(
        acc,
        numbers->sum(i =>
          (i - pos)->Js.Math.abs_int->Belt.Float.fromInt->fuelUse->Belt.Int.fromFloat
        ),
      )
    }, Js.Int.max)
  }
}

Solution.make(Part01.make, "day07/input")
Solution.make(Part02.make, "day07/input")
