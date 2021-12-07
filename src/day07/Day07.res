module Part01 = {
  let make = input => {
    let numbers =
      input
      ->Js.String2.split(",")
      ->Js.Array2.map(Belt.Int.fromString)
      ->Js.Array2.map(Belt.Option.getExn)

    let max = numbers->Js.Math.maxMany_int

    let (pos, fuelUsed) =
      Belt.Array.makeBy(max - 1, i => i + 1)->Js.Array2.reduce(
        ((bestPos, bestPosFuelUsed), pos) => {
          let fuel = numbers->Js.Array2.reduce((acc, i) => acc + Js.Math.abs_int(i - pos), 0)

          switch bestPosFuelUsed {
          | Some(bestPosFuelUsed) if fuel < bestPosFuelUsed => (Some(pos), Some(fuel))
          | Some(_) => (bestPos, bestPosFuelUsed)
          | None => (Some(pos), Some(fuel))
          }
        },
        (None, None),
      )

    Js.log2("best pos is", pos)
    Js.log2("fuel used", fuelUsed)
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

    let max = numbers->Js.Math.maxMany_int

    let (pos, fuelUsed) =
      Belt.Array.makeBy(max, i => i + 1)->Js.Array2.reduce(((bestPos, bestPosFuelUsed), pos) => {
        let fuel = numbers->Js.Array2.reduce((acc, i) => {
          let fuelUsed = fuelUse(Js.Math.abs_float(i->Js.Int.toFloat -. pos->Js.Int.toFloat))

          // Js.log4(i, pos, Js.Math.abs_int(i - pos), fuelUsed)

          acc + fuelUsed->Belt.Int.fromFloat
        }, 0)

        // Js.log2(pos, fuel)

        switch bestPosFuelUsed {
        | Some(bestPosFuelUsed) if fuel < bestPosFuelUsed => (Some(pos), Some(fuel))
        | Some(_) => (bestPos, bestPosFuelUsed)
        | None => (Some(pos), Some(fuel))
        }
      }, (None, None))

    Js.log2("best pos is", pos)
    Js.log2("fuel used", fuelUsed)
  }
}

Solution.make(Part01.make, "day07/input")

Solution.make(Part02.make, "day07/input")
