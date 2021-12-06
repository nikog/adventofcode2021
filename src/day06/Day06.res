module Part01 = {
  let make = input => {
    let initial =
      input->Js.String2.split(",")->Js.Array2.map(x => x->Belt.Int.fromString->Belt.Option.getExn)

    let result = Belt.Array.makeBy(256, i => i)->Js.Array2.reduce((acc, day) => {
        Js.log(
          "days gone " ++
          Js.Int.toString(day) ++
          ". fishes: " ++
          acc->Js.Array2.length->Js.Int.toString,
        )

        let len = acc->Js.Array2.length

        for i in 0 to len - 1 {
          let num = acc->Js.Array2.unsafe_get(i)

          let next = num - 1

          if next == -1 {
            acc->Js.Array2.unsafe_set(i, 6)
            let _ = acc->Js.Array2.push(8)
          } else {
            acc->Js.Array2.unsafe_set(i, next)
          }
        }

        acc

        // let newAcc = acc->Js.Array2.reducei((acc, num, i) => {
        //   let next = num - 1

        //   if next == -1 {
        //     acc->Js.Array2.unsafe_set(i, 6)
        //     let _ = acc->Js.Array2.push(8)
        //     acc
        //   } else {
        //     acc->Js.Array2.unsafe_set(i, next)
        //     acc
        //   }
        // }, acc)

        // Js.log(newAcc->Js.Array2.map(x => x > 9 ? x - 10 : x)->Js.Array2.joinWith(", "))

        // newAcc
      }, initial)->Js.Array2.length

    result
  }
}

module Part02 = {
  let howManyFishes = (date, num) => (date - num) / 8

  let make = input => {
    let initial =
      input->Js.String2.split(",")->Js.Array2.map(x => x->Belt.Int.fromString->Belt.Option.getExn)

    let fishesByMaturity = Belt.Array.makeBy(9, i =>
      initial->Js.Array2.filter(x => x == i)->Js.Array2.length->Belt.Float.fromInt
    )

    let result = Belt.Array.makeBy(256, i => i)->Js.Array2.reduce((fishesByMaturity, _) => {
        fishesByMaturity[7] = fishesByMaturity[7] +. fishesByMaturity[0]
        let _ = fishesByMaturity->Js.Array2.push(fishesByMaturity[0])
        let _ = fishesByMaturity->Js.Array2.shift

        fishesByMaturity
      }, fishesByMaturity)->Js.Array2.reduce((acc, i) => acc +. i, 0.0)

    result
  }
}

Solution.make(Part02.make, "day06/input")
