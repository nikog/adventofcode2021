module Part01 = {
  let getDefault = (arr, i, j) =>
    arr
    ->Belt.Array.get(i)
    ->Belt.Option.mapWithDefault(Js.Int.max, x =>
      x->Belt.Array.get(j)->Belt.Option.getWithDefault(Js.Int.max)
    )

  let make = input => {
    let heightmap =
      input
      ->Js.String2.split("\n")
      ->Js.Array2.map(x =>
        x->Js.String2.split("")->Js.Array2.map(x => x->Belt.Int.fromString->Belt.Option.getExn)
      )

    heightmap->Js.Array2.reducei((acc, row, i) => {
      acc + row->Js.Array2.reducei((acc, number, j) => {
        let top = heightmap->getDefault(i - 1, j)
        let right = heightmap->getDefault(i, j + 1)
        let bottom = heightmap->getDefault(i + 1, j)
        let left = heightmap->getDefault(i, j - 1)

        if number < top && number < right && number < bottom && number < left {
          acc + number + 1
        } else {
          acc
        }
      }, 0)
    }, 0)
  }
}

module Part02 = {
  let getDefault = (arr, i, j) =>
    arr
    ->Belt.Array.get(i)
    ->Belt.Option.mapWithDefault(9, x => x->Belt.Array.get(j)->Belt.Option.getWithDefault(9))

  let rec floodfill = (map, i, j) => {
    let node = map->getDefault(i, j)

    if node != 9 && node !== -1 {
      map[i][j] = -1

      let top = map->floodfill(i - 1, j)
      let right = map->floodfill(i, j + 1)
      let bottom = map->floodfill(i + 1, j)
      let left = map->floodfill(i, j - 1)

      1 + top + right + bottom + left
    } else {
      0
    }
  }

  let make = input => {
    let heightmap =
      input
      ->Js.String2.split("\n")
      ->Js.Array2.map(x =>
        x->Js.String2.split("")->Js.Array2.map(x => x->Belt.Int.fromString->Belt.Option.getExn)
      )

    heightmap->Js.Array2.reducei((acc, row, i) => {
      acc->Js.Array2.concat(row->Js.Array2.reducei((acc, _, j) => {
          let size = heightmap->floodfill(i, j)

          if size > 0 {
            let _ = acc->Js.Array2.push(size)
          }

          acc
        }, []))
    }, [])->Belt.SortArray.Int.stableSort->Belt.Array.reverse->Js.Array2.slice(
      ~start=0,
      ~end_=3,
    )->Js.Array2.reduce((acc, i) => acc * i, 1)
  }
}

Solution.make(Part01.make, "day09/input")
Solution.make(Part02.make, "day09/input")
