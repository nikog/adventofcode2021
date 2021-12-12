let splitTwoDimensional = str =>
  str
  ->Js.String2.split("\n")
  ->Js.Array2.map(line =>
    line->Js.String2.split("")->Js.Array2.map(cell => cell->Belt.Int.fromString->Belt.Option.getExn)
  )

let traverse = (arr, fn, acc) => arr->Js.Array2.reducei((acc, line, i) => {
    line->Js.Array2.reducei((acc, _, j) => {
      fn(acc, i, j)
    }, acc)
  }, acc)

let getDefault = (arr, i, j, default) =>
  arr
  ->Belt.Array.get(i)
  ->Belt.Option.mapWithDefault(default, x =>
    x->Belt.Array.get(j)->Belt.Option.getWithDefault(default)
  )

let neighbors = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

let increment = (arr, (x, y)) =>
  switch arr->getDefault(y, x, 0) {
  | 0 => arr
  | number => {
      arr[y][x] = number + 1
      arr
    }
  }

let rec flash = (arr, (x, y)) => {
  let node = arr->getDefault(y, x, 0)

  switch node {
  | _ if node > 9 => {
      // flash
      arr[y][x] = 0
      neighbors->Js.Array2.reduce(
        (acc, (dx, dy)) => acc->increment((x + dx, y + dy))->flash((x + dx, y + dy)),
        arr,
      )
    }
  | 0
  | _ => arr
  }
}

module Part01 = {
  let make = input => {
    let map = input->splitTwoDimensional
    let steps = Belt.Array.make(100, 0)

    let flashes = ref(0)

    let _ = steps->Js.Array2.reduce((map, _) => {
      let map = map->traverse((acc, i, j) => {
          acc[i][j] = acc[i][j] + 1
          acc
        }, map)->traverse((map, i, j) => {
          map->flash((j, i))
        }, map)

      flashes := flashes.contents + map->traverse((acc, i, j) => acc + (map[i][j] == 0 ? 1 : 0), 0)

      //Js.log(map->Js.Array2.map(x => x->Js.Array2.joinWith("")))
      map
    }, map)

    flashes.contents
  }
}

module Part02 = {
  let make = input => {
    let map = input->splitTwoDimensional

    let flashes = ref(false)
    let mapRef = ref(map)
    let stepCount = ref(0)

    while !flashes.contents {
      let map = mapRef.contents
      let map = map->traverse((acc, i, j) => {
          acc[i][j] = acc[i][j] + 1
          acc
        }, map)->traverse((map, i, j) => {
          map->flash((j, i))
        }, map)

      flashes := map->Js.Array2.every(line => line->Js.Array2.every(item => item == 0))

      mapRef := map
      stepCount := stepCount.contents + 1
    }

    stepCount.contents
  }
}

Solution.make(Part01.make, "day11/input")
Solution.make(Part02.make, "day11/input")
