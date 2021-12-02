open Belt

let part01 = input => {
  let (count, _) =
    input
    ->Js.String2.split("\n")
    ->Array.map(Int.fromString)
    ->Array.reduce((0, Some(max_int)), ((count, prev), value) => (
      count + (value > prev ? 1 : 0),
      value,
    ))

  count
}

let part02 = input => {
  let arr = input->Js.String2.split("\n")->Array.map(Int.fromString)

  let mappedArr = arr->Array.mapWithIndex((index, value) => {
    switch (arr->Array.get(index - 2), arr->Array.get(index - 1), value) {
    | (Some(Some(prev1)), Some(Some(prev2)), Some(value)) => prev1 + value + prev2
    | (_, _, _) => max_int
    }
  })

  let (count, _) =
    mappedArr->Array.reduce((0, max_int), ((count, prev), value) => (
      count + (value > prev ? 1 : 0),
      value,
    ))

  count
}

Solution.make(part02, "day01/input")
