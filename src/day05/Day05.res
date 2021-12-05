exception ParseError

let parseCoordinate = string => {
  let coords = string->Js.String2.split(",")->Js.Array2.map(Belt.Int.fromString)
  switch coords {
  | [Some(x), Some(y)] => (x, y)
  | _ => raise(ParseError)
  }
}
let parseLine = line => line->Js.String2.split(" -> ")->Js.Array2.map(parseCoordinate)

let makeRange = ([(x1, y1), (x2, y2)]) => {
  let yDiff = y2 > y1 ? 1 : -1
  let xDiff = x2 > x1 ? 1 : -1

  if x1 == x2 {
    Belt.Array.makeBy(y2 - y1 + 1, i => (x1, y1 + i * yDiff))
  } else if y1 == y2 {
    Belt.Array.makeBy(x2 - x1 + 1, i => (x1 + i * xDiff, y1))
  } else {
    Belt.Array.makeBy(x2 - x1 + 1, i => (x1 + i * xDiff, y1 + i * yDiff))
  }
}

let isHorizontalOrVertical = ([(x1, y1), (x2, y2)]) => x1 == x2 || y1 == y2
let checkIntersect = ([(x1, y1), (x2, y2)], [(ax1, ay1), (ax2, ay2)]) => {
  if x1 == x2 && x1 == ax1 && x1 == ax2 {
    // overlapping
    let jooh = Js.Math.max_int(0, Js.Math.min_int(y2, ay2) - Js.Math.max_int(y1, ay1) + 1)
    Belt.Array.makeBy(jooh, i => (x1, i + Js.Math.max_int(y1, ay1)))
  } else if y1 == y2 && y1 == ay1 && y1 == ay2 {
    // overlapping
    let jooh = Js.Math.max_int(0, Js.Math.min_int(x2, ax2) - Js.Math.max_int(x1, ax1) + 1)
    Belt.Array.makeBy(jooh, i => (i + Js.Math.max_int(x1, ax1), y1))
  } else if x1 == x2 && x1 >= ax1 && x1 <= ax2 && ay1 >= y1 && ay1 <= y2 {
    [(x1, ay1)]
  } else if y1 == y2 && y1 >= ay1 && y1 <= ay2 && ax1 >= x1 && ax1 <= x2 {
    [(ax1, y1)]
  } else {
    []
  }
}

module Part01 = {
  let make = input => {
    let lines =
      input
      ->Js.String2.split("\n")
      ->Js.Array2.map(parseLine)
      ->Js.Array2.filter(isHorizontalOrVertical)
      ->Js.Array2.map(x =>
        x->Belt.SortArray.stableSortBy(((x1, y1), (x2, y2)) => {
          if x1 !== x2 {
            x1 - x2
          } else {
            y1 - y2
          }
        })
      )

    Js.log(lines)
    Js.log("----------")

    let intersections = lines->Js.Array2.reducei((acc, line, i) => {
      acc->Js.Array2.concat(lines->Js.Array2.reducei((acc, line2, i2) => {
          if i !== i2 {
            //Js.log3(line, line2, checkIntersect(line, line2))
            //sJs.log("-------------------------")
            acc->Js.Array2.concat(checkIntersect(line, line2))
          } else {
            acc
          }
        }, []))
    }, [])

    let uniqueIntersections =
      intersections->Js.Array2.filteri(((x, y), i) =>
        intersections->Js.Array2.findIndex(((x1, y2)) => x == x1 && y == y2) == i
      )

    uniqueIntersections->Js.Array2.length
  }
}

let unique = array => {
  module PairComparator = Belt.Id.MakeComparable({
    type t = (int, int)
    let cmp = ((a0, a1), (b0, b1)) =>
      switch Pervasives.compare(a0, b0) {
      | 0 => Pervasives.compare(a1, b1)
      | c => c
      }
  })

  let (_, duplicates) = array->Js.Array2.reduce(((seen, duplicates), item) => {
    if !Belt.Set.has(seen, item) {
      let seen = Belt.Set.add(seen, item)
      (seen, duplicates->Js.Array2.concat([item]))
    } else {
      (seen, duplicates)
    }
  }, (Belt.Set.make(~id=module(PairComparator)), []))

  duplicates
}

module Part02 = {
  let make = input => {
    let lines =
      input
      ->Js.String2.split("\n")
      ->Js.Array2.map(parseLine)
      //->Js.Array2.filter(isHorizontalOrVertical)
      ->Js.Array2.map(x =>
        x->Belt.SortArray.stableSortBy(((x1, y1), (x2, y2)) => {
          if x1 !== x2 {
            x1 - x2
          } else {
            y1 - y2
          }
        })
      )
      ->Js.Array2.map(makeRange)
      //->Js.Array2.reduce((acc, i) => acc->Js.Array2.concat(i), [])
      ->Js.Array2.reduce((acc, i) => {
        i->Js.Array2.reduce((acc, (x, y)) => {
          let id = Js.Int.toString(x) ++ "." ++ Js.Int.toString(y)
          let count = acc->Belt.Map.String.get(id)->Belt.Option.getWithDefault(0)
          acc->Belt.Map.String.set(id, count + 1)
        }, acc)
      }, Belt.Map.String.fromArray([]))
      ->Belt.Map.String.valuesToArray
      ->Js.Array2.filter(x => x > 1)
      ->Js.Array2.length

    lines

    // let filteredLines = lines->Js.Array2.reducei((acc, (x, y), i) => {
    //   let exists = acc->Js.Array2.some(((x2, y2)) => x == x2 && y == y2)

    //   if !exists {
    //     let hasIntersections = lines->Js.Array2.findIndex(((x2, y2)) => x == x2 && y == y2) != i
    //     if hasIntersections {
    //       acc->Js.Array2.concat([(x, y)])
    //     } else {
    //       acc
    //     }
    //   } else {
    //     acc
    //   }
    // }, [])

    // filteredLines->Js.Array2.length
  }
}

Solution.make(Part02.make, "day05/input")
