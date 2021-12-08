module Part01 = {
  let make = input => {
    input
    ->Js.String2.split("\n")
    ->Js.Array2.map(line =>
      line
      ->Js.String2.split(" | ")
      ->Belt.Array.getUnsafe(1)
      ->Js.String2.split(" ")
      ->Js.Array2.map(x => x->Js.String2.split("")->Js.Array2.length)
      ->Js.Array2.filter(x => {
        switch x {
        | 2 | 4 | 3 | 7 => true
        | _ => false
        }
      })
      ->Js.Array2.length
    )
    ->Js.Array2.reduce((acc, i) => acc + i, 0)
  }
}

module Part02 = {
  let includes = (~partial=0, str1, str2) => {
    if str2->Js.Array2.length > 0 {
      if partial > 0 {
        str2->Js.Array2.filter(char => str1->Js.Array2.indexOf(char) > -1)->Js.Array2.length ==
          partial
      } else {
        str2->Js.Array2.every(char => str1->Js.Array2.indexOf(char) > -1)
      }
    } else {
      false
    }
  }

  let uniqStrToDigit = str => {
    switch str->Js.Array2.length {
    | 2 => 1
    | 4 => 4
    | 3 => 7
    | 7 => 8
    | _ => -1
    }
  }

  let strToDigit = (str, found) => {
    switch str->Js.Array2.length {
    | 2 => 1
    | 4 => 4
    | 3 => 7
    | 7 => 8
    | 6 if str->includes(found[4]) => 9
    | 6 if str->includes(found[7]) => 0
    | 6 if !(str->includes(found[7])) => 6

    | 5 if str->includes(found[7]) => 3
    | 5 if str->includes(found[4], ~partial=3) => 5
    | 5 if str->includes(found[4], ~partial=2) => 2
    | _ => -1
    }
  }

  let make = input => {
    input
    ->Js.String2.split("\n")
    ->Js.Array2.map(line => {
      let digits = line->Js.String2.replace(" | ", " ")->Js.String2.split(" ")

      let found = digits->Js.Array2.reduce((acc, x) => {
        let chars = x->Js.String2.split("")
        let num = uniqStrToDigit(chars)

        if num > -1 {
          acc[num] = chars
        }

        acc
      }, Belt.Array.makeBy(10, _ => []))

      let jooh = digits->Js.Array2.reduce((acc, x) => {
        let chars = x->Js.String2.split("")
        let num = strToDigit(chars, found)

        if num > -1 {
          acc[num] = chars
        } else {
          Js.log2("unresolved", chars)
        }

        acc
      }, found)

      let output =
        line
        ->Js.String2.split(" | ")
        ->Belt.Array.getUnsafe(1)
        ->Js.String2.split(" ")
        ->Js.Array2.map(x => {
          let chars = x->Js.String2.split("")
          jooh->Js.Array2.findIndex(foundChars =>
            foundChars->Js.Array2.length == chars->Js.Array2.length && foundChars->includes(chars)
          )
        })

      output->Js.Array2.joinWith("")->Belt.Int.fromString->Belt.Option.getExn
    })
    ->Js.Array2.reduce((acc, i) => acc + i, 0)
  }
}

Solution.make(Part01.make, "day08/input")
Solution.make(Part02.make, "day08/input")
