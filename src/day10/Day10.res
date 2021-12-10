exception SyntaxError(int)
exception ActualError

module Part01 = {
  let scores = char =>
    switch char {
    | ")" => 3
    | "]" => 57
    | "}" => 1197
    | ">" => 25137
    | _ => 0
    }

  let opposite = char =>
    switch char {
    | "(" => ")"
    | "[" => "]"
    | "{" => "}"
    | "<" => ">"
    | _ => raise(ActualError)
    }

  let rec parse = (line, stack) => {
    let char = line->Js.String2.charAt(0)
    let nextLine = line->Js.String2.sliceToEnd(~from=1)

    switch char {
    | "" => ()
    | "("
    | "["
    | "{"
    | "<" =>
      let _ = stack->Js.Array2.push(char)
      parse(nextLine, stack)
    | _ if char == opposite(stack[stack->Js.Array2.length - 1]) => {
        let _ = Js.Array2.pop(stack)
        parse(nextLine, stack)
      }
    | _ => raise(SyntaxError(scores(char)))
    }
  }

  let make = input => input->Js.String2.split("\n")->Js.Array2.reduce((acc, line) => {
      try {
        line->parse([])
        acc
      } catch {
      | SyntaxError(score) => acc + score
      }
    }, 0)
}

module Part02 = {
  let scores = char =>
    switch char {
    | ")" => 1
    | "]" => 2
    | "}" => 3
    | ">" => 4
    | _ => 0
    }

  let opposite = char =>
    switch char {
    | "(" => ")"
    | "[" => "]"
    | "{" => "}"
    | "<" => ">"
    | _ => raise(ActualError)
    }

  let getMiddle = arr => arr->Belt.Array.getUnsafe(arr->Js.Array2.length / 2)
  let tap = arr => {
    Js.log(arr)
    arr
  }

  let rec parse = (line, stack) => {
    let char = line->Js.String2.charAt(0)
    let nextLine = line->Js.String2.sliceToEnd(~from=1)

    switch char {
    | "" => stack
    | "("
    | "["
    | "{"
    | "<" =>
      let _ = stack->Js.Array2.push(char)
      parse(nextLine, stack)
    | _ if char == opposite(stack[stack->Js.Array2.length - 1]) => {
        let _ = Js.Array2.pop(stack)
        parse(nextLine, stack)
      }
    | _ => raise(SyntaxError(scores(char)))
    }
  }

  let autocompleteScore = (prev, char) => {
    let score = char->opposite->scores->Belt.Float.fromInt
    prev *. 5. +. score
  }

  let getAutoCompleteScore = (scores, line) => {
    try {
      let stack = line->parse([])
      let score = stack->Js.Array2.reduceRight(autocompleteScore, 0.)
      let _ = scores->Js.Array2.push(score)
    } catch {
    | SyntaxError(_) => ()
    }
    scores
  }

  let make = input =>
    input
    ->Js.String2.split("\n")
    ->Js.Array2.reduce(getAutoCompleteScore, [])
    ->Belt.SortArray.stableSortBy((a, b) => a > b ? 1 : -1)
    ->getMiddle
}

Solution.make(Part01.make, "day10/input")
Solution.make(Part02.make, "day10/input")
