module Part01 = {
  open Belt

  let parseCommands = string => {
    let matches = string->Js.String2.match_(%re("/(.*)\s(\d*)/"))

    let value = switch matches {
    | Some([_, _, value]) => value->Int.fromString
    | _ => None
    }

    switch (matches, value) {
    | (Some([_, "forward", _]), Some(value)) => (value, 0)
    | (Some([_, "up", _]), Some(value)) => (0, value * -1)
    | (Some([_, "down", _]), Some(value)) => (0, value)
    | _ => (0, 0)
    }
  }

  let make = input => {
    let (x, y) =
      input
      ->Js.String2.split("\n")
      ->Array.reduce((0, 0), ((x, y), command) => {
        let (newX, newY) = parseCommands(command)

        (x + newX, y + newY)
      })

    x * y
  }
}

module Part02 = {
  let parseCommands = ((x, y, aim), string) => {
    let matches = switch string->Js.String2.split(" ") {
    | [command, value] => Some(command, value->Belt.Int.fromString)
    | _ => None
    }

    switch matches {
    | Some("forward", Some(value)) => (x + value, y + value * aim, aim)
    | Some("up", Some(value)) => (x, y, aim - value)
    | Some("down", Some(value)) => (x, y, aim + value)
    | _ => (0, 0, 0)
    }
  }

  let make = input => {
    let (x, y, _) = input->Js.String2.split("\n")->Js.Array2.reduce(parseCommands, (0, 0, 0))
    x * y
  }
}

Solution.make(Part02.make, "day02/input")
