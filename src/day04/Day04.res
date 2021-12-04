let getNumbers = lines => lines[0]->Js.String2.split(",")
let getBoards = lines =>
  lines
  ->Js.Array2.slice(~start=2, ~end_=lines->Js.Array2.length)
  ->Js.Array2.joinWith("\n")
  ->Js.String2.splitByRe(%re("/\\n\\n/gm"))
  ->Js.Array2.map(boardString =>
    boardString
    ->Belt.Option.getExn
    ->Js.String2.split("\n")
    ->Js.Array2.map(line =>
      line->Js.String2.trim->Js.String2.splitByRe(%re("/ +/g"))->Js.Array2.map(Belt.Option.getExn)
    )
  )
let score = (board, number) =>
  board->Js.Array2.map(row => row->Js.Array2.map(cell => cell == number ? "x" : cell))
let transpose = board => board[0]->Js.Array2.mapi((_, i) => board->Js.Array2.map(row => row[i]))
let hasBingo = board =>
  board->Js.Array2.some(row => row->Js.Array2.every(cell => cell == "x")) ||
    board->transpose->Js.Array2.some(row => row->Js.Array2.every(cell => cell == "x"))

module Part01 = {
  let make = input => {
    let lines = input->Js.String2.split("\n")
    let numbers = lines->getNumbers
    let boards = lines->getBoards

    let (_, board, number) =
      numbers->Js.Array2.reduce(((modifiedBoards, winningBoard, winningNumber), number) => {
        switch winningBoard {
        | Some(winningBoard) => (modifiedBoards, Some(winningBoard), winningNumber)
        | None => {
            let bingo = ref(false)
            let bingoBoardIndex = ref(-1)

            let newBoards = modifiedBoards->Js.Array2.mapi((board, i) => {
              let newBoard = score(board, number)

              if !bingo.contents {
                bingo := hasBingo(newBoard)

                if bingo.contents {
                  bingoBoardIndex := i
                }
              }

              newBoard
            })

            if bingo.contents {
              (newBoards, Some(newBoards[bingoBoardIndex.contents]), Some(number))
            } else {
              (newBoards, None, None)
            }
          }
        }
      }, (boards, None, None))

    Js.log2(board, number)

    let score = switch board {
    | Some(board) =>
      board->Js.Array2.reduce(
        (acc, row) =>
          acc +
          row->Js.Array2.reduce(
            (acc, item) => acc + item->Belt.Int.fromString->Belt.Option.getWithDefault(0),
            0,
          ),
        0,
      )
    | None => 0
    }

    score * number->Belt.Option.flatMap(Belt.Int.fromString)->Belt.Option.getWithDefault(0)
  }
}

module Part02 = {
  let make = input => {
    let lines = input->Js.String2.split("\n")
    let numbers = lines->getNumbers
    let boards = lines->getBoards

    let (_, board, number) =
      numbers->Js.Array2.reduce(((modifiedBoards, winningBoard, winningNumber), number) => {
        switch winningBoard {
        | Some(_)
        | None => {
            let bingo = ref(false)
            let bingoBoardIndex = ref(-1)

            let newBoards = modifiedBoards->Js.Array2.mapi((board, i) => {
              let newBoard = score(board, number)

              let isBingo = hasBingo(newBoard)

              if isBingo {
                bingo := isBingo
                bingoBoardIndex := i
              }

              newBoard
            })

            if bingo.contents {
              let winningBoard = Some(newBoards[bingoBoardIndex.contents])
              let newBoards = newBoards->Js.Array2.filteri((board, _) => !hasBingo(board))

              (newBoards, winningBoard, Some(number))
            } else {
              (newBoards, winningBoard, winningNumber)
            }
          }
        }
      }, (boards, None, None))

    let score = switch board {
    | Some(board) =>
      board->Js.Array2.reduce(
        (acc, row) =>
          acc +
          row->Js.Array2.reduce(
            (acc, item) => acc + item->Belt.Int.fromString->Belt.Option.getWithDefault(0),
            0,
          ),
        0,
      )
    | None => 0
    }

    score * number->Belt.Option.flatMap(Belt.Int.fromString)->Belt.Option.getWithDefault(0)
  }
}

Solution.make(Part02.make, "day04/input")
