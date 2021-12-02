@val external __dirname: string = "__dirname"

let make = (solution, path) => {
  Node.Path.resolve(__dirname, path)->Node.Fs.readFileAsUtf8Sync->solution->Js.log
}
