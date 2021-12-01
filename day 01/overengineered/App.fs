module App

let solve combiner =
    Seq.map Parser.parse
    >> combiner
    >> Seq.pairwise
    >> Seq.filter (fun pair -> pair ||> BusinessLogic.isIncrease)
    >> Seq.length
