module rec Problang.Machine

type Block = (string * float) list

type LazyMemory (``void``: Block) =
    new() = LazyMemory([])



type Machine() =
    let Observations: Parser.Observation list = []
    let Memory: LazyMemory = LazyMemory()

    member this.Load (content: string) =
        () // failwith "NI"
    member this.Step () =
        () // failwith "NI"