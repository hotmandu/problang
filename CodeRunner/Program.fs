open System
open Problang

let stringRepeat n input =
    if System.String.IsNullOrEmpty input then
        input
    else
        let result =
            new System.Text.StringBuilder(input.Length * n)

        result.Insert(0, input, n).ToString()

let REP () =
    let mach = Problang.Create()
    printfn "Type @@end to run code / @@quit to quit program --->"
    let mutable flag = true
    let mutable code = ""
    let mutable qflag = false

    while flag do
        printf "  > "
        let str = Console.ReadLine()

        if str = "@@end" then
            flag <- false
        elif str = "@@quit" then
            qflag <- true
            flag <- false
        else
            code <- code + str

    if qflag = true then
        false
    else
        mach.Load(code)
        flag <- true

        let promptAction () =
            printf "Control > "
            let controls = Console.ReadKey true
            // To clear "Invalid key"
            printf "%s" (" " |> stringRepeat (Console.BufferWidth - 10))
            Console.SetCursorPosition(10, Console.CursorTop)

            match controls.Key with
            | ConsoleKey.RightArrow ->
                mach.Step()
                Ok "Step"
            | ConsoleKey.Escape
            | ConsoleKey.Q ->
                flag <- false
                Ok "Escape"
            | _ ->
                Error $"Invalid key: {controls.Key}"

        let resultToBool rt = 
            match rt with
            | Ok(str) -> printfn "%s" str; false
            | Error(str) -> 
                printf "%s" str
                Console.SetCursorPosition(0, Console.CursorTop)
                true

        while flag do
            // print machine states
            printfn "Some states"
            printfn "..."

            while () |> promptAction |> resultToBool do
                ()

        printfn "..."
        true

let Loop func =
    while func () do
        printfn "--------------------------------"

printfn "Problang Runner"
printfn ""
REP |> Loop
