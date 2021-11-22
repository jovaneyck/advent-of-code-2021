// #r "nuget: Unquote"
// open Swensen.Unquote

// printf "Testing..."
// test <@ 1 + 1 = 3 @>
// printfn "...done!"

let input =
    System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\input.txt")
