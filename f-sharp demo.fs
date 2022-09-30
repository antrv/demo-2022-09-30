let i = 4
let s = "Hello, world!"

let s1 =
    let name = "Anton"
    "Hello, " + name

printfn $"{s1}"

let add x y = x + y
let add_ = fun x y -> x + y
let subtract = (-)
let z1 = add 1.5 1.4
let z2 = add_ "Hello, " "World!"

let inline add x y = x + y
let z1 = add 1.5 1.4
let z2 = add "Hello, " "World!"

let add5 = add 5
let a = add5 10
printfn $"{a}"

let inline mul x y = x * y

let a =
    2
    |> add 3
    |> mul 9

let add3mul9 = add 3 >> mul 5

let rec fib n =
    if n <= 1 then 1 else fib (n-1) + fib (n-2)

let t = 1, "string"
let t1 = 1.0, (fun x -> x * 6 + 7), "string"

let l = [1; 5; 7]
let l2 = [1.0..2.0..10.0]

let l3 = l2 |> List.map (fun x -> x * 2.0)
let l4 = l2 |> List.map ((*) 2.0)

let l5 = [ for i = 1 to 10 do i * i ]

let a1 = [| "a"; "b"; "c" |]

let sq = seq { 1L..1_000_000_000_000_000_000L }

sq
|> Seq.where (fun x -> x % 2L = 0L)
|> Seq.take 10
|> Seq.iter (printfn "%d")


let sq1 = seq {
             for x in 1..100 do
                 for y in 1..100 do
                     x, y
          }

for i in sq1 do
    printfn $"{i}"


-----------------------------------------------------------------------------------------------


type Line(x1, y1, x2, y2) as self =
    do
        printfn $"Creating Line: ({x1}, {y1}), ({x2}, {y2})"
        printfn $"Length: {self.Length}"
       
    member _.X1 = x1
    member _.Y1 = y1
    member _.X2 = x2
    member _.Y2 = y2
       
    member _.Length =
        let sqr x = x * x
        sqrt(sqr(x1 - x2) + sqr(y1 - y2))

let aLine = Line(1.0, 1.0, 4.0, 5.0)


-----------------------------------------------------------------------------------------------

type Address =
    { Country: string; City: string; PostCode: string
      AddressLine1: string; AddressLine2: string option }

let getCity addr = addr.City

type Contact =
    | Phone of string
    | Email of string
    | Address of Address

let printContact contact =
    match contact with
    | Phone phone -> printfn $"Mobile: {phone}"
    | Email email -> printfn $"E-mail: {email}"
    | Address address -> printfn $"Address: {address.Country} {address.City} {address.PostCode} {address.AddressLine1} {address.AddressLine2}"

let extractCity contact =
    match contact with
    | Address address -> Some address.City
    | _ -> None
    
-----------------------------------------------------------------------------------------------

open System
open System.Numerics

type ParserInput = { Text: string; Pos: int }

let input s = { Text = s; Pos = 0 }
let isEndOfInput input = input.Pos >= input.Text.Length
let next input = { input with Pos = input.Pos + 1 }
let current input = input.Text[input.Pos]

type ParserResult<'T> =
    | Success of value: 'T * next: ParserInput
    | Failure of message: string * input: ParserInput 

let chrPred predicate failureMessage input =
    if isEndOfInput input || not(predicate(current input)) then
        Failure(failureMessage, input)
    else
        Success(current input, next input)

let chr c = chrPred (fun ch -> ch = c) $"'{c}' expected"
let digit = chrPred (fun c -> c >= '0' && c <= '9') "Digit expected"
let whitespace = chrPred Char.IsWhiteSpace "Whitespace expected"

let map fn parser input =
    match parser input with
    | Success(value, next) -> Success(fn value, next)
    | Failure(message, input) -> Failure(message, input)

let digitAsInteger =
    digit
    |> map (fun d -> int d - int '0')

let many parser input =
    let unfold input =
        match parser input with
        | Success(value, next) -> Some((value, next), next)
        | _ -> None
    input
    |> List.unfold unfold
    |> List.mapFold (fun _ (value, next) -> value, next) input
    |> Success

let digits = many digit

let asString values =
    values
    |> Seq.map string
    |> String.concat ""
    
let digitsAsString =
    digits
    |> map asString

let whitespaces =
    whitespace
    |> many
    |> map asString

let optional parser input =
    match parser input with
    | Failure(_, input) -> Success(None, input)
    | Success(value, next) -> Success(Some value, next)

let minus =
    chr '-'
    |> optional

let then_ secondParser firstParser input =
    match firstParser input with
    | Failure(message, input) -> Failure(message, input)
    | Success(value, next) ->
        match secondParser next with
        | Failure(message, input) -> Failure(message, input)
        | Success(value2, next) -> Success((value, value2), next)

let atLeastOne parser =
    parser
    |> then_ (many parser)  
    |> map (fun (firstValue, list) -> [firstValue] @ list)

let integer =
    minus
    |> map (Option.map string >> Option.defaultValue "")
    |> then_ (atLeastOne digit |> map asString)
    |> map (fun (minus, digits) -> BigInteger.Parse(minus + digits))

let endOfInput input =
    if isEndOfInput input then
        Success(true, input)
    else
        Failure("End of input expected", input)
        
let thenEndOfInput parser =
    parser
    |> then_ endOfInput 
    |> map fst

let trimWhitespaces parser =
    whitespaces
    |> then_ parser
    |> map snd
    |> then_ whitespaces
    |> map fst
    
let result = "    -100500     a"
             |> input
             |> (integer |> trimWhitespaces |> thenEndOfInput)

printfn $"{result}"

let or_ parsers firstParser input =
    match firstParser input with
    | Failure(_, input) as result ->
        parsers
        |> Seq.map (fun p -> p input)
        |> Seq.tryFind (fun r -> match r with | Success _ -> true | _ -> false)
        |> Option.defaultValue result
    | Success _ as result -> result

let plusOrMinus =
    chr '+'
    |> or_ [chr '-']

let starOrSlash =
    chr '*'
    |> or_ [chr '/']

let atLeastOneWithSep sepParser parser =
    parser
    |> then_ (many (sepParser |> then_ parser))  

type Expression =
    | Int of bigint
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression

let rec expression input =
    let bracketExpr =
        chr '('
        |> trimWhitespaces
        |> then_ expression
        |> map snd
        |> then_ (chr ')' |> trimWhitespaces)
        |> map fst
    
    let simpleExpr =
        integer
        |> trimWhitespaces
        |> map Int
        |> or_ [bracketExpr]
    
    let product =
        simpleExpr
        |> atLeastOneWithSep starOrSlash
        |> map (fun (first, list) -> list |> List.fold (fun expr (op, expr2) -> if op = '*' then Mul(expr, expr2) else Div(expr, expr2)) first)
        
    let sum =
        product
        |> atLeastOneWithSep plusOrMinus
        |> map (fun (first, list) -> list |> List.fold (fun expr (op, expr2) -> if op = '+' then Add(expr, expr2) else Sub(expr, expr2)) first)

    sum input

let rec calc expr =
    match expr with
    | Int value -> value
    | Add(left, right) -> calc left + calc right
    | Sub(left, right) -> calc left - calc right
    | Mul(left, right) -> calc left * calc right
    | Div(left, right) -> calc left / calc right

let printResult s =
    let result = s
                 |> input
                 |> (expression |> thenEndOfInput)
    match result with
    | Success(expr, _) -> printfn $"Result: {calc expr}"
    | Failure(message, input) -> printfn $"Error: {message} at position {input.Pos}"
    
printResult "   6 + 8 * (1 + (-2)) -  10/5 "
printResult "6 + 8 * (1 + (-2 + x)) "
