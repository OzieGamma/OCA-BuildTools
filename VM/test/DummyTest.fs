module OCA.VM.DummyTest

open NUnit.Framework
open OFuncLib

[<Test>]
let ``Tests should run``() =
    true |> shouldEq true