module FsCheck.NUnit

open FsCheck
open NUnit.Framework

let private nunitRunner =
    { new IRunner with
        member x.OnStartFixture t = ()
        member x.OnArguments(ntest, args, every) = ()
        member x.OnShrink(args, everyShrink) = ()
        member x.OnFinished(name, result) = 
            match result with 
            | TestResult.True(data, _) -> 
                printfn "%s" (Runner.onFinishedToString name result)
            | _ -> Assert.Fail(Runner.onFinishedToString name result) }
   
let private nunitConfig = { Config.Default with Runner = nunitRunner }

let fsCheck name testable =
    FsCheck.Check.One (name, nunitConfig, testable)
