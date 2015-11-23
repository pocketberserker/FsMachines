namespace FsCheck.NUnit

open NUnit.Core.Extensibility
open FsCheck
open FsCheck.NUnit.Addin

[<NUnitAddin(Description = "FsCheck addin")>]
type FsCheckAddin() =
  interface IAddin with
    override x.Install host =
      let tcBuilder = new FsCheckTestCaseBuilder()
      host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
      true
