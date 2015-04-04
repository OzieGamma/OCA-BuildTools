// Heavily inspired from https://github.com/fsharp/FAKE/blob/master/build.fsx (Apache 2.0)

#I @"packages/FAKE/tools/"
#I @"packages/FSharp.Formatting/lib/net40"

#r @"FakeLib.dll"
#r @"FSharp.CodeFormat.dll"
#r @"FSharp.Literate.dll"

open Fake
open Fake.Git
open Fake.FSharpFormatting
open Fake.AssemblyInfoFile

// properties
let projectName = "OCA-BuildTools"
let projectSummary = "BuildTools for OCA"
let authors = ["Oswald MASKENS"]

// Version info
let version = "1.0.0"

let buildDir = "./build/"
let binBuildDir = buildDir @@ "bin/"
let docsDir = buildDir @@ "docs/"
let apidocsDir = docsDir @@ "apidocs/"

let docsInputDir = "Help/"
let docsLiterateTemplate = docsInputDir @@ "/templates/template.html"
let docsDllTemplates = docsInputDir @@ "/templates/reference/" 

let ghPagesDir = "gh-pages/"
let testResultsFile = buildDir @@ "testResults.xml"

let additionalFiles = [
    "LICENSE"
    "CONTRIBUTIONS.md"
    "CONTRIBUTORS.md"
    "README.md" ]

// Targets
Target "Clean" (fun _ -> CleanDirs [buildDir; binBuildDir; docsDir; apidocsDir; ghPagesDir])

Target "AssemblyInfo" (fun _ ->
    let common = [
        Attribute.Product projectSummary
        Attribute.Version version
        Attribute.FileVersion version
    ] 
    [ Attribute.Title "Lib for OCA asm handling"
      Attribute.Description projectSummary
      Attribute.InternalsVisibleTo "OCA.AsmLib.Test" ] @ common |> CreateFSharpAssemblyInfo "./AsmLib/src/AssemblyInfo.fs"
    [ Attribute.Title "Assembler for OCA"
      Attribute.Description projectSummary
      Attribute.InternalsVisibleTo "OCA.Assembler.Test" ] @ common |> CreateFSharpAssemblyInfo "./Assembler/src/AssemblyInfo.fs"
)

Target "Build" (fun _ -> MSBuildRelease binBuildDir "Build" ["OCA-BuildTools.sln"] |> Log "Build-Output: ")

Target "Test" (fun _ -> 
    !!(binBuildDir + "/*.Test.dll") |> NUnit(fun p -> 
                                  { p with DisableShadowCopy = true
                                           TimeOut = System.TimeSpan.FromMinutes 5.0
                                           Framework = "4.5"
                                           Domain = NUnitDomainModel.DefaultDomainModel
                                           OutputFile = testResultsFile }))



Target "BuildDocs" (fun _ ->
    let githubLink = "https://github.com/oziegamma/OCA-BuildTools"
    
    let dllFiles = 
        !! (binBuildDir @@ "**/OCA.*.dll")
        -- (binBuildDir @@ "**/OCA.*.Test.dll")

    let projInfo =
      [ "page-description", projectSummary
        "page-author", separated ", " authors
        "project-author", separated ", " authors
        "github-link", githubLink
        "project-github", githubLink
        "root", "http://www.beglobal.me/OCA-BuildTools"
        "project-name", projectName]

    Copy docsDir additionalFiles

    CreateDocs docsInputDir docsDir docsLiterateTemplate projInfo

    CreateDocsForDlls apidocsDir docsDllTemplates projInfo (githubLink @@ "blob/master") dllFiles

    WriteStringToFile false (docsDir @@ ".nojekyll") ""

    CopyDir (docsDir @@ "content") (docsInputDir @@ "content") allFiles
)

Target "ReleaseDocs" (fun _ ->
    cloneSingleBranch "" "https://github.com/oziegamma/OCA-BuildTools.git" "gh-pages" ghPagesDir

    fullclean ghPagesDir
    CopyRecursive docsDir ghPagesDir true |> printfn "%A"
    StageAll ghPagesDir
    Commit ghPagesDir "Update generated documentation"
    Branches.push ghPagesDir
)

Target "MainBuild" DoNothing

// Build order
"Clean" ==> "AssemblyInfo" ==> "Build" ==> "Test" ==> "BuildDocs" ==> "MainBuild" ==> "ReleaseDocs"

// start build
RunTargetOrDefault "MainBuild"