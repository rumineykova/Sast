// include Fake lib
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.Git
open Fake.ReleaseNotesHelper

let buildDir = "./build/"
let appReferences = !!  "src/**/*.fsproj"

let release = LoadReleaseNotes "RELEASE_NOTES.md"

let project = "Sast"
let summary = "F# generative type provider providing an implementation of session types"

// Clean Target
Target "Clean" (fun _ ->
    CleanDirs [buildDir]
)

// Build in Debug
Target "Build" (fun _ ->
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)



// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title (projectName)
          Attribute.Product project
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath, 
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/*/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName @@ "AssemblyInfo.fs") attributes
        | _ -> ()
        )
)

//// Build a NuGet package
//Target "NuGet" (fun _ ->
//    Paket.Pack(fun p -> 
//        { p with
//            OutputPath = buildDir
//            Version = release.NugetVersion
//            ReleaseNotes = toLines release.Notes})
//)
//
//Target "PublishNuget" (fun _ ->
//    Paket.Push(fun p -> 
//        { p with
//            WorkingDir = buildDir })
//)

"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
//  ==> "NuGet"
//  ==> "PublishNuget"


RunTargetOrDefault "Build"
