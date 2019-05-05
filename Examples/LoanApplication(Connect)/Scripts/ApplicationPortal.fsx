#r "../../../src/Sast/bin/Debug/net452/Sast.dll"

open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "applyForLoan", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "checkEligibility", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "respond", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "sendLoanAmount", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] }}, 
                   {"label" : "requestConfirmation", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] }}, 
                   {"label" : "reject", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] }}, 
                   {"label" : "getLoanAmount", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "Int", "type": "System.Int32"}, 
          {"alias" : "String", "type": "System.String"},
          {"alias" : "Bool", "type": "System.Boolean"}] """

// C:/cygwin64/home/rhu/code/vs/scribble/github.com/rumineykova/Sast/Examples/Fibonacci/
type LoanApp = 
    Provided.TypeProviderFile<"../../../Examples/LoanApplication/LoanApplication.scr" // Fully specified path to the scribble file
                               ,"BuyerBrokerSupplier" // name of the protocol
                               ,"ApplicationPortal" // local role
                               ,"../../../Examples/LoanApplication/config_appP.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.LocalExecutable, // choose one of the following options: (LocalExecutable | WebAPI | File)
                              ExplicitConnection=true>

let numIter = 3
let AppPortal = LoanApp.ApplicationPortal.instance
let App = LoanApp.Applicant.instance
let PDept = LoanApp.ProcessingDept.instance

let loanApp = new LoanApp()

let first = loanApp.Start()

first.accept(App).request(PDept).receiveapplyForLoan()



    
