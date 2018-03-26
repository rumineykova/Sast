module ScribbleGenerativeTypeProvider.ErrorMsg

let incorrectType = "Incorrect type evaluated : Either not enough argument given to the assertion or the assertion is not a function that returns a boolean"
let assertionInvalid = "Assertion Constraint not met"
let methodNameNotFound = "The only method name that should be available should be send/receive/choice/finish"
let wrongLable = "Received a wrong Label, that doesn't belong to the possible Labels at this state"
let wrongNumberOfRoles = "you don't have the correct number of roles in the YAML Configuration file"
let agentNotInstantiated = "agent not instanciated yet"
let wrongDelim str1 str2 = sprintf "Error with delimitations : For the moment avoid to have a label equal to a label + delimiter \n Example : %s AND %s" str1 str2
let valueNotInCache k = (sprintf "Cannot retrieve value from cache: %s" k)
let wrongRole name= (sprintf "The following role : %s from the config file doesnt belong to the protocol: Check If you have spelled it correctly, be aware, the role is case-sensitive"  name )
let unexpectedMethod methodName= (sprintf " Mistake you have a method named : %s, that is not expected !" methodName )
let valueNotInit = "no value has been initialize yet"
let valueNotInCacheArg = "This arg hasn't been added yet to the dictionary"
let valueNotInCacheFoo = "This function has not been added to the dictionary yet"
let labelNotInDelim label = sprintf "the following label %s has not been defined in the list of delimiters" label
