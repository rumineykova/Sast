module SHData =                         
    [<Literal>]
    let delims = """ [ {"label" : "IsAbove", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                       {"label" : "Res", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                       {"label" : "hello", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                       {"label" : "BothIn", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                       {"label" : "BothOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                       {"label" : "Intersct", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                       {"label" : "Res1", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                       {"label" : "One", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                       {"label" : "Two", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                       {"label" : "Plane", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                       {"label" : "plane", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                       {"label" : "SecOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                       {"label" : "SecIn", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                       {"label" : "Inersect", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                       {"label" : "Close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""                       
    [<Literal>]
    let typeAliasing =
        """ [ {"alias" : "int", "type": "System.Int32"} ] """
