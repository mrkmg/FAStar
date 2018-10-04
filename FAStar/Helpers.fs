module FAStar.Helpers

let rec internal transformAppendListToMap inputList calculator outputMap =
    if (List.length inputList) = 0 then outputMap
    else
        let k = inputList.Head
        let v = calculator k
        transformAppendListToMap inputList.Tail calculator (Map.add k v outputMap)

let rec internal appendListToSet inputList outputSet =
    if (List.length inputList) = 0 then outputSet
    else appendListToSet inputList.Tail (Set.add inputList.Head outputSet)

let internal tap (action : 'T -> unit) (value : 'T) : 'T = action value; value