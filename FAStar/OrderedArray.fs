namespace FAStar
open System
open System.Collections

type OrderedArray<'k, 'v when 'v : comparison> =
    {
        Keys: Map<'v, 'k>
        Arr: 'v[]
    }

module OrderedArray =
    let internal presortedArrayInsert item lookup arr =
        let iVal = lookup item
        let maxIndex = Array.length arr

        let rec inSortAdd index =
            if index = maxIndex then Array.append arr [|item|]
            else
                if iVal < lookup arr.[index] then
                    if index = 0 then Array.append [|item|] arr
                    else Array.concat [ arr.[0 .. (index - 1)]; [|item|]; arr.[(index)..] ]
                else inSortAdd (index + 1)
        inSortAdd 0

    let empty =
        {
            Keys = Map.empty
            Arr = Array.empty
        }

    let add key value queue =
        let keyMap = queue.Keys |> Map.add value key
        let lookup t = keyMap.[t]
        {
            queue with
                Keys = keyMap
                Arr = if Map.containsKey value queue.Keys then
                            Array.sortBy (lookup) queue.Arr
                        else
                            presortedArrayInsert value (lookup) queue.Arr
        }

    let contains value queue = Map.containsKey value queue.Keys

    let count queue = Array.length queue.Arr
    let head queue = Array.head queue.Arr
    let tail queue = {
        queue with
            Keys = queue.Keys |> Map.remove (Array.head queue.Arr)
            Arr = Array.tail queue.Arr
    }

    let pop queue =
        (head queue, tail queue)

