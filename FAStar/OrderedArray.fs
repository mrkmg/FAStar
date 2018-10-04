namespace FAStar
open System
open System.Collections

type OrderedArray<'k, 'v when 'v : comparison> =
    {
        Keys: Map<'v, 'k>
        Arr: 'v[]
    }

module OrderedArray =
    let internal presortedArrayInsert (item: 'T) lookup (arr: 'T[]) =
        let iVal = lookup item
        match Array.tryFindIndex (fun i -> iVal < lookup i) arr with
            | Some index -> Array.concat [ arr.[0 .. (index - 1)]; [|item|]; arr.[(index)..] ]
            | None -> Array.append arr [|item|]

    let contains value queue = Map.containsKey value queue.Keys
    let count queue = Array.length queue.Arr
    let head queue = Array.head queue.Arr

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
                Arr = if contains value queue then
                            Array.sortBy (lookup) queue.Arr
                        else
                            presortedArrayInsert value (lookup) queue.Arr
        }

    let tail queue =
        {
            queue with
                Keys = queue.Keys |> Map.remove (Array.head queue.Arr)
                Arr = Array.tail queue.Arr
        }
    let pop queue = (head queue, tail queue)

