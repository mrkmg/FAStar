namespace FAStar
open System
open System.Collections

type OrderedList<'k, 'v when 'v : comparison and 'k :comparison> =
    {
        Keys: Map<'v, 'k>
        List: 'v list
    }

module OrderedList =
    let internal presortedListInsert (item: 'T) lookup (list: 'T list) =
        let iVal = lookup item

        match List.tryFindIndex (fun i -> iVal < lookup i) list with
            | Some index -> List.concat [ list.[0 .. (index - 1)]; [item]; list.[(index)..] ]
            | None -> List.append list [item]

    let contains value queue = Map.containsKey value queue.Keys
    let count queue = List.length queue.List
    let head queue = List.head queue.List

    let empty =
        {
            Keys = Map.empty
            List = List.empty
        }

    let add key value queue =
        let keyMap = queue.Keys |> Map.add value key
        let lookup t = keyMap.[t]
        {
            queue with
                Keys = keyMap
                List = if contains value queue then
                            List.sortBy (lookup) queue.List
                        else
                            presortedListInsert value (lookup) queue.List
        }

    let tail queue =
        {
            queue with
                Keys = queue.Keys |> Map.remove (List.head queue.List)
                List = List.tail queue.List
        }
    let pop queue = (head queue, tail queue)

