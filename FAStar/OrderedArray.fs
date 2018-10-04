namespace FAStar
open System
open System.Collections

type OrderedArray<'k, 'v when 'v : comparison> =
    {
        Keys: Map<'v, 'k>
        Arr: 'v[]
    }

module OrderedArray =
    let internal sortAdd item lookup arr =
        let iVal = lookup item

        let rec inSortAdd p r =
            if r = [||] then Array.append p [|item|]
            else
                let n = Array.head r
                let nVal = n |> lookup
                if iVal < nVal then Array.append p (Array.append [|item|] r)
                else inSortAdd (Array.append p [|n|]) (Array.tail r)

        inSortAdd [||] arr

    let empty =
        {
            Keys = Map.empty
            Arr = Array.empty
        }

    let add key value queue =
        let keyMap = queue.Keys |> Map.add value key
        {
            queue with
                Keys = keyMap
                Arr = queue.Arr |> sortAdd value (fun v -> keyMap.[v])
        }

    let containsValue value queue = Map.containsKey value queue.Keys

    let remove value queue =
        {
            queue with
                Keys = queue.Keys |> Map.remove value
                Arr = queue.Arr |> Array.except [|value|]
        }

    let count queue = Array.length queue.Arr
    let head queue = Array.head queue.Arr
    let tail queue = remove (head queue) queue

