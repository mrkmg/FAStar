namespace FAStar

open System

[<CustomEquality; CustomComparison>]
type OrderedItem<'k, 'v when 'k : comparison and 'v: comparison> =
    {
        Key: 'k
        Value: 'v
    } with
    interface IComparable<OrderedItem<'k, 'v>> with
        member this.CompareTo other = compare this.Key other.Key
    interface IComparable with
        member this.CompareTo obj =
            match obj with
                | null -> 1
                | :? OrderedItem<'k, 'v> as other -> (this :> IComparable<_>).CompareTo other
                | _ -> invalidArg "obj" "is not a OrdredItem"
    interface IEquatable<OrderedItem<'k, 'v>> with
        member this.Equals other = this.Value = other.Value
    override this.Equals obj = 
        match obj with
        | :? OrderedItem<'k, 'v> as other -> (this :> IEquatable<_>).Equals other
        | _ -> false
    override this.GetHashCode () = this.Value.GetHashCode()

type OrderedList<'k, 'v when 'v : comparison and 'k : comparison>(items: OrderedItem<'k, 'v> list, knownItems: Set<'v>)  =
    let _insert (item: OrderedItem<'k, 'v>) =
        let rec __insert isReplace isPending ret (rem: OrderedItem<'k, 'v> list) =
            match (isReplace, isPending, rem) with
            | (_,    true,  []          )                              -> item :: ret |> List.rev
            | (_,    false, []          )                              -> ret |> List.rev
            | (_,    true,  head :: _   ) when item.Key <= head.Key    -> __insert isReplace false (item :: ret) rem
            | (true, _,     head :: tail) when head.Value = item.Value -> __insert false isPending ret tail
            | (_,    _,     head :: tail)                              -> __insert isReplace isPending (head :: ret) tail
        __insert (Set.contains item.Value knownItems) true [] items

    member __.contains (value: 'v) = Set.contains value knownItems
    member __.count = Set.count knownItems
    member __.head = (List.head items).Value
    member this.tail = new OrderedList<'k, 'v>(List.tail items, Set.remove (this.head) knownItems)
    member this.pop = (this.head, this.tail)
    member __.add key value = new OrderedList<'k, 'v>(_insert {Key = key; Value = value}, Set.add value knownItems)

module OrderedList =
    let empty<'k, 'v when 'v : comparison and 'k : comparison> = new OrderedList<'k, 'v>(List.empty, Set.empty)
    let contains value (ol: OrderedList<'k, 'v>) = ol.contains value
    let count (ol: OrderedList<'k, 'v>) = ol.count
    let head (ol: OrderedList<'k, 'v>) = ol.head
    let add key value (ol: OrderedList<'k, 'v>) = ol.add key value
    let tail (ol: OrderedList<'k, 'v>) = ol.tail
    let pop (ol: OrderedList<'k, 'v>) = ol.pop




