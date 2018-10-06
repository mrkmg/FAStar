namespace FAStar

open System

type OrderedItem<'k, 'v when 'k : comparison and 'v: comparison>(key: 'k, value: 'v) =
    member __.Key = key
    member __.Value = value
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
    member __.contains (value: 'v) = Set.contains value knownItems
    member __.count = List.length items
    member __.head = (List.head items).Value
    member __.tail = new OrderedList<'k, 'v>(List.tail items, Set.remove (List.head items).Value knownItems)
    member this.pop = (this.head, this.tail)
    member this.add key value = 
        let item = new OrderedItem<'k, 'v>(key, value)
        if items = [] then new OrderedList<'k, 'v>(item :: items, Set.add value knownItems)
        else
            new OrderedList<'k, 'v>(
                List.ofSeq (
                    seq {
                        let mutable didInsert = false
                        for i in items do
                            if not (i.Value = value) then
                                if not(didInsert) && key < i.Key then 
                                    didInsert <- true    
                                    yield item
                                yield i
                        }),  Set.add value knownItems)

module OrderedList =
    let empty<'k, 'v when 'v : comparison and 'k : comparison> = new OrderedList<'k, 'v>(List.empty, Set.empty)
    let contains value (ol: OrderedList<'k, 'v>) = ol.contains value
    let count (ol: OrderedList<'k, 'v>) = ol.count
    let head (ol: OrderedList<'k, 'v>) = ol.head
    let add key value (ol: OrderedList<'k, 'v>) = ol.add key value
    let tail (ol: OrderedList<'k, 'v>) = ol.tail
    let pop (ol: OrderedList<'k, 'v>) = ol.pop

