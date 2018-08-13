module SeqUtils

    let hasExactly x (seq:seq<'T>) = 
        let mutable count  = 0
        let iter = seq.GetEnumerator()
        let mutable brk = false
        while iter.MoveNext() && not brk do 
                count <- count + 1
                if count > x then brk <- true
        count = x