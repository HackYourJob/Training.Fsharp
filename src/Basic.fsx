// variable
let a: string = "Hello"

// inference
let b = "hello"

// immutable
b = "World"
//b <- "World" // not working

let mutable c = "bad"
c <- "good"

// function
let add1 x = x + 1
let add2 (x: int) : int =
    let y = add1 x
    add1 y

add1 5
add2 5

// module
module m1 =
    let add x = x + 1

module m2 =
    let add' (x: int) : int =
        let y = m1.add x
        m1.add y

    open m1

    let add'' (x: int) : int =
        let y = add x
        add y

m2.add' 5

