let c1 in
let c1 f =
    fun(
        x: A [int, int -> int],
        y: A int
    ) int-> x(y,y) end in
let c1 f2 = fun(a: A int, b: A int) int -> a+b+1 end in
print(f(f2, 1))