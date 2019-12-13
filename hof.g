let hof = fun (c1 | f : c1 (c2 | x : c2 int)->int) (c3| n : c3 int)->int {
    let r = f(capof(n), n) in
    f(capof(r), r)
} in
let double = fun (c | x : c int)->int { x * 2 } in
let init = 3 in
hof(capof(double), double, capof(init), init)
