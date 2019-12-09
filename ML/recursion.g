class C { 
    mut w : (x : c0 C |) (c1 | a : c1 int) (c2 | b : c2 int)->int 
}; 
let f = fun (x : c0 C |) (c1 | a : c1 int) (c2 | b : c2 int)->int { 
    if (a == 0) { 
       b 
    } else { 
        let aa = a - 1 in 
        let bb = b + 2 in 
        x.w(x, capof(aa),aa,capof(bb),bb)
    }
} in
let x = C(capof(f), f) in 
focus x { 
    let iters = 2 in 
    let init = 0 in 
    x.w(x, capof(iters),iters,capof(init),init) 
}