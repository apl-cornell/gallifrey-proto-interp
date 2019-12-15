class C { 
    mut w : (x : c0 C |) (c1 | a : c1 int)->int 
}; 
let fibonacci = fun (x : c0 C |) (c1 | a : c1 int)->int { 
    if (a <= 2) { 
       1
    } else { 
        let aa = a - 1 in 
        let bb = a - 2 in
        x.w(x, capof(aa),aa) + x.w(x, capof(bb),bb) 
    }
} in
let x = C(capof(fibonacci), fibonacci) in 
focus x { 
    let n = 10 in 
    x.w(x, capof(n), n) 
}