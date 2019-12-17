class C { 
    mut w : (x : c0 C |) (c1 | a : c1 int)->int 
}; 
let factorial = fun (x : c0 C |) (c1 | a : c1 int)->int { 
    if (a <= 1) { 
       1
    } else { 
        let aa = a - 1 in 
        x.w(x, capof(aa),aa) * a
    }
} in
let x = C(capof(factorial), factorial) in 
focus x { 
    let n = 5 in 
    x.w(x, capof(n), n) 
}