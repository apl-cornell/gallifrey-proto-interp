class C { 
    mut w : (x : c0 C |)->unit 
}; 
let f = fun (x : c0 C |)->unit { 
    x.w(x)
} in
let x = C(capof(f), f) in 
focus x { 
    x.w(x) 
}