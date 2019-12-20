class C {mut a : int}; 
let c = 1 in 
let x = C(capof(c), c) in 
let y = x in 
x.a