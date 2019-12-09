class C {mut a : int}; class C2 {mut o : C}; let c = 1 in let d = 2 in 
let x = C(capof(c),c) in let y = C(capof(d),d) in let z = C2(capof(x),x) 
in z.o = y