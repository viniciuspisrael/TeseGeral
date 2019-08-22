likelihood <-function(beta, sig2, tau2, phi, D){ 
   -0.5*(n*log(2*pi) + log(abs(sig2*R(pi)+tau2*I)) + t((y-D*beta)*solve(sig2*R(phi)+tau2*I))*(y-D*beta))
}
