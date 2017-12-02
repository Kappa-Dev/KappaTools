"${KAPPABIN}"KaDE -mode batch ../kappa/sym.ka
grep -n -m 1 'nodevar=' ode.m > ../generated_img/n_var.txt
grep -n -m 1 'tinit=' ode.m --after-context 8 > ../generated_img/int_parameters.txt
grep -n -m 1 'rule    : A(x,y), A(x,y) -> A(x!1,y), A(x,y!1)' ode.m --after-context 6 > ../generated_img/rule.txt
grep -n -m 1 'function Init' ode.m --after-context 11 > ../generated_img/init.txt
grep -n -m 1 'function dydt' ode.m --after-context 38 > ../generated_img/dydt.txt
"${KAPPABIN}"KaDE --rate-convention Biochemist ../kappa/sym.ka -mode batch --show-symmetries |grep "Symmetries" --after-context 15 > ../generated_img/sym.txt
grep -n -m 1 'function obs' ode.m --after-context 12 > ../generated_img/obs.txt
grep -n -m 1 '%% variables' ode.m  > ../generated_img/convention.txt

"${KAPPABIN}"KaDE -mode batch ../kappa/sym.ka --with-symmetries Backward --rate-convention Biochemist
grep -n -m 1 'nodevar=' ode.m > ../generated_img/n_var_bwd.txt
grep -n -m 1 'tinit' ode.m --after-context 8 > ../generated_img/int_parameters_bwd.txt
grep -n -m 1 'rule    : A(x,y), A(x,y) -> A(x!1,y), A(x,y!1)' ode.m --after-context 6 > ../generated_img/rule_bwd.txt
grep -n -m 1 'function Init' ode.m --after-context 11 > ../generated_img/init_bwd.txt
grep -n -m 1 'function dydt' ode.m --after-context 38 > ../generated_img/dydt_bwd.txt
grep -n -m 1 'function obs' ode.m --after-context 12 > ../generated_img/obs_bwd.txt
grep -n -m 1 '%% variables' ode.m  > ../generated_img/convention_bwd.txt

"${KAPPABIN}"KaSim -mode batch -i ../kappa/occ.ka -l 1 -p 0.5 -d ../generated_img/ -o occ.csv
