KaDe -mode batch ../kappa/sym.ka
grep -n -m 1 'tinit' ode.m --after-context 8 > ../generated_img/int_parameters.txt
grep -n -m 1 '% rule  ' ode.m --after-context 7 > ../generated_img/rule.txt
grep -n -m 1 'function Init' ode.m --after-context 11 > ../generated_img/init.txt
grep -n -m 1 'function dydt' ode.m --after-context 38 > ../generated_img/dydt.txt
KaDE --rate-convention Biochemist ../kappa/sym.ka -mode batch --show-symmetries |grep "Symmetries" --after-context 20 > ../generated_img/sym.txt
grep -n -m 1 'function obs' ode.m --after-context 12 > ../generated_img/obs.txt
