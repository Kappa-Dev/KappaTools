function main=main()
% command line: 
%      'KaDE' '--compute-jacobian' 'true''expr.ka'
%% THINGS THAT ARE KNOWN FROM KAPPA FILE AND KaSim OPTIONS;
%% 
%% init - the initial abundances of each species and token
%% tinit - the initial simulation time (likely 0)
%% tend - the final simulation time 
%% initialstep - initial time step at the beginning of numerical integration
%% period_t_point - the time period between points to return
%%
%% variables (init(i),y(i)) denote numbers of embeddings 
%% rule rates are corrected by the number of automorphisms in the lhs of rules


tinit=0;
tend=1;
initialstep=1e-06;
period_t_point=1;

global nodevar
nodevar=6;
nvar=3;
nobs=1;
nrules=5;

global var
var=zeros(nvar,1);
global init
init=sparse(nodevar,1);

t = 0.000000;

init(6)=t;
init(1)=10; % A(x~u?, y~u?)
var(1)=init(1)+init(3)+init(2)+init(5);
var(2)=var(1)*var(1)*var(1)*t;

global jack
global jackd
global jackun
global jackund

k(1)=1; % A(x~u) -> A(x~p)
k(3)=1; % A(y~u) -> A(y~p)

uiIsOctave = false;
uiIsMatlab = false;
LIC = license('inuse');
for elem = 1:numel(LIC)
    envStr = LIC(elem).feature
    if strcmpi(envStr,'octave')
       LICname=envStr;
       uiIsOctave = true;
       break
    end
    if strcmpi(envStr,'matlab')
       LICname=envStr
       uiIsMatlab = true;
       break
    end
end


options = odeset('RelTol', 1e-3, ...
                 'AbsTol', 1e-3, ...
                 'InitialStep', initialstep, ...
                 'MaxStep', tend, ...
                 'Jacobian', @ode_jacobian);


if uiIsMatlab
   soln =  ode15s(@ode_aux,[tinit tend],ode_init(),options);
   soln.y=soln.y';
elseif uiIsOctave
   soln = ode2r(@ode_aux,[tinit tend],ode_init(),options);
end


nrows = length(soln.x);
tmp = zeros(nodevar,1);

n_points = floor ((tend-tinit)/period_t_point)+1;
t = linspace(tinit, tend, n_points);
obs = zeros(nrows,nobs);

for j=1:nrows
    for i=1:nodevar
        z(i)=soln.y(j,i);
    end
    h=ode_obs(z);
    for i=1:nobs
        obs(j,i)=h(i);
    end
end
if nobs==1
   y = interp1(soln.x, obs, t, 'pchip')';
else
   y = interp1(soln.x, obs, t, 'pchip');
end


filename = 'data.csv';
fid = fopen (filename,'w');
fprintf(fid,'# KaDE --compute-jacobian true expr.ka\n')
fprintf(fid,'# ')
fprintf(fid,'[T],')
fprintf(fid,'\n')
for j=1:n_points
    for i=1:nobs
        fprintf(fid,'%f,',y(j,i));
    end
    fprintf(fid,'\n');
end
fclose(fid);


end



function Init=ode_init()

global nodevar
global init
Init=zeros(nodevar,1);

Init(1) = init(1); % A(x~u?, y~u?)
Init(2) = init(2); % A(x~p?, y~u?)
Init(3) = init(3); % A(x~u?, y~p?)
Init(4) = init(4); 
Init(5) = init(5); % A(x~p?, y~p?)
Init(6) = init(6); % t
end


function dydt=ode_aux(t,y)

global nodevar
global var
global jack
global jackd
global jackun
global jackund

var(1)=y(1)+y(3)+y(2)+y(5);
var(2)=var(1)*var(1)*var(1)*t;

k(2)=var(1);
k(4)=var(2);
k(5)=var(2);

dydt=zeros(nodevar,1);

% rule    : A() -> A() | ((a * a) * [T]):tt
% reaction: A(x~p?, y~u?) -> A(x~p?, y~u?) | ((a * a) * [T]):tt 

dydt(2)=dydt(2)-k(5)*y(2);
dydt(2)=dydt(2)+k(5)*y(2);
dydt(4)=dydt(4)+k(5)*y(2)*var(1)*var(1)*t;

% rule    : A(y~u) -> A(y~p)
% reaction: A(x~p?, y~u?) -> A(x~p?, y~p?) 

dydt(2)=dydt(2)-k(3)*y(2);
dydt(5)=dydt(5)+k(3)*y(2);

% rule    : A(x~p) -> A(x~u)
% reaction: A(x~p?, y~u?) -> A(x~u?, y~u?) 

dydt(2)=dydt(2)-k(2)*y(2);
dydt(1)=dydt(1)+k(2)*y(2);

% rule    : A() -> A() | ((a * a) * [T]):tt
% reaction: A(x~p?, y~p?) -> A(x~p?, y~p?) | ((a * a) * [T]):tt 

dydt(5)=dydt(5)-k(5)*y(5);
dydt(5)=dydt(5)+k(5)*y(5);
dydt(4)=dydt(4)+k(5)*y(5)*var(1)*var(1)*t;

% rule    : A(y~p) -> A(y~u)
% reaction: A(x~p?, y~p?) -> A(x~p?, y~u?) 

dydt(5)=dydt(5)-k(4)*y(5);
dydt(2)=dydt(2)+k(4)*y(5);

% rule    : A(x~p) -> A(x~u)
% reaction: A(x~p?, y~p?) -> A(x~u?, y~p?) 

dydt(5)=dydt(5)-k(2)*y(5);
dydt(3)=dydt(3)+k(2)*y(5);

% rule    : A() -> A() | ((a * a) * [T]):tt
% reaction: A(x~u?, y~p?) -> A(x~u?, y~p?) | ((a * a) * [T]):tt 

dydt(3)=dydt(3)-k(5)*y(3);
dydt(3)=dydt(3)+k(5)*y(3);
dydt(4)=dydt(4)+k(5)*y(3)*var(1)*var(1)*t;

% rule    : A(y~p) -> A(y~u)
% reaction: A(x~u?, y~p?) -> A(x~u?, y~u?) 

dydt(3)=dydt(3)-k(4)*y(3);
dydt(1)=dydt(1)+k(4)*y(3);

% rule    : A(x~u) -> A(x~p)
% reaction: A(x~u?, y~p?) -> A(x~p?, y~p?) 

dydt(3)=dydt(3)-k(1)*y(3);
dydt(5)=dydt(5)+k(1)*y(3);

% rule    : A() -> A() | ((a * a) * [T]):tt
% reaction: A(x~u?, y~u?) -> A(x~u?, y~u?) | ((a * a) * [T]):tt 

dydt(1)=dydt(1)-k(5)*y(1);
dydt(1)=dydt(1)+k(5)*y(1);
dydt(4)=dydt(4)+k(5)*y(1)*var(1)*var(1)*t;

% rule    : A(y~u) -> A(y~p)
% reaction: A(x~u?, y~u?) -> A(x~u?, y~p?) 

dydt(1)=dydt(1)-k(3)*y(1);
dydt(3)=dydt(3)+k(3)*y(1);

% rule    : A(x~u) -> A(x~p)
% reaction: A(x~u?, y~u?) -> A(x~p?, y~u?) 

dydt(1)=dydt(1)-k(1)*y(1);
dydt(2)=dydt(2)+k(1)*y(1);
dydt(6)=1;

end


function jacobian=ode_jacobian(t,jac)

global jacvar
global var
global jack
global jackd
global jackun
global jackund

global k
global kd
global kun
global kdun

var(1)=y(1)+y(3)+y(2)+y(5);
var(2)=var(1)*var(1)*var(1)*t;

k(2)=var(1);
k(4)=var(2);
k(5)=var(2);
jacvar(1,1)=1;
jacvar(1,2)=1;
jacvar(1,3)=1;
jacvar(1,5)=1;
jacvar(2,1)=t*(var(1)*var(1)*jac_var(1,1)+var(1)*(var(1)*jac_var(1,1)+var(1)*jac_var(1,1)));
jacvar(2,2)=t*(var(1)*var(1)*jac_var(1,2)+var(1)*(var(1)*jac_var(1,2)+var(1)*jac_var(1,2)));
jacvar(2,3)=t*(var(1)*var(1)*jac_var(1,3)+var(1)*(var(1)*jac_var(1,3)+var(1)*jac_var(1,3)));
jacvar(2,5)=t*(var(1)*var(1)*jac_var(1,5)+var(1)*(var(1)*jac_var(1,5)+var(1)*jac_var(1,5)));
jacvar(2,6)=var(1)*var(1)*var(1)+t*(var(1)*var(1)*jac_var(1,6)+var(1)*(var(1)*jac_var(1,6)+var(1)*jac_var(1,6)));

jack(2,1)=jac_var(1,1);
jack(2,2)=jac_var(1,2);
jack(2,3)=jac_var(1,3);
jack(2,5)=jac_var(1,5);
jack(4,1)=jac_var(2,1);
jack(4,2)=jac_var(2,2);
jack(4,3)=jac_var(2,3);
jack(4,5)=jac_var(2,5);
jack(4,6)=jac_var(2,6);
jack(5,1)=jac_var(2,1);
jack(5,2)=jac_var(2,2);
jack(5,3)=jac_var(2,3);
jack(5,5)=jac_var(2,5);
jack(5,6)=jac_var(2,6);


% rule    : A() -> A() | ((a * a) * [T]):tt
% reaction: A(x~p?, y~u?) -> A(x~p?, y~u?) | ((a * a) * [T]):tt 

jac(2,1)=jac(2,1)-jack(5,1)*y(2);
jac(2,2)=jac(2,2)-jack(5,2)*y(2);
jac(2,3)=jac(2,3)-jack(5,3)*y(2);
jac(2,5)=jac(2,5)-jack(5,5)*y(2);
jac(2,6)=jac(2,6)-jack(5,6)*y(2);
jac(2,2)=jac(2,2)-k(5);
jac(2,1)=jac(2,1)+jack(5,1)*y(2);
jac(2,2)=jac(2,2)+jack(5,2)*y(2);
jac(2,3)=jac(2,3)+jack(5,3)*y(2);
jac(2,5)=jac(2,5)+jack(5,5)*y(2);
jac(2,6)=jac(2,6)+jack(5,6)*y(2);
jac(2,2)=jac(2,2)+k(5);
jac(4,1)=jac(4,1)+jack(5,1)*y(2)*var(1)*var(1)*t;
jac(4,2)=jac(4,2)+jack(5,2)*y(2)*var(1)*var(1)*t;
jac(4,3)=jac(4,3)+jack(5,3)*y(2)*var(1)*var(1)*t;
jac(4,5)=jac(4,5)+jack(5,5)*y(2)*var(1)*var(1)*t;
jac(4,6)=jac(4,6)+jack(5,6)*y(2)*var(1)*var(1)*t;
jac(4,1)=jac(4,1)+k(5)*y(2)*t*(var(1)*jac_var(1,1)+var(1)*jac_var(1,1));
jac(4,2)=jac(4,2)+k(5)*y(2)*t*(var(1)*jac_var(1,2)+var(1)*jac_var(1,2));
jac(4,3)=jac(4,3)+k(5)*y(2)*t*(var(1)*jac_var(1,3)+var(1)*jac_var(1,3));
jac(4,5)=jac(4,5)+k(5)*y(2)*t*(var(1)*jac_var(1,5)+var(1)*jac_var(1,5));
jac(4,6)=jac(4,6)+k(5)*y(2)*(var(1)*var(1)+t*(var(1)*jac_var(1,6)+var(1)*jac_var(1,6)));
jac(4,2)=jac(4,2)+k(5)*y(2)*var(1)*var(1)*t;

% rule    : A(y~u) -> A(y~p)
% reaction: A(x~p?, y~u?) -> A(x~p?, y~p?) 

jac(2,2)=jac(2,2)-k(3);
jac(5,2)=jac(5,2)+k(3);

% rule    : A(x~p) -> A(x~u)
% reaction: A(x~p?, y~u?) -> A(x~u?, y~u?) 

jac(2,1)=jac(2,1)-jack(2,1)*y(2);
jac(2,2)=jac(2,2)-jack(2,2)*y(2);
jac(2,3)=jac(2,3)-jack(2,3)*y(2);
jac(2,5)=jac(2,5)-jack(2,5)*y(2);
jac(2,2)=jac(2,2)-k(2);
jac(1,1)=jac(1,1)+jack(2,1)*y(2);
jac(1,2)=jac(1,2)+jack(2,2)*y(2);
jac(1,3)=jac(1,3)+jack(2,3)*y(2);
jac(1,5)=jac(1,5)+jack(2,5)*y(2);
jac(1,2)=jac(1,2)+k(2);

% rule    : A() -> A() | ((a * a) * [T]):tt
% reaction: A(x~p?, y~p?) -> A(x~p?, y~p?) | ((a * a) * [T]):tt 

jac(5,1)=jac(5,1)-jack(5,1)*y(5);
jac(5,2)=jac(5,2)-jack(5,2)*y(5);
jac(5,3)=jac(5,3)-jack(5,3)*y(5);
jac(5,5)=jac(5,5)-jack(5,5)*y(5);
jac(5,6)=jac(5,6)-jack(5,6)*y(5);
jac(5,5)=jac(5,5)-k(5);
jac(5,1)=jac(5,1)+jack(5,1)*y(5);
jac(5,2)=jac(5,2)+jack(5,2)*y(5);
jac(5,3)=jac(5,3)+jack(5,3)*y(5);
jac(5,5)=jac(5,5)+jack(5,5)*y(5);
jac(5,6)=jac(5,6)+jack(5,6)*y(5);
jac(5,5)=jac(5,5)+k(5);
jac(4,1)=jac(4,1)+jack(5,1)*y(5)*var(1)*var(1)*t;
jac(4,2)=jac(4,2)+jack(5,2)*y(5)*var(1)*var(1)*t;
jac(4,3)=jac(4,3)+jack(5,3)*y(5)*var(1)*var(1)*t;
jac(4,5)=jac(4,5)+jack(5,5)*y(5)*var(1)*var(1)*t;
jac(4,6)=jac(4,6)+jack(5,6)*y(5)*var(1)*var(1)*t;
jac(4,1)=jac(4,1)+k(5)*y(5)*t*(var(1)*jac_var(1,1)+var(1)*jac_var(1,1));
jac(4,2)=jac(4,2)+k(5)*y(5)*t*(var(1)*jac_var(1,2)+var(1)*jac_var(1,2));
jac(4,3)=jac(4,3)+k(5)*y(5)*t*(var(1)*jac_var(1,3)+var(1)*jac_var(1,3));
jac(4,5)=jac(4,5)+k(5)*y(5)*t*(var(1)*jac_var(1,5)+var(1)*jac_var(1,5));
jac(4,6)=jac(4,6)+k(5)*y(5)*(var(1)*var(1)+t*(var(1)*jac_var(1,6)+var(1)*jac_var(1,6)));
jac(4,5)=jac(4,5)+k(5)*y(5)*var(1)*var(1)*t;

% rule    : A(y~p) -> A(y~u)
% reaction: A(x~p?, y~p?) -> A(x~p?, y~u?) 

jac(5,1)=jac(5,1)-jack(4,1)*y(5);
jac(5,2)=jac(5,2)-jack(4,2)*y(5);
jac(5,3)=jac(5,3)-jack(4,3)*y(5);
jac(5,5)=jac(5,5)-jack(4,5)*y(5);
jac(5,6)=jac(5,6)-jack(4,6)*y(5);
jac(5,5)=jac(5,5)-k(4);
jac(2,1)=jac(2,1)+jack(4,1)*y(5);
jac(2,2)=jac(2,2)+jack(4,2)*y(5);
jac(2,3)=jac(2,3)+jack(4,3)*y(5);
jac(2,5)=jac(2,5)+jack(4,5)*y(5);
jac(2,6)=jac(2,6)+jack(4,6)*y(5);
jac(2,5)=jac(2,5)+k(4);

% rule    : A(x~p) -> A(x~u)
% reaction: A(x~p?, y~p?) -> A(x~u?, y~p?) 

jac(5,1)=jac(5,1)-jack(2,1)*y(5);
jac(5,2)=jac(5,2)-jack(2,2)*y(5);
jac(5,3)=jac(5,3)-jack(2,3)*y(5);
jac(5,5)=jac(5,5)-jack(2,5)*y(5);
jac(5,5)=jac(5,5)-k(2);
jac(3,1)=jac(3,1)+jack(2,1)*y(5);
jac(3,2)=jac(3,2)+jack(2,2)*y(5);
jac(3,3)=jac(3,3)+jack(2,3)*y(5);
jac(3,5)=jac(3,5)+jack(2,5)*y(5);
jac(3,5)=jac(3,5)+k(2);

% rule    : A() -> A() | ((a * a) * [T]):tt
% reaction: A(x~u?, y~p?) -> A(x~u?, y~p?) | ((a * a) * [T]):tt 

jac(3,1)=jac(3,1)-jack(5,1)*y(3);
jac(3,2)=jac(3,2)-jack(5,2)*y(3);
jac(3,3)=jac(3,3)-jack(5,3)*y(3);
jac(3,5)=jac(3,5)-jack(5,5)*y(3);
jac(3,6)=jac(3,6)-jack(5,6)*y(3);
jac(3,3)=jac(3,3)-k(5);
jac(3,1)=jac(3,1)+jack(5,1)*y(3);
jac(3,2)=jac(3,2)+jack(5,2)*y(3);
jac(3,3)=jac(3,3)+jack(5,3)*y(3);
jac(3,5)=jac(3,5)+jack(5,5)*y(3);
jac(3,6)=jac(3,6)+jack(5,6)*y(3);
jac(3,3)=jac(3,3)+k(5);
jac(4,1)=jac(4,1)+jack(5,1)*y(3)*var(1)*var(1)*t;
jac(4,2)=jac(4,2)+jack(5,2)*y(3)*var(1)*var(1)*t;
jac(4,3)=jac(4,3)+jack(5,3)*y(3)*var(1)*var(1)*t;
jac(4,5)=jac(4,5)+jack(5,5)*y(3)*var(1)*var(1)*t;
jac(4,6)=jac(4,6)+jack(5,6)*y(3)*var(1)*var(1)*t;
jac(4,1)=jac(4,1)+k(5)*y(3)*t*(var(1)*jac_var(1,1)+var(1)*jac_var(1,1));
jac(4,2)=jac(4,2)+k(5)*y(3)*t*(var(1)*jac_var(1,2)+var(1)*jac_var(1,2));
jac(4,3)=jac(4,3)+k(5)*y(3)*t*(var(1)*jac_var(1,3)+var(1)*jac_var(1,3));
jac(4,5)=jac(4,5)+k(5)*y(3)*t*(var(1)*jac_var(1,5)+var(1)*jac_var(1,5));
jac(4,6)=jac(4,6)+k(5)*y(3)*(var(1)*var(1)+t*(var(1)*jac_var(1,6)+var(1)*jac_var(1,6)));
jac(4,3)=jac(4,3)+k(5)*y(3)*var(1)*var(1)*t;

% rule    : A(y~p) -> A(y~u)
% reaction: A(x~u?, y~p?) -> A(x~u?, y~u?) 

jac(3,1)=jac(3,1)-jack(4,1)*y(3);
jac(3,2)=jac(3,2)-jack(4,2)*y(3);
jac(3,3)=jac(3,3)-jack(4,3)*y(3);
jac(3,5)=jac(3,5)-jack(4,5)*y(3);
jac(3,6)=jac(3,6)-jack(4,6)*y(3);
jac(3,3)=jac(3,3)-k(4);
jac(1,1)=jac(1,1)+jack(4,1)*y(3);
jac(1,2)=jac(1,2)+jack(4,2)*y(3);
jac(1,3)=jac(1,3)+jack(4,3)*y(3);
jac(1,5)=jac(1,5)+jack(4,5)*y(3);
jac(1,6)=jac(1,6)+jack(4,6)*y(3);
jac(1,3)=jac(1,3)+k(4);

% rule    : A(x~u) -> A(x~p)
% reaction: A(x~u?, y~p?) -> A(x~p?, y~p?) 

jac(3,3)=jac(3,3)-k(1);
jac(5,3)=jac(5,3)+k(1);

% rule    : A() -> A() | ((a * a) * [T]):tt
% reaction: A(x~u?, y~u?) -> A(x~u?, y~u?) | ((a * a) * [T]):tt 

jac(1,1)=jac(1,1)-jack(5,1)*y(1);
jac(1,2)=jac(1,2)-jack(5,2)*y(1);
jac(1,3)=jac(1,3)-jack(5,3)*y(1);
jac(1,5)=jac(1,5)-jack(5,5)*y(1);
jac(1,6)=jac(1,6)-jack(5,6)*y(1);
jac(1,1)=jac(1,1)-k(5);
jac(1,1)=jac(1,1)+jack(5,1)*y(1);
jac(1,2)=jac(1,2)+jack(5,2)*y(1);
jac(1,3)=jac(1,3)+jack(5,3)*y(1);
jac(1,5)=jac(1,5)+jack(5,5)*y(1);
jac(1,6)=jac(1,6)+jack(5,6)*y(1);
jac(1,1)=jac(1,1)+k(5);
jac(4,1)=jac(4,1)+jack(5,1)*y(1)*var(1)*var(1)*t;
jac(4,2)=jac(4,2)+jack(5,2)*y(1)*var(1)*var(1)*t;
jac(4,3)=jac(4,3)+jack(5,3)*y(1)*var(1)*var(1)*t;
jac(4,5)=jac(4,5)+jack(5,5)*y(1)*var(1)*var(1)*t;
jac(4,6)=jac(4,6)+jack(5,6)*y(1)*var(1)*var(1)*t;
jac(4,1)=jac(4,1)+k(5)*y(1)*t*(var(1)*jac_var(1,1)+var(1)*jac_var(1,1));
jac(4,2)=jac(4,2)+k(5)*y(1)*t*(var(1)*jac_var(1,2)+var(1)*jac_var(1,2));
jac(4,3)=jac(4,3)+k(5)*y(1)*t*(var(1)*jac_var(1,3)+var(1)*jac_var(1,3));
jac(4,5)=jac(4,5)+k(5)*y(1)*t*(var(1)*jac_var(1,5)+var(1)*jac_var(1,5));
jac(4,6)=jac(4,6)+k(5)*y(1)*(var(1)*var(1)+t*(var(1)*jac_var(1,6)+var(1)*jac_var(1,6)));
jac(4,1)=jac(4,1)+k(5)*y(1)*var(1)*var(1)*t;

% rule    : A(y~u) -> A(y~p)
% reaction: A(x~u?, y~u?) -> A(x~u?, y~p?) 

jac(1,1)=jac(1,1)-k(3);
jac(3,1)=jac(3,1)+k(3);

% rule    : A(x~u) -> A(x~p)
% reaction: A(x~u?, y~u?) -> A(x~p?, y~u?) 

jac(1,1)=jac(1,1)-k(1);
jac(2,1)=jac(2,1)+k(1);
dydt(6)=1;

end


function obs=ode_obs(y)

global nobs
global var
obs=zeros(nobs,1);

t = y(6);
var(1)=y(1)+y(3)+y(2)+y(5);
var(2)=var(1)*var(1)*var(1)*t;

obs(1)=t;

end


main();
