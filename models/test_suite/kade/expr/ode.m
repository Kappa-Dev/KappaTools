function main=main()
% command line: 
%      'KaDE' 'expr.ka' '--compute-jacobian''true'
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
nodevar=5;
nvar=3;
nobs=1;
nrules=4;

global var
var=zeros(nvar,1);
global init
init=sparse(nodevar,1);

t = 0.000000;

init(5)=t;
init(1)=10; % A(x~u?, y~u?)
var(1)=init(1)+init(3)+init(2)+init(4);
var(2)=var(1)*var(1)*var(1);

global k
global kd
global kun
global kdun

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
                 'MaxStep', tend);


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
fprintf(fid,'# KaDE expr.ka --compute-jacobian true\n')
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
Init(4) = init(4); % A(x~p?, y~p?)
Init(5) = init(5); % t
end


function dydt=ode_aux(t,y)

global nodevar
global var
global k
global kd
global kun
global kdun

var(1)=y(1)+y(3)+y(2)+y(4);
var(2)=var(1)*var(1)*var(1);

k(2)=var(1);
k(4)=var(2);

dydt=zeros(nodevar,1);

% rule    : A(y~u) -> A(y~p)
% reaction: A(x~p?, y~u?) -> A(x~p?, y~p?) 

dydt(2)=dydt(2)-k(3)*y(2);
dydt(4)=dydt(4)+k(3)*y(2);

% rule    : A(x~p) -> A(x~u)
% reaction: A(x~p?, y~u?) -> A(x~u?, y~u?) 

dydt(2)=dydt(2)-k(2)*y(2);
dydt(1)=dydt(1)+k(2)*y(2);

% rule    : A(y~p) -> A(y~u)
% reaction: A(x~p?, y~p?) -> A(x~p?, y~u?) 

dydt(4)=dydt(4)-k(4)*y(4);
dydt(2)=dydt(2)+k(4)*y(4);

% rule    : A(x~p) -> A(x~u)
% reaction: A(x~p?, y~p?) -> A(x~u?, y~p?) 

dydt(4)=dydt(4)-k(2)*y(4);
dydt(3)=dydt(3)+k(2)*y(4);

% rule    : A(y~p) -> A(y~u)
% reaction: A(x~u?, y~p?) -> A(x~u?, y~u?) 

dydt(3)=dydt(3)-k(4)*y(3);
dydt(1)=dydt(1)+k(4)*y(3);

% rule    : A(x~u) -> A(x~p)
% reaction: A(x~u?, y~p?) -> A(x~p?, y~p?) 

dydt(3)=dydt(3)-k(1)*y(3);
dydt(4)=dydt(4)+k(1)*y(3);

% rule    : A(y~u) -> A(y~p)
% reaction: A(x~u?, y~u?) -> A(x~u?, y~p?) 

dydt(1)=dydt(1)-k(3)*y(1);
dydt(3)=dydt(3)+k(3)*y(1);

% rule    : A(x~u) -> A(x~p)
% reaction: A(x~u?, y~u?) -> A(x~p?, y~u?) 

dydt(1)=dydt(1)-k(1)*y(1);
dydt(2)=dydt(2)+k(1)*y(1);
dydt(5)=1;

end


function jacobian=ode_jac_aux(t,jac)

global jacvar
global var
global k
global kd
global kun
global kdun

var(1)=y(1)+y(3)+y(2)+y(4);
var(2)=var(1)*var(1)*var(1);

k(2)=var(1);
k(4)=var(2);
global k
global kd
global kun
global kdun

jacvar(1,1)=1;
jacvar(1,2)=1;
jacvar(1,3)=1;
jacvar(1,4)=1;
jacvar(2,1)=var(1)*var(1)*jac_var(1,1)+var(1)*(var(1)*jac_var(1,1)+var(1)*jac_var(1,1));
jacvar(2,2)=var(1)*var(1)*jac_var(1,2)+var(1)*(var(1)*jac_var(1,2)+var(1)*jac_var(1,2));
jacvar(2,3)=var(1)*var(1)*jac_var(1,3)+var(1)*(var(1)*jac_var(1,3)+var(1)*jac_var(1,3));
jacvar(2,4)=var(1)*var(1)*jac_var(1,4)+var(1)*(var(1)*jac_var(1,4)+var(1)*jac_var(1,4));

jack(2,1)=jac_var(1,1);
jack(2,2)=jac_var(1,2);
jack(2,3)=jac_var(1,3);
jack(2,4)=jac_var(1,4);
jack(4,1)=jac_var(2,1);
jack(4,2)=jac_var(2,2);
jack(4,3)=jac_var(2,3);
jack(4,4)=jac_var(2,4);

dydt=zeros(nodevar,1);

% rule    : A(y~u) -> A(y~p)
% reaction: A(x~p?, y~u?) -> A(x~p?, y~p?) 

dydt(2)=dydt(2)-k(3)*y(2);
dydt(4)=dydt(4)+k(3)*y(2);

% rule    : A(x~p) -> A(x~u)
% reaction: A(x~p?, y~u?) -> A(x~u?, y~u?) 

dydt(2)=dydt(2)-k(2)*y(2);
dydt(1)=dydt(1)+k(2)*y(2);

% rule    : A(y~p) -> A(y~u)
% reaction: A(x~p?, y~p?) -> A(x~p?, y~u?) 

dydt(4)=dydt(4)-k(4)*y(4);
dydt(2)=dydt(2)+k(4)*y(4);

% rule    : A(x~p) -> A(x~u)
% reaction: A(x~p?, y~p?) -> A(x~u?, y~p?) 

dydt(4)=dydt(4)-k(2)*y(4);
dydt(3)=dydt(3)+k(2)*y(4);

% rule    : A(y~p) -> A(y~u)
% reaction: A(x~u?, y~p?) -> A(x~u?, y~u?) 

dydt(3)=dydt(3)-k(4)*y(3);
dydt(1)=dydt(1)+k(4)*y(3);

% rule    : A(x~u) -> A(x~p)
% reaction: A(x~u?, y~p?) -> A(x~p?, y~p?) 

dydt(3)=dydt(3)-k(1)*y(3);
dydt(4)=dydt(4)+k(1)*y(3);

% rule    : A(y~u) -> A(y~p)
% reaction: A(x~u?, y~u?) -> A(x~u?, y~p?) 

dydt(1)=dydt(1)-k(3)*y(1);
dydt(3)=dydt(3)+k(3)*y(1);

% rule    : A(x~u) -> A(x~p)
% reaction: A(x~u?, y~u?) -> A(x~p?, y~u?) 

dydt(1)=dydt(1)-k(1)*y(1);
dydt(2)=dydt(2)+k(1)*y(1);
dydt(5)=1;

end


function obs=ode_obs(y)

global nobs
global var
obs=zeros(nobs,1);

t = y(5);
var(1)=y(1)+y(3)+y(2)+y(4);
var(2)=var(1)*var(1)*var(1);

obs(1)=t;

end


main();
