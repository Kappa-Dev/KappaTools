function main=main()
% command line: 
%      'KaDE' '--with-symmetries' 'true''sym3.ka'
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
nodevar=10;
nvar=1;
nobs=1;
nrules=8;

global var
var=zeros(nvar,1);
global init
init=sparse(nodevar,1);

t = 0.000000;

init(10)=t;
init(1)=(1*10); % A(x~u?, y~u?, w~u?, t~u?)

global k
global kd
global kun
global kdun

k(1)=1; % A(x~u,y~u) -> A(x~p,y~u)
k(2)=1; % A(x~u,y~u) -> A(x~u,y~p)
k(3)=2; % A(x~u,y~p) -> A(x~p,y~p)
k(4)=2; % A(x~p,y~u) -> A(x~p,y~p)
k(5)=1; % A(w~u,t~u) -> A(w~p,t~u)
k(6)=1; % A(w~u,t~u) -> A(w~u,t~p)
k(7)=2; % A(w~u,t~p) -> A(w~p,t~p)
k(8)=2; % A(w~p,t~u) -> A(w~p,t~p)

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
fprintf(fid,'# KaDE --with-symmetries true sym3.ka\n')
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

Init(1) = init(1); % A(x~u?, y~u?, w~u?, t~u?)
Init(2) = init(2); % A(x~u?, y~p?, w~u?, t~u?)
Init(3) = init(3); % A(x~u?, y~u?, w~p?, t~u?)
Init(4) = init(4); % A(x~u?, y~p?, w~p?, t~u?)
Init(5) = init(5); % A(x~u?, y~u?, w~p?, t~p?)
Init(6) = init(6); % A(x~u?, y~p?, w~p?, t~p?)
Init(7) = init(7); % A(x~p?, y~p?, w~p?, t~p?)
Init(8) = init(8); % A(x~p?, y~p?, w~p?, t~u?)
Init(9) = init(9); % A(x~p?, y~p?, w~u?, t~u?)
Init(10) = init(10); % t
end


function dydt=ode_aux(t,y)

global nodevar
global var
global k
global kd
global kun
global kdun



dydt=zeros(nodevar,1);

% rule    : A(w~u,t~u) -> A(w~u,t~p)
% reaction: A(x~p?, y~p?, w~u?, t~u?) -> A(x~p?, y~p?, w~p?, t~u?) 

dydt(9)=dydt(9)-k(6)*y(9);
dydt(8)=dydt(8)+k(6)*y(9);

% rule    : A(w~u,t~u) -> A(w~p,t~u)
% reaction: A(x~p?, y~p?, w~u?, t~u?) -> A(x~p?, y~p?, w~p?, t~u?) 

dydt(9)=dydt(9)-k(5)*y(9);
dydt(8)=dydt(8)+k(5)*y(9);

% rule    : A(w~u,t~u) -> A(w~u,t~p)
% reaction: A(x~u?, y~p?, w~u?, t~u?) -> A(x~u?, y~p?, w~p?, t~u?) 

dydt(2)=dydt(2)-k(6)*y(2);
dydt(4)=dydt(4)+k(6)*y(2);

% rule    : A(w~u,t~u) -> A(w~p,t~u)
% reaction: A(x~u?, y~p?, w~u?, t~u?) -> A(x~u?, y~p?, w~p?, t~u?) 

dydt(2)=dydt(2)-k(5)*y(2);
dydt(4)=dydt(4)+k(5)*y(2);

% rule    : A(x~u,y~p) -> A(x~p,y~p)
% reaction: A(x~u?, y~p?, w~u?, t~u?) -> A(x~p?, y~p?, w~u?, t~u?) 

dydt(2)=dydt(2)-k(3)*y(2);
dydt(9)=dydt(9)+k(3)*y(2);

% rule    : A(w~p,t~u) -> A(w~p,t~p)
% reaction: A(x~p?, y~p?, w~p?, t~u?) -> A(x~p?, y~p?, w~p?, t~p?) 

dydt(8)=dydt(8)-k(8)*y(8);
dydt(7)=dydt(7)+k(8)*y(8);

% rule    : A(w~p,t~u) -> A(w~p,t~p)
% reaction: A(x~u?, y~p?, w~p?, t~u?) -> A(x~u?, y~p?, w~p?, t~p?) 

dydt(4)=dydt(4)-k(8)*y(4);
dydt(6)=dydt(6)+k(8)*y(4);

% rule    : A(x~u,y~p) -> A(x~p,y~p)
% reaction: A(x~u?, y~p?, w~p?, t~u?) -> A(x~p?, y~p?, w~p?, t~u?) 

dydt(4)=dydt(4)-k(3)*y(4);
dydt(8)=dydt(8)+k(3)*y(4);

% rule    : A(x~u,y~p) -> A(x~p,y~p)
% reaction: A(x~u?, y~p?, w~p?, t~p?) -> A(x~p?, y~p?, w~p?, t~p?) 

dydt(6)=dydt(6)-k(3)*y(6);
dydt(7)=dydt(7)+k(3)*y(6);

% rule    : A(x~u,y~u) -> A(x~u,y~p)
% reaction: A(x~u?, y~u?, w~p?, t~p?) -> A(x~u?, y~p?, w~p?, t~p?) 

dydt(5)=dydt(5)-k(2)*y(5);
dydt(6)=dydt(6)+k(2)*y(5);

% rule    : A(x~u,y~u) -> A(x~p,y~u)
% reaction: A(x~u?, y~u?, w~p?, t~p?) -> A(x~u?, y~p?, w~p?, t~p?) 

dydt(5)=dydt(5)-k(1)*y(5);
dydt(6)=dydt(6)+k(1)*y(5);

% rule    : A(w~p,t~u) -> A(w~p,t~p)
% reaction: A(x~u?, y~u?, w~p?, t~u?) -> A(x~u?, y~u?, w~p?, t~p?) 

dydt(3)=dydt(3)-k(8)*y(3);
dydt(5)=dydt(5)+k(8)*y(3);

% rule    : A(x~u,y~u) -> A(x~u,y~p)
% reaction: A(x~u?, y~u?, w~p?, t~u?) -> A(x~u?, y~p?, w~p?, t~u?) 

dydt(3)=dydt(3)-k(2)*y(3);
dydt(4)=dydt(4)+k(2)*y(3);

% rule    : A(x~u,y~u) -> A(x~p,y~u)
% reaction: A(x~u?, y~u?, w~p?, t~u?) -> A(x~u?, y~p?, w~p?, t~u?) 

dydt(3)=dydt(3)-k(1)*y(3);
dydt(4)=dydt(4)+k(1)*y(3);

% rule    : A(w~u,t~u) -> A(w~u,t~p)
% reaction: A(x~u?, y~u?, w~u?, t~u?) -> A(x~u?, y~u?, w~p?, t~u?) 

dydt(1)=dydt(1)-k(6)*y(1);
dydt(3)=dydt(3)+k(6)*y(1);

% rule    : A(w~u,t~u) -> A(w~p,t~u)
% reaction: A(x~u?, y~u?, w~u?, t~u?) -> A(x~u?, y~u?, w~p?, t~u?) 

dydt(1)=dydt(1)-k(5)*y(1);
dydt(3)=dydt(3)+k(5)*y(1);

% rule    : A(x~u,y~u) -> A(x~u,y~p)
% reaction: A(x~u?, y~u?, w~u?, t~u?) -> A(x~u?, y~p?, w~u?, t~u?) 

dydt(1)=dydt(1)-k(2)*y(1);
dydt(2)=dydt(2)+k(2)*y(1);

% rule    : A(x~u,y~u) -> A(x~p,y~u)
% reaction: A(x~u?, y~u?, w~u?, t~u?) -> A(x~u?, y~p?, w~u?, t~u?) 

dydt(1)=dydt(1)-k(1)*y(1);
dydt(2)=dydt(2)+k(1)*y(1);
dydt(10)=1;

end


function obs=ode_obs(y)

global nobs
global var
obs = zeros(nobs,1);

t = y(10);

obs(1)=t;

end


main();
