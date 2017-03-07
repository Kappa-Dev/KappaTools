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
nodevar=2;
nvar=1;
nobs=1;
nrules=8;

global var
var=zeros(nvar,1);
global init
init=sparse(nodevar,1);

t = 0.000000;

init(2)=t;
init(1)=(1*10); % A(x~p?, y~p?, w~p?, t~p?)

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

Init(1) = init(1); % A(x~p?, y~p?, w~p?, t~p?)
Init(2) = init(2); % t
end


function dydt=ode_aux(t,y)

global nodevar
global var
global k
global kd
global kun
global kdun



dydt=zeros(nodevar,1);
dydt(2)=1;

end


function obs=ode_obs(y)

global nobs
global var
obs = zeros(nobs,1);

t = y(2);

obs(1)=t;

end


main();
