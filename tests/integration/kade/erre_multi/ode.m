function main=main()
%% command line: 
%%     'KaDE' 'erre.ka' '-p' '10' '-t' '1''--debug'
%% THINGS THAT ARE KNOWN FROM KAPPA FILE AND KaSim OPTIONS;
%% 
%% init - the initial abundances of each species and token
%% tinit - the initial simulation time (likely 0)
%% tend - the final simulation time 
%% initialstep - initial time step at the beginning of numerical integration
%% num_t_point - the number of time points to return


tinit=0.000000;
tend=1.000000;
initialstep=0.000001;
num_t_point=10;

global nodevar
nodevar=8;
nvar=6;
nobs=2;
nrules=8;

global var
var=zeros(nvar,1);
global init
init=sparse(nodevar,1);

t = 0.000000;

init(8)=t;
init(1)=6;
init(2)=4.500000;
init(4)=10;
init(3)=100;
var(2)=(0+(1*(init(3)+init(5))));
var(1)=((0+(1*(2*init(6))))/2);

global k
global kd
global kun
global kdun

k(1)=1;
k(2)=2;
k(3)=3;
k(4)=4;
k(5)=5;
k(6)=6;
k(7)=12;
k(8)=13;

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

t = linspace(tinit, tend, num_t_point+1);
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


filename = 'data.out';
fid = fopen (filename,'w');
fprintf(fid,'# KaDE erre.ka -p 10 -t 1 --debug\n')
fprintf(fid,'# t')
fprintf(fid,' |dimmers|')
fprintf(fid,' |monomers|')
fprintf(fid,'\n')
for j=1:num_t_point+1
    fprintf(fid,'%f',t(j));
    for i=1:nobs
        fprintf(fid,' %f',y(j,i));
    end
    fprintf(fid,'\n');
end
fclose(fid);


end



function dydt=ode_aux(t,y)

global nodevar
global var
global k
global kd
global kun
global kdun

var(2)=(0+(1*(y(3)+y(5))));
var(1)=((0+(1*(2*y(6))))/2);


dydt=zeros(nodevar,1);
dydt(7)=dydt(7)-k(8)*y(7);
dydt(1)=dydt(1)+k(8)*y(7);
dydt(2)=dydt(2)+k(8)*y(7);
dydt(2)=dydt(2)-k(7)*y(2)*y(1);
dydt(1)=dydt(1)-k(7)*y(2)*y(1);
dydt(7)=dydt(7)+k(7)*y(2)*y(1);
dydt(6)=dydt(6)-2/2*k(5)*y(6);
dydt(5)=dydt(5)+1/2*k(5)*y(6);
dydt(5)=dydt(5)+1/2*k(5)*y(6);
dydt(6)=dydt(6)-2/2*k(5)*y(6);
dydt(5)=dydt(5)+1/2*k(5)*y(6);
dydt(5)=dydt(5)+1/2*k(5)*y(6);
dydt(6)=dydt(6)-2/2*k(4)*y(6);
dydt(5)=dydt(5)+1/2*k(4)*y(6);
dydt(4)=dydt(4)+1/2*k(4)*y(6);
dydt(6)=dydt(6)-2/2*k(4)*y(6);
dydt(4)=dydt(4)+1/2*k(4)*y(6);
dydt(5)=dydt(5)+1/2*k(4)*y(6);
dydt(5)=dydt(5)-1/2*k(3)*y(5)*y(5);
dydt(5)=dydt(5)-1/2*k(3)*y(5)*y(5);
dydt(6)=dydt(6)+2/2*k(3)*y(5)*y(5);
dydt(5)=dydt(5)-k(2)*y(5);
dydt(3)=dydt(3)+k(2)*y(5);
dydt(4)=dydt(4)+k(2)*y(5);
dydt(4)=dydt(4)-k(1)*y(4)*y(3);
dydt(3)=dydt(3)-k(1)*y(4)*y(3);
dydt(5)=dydt(5)+k(1)*y(4)*y(3);
dydt(3)=dydt(3)+k(6);
dydt(8)=1;

end


function Init=ode_init()

global nodevar
global init
Init=zeros(nodevar,1);

Init(1) = init(1);
Init(2) = init(2);
Init(3) = init(3);
Init(4) = init(4);
Init(5) = init(5);
Init(6) = init(6);
Init(7) = init(7);
Init(8) = init(8);
end


function obs=ode_obs(y)

global nobs
global var
obs = zeros(nobs,1);

t = y(8);
var(2)=(0+(1*(y(3)+y(5))));
var(1)=((0+(1*(2*y(6))))/2);

obs(1)=var(1);
obs(2)=var(2);

end


main();
