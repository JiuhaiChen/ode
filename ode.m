clear all
close all
clc
addpath('./utils');

    
%% generate Data
polyorder = 5;  % search space up to fifth order polynomials
usesine = 0;    % no trig functions
eps = .2; 
n = 2;          % 2D system
A = [-.5 2; -2 -.5];  % dynamics
rhs = @(x)A*x;   % ODE right hand side
tspan=.01:.01:30;   % time span
pool_size = size(tspan,2);
x0 = [2; 0];        % initial conditions
options = odeset('RelTol',1e-10,'AbsTol',1e-10*ones(1,n));
[t,xs]=ode45(@(t,x)rhs(x),tspan,x0,options);  % integrate
Base = {'1' 'y_1' 'y_2' 'y_1^2' 'y_1y_2' 'y_2^2' 'y_1^3' 'y_1^2y_2' 'y_1y_2^2' 'y_2^3'...
    'y_1^4' 'y_1^3y_2' 'y_1^2y_2^2' 'y_1y_2^3' 'y_2^4' 'y_1^5' 'y_1^4y_2' 'y_1^3y_2^2' 'y_1^2y_2^3' 'y_1y_2^4' 'y_2^5'};
Theta_true = poolData(xs,n,polyorder,usesine);
[m_x,m_y] = size(Theta_true);

Xi_true = zeros(m_y,2);
Xi_true(2,1) = -.5;
Xi_true(3,1) = 2;
Xi_true(2,2) = -2;
Xi_true(3,2) = -.5;

Xi_log = zeros(m_y+1,2);
Xi_log(2,1) = 1;
Xi_log(3,1) = 1;
Xi_log(2,2) = 1;
Xi_log(3,2) = 1;


time = 50;
error = zeros(time,1);
error_l0 = zeros(time,1);
sample = zeros(time,1);
ns_max = 6;

for times=1:time
    chosen_col_last = ones(22,2);
    
    %% compute Derivative 
    dxs = xs*A';
    dxs = dxs + eps*randn(size(dxs));  % add noise
    
    %% initial design
    k = 16;         %initial data size
    n_s = 16;      %optimal design size
    chosen_index = randi([1,pool_size],1,k);
    time = chosen_index'/100;
    x = xs(chosen_index,:);
    s = 0;
    
    %% squential experiment 
    while(1)
        s = s+1;
        %% Gaussian process
        [mu,~,~,sigma1] = gp_new(time,x(:,1));
        y_predict(:,1) = mu(tspan');
        sigma1 = sigma1/std(x(:,1))^2;
        [mu,~,~,sigma2] = gp_new(time,x(:,2));
        y_predict(:,2) = mu(tspan');
        sigma2 = sigma2/std(x(:,2))^2;
        sigma = (sigma1+sigma2)/2;

        %% pool Data  (i.e., build library of nonlinear time series)
        Theta_predict = poolData(y_predict,n,polyorder,usesine);
        
         %% pool Data  (i.e., build library of nonlinear time series)
        Theta_now = poolData(x,n,polyorder,usesine);
        dx = dxs(chosen_index,:);

        %% compute Sparse regression
        mdl = stepwiselm(Theta_now,dx(:,1),'Criterion','bic');
        chosen_col_1 = mdl.Formula.InModel;
        Theta = Theta_now(:,chosen_col_1);
        mdl = fitlm(Theta,dx(:,1));
        tol_1 = mdl.RMSE;
        tol_1 = tol_1/std(dx(:,1));
        cof_1 = table2array(mdl.Coefficients(2:end,1));

        mdl = stepwiselm(Theta_now,dx(:,2),'Criterion','bic');
        chosen_col_2 = mdl.Formula.InModel;
        Theta = Theta_now(:,chosen_col_2);
        mdl = fitlm(Theta,dx(:,2));
        tol_2 = mdl.RMSE;
        tol_2 = tol_2/std(dx(:,2));
        cof_2 = table2array(mdl.Coefficients(2:end,1));

        chosen_col = [double(chosen_col_1)' double(chosen_col_2)'];
        cof = [cof_1 ; cof_2];
        tol = (tol_1+tol_2)/2;
        
        error_1 = calError(chosen_col,chosen_col_last);
        
        if  error_1 == 0
            error_2 = norm(cof - cof_last,2)/norm(cof_last,2);
            if error_2 < .1e-9
                break
            end
        end
        chosen_col_last = chosen_col;
        cof_last = cof;
        
        %% max sample size
        if s > ns_max
            break
        end

        %% ACDS
       
        [chosen_index] = optimal_design(Theta_true,Theta_predict,chosen_index,n_s,sigma,tol,tspan');
        time = chosen_index'/100;
        x = xs(chosen_index,:);
       

    end
    z1 = zeros(m_y,1);
    z1(chosen_col_1) = cof_1;
    z2 = zeros(m_y,1);
    z2(chosen_col_2) = cof_2;
    Xi = [z1 z2];
    error(times,1) = calError(chosen_col,Xi_log);
    error_l0(times,1) = norm(Xi-Xi_true,2);
    sample(times,1) = size(chosen_index,2);

    
   
end


dlmwrite('ode2.txt',error);
dlmwrite('ode2.txt',error_l0,'-append');
dlmwrite('ode2.txt',sample,'-append');

dlmwrite('data2.txt',mean(error));
dlmwrite('data2.txt',std(error),'-append');
dlmwrite('data2.txt',mean(error_l0),'-append');
dlmwrite('data2.txt',std(error_l0),'-append');
dlmwrite('data2.txt',mean(sample),'-append');
dlmwrite('data2.txt',std(sample),'-append');
