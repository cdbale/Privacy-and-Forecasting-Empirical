%% Optimization function bigraph for even number of points

% input data with index
% use the upper triangle nh*(nh-1)/2 variables
% reshape(x,nh,nh)

function [ind1,ind2,intruder,loss] = bigraphe(dataX, J, nh, lambda, largeN, m)
% intruder's utility, kernel estimation with Gaussian kernel

x0 = ones(nh*(nh-1)/2,1);
indx = dataX(1,:);                                                         % index for each time series data
dataX(1,:) = [];                                                           % data without index

%% intruder's utility
pool = dataX(m,:);                                                         % value at time i (for shuffling)


w = zeros(1,nh*(nh-1)/2);
ww = cell(1,nh);

for i = 1:nh
    pool1 = pool;
    pool1(i) = [];
    [fcaret,xi] = ksdensity(dataX(1:m,i),pool1);                         % kernel smoothing function estimate 
    fcaret(find(fcaret<1e-5)) = 1/largeN;                                  % has to make assumption here because we haven't do cluster                                                                         % extreme value exists here (->inf)
                                                                     
    w1 = sqrt(1./fcaret);                                                  % use uth-root (<10: 2; 10-100: 3; >100 :4) 

    
    w1 = [w1(1:i-1),1,w1(i:end)]';                                         % make w(j,j) = 1, for line 35
    ww(1,i) = {w1};    
end

ww = cell2mat(ww);
ww = ww./max(ww(:));
ww = ww + ww';

tt = 1;
for i = 1:nh-1
    for j = i+1:nh      
        w(tt) = ww(i,j);
        tt = tt +1;
    end
end


%% f: forcast loss: only focus on the last row

ff = [];

for i = 1: nh-1
    for j = i+1:nh
    ff = [ff,abs(dataX(end,i)-dataX(end,j))];
    end
end

ff = ff./max(ff(:));
ff = 2.*ff;

%% objective function 


f = ff.*(1-lambda)+w.*lambda;
f = f';
%% constraints


Aeq = zeros(nh,nh*(nh-1)/2);
aa = 1;
for i = 1:nh-1
    bb = nh-i;
    Aeq(i,aa:aa+bb-1) = ones(1,bb);
    Aeq(i+1:nh-1,aa:aa+bb-2)=eye(nh-i-1);
    aa = aa+bb;
end    

aa = 0;
for i = 1:nh-2
    aa = aa + nh-i;
    Aeq(nh,aa) = 1;
end                                                                        % last row

Aeq(end,end) = 1;                                                         

beq = ones(nh,1);                                                          % only switch once                                              


lb = zeros(nh*(nh-1)/2,1);
ub = ones(nh*(nh-1)/2,1);
intcon = [1:nh*(nh-1)/2];

%% ILP
options = optimoptions('intlinprog','Display','off');
x = intlinprog(f,intcon,[],[],Aeq,beq,lb,ub,x0,options);
XX = zeros(nh,nh);
y = x;

for i = 1:nh-1
    XX(i,i+1:nh) = y(1:nh-i)';
    y(1:nh-i)=[];
end

XX = XX + XX';

[ind1,ind2] = find(reshape(XX,nh,nh)>0.5);                                 % shuffling pair node 2, set 0.5 for precision issue
intruder = w*x;                                                    % intruder's utility
loss = ff*x;                                                   % prediction loss

end