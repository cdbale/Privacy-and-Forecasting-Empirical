% with clustering index as inputs


clc
clear all
%% 
trans = 1.1;                                                               % x(end) = x(end)*trans when only 1 time series in a cluster
largeN = 1e+5;
k = [7];                                                                   % # of clusters (clustering on only 25 rows)           %%%%%%%
                                                                           % 21 not good

lambda = [0.9];                                                    % for lambda  

%% inputs
data0 = readmatrix("data2_jl.csv");                                        % read the original data
ind = data0(1,2:size(data0,2));                                            % read in the index
data = data0(1:size(data0,1),2:size(data0,2));                             % actual  data in column with index   


%% parameters

T = 25;                                                                    % past publicized data until time t  
                                                                                       
% n = size(data,1);                                                        % rolling window size                                                        
i = T+1;                                                                   % pattern protection until i
J = size(data,1)-1;                                                        % pattern protection until i

%% Output
                                                                           % Output csv file
w = 0;
f = 0;

%%
for t = 1: size(lambda,2)
     
    for i = 1:size(k,2)
                
        str = strcat('k=',num2str(k(i)),'_m=26_index.mat'); 
        load(str);                                                         % load the clustering index
        [GC,GN] = groupcounts(idx);
        nj = GC;                                                           % number of time series data in each cluster
        dataO = [];     

        w = zeros(k(i),1);                                                 % each cluster has w and f
        f = zeros(k(i),1);
        ne = 1e5;                                                          % for cluster with odd number of time series data
   
        for j = 1:GN(end)                                                  % seperate data into each cluster
            [row,col] = find(idx == j);
            dataX = data(:,row);                                           % dataX include the data in cluster k(i)
                
            if nj(j) == 1                                                  % cluster with only one time series data
               dataY = dataX(1:T+1,:);
               for m = T+1:J
                   sig = 0.1*std(dataX(2:m+1,:));
                   noise = (rand(1)*2-1)*sig;
                   dataS = [dataX(m+1,:) + noise; dataX(1,:)];            % first row is the protected value, second row is the index for the swapped value      
%                    dataS = [dataX(m+1,:)*trans; dataX(1,:)];
                   dataY = [dataY;dataS];
                   ww = 0;                                                 % do not record, place holder
                   ff = 0;
               end
            
            elseif mod(nj(j),2) == 0

                    [dataS,ww,ff] = evenfunc(dataX, J, nj(j), lambda(t), largeN, T);
                    dataY = [dataX(1:T+1,:);dataS];
            else
                    ysol = [ne;mean(dataX(2:end,:),2)];                    % find the centroid     
                    dataX=[dataX,ysol];
                    ne = ne + 1;
                    [dataS,ww,ff] = evenfunc(dataX, J, nj(j)+1, lambda(t), largeN, T);
                    dataY = [dataX(1:T+1,:);dataS];
            end      
            w(j) = ww;
            f(j) = ff;
            dataO = [dataO, dataY];

        end 
            
        Solution1 = sortrows(dataO')';
        w(find(w==0))=[];
        w = mean(w);
        f(find(f==0))=[];
        f = mean(f);
        Solution2 =[w,f];

        str1 = strcat('solution_k=',num2str(GN(end)),'_lambda=',num2str(lambda(t)),'_index.txt'); 
        str2 = strcat('w_f_k=',num2str(GN(end)),'_lambda=',num2str(lambda(t)),'_index.txt'); 
        writematrix(Solution1,str1,'Delimiter','tab')
        writematrix(Solution2,str2,'Delimiter','tab')    
            
    end
    

end


function [dataS,w,f] = evenfunc(dataX, J, nj, lambda, largeN, T)

dataS = [];
    for m = T+1:J
        tic
        [s1,s2,ww,ff] = bigraphe(dataX(1:m+1,:), J, nj, lambda, largeN,m);   % cautious about the index
        toc
        dataS = [dataS;dataX(m+1,s1);dataX(1,s1)];
    end
    w = ww;
    f = ff;
end

