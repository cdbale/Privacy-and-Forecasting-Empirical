% require Statistics and Machine Learning Toolbox

% generate k-clusters
clear all
clc

%% k set up

k = [3];
% k = 30;
% m = [26:1:35];
m = [26];

%% data input 
data0 = readmatrix("data2_jl.csv");                                        % read the original data
ind = data0(1,2:size(data0,2));                                            % read in the index
data = data0(2:size(data0,1),2:size(data0,2));                             % actual  data in column


%% parameters
% lambda = 0.5;                                                            % for lambda
% t = size(data0,1)-1;                                                       % past publicated data until time t
% n = size(data,1);                                                          % rolling window size                                                                  %  
% i = 1;                                                                     % pattern protection until i
% J = size(data,2);
% X = data(t-n+1:t,:);                                                       % base data

%% K-means

% For generating the k-means

FirstN = 160;                                                              % break down to smaller clusters

for t = 1:size(m,2)
    
    X = data(1:m,:);
    for i = 1: size(k,2)
        
        [idx,C] = kmeans(X',k(i));      
        [GC,GN] = groupcounts(idx);
        [row, col] = find(GC > 150);


        for j = 1:size(row,1)                                                      % extra clusters to lower the # of time series
            GCextra = ceil(GC(row(j))/FirstN);
            idx(find(idx ==row(j))) = 0;
            idx(find(idx ==0,FirstN)) = row(j);
            for l = 1:GCextra-2
                idx(find(idx ==0,FirstN)) = max(idx)+1;
            end
            idx(find(idx ==0)) = max(idx)+1;
        end

        [GCnew,GNnew] = groupcounts(idx);
%         str = strcat('k=',num2str(GNnew(end)),'_index.mat'); 
        str = strcat('k=',num2str(GNnew(end)),'_m=',num2str(m(t)),'_index.mat'); 
        save(str,'idx');
    end
end
