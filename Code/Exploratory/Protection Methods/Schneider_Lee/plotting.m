% plotting
clear all
clc

k = [7:2:29];                                                             
lambda = [0.1:0.2:0.9];  

w = zeros(size(k,2),size(lambda,2));                                       % without lambda
f = zeros(size(k,2),size(lambda,2));

f1 = f;
w1 = w;

for t = 1: size(lambda,2)
    
    for m = 1: size(k,2)
        str = strcat('w_f_k=',num2str(k(m)),'_lambda=',num2str(lambda(t)),'_index.txt'); 
        Sol = load(str);
        w(m,t) = Sol(1);                                                   
        f(m,t) = Sol(2);                                                   
        w1(m,t) = Sol(1)*lambda(t);                                        % with lambda    
        f1(m,t) = Sol(2)*(1-lambda(t));
    
    end
end

p1 = w./f;

figure(1)
hold on

% plot(k,p1(:,1),'color', 'b','linewidth',1,'LineStyle', '-' )
% plot(k,p1(:,2),'color', 'b','linewidth',1,'LineStyle', '-' )
% plot(k,p1(:,3),'color', 'b','linewidth',1,'LineStyle', '-' )
% plot(k,p1(:,4),'color', 'b','linewidth',1,'LineStyle', '-' )
% plot(k,p1(:,5),'color', 'b','linewidth',1,'LineStyle', '-' )

dif=fliplr(p1);
diff2=[dif(:,1)   diff(dif,1,2 )];

area(k,diff2 )
% newcolors = [0.0000 0.3262 0.3015; 0.2556 0.3032 0.4201;0.5083 0.5005 0.4732;0.6518 0.6545 0.5307;1.0000 0.9169 0.2731];
c = bone(8);
cc = c(3:7,:);

% c = bone(14);
% cc = c(4:2:12,:);

newcolors = cc;
colororder(newcolors)


xlim([7 29])
% xlabel('k');
% ylabel('$\frac{\text{Intruders utility}{\text{forecasting loss}}$','Interpreter','latex');

%%


plot(k, w)

%%

plot(k,f)

%%

Y = [1 5 3; 3 2 7; 1 5 3; 2 6 1];
area(Y)
%% find multiplier

s = w+f;
s1 = w1+f1;                                                                % with lambda

p = w./f;
p1 = w1./f1;                                                               % with lambda

x = lambda;

figure(1)
hold on

plot(x,s,'color', 'b','linewidth',1,'LineStyle', '-' )
plot(x,s1,'color', 'r','linewidth',1,'LineStyle', '-' )
legend('intruders utility plus forecasting loss','objective value (with lambda weight)')

%% intruder's utility over forecast loss


p = w./f;
p1 = w1./f1;

x = lambda;

figure(1)
hold on

plot(x,p,'color', 'b','linewidth',1,'LineStyle', '-' )
plot(x,p1,'color', 'r','linewidth',1,'LineStyle', '-' )
legend('intruder utility over forecasting loss', 'intruder utility over forecasting loss (with lambda weight)')


%% forecast loss over intruder's utility

p2 = f./w;
p3 = f1./w1;

figure(1)
hold on
plot(x,p2,'color', 'b','linewidth',1,'LineStyle', '-' )
plot(x,p3,'color', 'r','linewidth',1,'LineStyle', '-' )

legend('forecasting loss over intruder utility', 'forecasting loss over intruder utility (with lambda weight)')


%% 

figure(1)
hold on
plot(x,p2,'color', 'r','linewidth',1,'LineStyle', '-' )
plot(x,p,'color', 'b','linewidth',1,'LineStyle', '-' )
legend('forecasting loss over intruder utility', 'intruder utility over forecasting loss')


%% data shuffle index

