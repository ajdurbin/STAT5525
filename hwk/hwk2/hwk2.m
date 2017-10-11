% load data
load spam.data.txt
X = spam_data(:, 1:57);
Y = spam_data(:, 58);

%% single run
% classification tree
t = classregtree(X, Y, 'method', 'classification');
% view(t);
% total nodes in tree
numnodes(t);

PredVals = eval(t,X);
PredVals = str2num(cell2mat(PredVals));
pct = mean(abs(PredVals - Y));

folds = 10;
indices = cvpartition(length(Y), 'k', folds);
testset = test(indices,1); trainset = ~testset;
Ytest = Y(testset); Xtest = X(testset);
Ytrain = Y(trainset); Xtrain = X(trainset);
t = classregtree(Xtrain, Ytrain, 'method', 'classification');
% prune for best tree

[c,s,n,best] = test(t, 'test', Xtrain, Ytrain);
tmin = prune(t, 'level', best);
optPredVals = eval(tmin, Xtest);
optPredVals = str2num(cell2mat(optPredVals));
opterr = mean(abs(optPredVals - Ytest));

% no pruning
PredVals = eval(t, Xtest);
PredVals = str2num(cell2mat(PredVals));
err = mean(abs(PredVals - Ytest));


%% all together
% now have this working some, time to cross validate it
load spam.data.txt
X = spam_data(:, 1:57);
Y = spam_data(:, 58);
folds = 10;
indices = cvpartition(length(Y), 'k', folds);
err = zeros(10, 1);
opterr = zeros(10, 1);
for i = 1:10
    % get training and testing
    testset = test(indices,i); trainset = ~testset;
    Ytest = Y(testset); Xtest = X(testset);
    Ytrain = Y(trainset); Xtrain = X(trainset);
    t = classregtree(Xtrain, Ytrain, 'method', 'classification');
    % prune for best tree
    
    [c,s,n,best] = test(t, 'test', Xtrain, Ytrain);
    tmin = prune(t, 'level', best);
    optPredVals = eval(tmin, Xtest);
    optPredVals = str2num(cell2mat(optPredVals));
    opterr(i) = mean(abs(optPredVals - Ytest));
    
    % no pruning
    PredVals = eval(t, Xtest);
    PredVals = str2num(cell2mat(PredVals));
    err(i) = mean(abs(PredVals - Ytest));
end
mean(err)
mean(opterr)