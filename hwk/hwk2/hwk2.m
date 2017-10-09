% load data
load spam.data.txt
X = spam_data(:, 1:57);
Y = spam_data(:, 58);

% classification tree
t = classregtree(X, Y, 'method', 'classification');
view(t)
% total nodes in tree
numnodes(t)

PredVals = eval(t,X);
PredVals = str2num(cell2mat(PredVals));
pct = mean(abs(PredVals - Y));