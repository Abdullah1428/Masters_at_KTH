% some commands to remember

% c(filename).

% P = spawn(fileName,function,args).

% register("name",P).

% for distributed computing

% erl -name foo@130.229.153.23 -setcookie secret
% erl -name bar@130.229.153.23 -setcookie secret

% P = spawn(wait,hello,[]).
% register(foo,P).

% execute this in other shell with bar and you will see output in foo
% {foo,'foo@130.229.153.23'} ! "a message from the bar side".