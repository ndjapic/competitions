program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, k, l, r: int8;
	ans: boolean;
	x: array [1 .. NN] of int8;
	friends: array [1 .. NN, 1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for r := 1 to n do
		for l := 1 to r-1 do friends[l, r] := false;

	for i := 1 to m do begin
		read(k);
		for r := 1 to k do begin
			read(x[r]);
			for l := 1 to r-1 do friends[x[l], x[r]] := true;
		end;
		readln;
	end;

	ans := true;
	for r := 1 to n do
		for l := 1 to r-1 do ans := ans and friends[l, r];

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
