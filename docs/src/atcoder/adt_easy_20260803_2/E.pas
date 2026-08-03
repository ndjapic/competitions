program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 10 * 1000;
	MM = 7;
var
	n, m, i, j: int32;
	ans: boolean;
	b: array [1 .. NN, 1 .. MM] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	ans := true;
	for i := 1 to n do begin
		for j := 1 to m do begin
			read(b[i, j]);
			if ans and (i > 1) then ans := b[i, j] - b[i-1, j] = MM;
			if ans and (j > 1) then ans := b[i, j] - b[i, j-1] = 1;
			if ans and (j = m) then ans := (b[i, m] - 1) div 7 = (b[i, 1] - 1) div 7;
		end;
		readln;
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
