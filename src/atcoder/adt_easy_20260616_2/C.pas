program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, j, c, f: int8;
	found: boolean;
	p: array [1 .. NN] of int32;
	fun: array [1 .. NN, 0 .. 1] of int64;
	superior: array [1 .. NN, 1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		read(p[i]);
		fun[i, 0] := 0;
		fun[i, 1] := 0;

		read(c);
		while c > 0 do begin
			dec(c);
			read(f);
			inc(fun[i, f mod 2], int64(1) shl (f div 2));
		end;
		readln;
	end;

	for i := 1 to n do
		for j := 1 to n do
			if i = j then
				superior[j, i] := false
			else if fun[i, 0] and fun[j, 0] <> fun[i, 0] then
				superior[j, i] := false
			else if fun[i, 1] and fun[j, 1] <> fun[i, 1] then
				superior[j, i] := false
			else
				superior[j, i] := true;

	found := false;
	for i := 1 to n do
		for j := 1 to n do
			if not found then
				found := (p[i] >= p[j]) and superior[j, i] and (
					(p[i] > p[j]) or not superior[i, j]);

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
