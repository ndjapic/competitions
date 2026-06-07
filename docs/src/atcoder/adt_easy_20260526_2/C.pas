program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	NN = 200 * 1000;
var
	n, q, i, j, k, aij: int32;
	l: array [1 .. NN] of int32;
	a: array [1 .. NN] of tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do begin
		read(l[i]);
		a[i] := tlist<int32>.create;
		for j := 0 to l[i] - 1 do begin
			read(aij);
			a[i].add(aij);
		end;
		readln;
	end;

	for k := 1 to q do begin
		readln(i, j);
		writeln(a[i][j-1]);
	end;

	for i := 1 to n do a[i].free;
end.
