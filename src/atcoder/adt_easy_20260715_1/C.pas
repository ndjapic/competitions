program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100 * 1000;
var
	n, i, x, ans: int32;
	a: array [1 .. NN] of int32;
	know: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	for i := 1 to n do begin
		read(a[i]);
		know[i] := false;
	end;
	readln;

	ans := 0;
	i := x;
	while not know[i] do begin
		know[i] := true;
		inc(ans);
		i := a[i];
	end;

	writeln(ans);
end.
