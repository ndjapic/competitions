program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, i, j, k, a: int32;
	called: array [1 .. NN] of boolean;
	x: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do called[i] := false;

	for i := 1 to n do begin
		read(a);
		if not called[i] then called[a] := true;
	end;
	readln;

	k := 0;
	for i := 1 to n do
		if not called[i] then begin
			inc(k);
			x[k] := i;
		end;

	writeln(k);
	for j := 1 to k-1 do write(x[j], ' ');
	if k > 0 then writeln(x[k]);
end.
