program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, i, k: int32;
	a, ans: array [1 .. NN] of int32;
	called: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;
begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do called[i] := false;

	for i := 1 to n do begin
		read(a[i]);
		if not called[i] then called[a[i]] := true;
	end;
	readln;

	k := 0;
	for i := 1 to n do
		if not called[i] then begin
			inc(k);
			ans[k] := i;
		end;

	writeln(k);
	for i := 1 to k-1 do write(ans[i], ' ');
	if k > 0 then writeln(ans[k]);
end.
