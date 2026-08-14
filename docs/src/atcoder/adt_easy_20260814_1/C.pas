program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 300;
	WW = 1000 * 1000;
var
	n, w, i, j, k, x, y, z, ans: int32;
	a: array [1 .. NN] of int32;
	seen: array [1 .. WW] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, w);

	for x := 1 to w do seen[x] := false;

	for i := 1 to n do begin
		read(a[i]);
		x := a[i];
		if x <= w then seen[x] := true;

		for j := 1 to i-1 do begin
			y := x + a[j];
			if y <= w then seen[y] := true;

			for k := 1 to j-1 do begin
				z := y + a[k];
				if z <= w then seen[z] := true;
			end;
		end;
	end;
	readln;

	ans := 0;
	for x := 1 to w do
		if seen[x] then inc(ans);

	writeln(ans);
end.
