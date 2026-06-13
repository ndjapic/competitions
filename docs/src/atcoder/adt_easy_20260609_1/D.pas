program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 300;
	WW = 1000 * 1000;
var
	n, w, i, j, k, x, ans: int32;
	a: array [0 .. NN] of int32;
	good: array [1 .. WW] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, w);

	for x := 0 to w do good[x] := false;

	a[0] := 0;
	for i := 1 to n do begin
		read(a[i]);
		x := a[i];
		good[x] := true;
		for j := 0 to i-1 do begin
			x := a[i] + a[j];
			if x <= w then good[x] := true;
			for k := 0 to j-1 do begin
				x := a[i] + a[j] + a[k];
				if x <= w then good[x] := true;
			end;
		end;
	end;
	readln;

	ans := 0;
	for x := 1 to w do
		if good[x] then inc(ans);
	writeln(ans);
end.
