program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, i, i1: int32;
	x: int64;
	found: boolean;
	w: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	w[0] := 0;
	for i := 1 to n do begin
		read(w[i]);
		inc(w[i], w[i-1]);
	end;

	i1 := 0;
	found := false;
	while not found and (i1 < n) do begin
		inc(i1);
		if w[n] mod w[i1] = 0 then begin
			x := w[i1];
			for i := i1 to n-1 do
				if w[i] = x then inc(x, w[i1]);
			found := x = w[n];
		end;
	end;

	writeln(w[n] div w[i1]);
end.
