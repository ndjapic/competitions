program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bisect
const
	NN = 300 * 1000;
var
	n, i, l, r, m, c, x: int32;
	a: array [1 .. NN] of int32;
	seen: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	l := 0;
	r := n+1;

	while r-l > 1 do begin
		m := (r+l) div 2;

		for x := 1 to m do seen[x] := false;

		c := 0;
		for i := 1 to n do begin
			x := a[i];
			if (x <= m) and not seen[x] then
				seen[x] := true
			else
				inc(c);
		end;

		x := 1;
		while (x <= m) and (c >= 0) do begin
			if not seen[x] then dec(c, 2);
			inc(x);
		end;

		if c >= 0 then
			l := m
		else
			r := m;
	end;

	writeln(l);
end.
