program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, m, i, j, k, f: int8;
	ans: boolean;
	p:  array [1 .. nn] of int32;
	c:  array [1 .. nn] of int8;
	a:  array [1 .. nn, 1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		for f := 1 to m do a[i, f] := false;
		read(p[i], c[i]);
		for k := 1 to c[i] do begin
			read(f);
			a[i, f] := true;
		end;
		readln;
	end;

	ans := false;
	for i := 1 to n do
		for j := 1 to n do
			if (i <> j) and (p[i] >= p[j]) and not ans then begin
				f := 1;
				while (f <= m) and not (a[i, f] and not a[j, f]) do inc(f);
				if f > m then
					ans := (p[i] > p[j]) or (c[i] < c[j]);
			end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
