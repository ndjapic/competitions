program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10;
	mm = 200 * 1000;
var
	n, i: int8;
	m, j: int32;
	ch: char;
	a, b: array [1 .. nn] of int8;
	s: array [1 .. mm] of string;
	active: array [1 .. nn, 'a' .. 'z'] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do begin
		readln(a[i], b[i]);
		for ch := 'a' to 'z' do active[i, ch] := false;
	end;

	readln(m);
	for j := 1 to m do begin
		readln(s[j]);
		for i := 1 to n do
			if length(s[j]) = a[i] then active[i, s[j][b[i]]] := true;
	end;

	for j := 1 to m do begin
		if length(s[j]) <> n then
			writeln('No')
		else begin
			i := 1;
			while (i <= n) and active[i, s[j][i]] do inc(i);

			if (i > n) then
				writeln('Yes')
			else
				writeln('No');
		end;
	end;
end.
