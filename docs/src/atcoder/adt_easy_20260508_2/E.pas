program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #merge
const
	nn = 100 * 1000;
var
	n, m, i, j, k: int32;
	a, b: array [1 .. nn] of int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(a[i]); readln;
	for j := 1 to m do read(b[j]); readln;

	k := 0;
	i := 1;
	j := 1;
	setlength(s, n+m);

	while (i <= n) or (j <= m) do begin
		inc(k);
		if (j > m) or (i <= n) and (a[i] < b[j]) then begin
			inc(i);
			s[k] := 'a';
		end else begin
			inc(j);
			s[k] := 'b';
		end;
	end;

	for k := 1 to n+m do
		if s[k] = 'a' then write(k, ' ');
	writeln;

	for k := 1 to n+m do
		if s[k] = 'b' then write(k, ' ');
	writeln;
end.
