program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	strutils;
var
	n, m, c, w, i, j: int8;
	s, t: string;
	found: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);
	i := pos(' ', s);
	m := n-i;
	n := i-1;
	t := rightstr(s, m);
	s := leftstr(s, n);

	found := false;
	w := 1;
	while not found and (w < n) do begin
		c := 1;
		while not found and (c <= w) do begin
			i := c;
			found := true;

			for j := 1 to m do begin
				if i <= n then
					found := found and (t[j] = s[i])
				else
					found := false;
				inc(i, w);
			end;

			inc(c);
		end;
		inc(w);
	end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
