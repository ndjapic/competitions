# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	hh = 100;
var
	h, w, x, y, i, j, k, c: int32;
	t: string;
	s: array [1 .. hh] of string;
	visited: array [1 .. hh, 1 .. hh] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, x, y);

	for i := 1 to h do begin
		readln(s[i]);
		for j := 1 to w do visited[i, j] := false;
	end;
	visited[x, y] := true;

	readln(t);
	for k := 1 to length(t) do begin
		case t[k] of
			'U': if s[x-1][y] <> '#' then dec(x);
			'D': if s[x+1][y] <> '#' then inc(x);
			'L': if s[x][y-1] <> '#' then dec(y);
			'R': if s[x][y+1] <> '#' then inc(y);
		end;
		visited[x, y] := true;
	end;

	c := 0;
	for i := 1 to h do
		for j := 1 to w do
			if (s[i][j] = '@') and visited[i, j] then inc(c);

	writeln(x, ' ', y, ' ', c);
end.

```
