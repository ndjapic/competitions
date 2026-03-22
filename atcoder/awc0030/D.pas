program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, k, c1: int32;
	t, c, ainv: array [1 .. nn] of int32;
	a: array [0 .. nn] of int32;
	visited: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		read(t[i]);
		c[i] := 0;
		visited[i] := false;
	end;
	readln;

	for i := 1 to n do begin

		if not visited[i] then begin
			k:= 0;
			a[k] := i;

			while not visited[a[k]] do begin
				ainv[a[k]] := k;
				visited[a[k]] := true;
				a[k+1] := t[a[k]];
				inc(k);
			end;

			if c[a[k]] = 0 then
				c1 := k - ainv[a[k]]
			else
				c1 := c[a[k]];

			while k > 0 do begin
				dec(k);
				c[a[k]] := c1;
			end;
		end;

		write(c[i]);
		if i < n then write(' ');
	end;
	writeln;
end.
