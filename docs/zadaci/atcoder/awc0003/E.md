# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 16;
var
	n, m, i, j: Int8;
	x: int32;
	d: int64;
	w, c: TList<Int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dfs(i: int8): boolean;
var
	l, r, h: int8;
begin
	result := i < 0;
	if not result then begin

		l := -1;
		r := n;
		while r-l > 1 do begin
			h := (r+l) div 2;
			if (h < 0) or (c[h] < w[i]) then
				l := h
			else
				r := h;
		end;

		while not result and (r < n) do begin
			if (r = 0) or (c[r] > c[r-1]) then begin

				c[r] := c[r] - w[i];
				l := r;
				while (l > 0) and (c[l] < c[l-1]) do begin
					c.Exchange(l, l-1);
					dec(l);
				end;

				result := dfs(i-1);

				while l < r do begin
					c.Exchange(l, l+1);
					inc(l);
				end;
				c[r] := c[r] + w[i];

			end;
			inc(r);
		end;

	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	read(n, m);

	w := TList<Int32>.Create;
	for i := 0 to n-1 do begin
		read(x);
		w.Add(x);
	end;
	w.Sort;

	c := TList<Int32>.Create;
	for j := 0 to m-1 do begin
		read(x);
		c.Add(x);
	end;

	for j := m to n-1 do c.Add(0);
	c.Sort;

	if m > n then
		for j := n downto 1 do
			c[n-j] := c[m-j];

	d := 0;
	j := 1;
	while (d >= 0) and (j <= n) do begin
		inc(d, c[n-j] - w[n-j]);
		inc(j);
	end;

	if (d >= 0) and dfs(n-1) then
		writeln('Yes')
	else
		writeln('No');

	w.Free;
	c.Free;
end.

```
