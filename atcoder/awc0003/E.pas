program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 16;
var
	n, m, i: Int8;
	x: int32;
	w, c: TList<Int32>;
	c0: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dfs(i: int8): boolean;
var
	l, r, h: int8;
begin
	result := i < 0;
	if not result then begin

		l := 0;
		r := m+1;
		while r-l > 1 do begin
			h := (r+l) div 2;
			if c[h] >= w[i] then
				r := h
			else
				l := h;
		end;

		while not result and (r <= m) do begin
			if c[r] > c[r-1] then begin

				c[r] := c[r] - w[i];
				l := r;
				while c[l] < c[l-1] do begin
					c.Exchange(l, l-1);
					dec(l);
				end;

				result := dfs(i-1);

				while l < r do begin
					c.Exchange(l, l+1);
					inc(l);
				end;
				c[r] := c[r] + w[i];

				if not result then begin
					if c[r] = w[i] then
						r := m+1
					else if c[r] = c0[r] then
						while (r < m) and (c0[r] = c0[r+1]) do inc(r);
				end;

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
	c.Add(-1);
	for i := 1 to m do begin
		read(x);
		c.Add(x);
	end;
	c.Sort;
	for i := 0 to m do c0[i] := c[i];

	if dfs(n-1) then
		writeln('Yes')
	else
		writeln('No');

	w.Free;
	c.Free;
end.
