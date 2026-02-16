program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;

var
	n, m, i: Int8;
	w, c: TList<Int32>;
	x: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dfs(i: int8): boolean;
var
	l, r: int8;
begin
	if i < 0 then
		result := True
	else begin

		result := False;
		r := m;
		while not result and (c[r] >= w[i]) do begin
			if (c[r] > c[r-1]) then begin

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

			end;
			dec(r);
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

	if dfs(n-1) then
		writeln('Yes')
	else
		writeln('No');

	w.Free;
	c.Free;
end.
