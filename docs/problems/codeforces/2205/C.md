# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 3000;
var
	notc, tci, n, m, i, j, o, x, y: int32;
	sz, p, p2, q: array [1 .. nn] of int32;
	a: array [1 .. nn] of array of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function compare(l, r: int32): int32;
var
	i, j: int32;
begin
	i := sz[l] - 1;
	j := sz[r] - 1;
	while (i >= 0) and (j >= 0) and (a[l][i] = a[r][j]) do begin
		dec(i);
		dec(j);
	end;

	if i < 0 then begin
		if j < 0 then
			compare := 0
		else
			compare := a[l][sz[l] - 1] - a[r][j];
	end else begin
		if j < 0 then
			compare := a[l][i] - a[r][sz[r] - 1]
		else
			compare := a[l][i] - a[r][j];
	end;
end;

procedure merge(l, m, r: int32);
var
	i, j, k: int32;
begin
	i := l;
	j := m;
	for k := l to r-1 do
		if (j = r) or (i < m) and (
			compare(p[i], p[j]) <= 0
		) then begin
			p2[k] := p[i];
			inc(i);
		end else begin
			p2[k] := p[j];
			inc(j);
		end;
	for k := l to r-1 do p[k] := p2[k];
end;

procedure msort(l, r: int32);
var
	m: int32;
begin
	if r-l > 1 then begin
		m := (l+r) div 2;
		msort(l, m);
		msort(m, r);
		merge(l, m, r);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do begin
			read(sz[i]);
			setlength(a[i], sz[i]);
			j := 0;

			while j < sz[i] do begin

				read(a[i][j]);
				x := j-1;
				while (x >= 0) and (a[i][j] <> a[i][x]) do dec(x);

				if x < 0 then
					inc(j)
				else begin
					while x < j do begin
						a[i][x] := a[i][x+1];
						inc(x);
					end;
					dec(sz[i]);
				end;

			end;
			readln;

			setlength(a[i], sz[i]);
			p[i] := i;
		end;

		msort(1, n+1);

		m := 0;
		for o := n downto 1 do begin
			i := p[o];
			for j := 0 to sz[i] - 1 do begin
				x := 1;
				while (x <= m) and (q[x] <> a[i][j]) do inc(x);
				m := max(m, x);
				for y := x downto 2 do q[y] := q[y-1];
				q[1] := a[i][j];
			end;
		end;

		for x := 1 to m-1 do write(q[x], ' ');
		writeln(q[m]);

	end;
end.

```
