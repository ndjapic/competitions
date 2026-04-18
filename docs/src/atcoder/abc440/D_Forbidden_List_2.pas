program D_Forbidden_List_2;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 300 * 1000;
var
	n, q, i, ai, x, y, l, r, m, bx, bm: int32;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1..65536] of Char;

function bisect(x: int32): int32;
var
	l, r, m: int32;
begin
	l := -1;
	r := n;

	while r-l > 1 do begin
		m := l + (r-l) div 2;
		if a[m] <= x then
			l := m
		else
			r := m;
	end;

	Result := r;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, q);

	a := TList<int32>.Create;
	for i := 1 to n do begin
		read(ai);
		a.Add(ai);
		a.Exchange(i-1, random(i));
	end;
	a.Sort;

	for i := 1 to q do begin
		readln(x, y);
		bx := bisect(x-1);

		l := x-1+y-1;
		r := x-1+y+n;
		while r-l > 1 do begin
			m := l + (r-l) div 2;
			bm := bisect(m);
			if bm - bx + y > m-x+1 then
				l := m
			else
				r := m;
		end;
		writeln(r);
	end;

	a.Free;
end.
