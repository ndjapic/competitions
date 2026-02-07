program D_Kadomatsu_Subsequence;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
var
	n, i, x, d3, d5{, d7}: int32;
	ans: int64;
	a: TList<int32>;

function bisect(x: int32): int32;
var
	l, r, m: int32;
begin
	l := -1;
	r := n;
	while r-l > 1 do begin
		m := (l+r) div 2;
		if a[m] <= x then
			l := m
		else
			r := m;
	end;
	bisect := r;
end;

begin
	randomize;
	a := TList<int32>.Create;
	try

		readln(n);

		for i := 0 to n-1 do begin
			read(x);
			a.Add(x);
			a.Exchange(i, random(i+1));
		end;
		readln;

		a.Sort;

		ans := 0;
		for i := 0 to n-1 do
			if a[i] mod 7 = 0 then begin
				x := a[i] div 7;
				d3 := bisect(3*x) - bisect(3*x-1);
				d5 := bisect(5*x) - bisect(5*x-1);
				{d7 := bisect(7*x) - bisect(7*x-1);}
				inc(ans, int64(d3) * d5);
				writeln(a[i], ' ', d3, ' ', d5, ' ', ans);
			end;

		writeln(ans);

	finally
		a.Free;
	end;
end.
