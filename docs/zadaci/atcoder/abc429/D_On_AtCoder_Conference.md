# Задатак: D_On_AtCoder_Conference.pas

```pascal
program D_On_AtCoder_Conference;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections;
const
	nn = 500 * 1000;
var
	n, i, c, l, r: int32;
	x, m, d, ans: int64;
	a: TList<int64>;

function get_a(i: int32): int64;
begin
	get_a := a[i mod n] + i div n * m;
end;

begin
	randomize;
	a := TList<int64>.Create;
	try

		readln(n, m, c);

		for i := 0 to n-1 do begin
			read(x);
			a.Add(x);
			a.Exchange(i, random(i+1));
		end;
		readln;
		a.Sort;

		l := 0;
		r := c;
		ans := 0;
		while get_a(l) < m do begin
			inc(l);
			while (r-l+1 < c) or (get_a(r) = get_a(r+1)) do inc(r);
			d := get_a(l) - get_a(l-1);
			inc(ans, d * (r-l+1));
		end;

		writeln(ans);

	finally
		a.Free;
	end;
end.

```
