program C;
{$MODE DELPHI}
uses
	math, Generics.Collections, Generics.Defaults;
var
	notc, tci, n, i, ai, l, r, ans: int32;
	a: TList<int32>;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		a := TList<int32>.Create;

		for i := 1 to n do begin
			read(ai);
			a.Add(ai);
		end;
		readln;
		a.Sort;

		ans := 1;
		l := 0;
		for r := 1 to n-1 do begin
			if a[r] - a[r-1] > 1 then l := r;
			ans := max(ans, a[r] - a[l] + 1);
		end;

		writeln(ans);
		a.Free;

	end;
end.
