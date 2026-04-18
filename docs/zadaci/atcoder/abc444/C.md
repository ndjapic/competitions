# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}
uses
	Generics.Collections, Generics.Defaults;
var
	n, i, x, l, r: int32;
	a: TList<int32>;

begin
	readln(n);

	a := TList<int32>.Create;
	for i := 1 to n do begin
		read(x);
		a.Add(x);
	end;
	readln;
	a.Sort;

	l := 0;
	r := n-1;
	x := a[r];

	while (r >= 0) and (a[r] = x) do dec(r);

	while (l < r) and (a[l] + a[r] = x) do begin
		inc(l);
		dec(r);
	end;
	if r < l then write(x, ' ');

	l := 0;
	r := n-1;
	x := a[l] + a[r];

	while (l < r) and (a[l] + a[r] = x) do begin
		inc(l);
		dec(r);
	end;
	if r < l then write(x);

	writeln;
	a.Free;
end.

```
