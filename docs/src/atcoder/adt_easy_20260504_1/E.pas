program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, Math;
const
	nn = 200 * 1000;
var
	n, k, i, elm, l, r, ans: int32;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	a := TList<int32>.Create;
	for i := 0 to n-1 do begin
		read(elm);
		a.Add(elm);
		a.Exchange(i, random(i+1));
	end;
	readln;
	a.Sort;

	l := 0;
	r := n-1;
	dec(r, k);

	ans := a[r] - a[l];
	while r < n do begin
		ans := min(ans, a[r] - a[l]);
		inc(l);
		inc(r);
	end;

	writeln(ans);
	a.Free;
end.
