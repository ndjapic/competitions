program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort #sliding #window
uses
	Generics.Collections, Generics.Defaults, Math;
var
	n, m, i, x, l, r, ans: int32;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, m);

	a := TList<int32>.Create;
	for i := 1 to n do begin
		read(x);
		a.Add(x);
		a.Exchange(i-1, Random(i));
	end;
	readln;
	a.Sort;

	ans := 0;
	r := 0;
	for l := 0 to n-1 do begin
		while (r < n) and (a[r] < a[l] + m) do inc(r);
		ans := max(ans, r-l);
	end;

	writeln(ans);
	a.Free;
end.
