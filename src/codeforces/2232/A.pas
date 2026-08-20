program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
var
	notc, tci, n, i, ai, l, r, ans: int32;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		a := TList<int32>.Create;
		a.Capacity := n;

		for i := 0 to n-1 do begin
			Read(ai);
			a.Add(ai);
			a.Exchange(i, Random(i+1));
		end;
		readln;
		a.Sort;

		ans := n;
		l := 0;
		for r := 0 to n-1 do begin
			while a[l] < a[r] do inc(l);
			ans := min(ans, max(l, n-1-r));
		end;
		writeln(ans);

		a.Free;

	end;
end.
