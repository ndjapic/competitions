program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections;
var
	n, i, x, k, l, r: int32;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	a := TList<int32>.Create;
	a.Capacity := n;

	for i := 1 to n do begin
		Read(x);
		a.Add(x);
	end;
	ReadLn;
	a.Sort;

	k := 0;
	r := n;
	for l := n-1 downto 0 do
		if (l = 0) or (a[l-1] < a[l]) then begin
			writeln(r-l);
			r := l;
			inc(k);
		end;

	while k < n do begin
		writeln(0);
		inc(k);
	end;

	a.Free;
end.
