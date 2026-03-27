program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, Math;
var
	n, t, p, i, elm, ans: int8;
	l: TList<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	l := TList<int8>.Create;

	readln(n, t, p);

	for i := 0 to n-1 do begin
		read(elm);
		l.Add(elm);
	end;
	readln;
	l.Sort;

	ans := max(t - l[n-p], 0);
	writeln(ans);

	l.Free;
end.
