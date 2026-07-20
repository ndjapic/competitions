program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
const
	NN = 300 * 1000;
var
	n, k, i: int32;
	ans: int64;
	a, b: array [1 .. NN] of int32;
	d: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, k);

	for i := 1 to n do read(a[i]);
	readln;

	for i := 1 to n do read(b[i]);
	readln;

	d := TList<int32>.Create;
	ans := 0;
	for i := 1 to n do begin
		inc(ans, a[i]);
		d.Add(a[i] - b[i]);
		d.Exchange(i-1, random(i));
	end;
	d.Sort;

	for i := 0 to k-1 do dec(ans, d[i]);
	writeln(ans);
	d.Free;
end.
