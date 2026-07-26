program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #counter
uses
	Generics.Collections, Math;
const
	NN = 300 * 1000;
var
	n, i, i0, c: int32;
	a: array [1 .. NN] of int32;
	d: TDictionary<int32, int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	d := TDictionary<int32, int32>.Create;

	readln(n);

	for i := 1 to n do begin
		read(a[i]);
		if not d.TryGetValue(a[i], c) then c := 0;
		d.AddOrSetValue(a[i], c+1);
	end;
	readln;

	i0 := -1;
	for i := 1 to n do
		if d.TryGetValue(a[i], c) and (c = 1) and ((i0 = -1) or (a[i0] < a[i])) then
			i0 := i;

	writeln(i0);
	d.Free;
end.
