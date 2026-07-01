program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	Generics.Defaults, Generics.Collections, Math;
const
	NN = 100 * 1000;
var
	n, i, j: int32;
	ind: array [1 .. NN] of tlist<int32>;
	f: array [0 .. NN] of int32;
	p: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CustomCompare(constref l, r: int32): Integer;
begin
	Result := CompareValue(f[l], f[r]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do ind[i] := tlist<int32>.create;

	for j := 1 to 3*n do begin
		read(i);
		ind[i].add(j);
	end;
	readln;

	p := tlist<int32>.create;
	f[0] := 0;
	p.add(0);

	for i := 1 to n do begin
		f[i] := ind[i][1];
		p.add(i);
	end;

	p.Sort(TComparer<int32>.Construct(CustomCompare));

	for i := 1 to n-1 do write(p[i], ' ');
	writeln(p[n]);

	for i := 1 to n do ind[i].free;
	p.free;
end.
