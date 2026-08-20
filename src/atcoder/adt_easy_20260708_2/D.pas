program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	generics.collections,
	generics.defaults, math;
const
	NN = 32;
var
	n, i: int8;
	t: array [1 .. NN] of int16;
	p: tlist<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function cmp(constref l, r: int8): int32;
begin
	result := CompareValue(t[l], t[r]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);
	p := tlist<int8>.create;

	for i := 1 to n do begin
		read(t[i]);
		p.add(i);
		p.exchange(i-1, random(i));
	end;
	readln;
	p.sort(TComparer<int8>.construct(cmp));

	writeln(p[0], ' ', p[1], ' ', p[2]);

	p.free;
end.
