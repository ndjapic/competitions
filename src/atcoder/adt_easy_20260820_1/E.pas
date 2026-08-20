program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort
uses
	generics.collections, generics.defaults, math;
const
	NN = 200 * 1000;
var
	n, i, o, ansa, ansb: int32;
	x, y, sa, sb: int64;
	a, b: array [1 .. NN] of int64;
	p: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareSweet(constref l, r: int32): int32;
begin
	result := CompareValue(a[r], a[l]);
	if result = 0 then
		result := CompareValue(b[r], b[l]);
end;

function CompareSalt(constref l, r: int32): int32;
begin
	result := CompareValue(b[r], b[l]);
	if result = 0 then
		result := CompareValue(a[r], a[l]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, x, y);

	for i := 1 to n do read(a[i]);
	readln;

	for i := 1 to n do read(b[i]);
	readln;

	p := tlist<int32>.create;
	for i := 1 to n do begin
		p.add(i);
		p.exchange(i-1, random(i));
	end;

	p.sort(tcomparer<int32>.construct(CompareSweet));
	sa := 0;
	sb := 0;
	o := 0;
	while (o < n) and (sa <= x) and (sb <= y) do begin
		i := p[o];
		inc(sa, a[i]);
		inc(sb, b[i]);
		inc(o);
	end;
	ansa := o;

	p.sort(tcomparer<int32>.construct(CompareSalt));
	sa := 0;
	sb := 0;
	o := 0;
	while (o < n) and (sa <= x) and (sb <= y) do begin
		i := p[o];
		inc(sa, a[i]);
		inc(sb, b[i]);
		inc(o);
	end;
	ansb := o;

	writeln(min(ansa, ansb));
	p.free;
end.
